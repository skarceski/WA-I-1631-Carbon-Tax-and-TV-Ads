
# this script conducts an aerial match that aggregates precinct-level voting data to census tracts

library(sf)
library(tidyverse)
library(tidycensus)

# get the precinct voting results and the geometry data for each precinct (all precincts)

########################################
# 2018
########################################

precinct_2018 <- read_csv("20181106_AllStatePrecincts_2019.csv")

precinct_2018 <- precinct_2018 %>%
    # create a precinct ID that can match
    mutate(precinct_code = str_pad(PrecinctCode, 8, side = "left", pad = "0"),
           # match the precinct ID from the shapefile
           precinct_id = paste0(CountyCode, precinct_code),
           race = case_when(str_detect(Race, "U.S. Senator") ~ "dem_senate",
                            str_detect(Race, "1631") ~ "yes_1631",
                            str_detect(Race, "U.S. Representative") ~ "dem_house")) %>%
    filter(PrecinctName != "Total",
           race %in% c("dem_senate", "yes_1631", "dem_house")) %>%
    select(-c(PrecinctCode, precinct_code, PrecinctName, Race)) %>%
    group_by(race, precinct_id) %>%
    mutate(total_votes = sum(Votes))

# create dem
dem_list <- unique(precinct_2018$Candidate)[c(1,4,6,7,10,11,13,15,17,20,21,23)]

# limit to the list
precinct_2018 <- precinct_2018 %>%
    filter(Candidate %in% dem_list) %>%
    ungroup()

# list of dem house candidates (including A. but not S. Smith)
house_dems <- dem_list[-c(1,12)]

# create the precinct vote measures for interpolation
precinct_2018 <- precinct_2018 %>% distinct(precinct_id) %>%
    left_join(precinct_2018 %>% filter(Candidate == "Maria Cantwell") %>%
                  select(-c(race, Candidate)) %>%
                  rename(dem_senate = Votes, tot_senate = total_votes)) %>%
    left_join(precinct_2018 %>% filter(Candidate == "Yes") %>%
                  select(-c(race, Candidate, CountyCode)) %>%
                  rename(yes_1631 = Votes, tot_1631 = total_votes)) %>%
    left_join(precinct_2018 %>% filter(Candidate %in% house_dems) %>%
                  select(-c(race, Candidate, CountyCode)) %>%
                  rename(dem_house = Votes, tot_house = total_votes)) %>%
    rename(county_code = CountyCode)

# since we're missing the precincts from King and Kitsap counties, we have to
# add them in manually (there are a total of 7,317 precincts in the state)

# King county results

king_match <- read_csv("king_precinct_match_2018.csv") %>%
    # create a precinct ID that can match
    mutate(precinct_no = str_pad(king_precinct_no, 8, side = "left", pad = "0"),
           # the precinct ID from the shapefile
           precinct_id = paste0(county_code, precinct_no))  %>%
    select(precinct_id, precinct_name)

king_results <- read_csv("king_county_2018.csv") %>%
    mutate(race = case_when(str_detect(Race, "US Representative") ~ "house",
                            str_detect(Race, "US Senator") ~ "senate",
                            str_detect(Race, "1631") ~ "i_1631"),
           count_type = case_when(CounterType %in% house_dems ~ "house_dem",
                                  CounterType == "Times Counted" ~ "total_votes",
                                  CounterType == "Maria Cantwell" ~ "senate_dem",
                                  CounterType == "Yes" ~ "yes_1631")) %>%
    rename(count = SumOfCount, precinct_name = Precinct) %>%
    filter(!(is.na(count_type))) %>%
    select(precinct_name, race, count_type, count) %>%
    pivot_wider(names_from = "count_type", values_from = "count")

king_results <- king_results %>%
    group_by(precinct_name) %>%
    summarize(total_votes = mean(total_votes),
              dem_house = mean(house_dem, na.rm = T),
              dem_senate = mean(senate_dem, na.rm = T),
              yes_1631 = mean(yes_1631, na.rm = T)) %>%
    mutate(tot_house = total_votes,
           tot_senate = total_votes,
           tot_1631 = total_votes,
           county_code = "KI") %>%
    select(precinct_name, county_code, dem_senate, tot_senate, yes_1631, tot_1631,
           dem_house, tot_house)

# join with the matching frame
king_results <- king_match %>%
    left_join(king_results) %>%
    select(-precinct_name) # drop precinct names

# kitsap county results

kitsap <- read_csv("20181106_KitsapPrecincts.csv") %>%
    mutate(race = case_when(str_detect(race, "U.S. Senator") ~ "senate",
                            str_detect(race, "U.S. Representative") ~ "house",
                            str_detect(race, "1631") ~ "i_1631"),
           precinct_code = precinct_code - 100000,
           # create a precinct ID that can match
           precinct_no = str_pad(precinct_code, 8, side = "left", pad = "0"),
           precinct_id = paste0(county_code, precinct_no)) %>%
    filter(!is.na(race),
           precinct_name != "Total") %>%
    select(precinct_id, county_code, race, candidate, votes)

kitsap_results <- kitsap %>%
    group_by(precinct_id, race) %>%
    summarize(total_votes = sum(votes)) %>%
    left_join(kitsap %>% filter(candidate %in% dem_list))

kitsap_precincts <- kitsap %>% distinct(precinct_id, county_code)

kitsap_results <- kitsap_precincts %>%
    left_join(kitsap_results %>% filter(race == "house") %>%
                  rename(tot_house = total_votes,
                         dem_house = votes) %>%
                  select(precinct_id, dem_house, tot_house)) %>%
    left_join(kitsap_results %>% filter(race == "senate") %>%
                  rename(tot_senate = total_votes,
                         dem_senate = votes) %>%
                  select(precinct_id, dem_senate, tot_senate)) %>%
    left_join(kitsap_results %>% filter(race == "i_1631") %>%
                  rename(tot_1631 = total_votes,
                         yes_1631 = votes) %>%
                  select(precinct_id, tot_1631, yes_1631)) %>%
    mutate(county_code = "KP")

# combine all the results into one master data frame

wa_results <- precinct_2018 %>%
    rbind(king_results) %>%
    rbind(kitsap_results)

# interpolation, areal style

# do the thing that Chuckles did to the shapefile

prec_shape_2018 <- st_read("2018Precincts_VERIFIED.shp", ### the 2018 precinct shape file
                        stringsAsFactors = F) %>%
    select(precinct_id = FullPrc, geometry)
prec_shape_2018 <- prec_shape_2018 %>%
   # st_transform(crs = 4326) %>% # we ain't transform'n
    ungroup() %>%
    filter(st_is_valid(.))

# plot(prec_shape_2018)

# create the dataframe containing the voting results and the precinct geometries

precinct_data <- prec_shape_2018 %>% full_join(wa_results)
precinct_data <- st_sf(precinct_data)
class(precinct_data)
# plot(precinct_data)

# match the media market with precincts (based on county)

precinct_data <- precinct_data %>%
    left_join(read_csv("media_market_counties.csv") %>%
                  select(media_market, county_code))

# match the media market ad count for yes and no ads

precinct_data <- precinct_data %>%
    left_join(read_csv("tv_spending_by_market_2018.csv") %>%
                  group_by(media_market) %>%
                  summarize(no_count = sum(no_count),
                            yes_count = sum(yes_count),
                            yn_ratio = yes_count/no_count,
                            ny_diff = no_count - yes_count,
                            ny_ldif = log(ny_diff)))

# ggplot(precinct_data, aes(fill = media_market)) +
#     geom_sf(aes(alpha = yes_1631), colour = "White", size = 0.01) +
#     theme_minimal()

# save it to a csv file

write_csv(precinct_data, "precinct_results_2018.csv")

# tidycensus - tract-level measures for WA state

library(tidycensus)

acs_tracts <- get_acs(geography = "tract", state = "WA", geometry = T,
                      variables = c(Med_Income = "B19013_001", Pop_Estimate = "B01001_001",
                                    # Race/ethnicity variables:
                                    Race_Total = "B02001_001",
                                    Race_White = "B02001_002", Race_Black = "B02001_003", Race_Native = "B02001_004",
                                    Race_Asian = "B02001_005", Race_HawPI = "B02001_006", Race_Other = "B02001_007",
                                    Race_2More = "B02001_008", Hisp_Total = "B03002_001", Hisp_Lat = "B03002_012",
                                    # education variables:
                                    Ed_Total = "B15003_001", Ed_Bach = "B15003_022", Ed_Mast = "B15003_023",
                                    Ed_Prof = "B15003_024", Ed_Doct = "B15003_025",
                                    #industry:
                                    Ind_Total = "C24070_001", Ind_AgMin = "C24070_002", Ind_Const = "C24070_003",
                                    Ind_Manuf = "C24070_004", Ind_Trans = "C24070_007",
                                    # age:
                                    Age_1 = "B01001_014", Age_2 = "B01001_015", Age_3 = "B01001_016",
                                    Age_4 = "B01001_017", Age_5 = "B01001_018", Age_6 = "B01001_019", Age_7 =  "B01001_020",
                                    Age_8 = "B01001_021", Age_9 = "B01001_022", Age_10 = "B01001_023", Age_11 = "B01001_024",
                                    Age_12 = "B01001_025", Age_13 = "B01001_038", Age_14 = "B01001_039", Age_15 = "B01001_040",
                                    Age_16 = "B01001_041", Age_17 = "B01001_042", Age_18 = "B01001_043", Age_19 = "B01001_044",
                                    Age_20 = "B01001_045", Age_21 = "B01001_046", Age_22 = "B01001_047", Age_23 = "B01001_048",
                                    Age_24 = "B01001_049",
                                    # misc. vars:
                                    Pov_Total = "C17002_001", Pov_Und.50 = "C17002_002", Pov_.50_100 = "C17002_003",
                                    Commute_Total = "B08301_001", Commute_Car = "B08301_002")) %>%
    select(-moe) %>% group_by(GEOID) %>% spread(variable, estimate)

# create new variables and ditch the useless ones

acs_tracts <- acs_tracts %>%
    mutate(over_40_count = Age_1 + Age_2 + Age_3 + Age_4 + Age_5 + Age_6 + Age_7 + Age_8 + Age_9 + Age_10 + Age_11 + Age_12 +
               Age_13 + Age_14 + Age_15 + Age_16 + Age_16 + Age_17 + Age_18 + Age_19 + Age_20 + Age_21 + Age_22 + Age_23 +
               + Age_24,
           Ed_4_plus = Ed_Bach + Ed_Mast + Ed_Prof + Ed_Doct,
           Pov_Und = Pov_Und.50 + Pov_.50_100) %>%
    select(-c(Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10, Age_11, Age_12,
              Age_13, Age_14, Age_15, Age_16, Age_16, Age_17, Age_18, Age_19, Age_20, Age_21, Age_22, Age_23,
              Age_24, Ed_Bach, Ed_Mast, Ed_Prof, Ed_Doct, Pov_Und.50, Pov_.50_100))

acs_tracts <- acs_tracts %>%
    mutate(race_black = 100*Race_Black/Race_Total,
           race_white = 100*Race_White/Race_Total,
           race_native = 100*Race_Native/Race_Total,
           race_asian = 100*Race_Asian/Race_Total,
           race_hawpi = 100*Race_HawPI/Race_Total,
           race_other = 100*Race_Other/Race_Total,
           race_2more = 100*Race_2More/Race_Total,
           hisp_lat = 100*Hisp_Lat/Hisp_Total,
           edu_bach = 100*Ed_4_plus/Ed_Total,
           ind_agmin = 100*Ind_AgMin/Ind_Total,
           ind_const = 100*Ind_Const/Ind_Total,
           ind_manuf = 100*Ind_Manuf/Ind_Total,
           ind_trans = 100*Ind_Trans/Ind_Total,
           under_pov = 100*Pov_Und/Pov_Total,
           commute_car = 100*Commute_Car/Commute_Total,
           over_40 = 100*over_40_count/Pop_Estimate) %>%
    select(GEOID, NAME, geometry, Med_Income, Pop_Estimate,
           race_black, race_white, race_native, race_asian, race_hawpi, race_other,
           race_2more, hisp_lat, edu_bach, ind_agmin, ind_const, ind_manuf, ind_trans,
           under_pov, commute_car, over_40) %>%
    rename(tract_id = GEOID, tract_name = NAME)

# craete county variable to match with code/media data

library(stringr)

test_tract = acs_tracts$tract_name[1]
test_county = str_split_fixed(str_split_fixed(test_tract,
                                              pattern = ", ",
                                              n = Inf)[1,2],
                              pattern = " County",
                              n = Inf)[1,1]

acs_tracts = acs_tracts %>%
    mutate(county = mapply(FUN = function(county)  str_split_fixed(str_split_fixed(county,
                                                                                   pattern = ", ",
                                                                                   n = Inf)[1,2],
                                                                   pattern = " County",
                                                                   n = Inf)[1,1],
                           tract_name))

acs_tracts <- acs_tracts %>% ungroup()

# aerial weighting with the "areal" package

library(areal)

# use this file with "Aerial_Match_Combined.R" - - - Census Data and Precinct Voting Data

names(precinct_data)
class(precinct_data)

acs_geometry <- acs_tracts %>% ungroup() %>% select(tract_id, geometry) %>%
    filter(st_is_valid(.))

# areal package validation function

ar_validate(source = precinct_data, target = acs_geometry,
            varList = c("v_cant", "base_cant", "v_1631", "base_1631"),
            method = "aw",
            verbose = T)

st_crs(precinct_data)
st_crs(acs_geometry)

precinct_data <- st_transform(precinct_data, crs = 26915)
precinct_data <- precinct_data %>% filter(st_is_valid(precinct_data))
acs_geometry <- st_transform(acs_geometry, crs = 26915)

ar_validate(source = precinct_data,
            target = acs_geometry,
            varList = c("dem_house", "tot_house",
                        "dem_senate", "tot_senate",
                        "yes_1631", "tot_1631"),
            method = "aw",
            verbose = T)

interp_results <- aw_interpolate(acs_geometry, tid = tract_id,
                                 source = precinct_data,
                                 sid = precinct_id,
                                 weight = "sum",
                                 output = "tibble",
                                 extensive = c("dem_house", "tot_house",
                                               "dem_senate", "tot_senate",
                                               "yes_1631", "tot_1631"))

## join the acs data and the interpolated data

acs_tracts <- left_join(acs_tracts, interp_results)

# create the new variable

acs_tracts <- acs_tracts %>%
    mutate(dem_house_p = 100*(dem_house/tot_house),
           dem_senate_p = 100*(dem_senate/tot_senate),
           yes_1631_p = 100*(yes_1631/tot_1631))

# add it to the acs_tracts data

acs_data <- acs_tracts %>%
    left_join(read_csv("media_market_counties.csv")) %>%
    left_join(read_csv("tv_spending_by_market_2018.csv") %>%
                  group_by(media_market) %>%
                  summarize(no_count = sum(no_count),
                            yes_count = sum(yes_count),
                            yn_ratio = yes_count/no_count,
                            ny_diff = no_count - yes_count,
                            ny_ldif = log(ny_diff)))

acs_data <- acs_data %>%
    mutate(density = Pop_Estimate/sf::st_area(.))

ggplot(acs_tracts, aes(density)) +
    geom_density() +
    theme_minimal()

density(acs_tracts$density, na.rm = T)

write_csv(acs_data, "interp_results_acs_2018.csv")
