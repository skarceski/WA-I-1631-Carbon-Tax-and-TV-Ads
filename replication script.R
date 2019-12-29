
library(tidyverse)
library(sf)
library(areal)

# 2018 WA State November election results

res_2018 <-
    read_csv("https://results.vote.wa.gov/results/20181106/export/20181106_AllStatePrecincts.csv") %>%
    mutate(race = case_when(str_detect(Race, "1631") ~ "i_1631",
                            str_detect(Race, "U.S. Senator") ~ "senate"),
           precinct_code = str_pad(PrecinctCode, 8, side = "left", pad = "0"),
           precinct_id = paste0(CountyCode, precinct_code)) %>%
    filter(race %in% c("i_1631", "senate"),
           PrecinctCode != -1) %>%
    group_by(race, PrecinctName) %>%
    mutate(tot_votes = sum(Votes)) %>%
    ungroup() %>%
    filter(Candidate %in% c("Maria Cantwell","Yes")) %>%
    group_by(precinct_id) %>%
    summarize(dem_sen = sum(ifelse(race == "senate", Votes, 0)),
              dem_sen_tot = sum(ifelse(race == "senate", tot_votes, 0)),
              i_1631 = sum(ifelse(race == "i_1631", Votes, 0)),
              i_1631_tot = sum(ifelse(race == "i_1631", tot_votes, 0)))

# add king and kitsap counties (excluded from data above)

res_2018 <- res_2018 %>%
    bind_rows(
        read_csv("https://data.kingcounty.gov/api/views/ghxg-x8xz/rows.csv?accessType=DOWNLOAD") %>%
            left_join(read_csv("king_precinct_match_2018.csv")) %>%
            mutate(race = case_when(Race == "US Senator" ~ "senate",
                                    str_detect(Race, "1631") ~ "i_1631"),
                   precinct_code = str_pad(king_precinct_no, 8, side = "left", pad = "0"),
                   precinct_id = paste0(county_code, precinct_code)) %>%
            filter(race %in% c("senate", "i_1631")) %>%
            select(race, precinct_id, votes = SumOfCount, type = CounterType) %>%
            filter(type %in% c("Times Counted", "Yes", "Maria Cantwell")) %>%
            group_by(precinct_id) %>%
            summarize(dem_sen = sum(ifelse(race == "senate" & type == "Maria Cantwell", votes, 0)),
                      dem_sen_tot = sum(ifelse(race == "senate" & type == "Times Counted", votes, 0)),
                      i_1631 = sum(ifelse(race == "i_1631" & type == "Yes", votes, 0)),
                      i_1631_tot = sum(ifelse(race == "i_1631" & type == "Times Counted", votes, 0)))) %>%
    bind_rows(
        read_csv("http://results.vote.wa.gov/results/20181106/export/20181106_KitsapPrecincts.csv",
                 col_names = c("race", "candidate", "precinct_name", "precinct_code", "vote_count")) %>%
            filter(precinct_code != -1) %>%
            mutate(race = case_when(str_detect(race, "1631") ~ "i_1631",
                                    str_detect(race, "U.S. Senator") ~ "senate"),
                   precinct_code = precinct_code - 100000,
                   precinct_code = str_pad(precinct_code, 8, side = "left", pad = "0"),
                   precinct_id = paste0("KP", precinct_code)) %>%
            group_by(race, precinct_id) %>%
            mutate(tot_votes = sum(vote_count)) %>%
            ungroup() %>%
            filter(candidate %in% c("Yes", "Maria Cantwell"),
                   race %in% c("senate", "i_1631")) %>%
            group_by(precinct_id) %>%
            summarize(dem_sen = sum(ifelse(race == "senate", vote_count, 0)),
                      dem_sen_tot = sum(ifelse(race == "senate", tot_votes, 0)),
                      i_1631 = sum(ifelse(race == "i_1631", vote_count, 0)),
                      i_1631_tot = sum(ifelse(race == "i_1631", tot_votes, 0))))

# join with shapefile via the WA SOS website:
# https://www.sos.wa.gov/elections/research/precinct-shapefiles.aspx

shape_2018 <- st_read("2018Precincts_VERIFIED.shp", ### the 2018 precinct shape file
                           stringsAsFactors = F) %>%
    select(precinct_id = FullPrc, geometry) %>%
    ungroup() %>%
    filter(st_is_valid(.))
res_2018 <- shape_2018 %>% left_join(res_2018)

# get ACS data with tidycensus

# tidycensus - tract-level measures for WA state

acs <- get_acs(geography = "tract", state = "WA", geometry = T,
               variables = c(
                   Med_Income = "B19013_001", Pop_Estimate = "B01001_001",
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
                   Age_4 = "B01001_017", Age_5 = "B01001_018", Age_6 = "B01001_019",
                   Age_7 =  "B01001_020", Age_8 = "B01001_021", Age_9 = "B01001_022",
                   Age_10 = "B01001_023", Age_11 = "B01001_024", Age_12 = "B01001_025",
                   Age_13 = "B01001_038", Age_14 = "B01001_039", Age_15 = "B01001_040",
                   Age_16 = "B01001_041", Age_17 = "B01001_042", Age_18 = "B01001_043",
                   Age_19 = "B01001_044", Age_20 = "B01001_045", Age_21 = "B01001_046",
                   Age_22 = "B01001_047", Age_23 = "B01001_048", Age_24 = "B01001_049",
                   # misc. vars:
                   Pov_Total = "C17002_001", Pov_Und.50 = "C17002_002", Pov_.50_100 = "C17002_003",
                   Commute_Total = "B08301_001", Commute_Car = "B08301_002")) %>%
    select(-moe) %>%
    group_by(GEOID) %>%
    spread(variable, estimate)

# create new vars, ditch unnecessary vars

acs <- acs %>%
    mutate(over_40_count = Age_1 + Age_2 + Age_3 + Age_4 + Age_5 + Age_6 + Age_7 + Age_8 + Age_9 +
               Age_10 + Age_11 + Age_12 + Age_13 + Age_14 + Age_15 + Age_16 + Age_16 + Age_17 +
               Age_18 + Age_19 + Age_20 + Age_21 + Age_22 + Age_23 + Age_24,
           Ed_4_plus = Ed_Bach + Ed_Mast + Ed_Prof + Ed_Doct,
           Pov_Und = Pov_Und.50 + Pov_.50_100) %>%
    select(-c(Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10, Age_11, Age_12,
              Age_13, Age_14, Age_15, Age_16, Age_16, Age_17, Age_18, Age_19, Age_20, Age_21, Age_22,
              Age_23, Age_24, Ed_Bach, Ed_Mast, Ed_Prof, Ed_Doct, Pov_Und.50, Pov_.50_100))

acs <- acs %>%
    mutate(Race_Black = 100*Race_Black/Race_Total,
           Race_White = 100*Race_White/Race_Total,
           Race_Native = 100*Race_Native/Race_Total,
           Race_Asian = 100*Race_Asian/Race_Total,
           Race_HawPI = 100*Race_HawPI/Race_Total,
           Race_Other = 100*Race_Other/Race_Total,
           Race_2More = 100*Race_2More/Race_Total,
           Hisp_Lat = 100*Hisp_Lat/Hisp_Total,
           Edu_Bach = 100*Ed_4_plus/Ed_Total,
           Ind_AgMin = 100*Ind_AgMin/Ind_Total,
           Ind_Const = 100*Ind_Const/Ind_Total,
           Ind_Manuf = 100*Ind_Manuf/Ind_Total,
           Ind_Trans = 100*Ind_Trans/Ind_Total,
           Under_Pov = 100*Pov_Und/Pov_Total,
           Commute_Car = 100*Commute_Car/Commute_Total,
           Over_40 = 100*over_40_count/Pop_Estimate) %>%
    select(GEOID, NAME, geometry, Med_Income, Pop_Estimate,
           Race_Black, Race_White, Race_Native, Race_Asian, Race_HawPI, Race_Other, Race_2More,
           Hisp_Lat, Edu_Bach, Ind_AgMin, Ind_Const, Ind_Manuf, Ind_Trans, Under_Pov, Commute_Car,
           Over_40) %>%
    rename(tract_id = GEOID, tract_name = NAME)
