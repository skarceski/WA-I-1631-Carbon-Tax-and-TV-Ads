
rm(list = ls())

library(sf)
library(tidyverse)
library(tidycensus)
 
# for sf file: geomety - voting precincts and census tracts | attributes - tract-level census data

precinct_shape <- st_read("./statewide_prec_2016/Statewide_Prec_2016.shp", ### need to change to the 2016 precincts
                          stringsAsFactors = F) %>%
    select(precinct_id = ST_CODE, geometry)

# read in the census data:

acs_data <- get_acs(geography = "tract", state = "WA", geometry = T,
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

acs_data <- acs_data %>% 
    mutate(over_40_count = Age_1 + Age_2 + Age_3 + Age_4 + Age_5 + Age_6 + Age_7 + Age_8 + Age_9 + Age_10 + Age_11 + Age_12 +
               Age_13 + Age_14 + Age_15 + Age_16 + Age_16 + Age_17 + Age_18 + Age_19 + Age_20 + Age_21 + Age_22 + Age_23 +
               + Age_24, 
           Ed_4_plus = Ed_Bach + Ed_Mast + Ed_Prof + Ed_Doct,
           Pov_Und = Pov_Und.50 + Pov_.50_100) %>% 
    select(-c(Age_1, Age_2, Age_3, Age_4, Age_5, Age_6, Age_7, Age_8, Age_9, Age_10, Age_11, Age_12,
                Age_13, Age_14, Age_15, Age_16, Age_16, Age_17, Age_18, Age_19, Age_20, Age_21, Age_22, Age_23,
               Age_24, Ed_Bach, Ed_Mast, Ed_Prof, Ed_Doct, Pov_Und.50, Pov_.50_100))

precinct_wgs84 <- precinct_shape %>% st_transform(crs = 4326) %>% filter(st_is_valid(.))
acs_wgs84 <- acs_data %>% st_transform(crs = 4326) %>%
    ungroup() %>%
    select(-GEOID, -NAME)


# write a loop

var_list <- c("Med_Income", "Pop_Estimate", "Race_Total", "Race_White", "Race_Black", "Race_Native", "Race_Asian",
    "Race_HawPI", "Race_Other", "Race_2More", "Hisp_Total", "Hisp_Lat", "Ed_Total", "Ed_4_plus",
    "Ind_Total", "Ind_AgMin", "Ind_Const", "Ind_Manuf", "Ind_Trans", "over_40_count", "Pov_Total",
    "Pov_Und", "Commute_Total", "Commute_Car")

interp_precinct <- precinct_wgs84

for (i in seq_along(var_list)) {

    interp <- st_interpolate_aw(acs_wgs84[var_list[i]], precinct_wgs84, extensive = T)
    interp_precinct[var_list[i]] <- interp[var_list[i]]

}

Precinct_Census <- tbl_df(interp_precinct) %>%
    select(-geometry) %>%
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
    select(precinct_id, Med_Income, Pop_Estimate, Race_Black, Race_White, Race_Native, Race_Asian, Race_HawPI, Race_Other, 
           Race_2More, Hisp_Lat, Edu_Bach, Ind_AgMin, Ind_Const, Ind_Manuf, Ind_Trans, Under_Pov, Commute_Car, Over_40) %>%
    write_csv("Precinct_Census_2016.csv")

#  if the code above has already been run; just run the code below

Census <- read_csv("Precinct_Census_2016.csv")

Precinct_Votes <- read_csv("2016Gen_Precinct_Results_GIS-Ready.csv") %>% 
    left_join(read_csv("2016_Precinct_Match.csv")) 

precinct_votes <- Precinct_Votes %>% 
    mutate(Yes_732 = 100*G16I0732Y/(G16I0732Y + G16I0732N),
           Clinton = 100*G16PRSCLIN/(G16PRSCLIN + G16PRSTRUM + G16PRSKENN + G16PRSLARI + G16PRSSTEI + G16PRSCAST + G16PRSJOHN),
           Murray = 100*G16SENMURR/(G16SENMURR + G16SENVANC),
           Yes_1433 = 100*G16I1433Y/(G16I1433Y + G16I1433N),
           Yes_1464 = 100*G16I1464Y/(G16I1464Y + G16I1464N), 
           Votes = G16PRSCLIN + G16PRSTRUM + G16PRSKENN + G16PRSLARI + G16PRSSTEI + G16PRSCAST + G16PRSJOHN,
           Vote_Gap = 100*(Murray - Yes_732)/Murray) %>% 
    select(precinct_id = PrecinctCode, CountyName, PrecinctName, Yes_732, Clinton, Murray, Vote_Gap, 
           Yes_1433, Yes_1464, Votes) %>% 
    left_join(Census) 

write_csv(precinct_votes, "2016_Precinct_Results_Census.csv")

