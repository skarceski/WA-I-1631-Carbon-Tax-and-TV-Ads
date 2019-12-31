
library(tidyverse)
library(lme4)
library(sjPlot)

data <- read_csv("interp_results_acs_2018.csv") %>% 
    mutate(density = as.numeric(density)) %>% 
    left_join(read_csv("interpolated_2016_results.csv") %>% 
                  mutate(yes_732_p = (100*v_yes_732)/base_732) %>% 
                  select(tract_id, yes_732_p, yes_732 = v_yes_732, 
                         tot_732 = base_732)) %>% 
    mutate(med_inc = Med_Income/1000,
           gap_732_1631 = 100*(yes_1631_p - yes_732_p)/yes_732_p)

tab_model(lmer(yes_1631_p ~ yn_ratio + yes_732_p + dem_senate_p + 
                   commute_car + ind_agmin + ind_const + ind_manuf + ind_trans + 
                   med_inc + over_40 + edu_bach + 
                   race_black + race_native + race_asian + race_hawpi + race_other + race_2more + hisp_lat + 
                   (1|media_market) + (1|media_market:county_code), data = data, 
               control = lmerControl(optimizer = "Nelder_Mead")),
          lmer(gap_732_1631 ~ yn_ratio + dem_senate_p + 
                   commute_car + ind_agmin + ind_const + ind_manuf + ind_trans + 
                   med_inc + over_40 + edu_bach + 
                   race_black + race_native + race_asian + race_hawpi + race_other + race_2more + hisp_lat + 
                   (1|media_market) + (1|media_market:county_code), data = data,
               control = lmerControl(optimizer = "Nelder_Mead")),
          show.ci = F, show.se = T, p.style = "a")

