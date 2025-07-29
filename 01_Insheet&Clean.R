
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyquant)
library(magrittr) 
library(dplyr)

library(Synth)
library(SCtools)
library(skimr)
library(zoo)
library(lubridate)
library(dplyr)
library(tidysynth)
library(janitor)
library(flextable)

# LOAD CRIME DATA
# latest crime data can be downloaded at: https://public.tableau.com/app/profile/portlandpolicebureau/viz/New_Monthly_Neighborhood/MonthlyOffenseTotals
files <- fs::dir_ls(path = "data\\Crime\\", glob = "*CrimeData*csv")
dfc<-readr::read_csv(files, id = "path")


# CONSTRUCT NEIGHBORHOOD QUARTER LEVEL DATASET

dfc <- dfc %>%
  mutate(quarter = zoo::as.yearqtr(as.Date(OccurDate, format = "%m/%d/%Y"))) %>%
  filter(quarter >= zoo::as.yearqtr(as.Date("01/01/2015", format = "%m/%d/%Y"))) %>%
  filter(OffenseCategory == "Assault Offenses" | OffenseCategory == "Motor Vehicle Theft" | OffenseCategory == "Vandalism" | OffenseCategory == "Larceny Offenses") %>%
  group_by(Neighborhood, quarter) %>%
  summarize(OffenseCount = sum(OffenseCount)) %>%
  ungroup()


universe<-expand.grid(
  Neighborhood = unique(dfc$Neighborhood),
  quarter = unique(dfc$quarter)) %>%
  mutate(year = lubridate::year(quarter)) %>%
  group_by(Neighborhood) %>%
  arrange(quarter) %>%
  mutate(time = row_number(),
         NeighNum = cur_group_id()) %>%
  ungroup()
  

dfc<-left_join(universe, dfc, join_by(Neighborhood, quarter)) %>%
  mutate(OffenseCount = if_else(is.na(OffenseCount) == TRUE, 0, OffenseCount),
         qtr_numeric = as.numeric(quarter))


universe %>%
  filter (Neighborhood == "Cully") %>%
  head()


# LOAD, CLEAN, & MERGE NEIGHBORHOOD COVARIATE DATA
#source: https://www.portland.gov/civic/myneighborhood/neighborhood-profile-maps

dfn<-read_excel("data\\pdx_ocl_profiles_ed2023_ndata.xlsx", sheet = "data")

glimpse(dfn)


dfn<-dfn %>%
  rename("Neighborhood" = "nname",
         "pop_2010" = "v11_total_10",
         "pop_2020" = "v11_total_20",
         "age_under18_2010" = "v1J_agelt18_10",
         "age_under18_2020" = "v1J_agelt18_20",
         "age_1864_2010" = "v1K_age1864_10",
         "age_1864_2020" = "v1K_age1864_20",
         "age_over65_2010" = "v1L_age65pl_10",
         "age_over65_2020" = "v1L_age65pl_20",
         "median_age_2010" = "v1M_medage_10",
         "median_age_2020" = "v1M_medage_20",
         "area_sq_mile_2010" = "v1N_sqmi_10",
         "area_sq_mile_2020" = "v1N_sqmi_20",
         "pop_black_2010" = "v24_black_10",
         "pop_black_2020" = "v24_black_20",
         "pop_white_2010" = "v27_white_10",
         "pop_white_2020" = "v27_white_20",
         "pop_hispanic_2010" = "v28_hispan_10",
         "pop_hispanic_2020" = "v28_hispan_20",
         "rentburd_pct_2010" = "v32_rentburd_pct_10",
         "rentburd_pct_2020" = "v32_rentburd_pct_20",
         "home_owner_pct_2010" = "v38_owner_pct_10",
         "home_owner_pct_2020" = "v38_owner_pct_20",
         "turnout_2020" = "v41_turnout_pct_20",
         "BA_or_higher_2010" = "v52_geba_pct_10",
         "BA_or_higher_2020" = "v52_geba_pct_20",
         "no_ged_2010" = "v51_noged_pct_10",
         "no_ged_2020" = "v51_noged_pct_20",
         "tree_canopy_pct_2020" = "v65_canopy_pct_20",
         "poverty_rate_2010" = "v71_pov_pct_10",
         "poverty_rate_2020" = "v71_pov_pct_20") %>%
  mutate(Neighborhood = case_when(Neighborhood == "Ardenwald-Johnson Creek" ~ "Ardenwald",
                                  Neighborhood == "Argay Terrace" ~ "Argay",
                                  Neighborhood == "St. Johns" ~ "St Johns",
                                  Neighborhood == "Lloyd District" ~ "Lloyd",
                                  Neighborhood == "Portland Downtown" ~ "Downtown",
                                  Neighborhood == "Pearl District" ~ "Pearl",
                                  Neighborhood == "Old Town" ~ "Old Town/Chinatown",
                                  Neighborhood == "Mt. Tabor" ~ "Mt Tabor",
                                  Neighborhood == "Northwest District" ~ "Northwest",
                                  Neighborhood == "West Northwest District" ~ "Northwest Industrial",
                                  Neighborhood == "Buckman" ~ "Buckman West",
                                  Neighborhood == "Mt. Scott-Arleta" ~ "Mt Scott-Arleta",
                                  .default = as.character(Neighborhood)),
         pct_white_2020 = pop_white_2020 / pop_2020,
         pct_black_2020 = pop_black_2020 / pop_2020,
         pct_hispanic_2020 = pop_hispanic_2020 / pop_2020) %>%
  select(-contains(c("v1","v2","v3","v4","v5","v6","v7","v8")))


# Neighborhoods in demographic data not in crime data
unique(dfn$Neighborhood)[!(unique(dfn$Neighborhood) %in% unique(dfc$Neighborhood))]

# Neighborhoods in crime data not in demographic data
unique(dfc$Neighborhood)[!(unique(dfc$Neighborhood) %in% unique(dfn$Neighborhood))]


# CONSTRUCT NEIGHBORHOOD-QUARTER LEVEL DATASET 


df <- left_join(dfc, dfn, join_by(Neighborhood)) %>%
  filter(is.na(level) == FALSE)

# generate number of crimes by neighborhood by year

#df <-  df %>%
  #mutate(year = lubridate::year(quarter)) %>%
#  group_by(Neighborhood) %>%
#  summarise(OffenseCount_2015 = sum(OffenseCount[year == 2015]),
#            OffenseCount_2016 = sum(OffenseCount[year == 2016]),
#            OffenseCount_2017 = sum(OffenseCount[year == 2017]),
#            OffenseCount_2018 = sum(OffenseCount[year == 2018]),
#            OffenseCount_2019 = sum(OffenseCount[year == 2019]),
#            OffenseCount_2020 = sum(OffenseCount[year == 2020]),
#            OffenseCount_2021 = sum(OffenseCount[year == 2021]),
#            OffenseCount_2022 = sum(OffenseCount[year == 2022]),
#            OffenseCount_2023 = sum(OffenseCount[year == 2023])) %>%
#  ungroup() %>%
#  left_join(df, join_by(Neighborhood))

write_csv(df, "data\\MultnomahAnalysisByQtr.csv", append=FALSE)







