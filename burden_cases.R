###Project: MI COVID Burden
###Purpose: Prepare Case Data by Age and Region
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "conf_withoutMDOC_phregion_ageyear.csv"
  #from MDSS
###Input: "mcmc/parameter_estimates_0-17.csv"
###Input: "mcmc/parameter_estimates_18-49.csv"
###Input: "mcmc/parameter_estimates_50-64.csv"
###Input: "mcmc/parameter_estimates_65+.csv"
###Output: "estimated_daily_region_cases_long.csv"
###Output: "estimated_daily_region_cases_wide.csv"
###Output: "estimated_weekly_region_cases_long.csv"
###Output: "estimated_weekly_region_cases_wide.csv"

# =========== Load Packages and set filepaths ==================
library(tidyverse)
library(openxlsx)
library(lubridate)
library(reshape2)
library(data.table)

lfp<- "original/data/filepath"
ofp<- "processed/data/filepath"
mcmc<- "mcmc/output/filepath"

# =========== Get daily adjusted case counts ==================
#Read MDSS Data
mdss <- fread(paste0(lfp,"/conf_withoutMDOC_phregion_ageyear.csv"),
                 stringsAsFactors = FALSE)

#convert dates
mdss <- mdss %>%
  mutate(Date = as.Date(Date))

#filter out deaths and hospitalizations; missing regions 
mdss <- as.data.frame(mdss) %>%
  filter(Area != 0 & Metric == "Case Count") %>%
  select(-Geography,-Metric)

#convert wide to long
long.cases <- gather(mdss, Age, cases, 
                     `0`:`999`, factor_key=TRUE) %>%
  mutate(Age = gsub("X","",Age)) %>%
  filter(Age != 999)

#create merge variables
long.cases <- long.cases %>%
  mutate(AgeGroup = case_when(Age < 18 ~ "0-17 Years",
                              Age < 50 ~ "18-49 Years",
                              Age < 65 ~ "50-64 Years",
                              Age >= 65 ~ "65+"),
         period_end_date = case_when(Date <= "2020-05-31" ~ "2020-05-31",
                                     Date <= "2020-09-30" ~ "2020-09-30",
                                     Date <= "2021-02-28" ~ "2021-02-28",
                                     Date <= "2021-05-31" ~ "2021-05-31",
                                     Date > "2021-05-31" ~ "2021-11-22"))

#Read adjustment factor tables
factor.0.17 <- fread(paste0(mcmc,"/parameter_estimates_0-17.csv"),
                   stringsAsFactors = FALSE) %>%
  mutate(AgeGroup = "0-17 Years")
factor.18.49 <- fread(paste0(mcmc,"/parameter_estimates_18-49.csv"),
                        stringsAsFactors = FALSE) %>%
  mutate(AgeGroup = "18-49 Years")
factor.50.64 <- fread(paste0(mcmc,"/parameter_estimates_50-64.csv"),
                        stringsAsFactors = FALSE) %>%
  mutate(AgeGroup = "50-64 Years")
factor.65 <- fread(paste0(mcmc,"/parameter_estimates_65+.csv"),
                        stringsAsFactors = FALSE) %>%
  mutate(AgeGroup = "65+")

factor <- bind_rows(factor.0.17,factor.18.49,factor.50.64,factor.65) %>%
  mutate(period_end_date = case_when(par =="rho_1" ~ "2020-05-31",
                                     par =="rho_2" ~ "2020-09-30",
                                     par =="rho_3" ~ "2021-02-28",
                                     par =="rho_4" ~ "2021-05-31",
                                     par =="rho_5" ~ "2021-11-22")) %>%
  select(-par,-accept)

#add adjustment factor
long.cases <- left_join(long.cases,factor,by=c("AgeGroup","period_end_date"))

#estimate total daily cases
long.cases <- long.cases %>%
  mutate(est.cases = cases*estimate,
         est.casesLower = cases*lower,
         est.casesUpper = cases*upper) 

#convert long to wide
wide.cases <- long.cases %>%
  select(-cases, -AgeGroup, -period_end_date, -estimate, -upper, -lower)

wide.cases <- wide.cases %>% 
  pivot_wider(names_from = Age, values_from = c(est.cases,est.casesLower,est.casesUpper))

names(wide.cases) <- gsub("_",".",names(wide.cases))

#write estimated daily case counts by region and year of age - long
fwrite(long.cases,paste0(ofp,"/estimated_daily_region_cases_long.csv"),row.names = FALSE,na="")

#write estimated daily case counts by region and year of age - wide
fwrite(wide.cases,paste0(ofp,"/estimated_daily_region_cases_wide.csv"),row.names = FALSE,na="")


# =========== Aggregate to weekly case counts by age group ==================
#create age group and week ending variables for aggregation
long.cases.week <- long.cases %>%
  mutate(AgeGroup = case_when(Age < 18 ~ "0to17",
                              Age < 20 ~ "18to19",
                              Age < 30 ~ "20to29",
                              Age < 40 ~ "30to39",
                              Age < 50 ~ "40to49",
                              Age < 60 ~ "50to59",
                              Age < 70 ~ "60to69",
                              Age < 80 ~ "70to79",
                              Age >= 80 ~ "80plus"),
         Week.Ending.Date = ceiling_date(Date, "week")-1) %>%
  select(-cases, -period_end_date, -estimate, -upper, -lower)

#get weekly case counts by age for each region
long.cases.week <- long.cases.week %>% 
  group_by(Area, AgeGroup, Week.Ending.Date) %>% 
  summarise(est.cases = sum(est.cases),
            est.casesLower = sum(est.casesLower),
            est.casesUpper = sum(est.casesUpper))

#convert long to wide
wide.cases.week <- long.cases.week %>% 
  pivot_wider(names_from = AgeGroup, values_from = c(est.cases,est.casesLower,est.casesUpper))

names(wide.cases.week) <- gsub("_",".",names(wide.cases.week))

#write estimated weekly case counts by region and age group - long
fwrite(long.cases.week,paste0(ofp,"/estimated_weekly_region_cases_long.csv"),
          row.names = FALSE,
          na="")

#write estimated weekly case counts by region and age group - wide
fwrite(wide.cases.week,paste0(ofp,"/estimated_weekly_region_cases_wide.csv"),
          row.names = FALSE,
          na="")


