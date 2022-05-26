###Project: B.1.1.7 Burden
###Purpose: Prepare Death Data by Age and Region
###Author: Josh Petrie
###Date: 7/23/2021
###Input: "S:/Monto_Ohmit/Sam Harrison/Burden - B117/data/original/conf_withoutMDOC_phregion_ageyear.csv"
###Input: "S:/Monto_Ohmit/Sam Harrison/Burden - B117/data/original/Excess_Deaths_Associated_with_COVID-19.csv"
###Output: "S:/Monto_Ohmit/Sam Harrison/Burden - B117/data/processed/burden_region_deaths.csv"

# =========== Load Packages and set filepaths ==================
library(dplyr)
library(lubridate)
library(reshape2)
library(gridExtra)
library(ggplot2)
library(tidyr)

lfp<- "S:/Monto_Ohmit/Sam Harrison/Burden - B117/data/original"
ofp<- "S:/Monto_Ohmit/Sam Harrison/Burden - B117/data/processed"
rfp<- "S:/Monto_Ohmit/Sam Harrison/Burden - B117/documents/results"

# =========== Read Data ==================
mdss <- read.csv(paste0(lfp,"/conf_withoutMDOC_phregion_ageyear.csv"),
                 stringsAsFactors = FALSE)

nvss <- read.csv(paste0(lfp,"/Excess_Deaths_Associated_with_COVID-19.csv"),
                 stringsAsFactors = FALSE) 

# =========== Prepare NVSS excess death data ==================
names(nvss)[1]<-"Week.Ending.Date"

nvss <- nvss %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  filter(State=="Michigan" & 
           Type=="Predicted (weighted)" & 
           Week.Ending.Date > as.Date("2020-03-01") &
           Outcome=="All causes") %>%
  select(Week.Ending.Date,Excess.Estimate)

# =========== Use MDSS death reports to infer age distribution ==================
#convert dates
mdss <- mdss %>%
  mutate(Date = as.Date(Date))

#filter out hospitalizations and cases; missing regions
mdss <- as.data.frame(mdss) %>%
  filter(Area != 0 & Metric == "Deaths") %>%
  select(-Geography,-Metric)

#convert wide to long
long.death <- gather(mdss, Age, death, 
                    X0:X999, factor_key=TRUE) %>%
  mutate(Age = gsub("X","",Age)) %>%
  filter(Age != 999)

#create aggregation variables
long.death <- long.death %>%
  mutate(AgeGroup = case_when(Age < 18 ~ "0to17",
                              Age < 20 ~ "18to19",
                              Age < 30 ~ "20to29",
                              Age < 40 ~ "30to39",
                              Age < 50 ~ "40to49",
                              Age < 60 ~ "50to59",
                              Age < 70 ~ "60to69",
                              Age < 80 ~ "70to79",
                              Age >= 80 ~ "80plus"),
         Week.Ending.Date = ceiling_date(Date, "week")-1)

#get weekly death counts by age for each region
long.death.week <- long.death %>% 
  group_by(Area, AgeGroup, Week.Ending.Date) %>% 
  summarise(death = sum(death))

#get total state deaths by week
total <- long.death.week %>% 
  group_by(Week.Ending.Date) %>% 
  summarise(total = sum(death))

total <- left_join(total, nvss, by="Week.Ending.Date") %>%
  mutate(combined = ifelse(Excess.Estimate>total,Excess.Estimate,total))

#convert long to wide
wide.death.week <- long.death.week %>% 
  pivot_wider(names_from = AgeGroup, values_from = c(death))

names(wide.death.week)[3:11] <- paste0("X",names(wide.death.week)[3:11])

#merge in total and calculate percent of all weekly deaths that happened in each region/age group
wide.death.week <- left_join(wide.death.week,total,by="Week.Ending.Date") %>%
  mutate(p.0to17 = ifelse(total==0, 0, X0to17/total),
         p.18to19 = ifelse(total==0, 0, X18to19/total),
         p.20to29 = ifelse(total==0, 0, X20to29/total),
         p.30to39 = ifelse(total==0, 0, X30to39/total),
         p.40to49 = ifelse(total==0, 0, X40to49/total),
         p.50to59 = ifelse(total==0, 0, X50to59/total),
         p.60to69 = ifelse(total==0, 0, X60to69/total),
         p.70to79 = ifelse(total==0, 0, X70to79/total),
         p.80plus = ifelse(total==0, 0, X80plus/total))

#calculate total deaths by age and region
wide.death.week <- wide.death.week %>%
  mutate(final.0to17 = p.0to17 * combined,
         final.18to19 = p.18to19 * combined,
         final.20to29 = p.20to29 * combined,
         final.30to39 = p.30to39 * combined,
         final.40to49 = p.40to49 * combined,
         final.50to59 = p.50to59 * combined,
         final.60to69 = p.60to69 * combined,
         final.70to79 = p.70to79 * combined,
         final.80plus = p.80plus * combined)

#final formatting
wide.death.week <- as.data.frame(wide.death.week) %>%
  filter(Week.Ending.Date>=as.Date("2020-09-01") & Area != 0 & !is.na(combined)) %>%
  select(Area, Week.Ending.Date,
         final.0to17,final.18to19,final.20to29,final.30to39,final.40to49,
         final.50to59,final.60to69,final.70to79,final.80plus) %>%
  rename(deaths.0to17 = final.0to17,
         deaths.18to19 = final.18to19,
         deaths.20to29 = final.20to29,
         deaths.30to39 = final.30to39,
         deaths.40to49 = final.40to49,
         deaths.50to59= final.50to59,
         deaths.60to69 = final.60to69,
         deaths.70to79 = final.70to79,
         deaths.80plus = final.80plus)

# =========== Write ==================
write.csv(wide.death.week,paste0(ofp,"/burden_region_deaths.csv"),
          row.names = FALSE,
          na="")