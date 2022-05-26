###Project: B.1.1.7 Burden
###Purpose: Prepare Vaccination Data by Age and Region
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "Covid_Vaccine_Coverage_by_County_718469_7.xlsx"
###Output: "burden_region_vaccine.csv"
###Output: "burden_region_vaccine_adjusted_age_groups.csv"

# =========== Load Packages and set filepaths ==================
library(tidyverse)
library(openxlsx)
library(lubridate)
library(reshape2)

rfp<- "original/data/filepath"
ofp<- "processed/data/filepath"

# =========== Read Data ==================
#read county level vaccine data
vaccine <- read.xlsx(paste0(rfp,"/Covid_Vaccine_Coverage_by_County_718469_7.xlsx"),sheet = 2)

# =========== Reformat ==================
#convert date to date format
vaccine <- vaccine %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date,format="%m/%d/%Y"))

#pull out age/sex/geography specific populations
pop <- as.data.frame(
  with(
    vaccine,
    table(
      `Person's.Residence.in.Preparedness.Region`,Age.Group,Sex,
      `2019.Census.MI.Population.(12.years.and.Older)`,
      `Person's.Residence.in.County`
      )
    )
  ) %>%
  filter(Freq != 0)

#collapse populations to get age/region specific
pop <- pop %>%
  group_by(Person.s.Residence.in.Preparedness.Region,Age.Group) %>%
  summarize(
    population = sum(
      as.numeric(as.character(X2019.Census.MI.Population..12.years.and.Older.))
      )
    ) 

pop <- as.data.frame(pop) %>%
  rename(Region = Person.s.Residence.in.Preparedness.Region)

#collapse populations to get age specific for whole state
state.pop <- pop %>%
  group_by(Age.Group) %>%
  summarise(population = sum(population))

#collapse doses to get age/region/completion specific
vaccine <- vaccine %>% 
  group_by(`Person's.Residence.in.Preparedness.Region`, 
           Age.Group, Administration.Level, Week.Ending.Date) %>% 
  summarise(doses = sum(Number.of.Doses))

#calculate age/region/completion specific cumulative doses
vaccine <- vaccine %>% 
  group_by(`Person's.Residence.in.Preparedness.Region`, 
           Age.Group, Administration.Level) %>% 
  mutate(cumulative.doses = cumsum(doses))

#keep only data for those with one or more doses: "initiation"
vaccine <- as.data.frame(vaccine) %>%
  filter(Administration.Level == "Initiation")

#collapse doses to get age specific for whole state
state.vaccine <- vaccine %>%
  group_by(Age.Group, Week.Ending.Date) %>%
  summarise(doses = sum(doses))

#calculate age specific cumulative doses for whole state
state.vaccine <- state.vaccine %>% 
  group_by(Age.Group) %>% 
  mutate(cumulative.doses = cumsum(doses))

#merge state doses and populations
state.vaccine <- left_join(state.vaccine,state.pop,by="Age.Group") %>%
  mutate(Region = "Michigan") %>%
  filter(Age.Group != "missing")

#merge region specific doses and populations
region.vaccine <- vaccine %>%
  filter(
    `Person's.Residence.in.Preparedness.Region` != "No Preparedness Region" & 
      Age.Group != "missing"
    ) %>%
  rename(Region = `Person's.Residence.in.Preparedness.Region`) %>%
  select(-Administration.Level)

region.vaccine <- left_join(region.vaccine,pop,by=c("Region","Age.Group"))

#merge state and region specific doses and populations
vaccine.all <- bind_rows(region.vaccine,state.vaccine) %>%
  mutate(cumulative.coverage = cumulative.doses/population,
         AgeGroup = case_when(Age.Group=="5-11 years" ~ "5to11",
                              Age.Group=="12-15 years" ~ "12to15",
                              Age.Group=="16-19 years" ~ "16to19",
                              Age.Group=="20-29 years" ~ "20to29",
                              Age.Group=="30-39 years" ~ "30to39",
                              Age.Group=="40-49 years" ~ "40to49",
                              Age.Group=="50-64 years" ~ "50to64",
                              Age.Group=="65-74 years" ~ "65to74",
                              Age.Group=="75+ years" ~ "75plus")) %>%
  select(-Age.Group)

# redistribute doses to age groups to match other data
groups <- c(
  "0to17","18to19","20to29","30to39","40to49","50to59","60to69","70to79","80plus"
  )

n1 <- length(unique(vaccine.all$Week.Ending.Date))
n2 <- length(groups)
n3 <- length(unique(vaccine.all$Region))

adj_vaccine <- data.frame(
  "PH_Region" = sort(rep(unique(vaccine.all$Region),n2*n1)),
  "AgeGroup" = rep(rep(groups,each=n1),n3),
  "Week.Ending.Date" = rep(unique(vaccine.all$Week.Ending.Date),n3)
  ) 

for(i in unique(adj_vaccine$PH_Region)){
  for(j in unique(adj_vaccine$AgeGroup)){
    for(k in unique(adj_vaccine$Week.Ending.Date)){
      if(j == "0to17"){
        adj_vaccine$doses[adj_vaccine$PH_Region==i & 
                            adj_vaccine$AgeGroup==j & 
                            adj_vaccine$Week.Ending.Date ==k] <- 
          (vaccine.all$doses[vaccine.all$Region==i & 
                              vaccine.all$AgeGroup=="16to19" & 
                              vaccine.all$Week.Ending.Date ==k]*.5) +
          (vaccine.all$doses[vaccine.all$Region==i & 
                               vaccine.all$AgeGroup=="5to11" & 
                               vaccine.all$Week.Ending.Date ==k]) +
          (vaccine.all$doses[vaccine.all$Region==i & 
                               vaccine.all$AgeGroup=="12to15" & 
                               vaccine.all$Week.Ending.Date ==k])
       } else if(j %in% c("0to17","18to19")){
        adj_vaccine$doses[adj_vaccine$PH_Region==i &
                            adj_vaccine$AgeGroup==j &
                            adj_vaccine$Week.Ending.Date ==k] <-
          vaccine.all$doses[vaccine.all$Region==i &
                              vaccine.all$AgeGroup=="16to19" &
                              vaccine.all$Week.Ending.Date ==k]*.5
      } else if(j %in% c("20to29","30to39","40to49")){
        adj_vaccine$doses[adj_vaccine$PH_Region==i &
                            adj_vaccine$AgeGroup==j &
                            adj_vaccine$Week.Ending.Date ==k] <-
          vaccine.all$doses[vaccine.all$Region==i &
                              vaccine.all$AgeGroup==j  &
                              vaccine.all$Week.Ending.Date ==k]
      } else if(j == "50to59"){
        adj_vaccine$doses[adj_vaccine$PH_Region==i &
                            adj_vaccine$AgeGroup=="50to59"  &
                            adj_vaccine$Week.Ending.Date ==k] <-
          vaccine.all$doses[vaccine.all$Region==i &
                              vaccine.all$AgeGroup=="50to64"  &
                              vaccine.all$Week.Ending.Date ==k]*(2/3)
      } else if(j == "60to69"){
        adj_vaccine$doses[adj_vaccine$PH_Region==i &
                            adj_vaccine$AgeGroup=="60to69"  &
                            adj_vaccine$Week.Ending.Date ==k] <-
          (vaccine.all$doses[vaccine.all$Region==i &
                               vaccine.all$AgeGroup=="50to64" &
                               vaccine.all$Week.Ending.Date ==k]*(1/3)) +
          (vaccine.all$doses[vaccine.all$Region==i &
                               vaccine.all$AgeGroup=="65to74" &
                               vaccine.all$Week.Ending.Date ==k]*.5)
      } else if(j == "70to79"){
        adj_vaccine$doses[adj_vaccine$PH_Region==i &
                            adj_vaccine$AgeGroup=="70to79"  &
                            adj_vaccine$Week.Ending.Date ==k] <-
          (vaccine.all$doses[vaccine.all$Region==i &
                               vaccine.all$AgeGroup=="65to74" &
                               vaccine.all$Week.Ending.Date ==k]*.5) +
          (vaccine.all$doses[vaccine.all$Region==i &
                               vaccine.all$AgeGroup=="75plus"  &
                               vaccine.all$Week.Ending.Date ==k]*.43)
      }  else if(j == "80plus"){
        adj_vaccine$doses[adj_vaccine$PH_Region==i &
                            adj_vaccine$AgeGroup=="80plus"  &
                            adj_vaccine$Week.Ending.Date ==k] <-
          (vaccine.all$doses[vaccine.all$Region==i &
                               vaccine.all$AgeGroup=="75plus"  &
                               vaccine.all$Week.Ending.Date ==k]*.57)
      }
    }
  }
}

#recalculate age/region specific cumulative doses
adj_vaccine <- adj_vaccine %>% 
  group_by(PH_Region, AgeGroup) %>% 
  mutate(cumulative.doses = cumsum(doses))

# =========== Write ==================
write.csv(vaccine.all,
          paste0(ofp,"/burden_region_vaccine.csv"),
          row.names = FALSE,na="")
write.csv(adj_vaccine,
          paste0(ofp,"/burden_region_vaccine_adjusted_age_groups.csv"),
          row.names = FALSE,na="")

