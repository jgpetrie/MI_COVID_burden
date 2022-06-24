###Project: MI COVID Burden
###Purpose: Estimate populations by region and age
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "total_census_tract.csv"
  #populations by county and age
###Input: "MI county details.xlsx"
  #county to public health preparedness region links
###Output: "estimated_population_by_age.csv"

# =========== Load Packages and set filepaths ==================
library(tidyverse)

lfp<- "original/data/filepath"
ofp<- "processed/data/filepath"

# =========== Read Data ==================
#population by county and age
census_pop <- read.csv(paste0(lfp,"/total_census_tract.csv"),
                 stringsAsFactors = FALSE) %>%
  group_by(county, Description) %>%
  summarise(population = sum(sum_row))

#total county populations with PH Region link
regions <- readxl::read_xlsx(paste0(lfp,"/MI county details.xlsx"), 
                             sheet = "Cumulative") %>%
  mutate(county = gsub(" County, Michigan","",County)) 

# =========== Estimate Populations ==================
#merge in PH Region 
census_pop <- merge(as.data.frame(census_pop), regions, by = "county", all.x = TRUE) %>%
  select(-County,-`2018_5y_Pop`)

#Aggregate to Region and Age Group
census_pop <- census_pop %>%
  group_by(PH_Region, Description) %>%
  summarise(population = sum(population))

#Make new age group variable that is closer to those in other data
census_pop <- as.data.frame(census_pop) %>%
  mutate(AgeGroup = case_when(Description %in% c("Under 5 years ",
                                                 "5 to 9 years ",
                                                 "10 to 14 years ",
                                                 "15 to 17 years ") ~ "0to17",
                              Description == "18 and 19 years " ~ "18to19",
                              Description %in% c("20 to 24 years ",
                                                 "25 to 29 years ") ~ "20to29",
                              TRUE ~ Description))

#Aggregate to Region and new Age Group
census_pop <- census_pop %>%
  group_by(PH_Region, AgeGroup) %>%
  summarise(population = sum(population))

#Exact age groups used in other data
groups <- c(
  "0to17","18to19","20to29","30to39","40to49","50to59","60to69","70to79","80plus"
  )

#Set up dataframe to store adjusted population
adj_pop <- data.frame("PH_Region" = sort(rep(unique(census_pop$PH_Region),9)),
                           "AgeGroup" = rep(groups,8)) 

#Adjust population by splitting groups by number of years overlapping preferred group 
#i.e. assume equal population distribution within each group.
for(i in unique(adj_pop$PH_Region)){
  for(j in unique(adj_pop$AgeGroup)){
    if(j %in% c("0to17","18to19","20to29")){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup==j] <- 
        census_pop$population[census_pop$PH_Region==i & census_pop$AgeGroup==j]
    } else if(j == "30to39"){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup=="30to39"] <-
        census_pop$population[census_pop$PH_Region==i & 
                                census_pop$AgeGroup=="30 to 34 years "] +
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="35 to 44 years "]*.5)
    } else if(j == "40to49"){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup=="40to49"] <- 
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="35 to 44 years "]*.5) +
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="45 to 54 years "]*.5)
    } else if(j == "50to59"){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup=="50to59"] <- 
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="45 to 54 years "]*.5) +
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="55 to 64 years "]*.5)
    } else if(j == "60to69"){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup=="60to69"] <- 
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="55 to 64 years "]*.5) +
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="65 to 74 years "]*.5)
    } else if(j == "70to79"){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup=="70to79"] <- 
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="65 to 74 years "]*.5) +
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="75 to 84 years "]*.5)
    }  else if(j == "80plus"){
      adj_pop$population[adj_pop$PH_Region==i & adj_pop$AgeGroup=="80plus"] <- 
        (census_pop$population[census_pop$PH_Region==i & 
                                 census_pop$AgeGroup=="75 to 84 years "]*.5) +
        census_pop$population[census_pop$PH_Region==i & 
                                census_pop$AgeGroup=="85 years and over "]
    }
  }
}

# =========== Write ==================
write.csv(adj_pop,
          paste0(ofp,"/estimated_population_by_age.csv"),
          row.names = FALSE,na="")

