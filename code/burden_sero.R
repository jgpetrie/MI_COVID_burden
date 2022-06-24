###Project: MI COVID Burden
###Purpose: Prepare Seroprevalence data
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "Nationwide_Commercial_Laboratory_Seroprevalence_Survey.csv"
###Output: "MIseroprevalence_full.csv"

# =========== Load Packages and set filepaths ==================
library(dplyr)
library(data.table)

lfp<- "original/data/filepath"
ofp<- "processed/data/filepath"

# =========== Reformat data ==================
#Read Seroprevalence Data (https://data.cdc.gov/Laboratory-Surveillance/Nationwide-Commercial-Laboratory-Seroprevalence-Su/d2tw-32xv)
nclss <- fread(
  paste0(lfp,"/Nationwide_Commercial_Laboratory_Seroprevalence_Survey.csv"),
  stringsAsFactors = FALSE
  ) %>%
  filter(Site == "MI") %>%
  select(1,2,7:26,37:41) 

names(nclss) <- c(
  "site", "dates", 
  "n_0_17", "est_0_17", "lower_0_17", "upper_0_17", "flag_0_17",
  "n_18_49", "est_18_49", "lower_18_49", "upper_18_49", "flag_18_49",
  "n_50_64", "est_50_64", "lower_50_64", "upper_50_64", "flag_50_64",
  "n_65", "est_65", "lower_65", "upper_65", "flag_65",
  "n_all", "est_all", "lower_all", "upper_all", "flag_all")

nclss <- melt(
  nclss, id.vars = c("site","dates"),
  variable.name = "AgeGroup",
  variable.factor = FALSE,
  measure.vars = list(
    N = c("n_0_17","n_18_49","n_50_64","n_65","n_all"),
    Estimate = c("est_0_17","est_18_49","est_50_64","est_65","est_all"),
    Lower = c("lower_0_17","lower_18_49","lower_50_64","lower_65","lower_all"),
    Upper = c("upper_0_17","upper_18_49","upper_50_64","upper_65","upper_all")
               )) %>%
  mutate(AgeGroup = case_when(AgeGroup == 1 ~ "0-17",
                              AgeGroup == 2 ~ "18-49",
                              AgeGroup == 3 ~ "50-64",
                              AgeGroup == 4 ~ "65+",
                              AgeGroup == 5 ~ "overall"),
         End_Date_char = trimws(gsub(".*?- ", "", dates),which = "left"),
         End_Date = as.Date(End_Date_char, "%b %d, %Y")) %>%
  select(End_Date, AgeGroup, Estimate, Lower, Upper, N)

# =========== Add data ==================
#Data displayed on website, but not currently in downloadable file (https://covid.cdc.gov/covid-data-tracker/#national-lab)

add <- data.frame("End_Date" = rep(as.Date("2021-10-02"),5), 
                  "AgeGroup" = c("0-17","18-49","50-64","65+","overall"), 
                  "Estimate" = c(39.0,31.8,26.5,15.8,29.7), 
                  "Lower" = c(30.6,26.6,21.1,12.1,26.7), 
                  "Upper" = c(47.7,37.5,31.9,19.2,33.0), 
                  "N" = c(135,301,294,453,1183))

nclss <- bind_rows(nclss,add) %>%
  mutate(Estimate = ifelse(Estimate==777, NA, Estimate))

# =========== Write data ==================

fwrite(nclss,paste0(ofp,"/MIseroprevalence_full.csv"),
       row.names = FALSE,
       na="")
