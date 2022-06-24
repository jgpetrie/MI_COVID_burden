###Project: MI COVID Burden
###Purpose: Figure 4
###Author: Josh Petrie
###Date: 12/23/2021

###Input: "estimated_weekly_region_cases_wide.csv"
###Input: "burden_region_deaths.csv"
###Input: "burden_region_hosp.csv"
###Input: "estimated_population_by_age.csv"

###Output: "figure4.pdf"

# =========== Load Libraries ==================
library(data.table)
library(tidyverse)
library(jsonlite)
library(Hmisc)
library(gridExtra)

# =========== Read data ==================
pfp <- "processed/data/filepath"

cases <- fread(
  paste0(pfp,"/estimated_weekly_region_cases_wide.csv"),
  stringsAsFactors = FALSE
  ) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  filter(Week.Ending.Date <= "2021-11-13")

deaths <- fread(
  paste0(pfp,"/burden_region_deaths.csv"),
  stringsAsFactors = FALSE
  ) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  filter(Week.Ending.Date <= "2021-11-13")

hosp <- fread(
  paste0(pfp,"/burden_region_hosp.csv"),stringsAsFactors = FALSE
  ) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  filter(Week.Ending.Date <= "2021-11-13")

pop <- fread(
  paste0(pfp,"/estimated_population_by_age.csv"),stringsAsFactors = FALSE
  ) %>% 
  rename(Area = PH_Region)
# =========== Reformat data ==================
#get populations by age for whole state
pop_state <- pop %>%
  group_by(AgeGroup) %>%
  summarise(population = sum(population)) %>%
  mutate(Area = "Michigan")

#add state populations to region populations and create mid point age variable for plots
pop <- bind_rows(pop,pop_state) %>%
  mutate(mid.point.age = case_when(AgeGroup=="0to17" ~ 9,
                                   AgeGroup=="18to19" ~ 18,
                                   AgeGroup=="20to29" ~ 25,
                                   AgeGroup=="30to39" ~ 35,
                                   AgeGroup=="40to49" ~ 45,
                                   AgeGroup=="50to59" ~ 55,
                                   AgeGroup=="60to69" ~ 65,
                                   AgeGroup=="70to79" ~ 75,
                                   AgeGroup=="80plus" ~ 85))

#merge case, death, and hospitaliztion data
plot.data <- left_join(deaths,hosp,by=c("Area","Week.Ending.Date"))
plot.data <- left_join(
  plot.data,
  filter(cases,Week.Ending.Date>=as.Date("2020-09-01")),
  by=c("Area","Week.Ending.Date")
  )

#collapse case, death, and hospitaliztion data to the entire state
state.plot.data <- plot.data %>%
  group_by(Week.Ending.Date) %>%
  summarise_if(is.numeric, sum)

state.plot.data$Area <- "Michigan"

#append state level case, death, and hospitalization data to region level data
plot.data <- bind_rows(plot.data,state.plot.data) %>%
  #remove deaths and hospitalizations from cases so that burden plot heights equal total cases
  mutate(adj.0to17 = est.cases.0to17-deaths.0to17-hosp.0to17,
         adj.18to19 = est.cases.18to19-deaths.18to19-hosp.18to19,
         adj.20to29 = est.cases.20to29-deaths.20to29-hosp.20to29,
         adj.30to39 = est.cases.30to39-deaths.30to39-hosp.30to39,
         adj.40to49 = est.cases.40to49-deaths.40to49-hosp.40to49,
         adj.50to59 = est.cases.50to59-deaths.50to59-hosp.50to59,
         adj.60to69 = est.cases.60to69-deaths.60to69-hosp.60to69,
         adj.70to79 = est.cases.70to79-deaths.70to79-hosp.70to79,
         adj.80plus = est.cases.80plus-deaths.80plus-hosp.80plus,
         adjLower.0to17 = est.casesLower.0to17-deaths.0to17-hosp.0to17,
         adjLower.18to19 = est.casesLower.18to19-deaths.18to19-hosp.18to19,
         adjLower.20to29 = est.casesLower.20to29-deaths.20to29-hosp.20to29,
         adjLower.30to39 = est.casesLower.30to39-deaths.30to39-hosp.30to39,
         adjLower.40to49 = est.casesLower.40to49-deaths.40to49-hosp.40to49,
         adjLower.50to59 = est.casesLower.50to59-deaths.50to59-hosp.50to59,
         adjLower.60to69 = est.casesLower.60to69-deaths.60to69-hosp.60to69,
         adjLower.70to79 = est.casesLower.70to79-deaths.70to79-hosp.70to79,
         adjLower.80plus = est.casesLower.80plus-deaths.80plus-hosp.80plus,
         adjUpper.0to17 = est.casesUpper.0to17-deaths.0to17-hosp.0to17,
         adjUpper.18to19 = est.casesUpper.18to19-deaths.18to19-hosp.18to19,
         adjUpper.20to29 = est.casesUpper.20to29-deaths.20to29-hosp.20to29,
         adjUpper.30to39 = est.casesUpper.30to39-deaths.30to39-hosp.30to39,
         adjUpper.40to49 = est.casesUpper.40to49-deaths.40to49-hosp.40to49,
         adjUpper.50to59 = est.casesUpper.50to59-deaths.50to59-hosp.50to59,
         adjUpper.60to69 = est.casesUpper.60to69-deaths.60to69-hosp.60to69,
         adjUpper.70to79 = est.casesUpper.70to79-deaths.70to79-hosp.70to79,
         adjUpper.80plus = est.casesUpper.80plus-deaths.80plus-hosp.80plus)

#subset to equal time periods capturing each wave and create wave indicator
plot.data.same <- plot.data %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date),
         wave = case_when(
           Week.Ending.Date>=as.Date("2020-10-11") &
             Week.Ending.Date<=as.Date("2021-01-30") ~ "Fall 2020",
           Week.Ending.Date>=as.Date("2021-02-28") &
             Week.Ending.Date<=as.Date("2021-06-19")~ "Spring 2021",
           Week.Ending.Date>=as.Date("2021-07-24") &
             Week.Ending.Date<=as.Date("2021-11-13")~ "Fall 2021",
           TRUE ~ NA_character_)) %>%
  filter(!is.na(wave))

final.format <- function(data,grp){
  grp1 <- enquo(grp)
  grp2 <- quo_name(grp1)
  data <- data %>%
    group_by(Area, !!grp1) %>% 
    summarise_if(is.numeric, sum)
  
  data <- as.data.frame(data) %>%
    mutate(p.died.0to17 = deaths.0to17/est.cases.0to17,
           p.died.18to19 = deaths.18to19/est.cases.18to19,
           p.died.20to29 = deaths.20to29/est.cases.20to29,
           p.died.30to39 = deaths.30to39/est.cases.30to39,
           p.died.40to49 = deaths.40to49/est.cases.40to49,
           p.died.50to59 = deaths.50to59/est.cases.50to59,
           p.died.60to69 = deaths.60to69/est.cases.60to69,
           p.died.70to79 = deaths.70to79/est.cases.70to79,
           p.died.80plus = deaths.80plus/est.cases.80plus,
           
           p.died.Lower.0to17 = deaths.0to17/est.casesLower.0to17,
           p.died.Lower.18to19 = deaths.18to19/est.casesLower.18to19,
           p.died.Lower.20to29 = deaths.20to29/est.casesLower.20to29,
           p.died.Lower.30to39 = deaths.30to39/est.casesLower.30to39,
           p.died.Lower.40to49 = deaths.40to49/est.casesLower.40to49,
           p.died.Lower.50to59 = deaths.50to59/est.casesLower.50to59,
           p.died.Lower.60to69 = deaths.60to69/est.casesLower.60to69,
           p.died.Lower.70to79 = deaths.70to79/est.casesLower.70to79,
           p.died.Lower.80plus = deaths.80plus/est.casesLower.80plus,
           
           p.died.Upper.0to17 = deaths.0to17/est.casesUpper.0to17,
           p.died.Upper.18to19 = deaths.18to19/est.casesUpper.18to19,
           p.died.Upper.20to29 = deaths.20to29/est.casesUpper.20to29,
           p.died.Upper.30to39 = deaths.30to39/est.casesUpper.30to39,
           p.died.Upper.40to49 = deaths.40to49/est.casesUpper.40to49,
           p.died.Upper.50to59 = deaths.50to59/est.casesUpper.50to59,
           p.died.Upper.60to69 = deaths.60to69/est.casesUpper.60to69,
           p.died.Upper.70to79 = deaths.70to79/est.casesUpper.70to79,
           p.died.Upper.80plus = deaths.80plus/est.casesUpper.80plus,
           
           p.hosp.0to17 = hosp.0to17/est.cases.0to17,
           p.hosp.18to19 = hosp.18to19/est.cases.18to19,
           p.hosp.20to29 = hosp.20to29/est.cases.20to29,
           p.hosp.30to39 = hosp.30to39/est.cases.30to39,
           p.hosp.40to49 = hosp.40to49/est.cases.40to49,
           p.hosp.50to59 = hosp.50to59/est.cases.50to59,
           p.hosp.60to69 = hosp.60to69/est.cases.60to69,
           p.hosp.70to79 = hosp.70to79/est.cases.70to79,
           p.hosp.80plus = hosp.80plus/est.cases.80plus,
           
           p.hosp.Lower.0to17 = hosp.0to17/est.casesLower.0to17,
           p.hosp.Lower.18to19 = hosp.18to19/est.casesLower.18to19,
           p.hosp.Lower.20to29 = hosp.20to29/est.casesLower.20to29,
           p.hosp.Lower.30to39 = hosp.30to39/est.casesLower.30to39,
           p.hosp.Lower.40to49 = hosp.40to49/est.casesLower.40to49,
           p.hosp.Lower.50to59 = hosp.50to59/est.casesLower.50to59,
           p.hosp.Lower.60to69 = hosp.60to69/est.casesLower.60to69,
           p.hosp.Lower.70to79 = hosp.70to79/est.casesLower.70to79,
           p.hosp.Lower.80plus = hosp.80plus/est.casesLower.80plus,
           
           p.hosp.Upper.0to17 = hosp.0to17/est.casesUpper.0to17,
           p.hosp.Upper.18to19 = hosp.18to19/est.casesUpper.18to19,
           p.hosp.Upper.20to29 = hosp.20to29/est.casesUpper.20to29,
           p.hosp.Upper.30to39 = hosp.30to39/est.casesUpper.30to39,
           p.hosp.Upper.40to49 = hosp.40to49/est.casesUpper.40to49,
           p.hosp.Upper.50to59 = hosp.50to59/est.casesUpper.50to59,
           p.hosp.Upper.60to69 = hosp.60to69/est.casesUpper.60to69,
           p.hosp.Upper.70to79 = hosp.70to79/est.casesUpper.70to79,
           p.hosp.Upper.80plus = hosp.80plus/est.casesUpper.80plus)
  
  
  data <- reshape2::melt(data,id.vars=c("Area",grp2))
  
  
  case.plot.data <- data %>%
    filter(grepl("adj",variable) & !grepl("Missing",variable)) %>%
    mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                     grepl("18to19",variable) ~ 18,
                                     grepl("20to29",variable) ~ 25,
                                     grepl("30to39",variable) ~ 35,
                                     grepl("40to49",variable) ~ 45,
                                     grepl("50to59",variable) ~ 55,
                                     grepl("60to69",variable) ~ 65,
                                     grepl("70to79",variable) ~ 75,
                                     grepl("80plus",variable) ~ 85))
  
  hosp.plot.data <- data %>%
    filter(!grepl("p.hosp",variable) & grepl("hosp",variable)) %>%
    mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                     grepl("18to19",variable) ~ 18,
                                     grepl("20to29",variable) ~ 25,
                                     grepl("30to39",variable) ~ 35,
                                     grepl("40to49",variable) ~ 45,
                                     grepl("50to59",variable) ~ 55,
                                     grepl("60to69",variable) ~ 65,
                                     grepl("70to79",variable) ~ 75,
                                     grepl("80plus",variable) ~ 85))
  
  death.plot.data <- data %>%
    filter(grepl("deaths",variable)) %>%
    mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                     grepl("18to19",variable) ~ 18,
                                     grepl("20to29",variable) ~ 25,
                                     grepl("30to39",variable) ~ 35,
                                     grepl("40to49",variable) ~ 45,
                                     grepl("50to59",variable) ~ 55,
                                     grepl("60to69",variable) ~ 65,
                                     grepl("70to79",variable) ~ 75,
                                     grepl("80plus",variable) ~ 85))
  
  
  data2 <- bind_rows(case.plot.data,hosp.plot.data,death.plot.data) %>%
    mutate(
      variable = as.factor(
        case_when(grepl("adjLower",variable) ~ "cases lower",
                  grepl("adjUpper",variable) ~ "cases upper",
                  grepl("adj",variable) ~ "cases",
                  grepl("hosp",variable) ~ "hospitalizations",
                  grepl("death",variable) ~ "deaths")
        )
      ) %>%
    mutate(variable = fct_relevel(
      variable, "deaths","hospitalizations","cases","cases lower","cases upper"
      )) %>%
    filter(!is.na(mid.point.age))
  l <- list(data,data2)
  return(l)
}

plot.data.final <- final.format(plot.data.same,wave)
plot.data.final[[2]] <- left_join(plot.data.final[[2]],
                                  pop,
                                  by=c("Area","mid.point.age")) %>%
  mutate(burden_per_100k = (value/population)*100000)

plot.data.final[[2]] <- plot.data.final[[2]] %>%
  mutate(forColor = paste0(wave," (",variable,")")) %>%
  filter(variable != "cases lower" & variable != "cases upper")

plot.data.final[[2]]$forColor <- factor(
  plot.data.final[[2]]$forColor,
  levels = c("Fall 2020 (cases)","Fall 2020 (hospitalizations)",
             "Fall 2020 (deaths)","Spring 2021 (cases)",
             "Spring 2021 (hospitalizations)","Spring 2021 (deaths)",
             "Fall 2021 (cases)","Fall 2021 (hospitalizations)",
             "Fall 2021 (deaths)"))

# =========== Estimated Variant cases ==================
epicurve <- cases %>%
  mutate(
    cases = est.cases.0to17+est.cases.18to19+est.cases.20to29+est.cases.30to39+
      est.cases.40to49+est.cases.50to59+est.cases.60to69+est.cases.70to79+
      est.cases.80plus
    ) %>%
  group_by(Week.Ending.Date) %>%
  summarise(cases = sum(cases)) 

#Emma B. Hodcroft. 2021. "CoVariants: SARS-CoV-2 Mutations and Variants of Interest." https://covariants.org/

url <- "https://raw.githubusercontent.com/hodcroftlab/covariants/master/cluster_tables/USAClusters_data.json"

var_perc <- fromJSON(txt=url)

var_perc <- var_perc[[1]]$Michigan

var_perc <- data.frame(
  "Week.Ending.Date" = as.Date(var_perc$week)+12,
  "total_sequences" = var_perc$total_sequences,
  "Alpha" = var_perc$`20I (Alpha, V1)`,
  "Delta" = var_perc$`21I (Delta)`+var_perc$`21J (Delta)`+var_perc$`21A (Delta)`
  )

var_perc <- cbind(var_perc,
                  binconf(var_perc$Alpha,
                          var_perc$total_sequences,
                          method = "wilson"), 
                  binconf(var_perc$Delta,
                          var_perc$total_sequences,
                          method = "wilson"))

var_perc_long <- bind_rows(var_perc[,c(1,5,6,7)],var_perc[,c(1,8,9,10)]) 

var_perc_long$variant <- c(
  rep("Alpha",nrow(var_perc)),rep("Delta",nrow(var_perc))
  )

var_perc_long <- var_perc_long %>%
  filter(Week.Ending.Date > "2020-12-01") %>%
  filter(variant == "Alpha" | Week.Ending.Date > "2021-04-01" ) %>%
  filter(variant == "Delta" | Week.Ending.Date < "2021-08-22")

names(var_perc) <- c(
  names(var_perc)[1:4],
  "p_alpha","p_alpha_l","p_alpha_u","p_delta","p_delta_l","p_delta_u"
  )

epicurve <- left_join(epicurve,var_perc,by="Week.Ending.Date") %>%
  fill(p_alpha, .direction="up") %>%
  fill(p_delta, .direction="up") %>%
  fill(p_alpha, .direction="down") %>%
  fill(p_delta, .direction="down") %>%
  mutate(alpha = cases*p_alpha,
         delta = cases*p_delta,
         nonVariant = cases-alpha-delta)

p_var <- epicurve %>%
  select(Week.Ending.Date,p_alpha,p_delta)


alpha_estimates <- left_join(plot.data.same,p_var,by="Week.Ending.Date") %>%
  mutate(p_alpha = ifelse(is.na(p_alpha),0,p_alpha),
         lineage = "Spring 2021: Alpha") %>%
  mutate_at(vars(starts_with("deaths")),~.*p_alpha) %>%
  mutate_at(vars(starts_with("hosp")),~.*p_alpha) %>%
  mutate_at(vars(starts_with("est.cases")),~.*p_alpha) %>%
  mutate_at(vars(starts_with("adj")),~.*p_alpha) %>%
  select(-starts_with("p_"), -wave)

delta_estimates <- left_join(plot.data.same,p_var,by="Week.Ending.Date") %>%
  mutate(p_delta = ifelse(is.na(p_delta),0,p_delta),
         lineage = "Fall 2021: Delta") %>%
  mutate_at(vars(starts_with("deaths")),~.*p_delta) %>%
  mutate_at(vars(starts_with("hosp")),~.*p_delta) %>%
  mutate_at(vars(starts_with("est.cases")),~.*p_delta) %>%
  mutate_at(vars(starts_with("adj")),~.*p_delta) %>%
  select(-starts_with("p_"), -wave)

other_estimates <- left_join(plot.data.same,p_var,by="Week.Ending.Date") %>%
  mutate(p_nonvar = 1-(p_delta+p_alpha),
         lineage = "Spring 2021: Non Variant") %>%
  mutate_at(vars(starts_with("deaths")),~.*p_nonvar) %>%
  mutate_at(vars(starts_with("hosp")),~.*p_nonvar) %>%
  mutate_at(vars(starts_with("est.cases")),~.*p_nonvar) %>%
  mutate_at(vars(starts_with("adj")),~.*p_nonvar) %>%
  select(-starts_with("p_"), -wave)

plot.data.var <- bind_rows(
  alpha_estimates,delta_estimates,other_estimates
  ) %>%
  mutate(
    lineage = ifelse(Week.Ending.Date <= "2021-01-02",
                     "Fall 2020: Non Variant",
                     lineage)
    )

plot.data.final.var <- final.format(plot.data.var,lineage)
plot.data.final.var[[2]] <- left_join(
  plot.data.final.var[[2]],
  pop,
  by=c("Area","mid.point.age")
  ) %>%
  mutate(burden_per_100k = (value/population)*100000) %>%
  filter(variable != "cases lower" & variable != "cases upper")

plot.data.final.var[[2]] <- plot.data.final.var[[2]] %>%
  mutate(forColor = paste0(gsub(".*: ","",lineage)," (",variable,")"))

plot.data.final.var[[2]]$forColor <- factor(
  plot.data.final.var[[2]]$forColor,
  levels = c("Non Variant (cases)","Non Variant (hospitalizations)",
             "Non Variant (deaths)","Alpha (cases)","Alpha (hospitalizations)",
             "Alpha (deaths)","Delta (cases)","Delta (hospitalizations)",
             "Delta (deaths)")
  )
plot.data.final.var[[2]]$lineage <- factor(
  plot.data.final.var[[2]]$lineage ,
  levels = c("Fall 2020: Non Variant","Spring 2021: Alpha",
             "Spring 2021: Non Variant","Fall 2021: Delta")
  )

# =========== Figure 4 ==================
hosp.plot.data <- plot.data.final[[1]] %>%
  filter(grepl("p.hosp",variable)) %>%
  mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                   grepl("18to19",variable) ~ 18,
                                   grepl("20to29",variable) ~ 25,
                                   grepl("30to39",variable) ~ 35,
                                   grepl("40to49",variable) ~ 45,
                                   grepl("50to59",variable) ~ 55,
                                   grepl("60to69",variable) ~ 65,
                                   grepl("70to79",variable) ~ 75,
                                   grepl("80plus",variable) ~ 85),
         wave = factor(wave,
                       levels = c("Fall 2020",
                                  "Spring 2021",
                                  "Fall 2021")))

hospEst <- hosp.plot.data %>%
  filter(!grepl("Lower",variable) & !grepl("Upper",variable)) %>%
  select(-variable)
hospLower <- hosp.plot.data %>%
  filter(grepl("Lower",variable)) %>%
  rename(Lower = value) %>%
  select(-variable)
hospUpper <- hosp.plot.data %>%
  filter(grepl("Upper",variable)) %>%
  rename(Upper = value) %>%
  select(-variable)

hosp.plot.data <- left_join(
  hospEst,
  hospLower,
  by=c("Area","wave","mid.point.age")
  )
hosp.plot.data <- left_join(
  hosp.plot.data,
  hospUpper,
  by=c("Area","wave","mid.point.age")
  )

death.plot.data <- plot.data.final[[1]] %>%
  filter(grepl("p.died",variable)) %>%
  mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                   grepl("18to19",variable) ~ 18,
                                   grepl("20to29",variable) ~ 25,
                                   grepl("30to39",variable) ~ 35,
                                   grepl("40to49",variable) ~ 45,
                                   grepl("50to59",variable) ~ 55,
                                   grepl("60to69",variable) ~ 65,
                                   grepl("70to79",variable) ~ 75,
                                   grepl("80plus",variable) ~ 85),
         wave = factor(wave,
                       levels = c("Fall 2020",
                                  "Spring 2021",
                                  "Fall 2021")))

deathEst <- death.plot.data %>%
  filter(!grepl("Lower",variable) & !grepl("Upper",variable)) %>%
  select(-variable)
deathLower <- death.plot.data %>%
  filter(grepl("Lower",variable)) %>%
  rename(Lower = value) %>%
  select(-variable)
deathUpper <- death.plot.data %>%
  filter(grepl("Upper",variable)) %>%
  rename(Upper = value) %>%
  select(-variable)

death.plot.data <- left_join(
  deathEst,
  deathLower,
  by=c("Area","wave","mid.point.age")
  )
death.plot.data <- left_join(
  death.plot.data,
  deathUpper,
  by=c("Area","wave","mid.point.age")
  )

hosp.plot.data.var <- plot.data.final.var[[1]] %>%
  filter(grepl("p.hosp",variable)) %>%
  mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                   grepl("18to19",variable) ~ 18,
                                   grepl("20to29",variable) ~ 25,
                                   grepl("30to39",variable) ~ 35,
                                   grepl("40to49",variable) ~ 45,
                                   grepl("50to59",variable) ~ 55,
                                   grepl("60to69",variable) ~ 65,
                                   grepl("70to79",variable) ~ 75,
                                   grepl("80plus",variable) ~ 85),
         lineage = factor(lineage,
                          levels = c("Fall 2020: Non Variant",
                                     "Spring 2021: Alpha",
                                     "Spring 2021: Non Variant",
                                     "Fall 2021: Delta")))

hospEst <- hosp.plot.data.var %>%
  filter(!grepl("Lower",variable) & !grepl("Upper",variable)) %>%
  select(-variable)
hospLower <- hosp.plot.data.var %>%
  filter(grepl("Lower",variable)) %>%
  rename(Lower = value) %>%
  select(-variable)
hospUpper <- hosp.plot.data.var %>%
  filter(grepl("Upper",variable)) %>%
  rename(Upper = value) %>%
  select(-variable)

hosp.plot.data.var <- left_join(
  hospEst,
  hospLower,
  by=c("Area","lineage","mid.point.age")
  )
hosp.plot.data.var <- left_join(
  hosp.plot.data.var,
  hospUpper,
  by=c("Area","lineage","mid.point.age")
  ) %>%
  filter(lineage != "Fall 2020: Non Variant")

death.plot.data.var <- plot.data.final.var[[1]] %>%
  filter(grepl("p.died",variable)) %>%
  mutate(mid.point.age = case_when(grepl("0to17",variable) ~ 9,
                                   grepl("18to19",variable) ~ 18,
                                   grepl("20to29",variable) ~ 25,
                                   grepl("30to39",variable) ~ 35,
                                   grepl("40to49",variable) ~ 45,
                                   grepl("50to59",variable) ~ 55,
                                   grepl("60to69",variable) ~ 65,
                                   grepl("70to79",variable) ~ 75,
                                   grepl("80plus",variable) ~ 85),
         lineage = factor(lineage,
                          levels = c("Fall 2020: Non Variant",
                                     "Spring 2021: Alpha",
                                     "Spring 2021: Non Variant",
                                     "Fall 2021: Delta")))

deathEst <- death.plot.data.var %>%
  filter(!grepl("Lower",variable) & !grepl("Upper",variable)) %>%
  select(-variable)
deathLower <- death.plot.data.var %>%
  filter(grepl("Lower",variable)) %>%
  rename(Lower = value) %>%
  select(-variable)
deathUpper <- death.plot.data.var %>%
  filter(grepl("Upper",variable)) %>%
  rename(Upper = value) %>%
  select(-variable)

death.plot.data.var <- left_join(
  deathEst,
  deathLower,
  by=c("Area","lineage","mid.point.age")
  )
death.plot.data.var <- left_join(
  death.plot.data.var,
  deathUpper,
  by=c("Area","lineage","mid.point.age")
  ) %>%
  filter(lineage != "Fall 2020: Non Variant")

p1 <- ggplot(hosp.plot.data[hosp.plot.data$Area == "Michigan", ], 
             aes(x=mid.point.age, 
                 y = value, 
                 group = wave, 
                 color = wave,
                 fill=wave)) + 
  geom_line() +
  geom_ribbon(
    aes(ymin=Upper, ymax=Lower, fill = wave), alpha=0.5, color = NA
    ) +
  scale_color_manual(
    name="",values=c("lightsteelblue4","mediumpurple4","palegreen4")
    ) +
  scale_fill_manual(
    name="",values=c("lightsteelblue4","mediumpurple4","palegreen4")
    ) +
  scale_y_continuous(breaks=seq(0,.25,.05),limits = c(0,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(9,18,25,35,45,55,65,75,85)) +
  ggtitle("A") +
  theme_classic() +
  ylab("Percent of Cases Hospitalized") + 
  xlab("Age Group Mid-point Age (years)")

p2 <- ggplot(hosp.plot.data.var[hosp.plot.data.var$Area == "Michigan", ], 
             aes(x=mid.point.age, 
                 y = value, 
                 group = lineage, 
                 color = lineage,
                 fill=lineage)) + 
  geom_line() +
  geom_ribbon(
    aes(ymin=Upper, ymax=Lower, fill = lineage), alpha=0.5, color = NA
    ) +
  scale_linetype_manual(values=c("solid", "dashed", "dashed")) +
  scale_color_manual(
    name="",values=c("firebrick4","lightsteelblue4","palegreen4"),
    labels = c("Spring 2021:\nAlpha\n","Spring 2021:\nNon Variant\n",
               "Fall 2021:\nDelta\n")
    ) +
  scale_fill_manual(
    name="",values=c("firebrick4","lightsteelblue4","palegreen4"),
    labels = c("Spring 2021:\nAlpha\n","Spring 2021:\nNon Variant\n",
               "Fall 2021:\nDelta\n")
    ) +
  scale_y_continuous(breaks=seq(0,.25,.05),limits = c(0,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(9,18,25,35,45,55,65,75,85)) +
  ggtitle("B") +
  theme_classic()  +
  ylab("Percent of Cases Hospitalized") + 
  xlab("Age Group Mid-point Age (years)")

p3 <- ggplot(death.plot.data[death.plot.data$Area == "Michigan", ], 
             aes(x=mid.point.age, 
                 y = value, 
                 group = wave, 
                 color = wave,
                 fill=wave)) + 
  geom_line() +
  geom_ribbon(aes(ymin=Upper, ymax=Lower, fill = wave), alpha=0.5, color = NA) +
  scale_color_manual(
    name="",values=c("lightsteelblue4","mediumpurple4","palegreen4")
    ) +
  scale_fill_manual(
    name="",values=c("lightsteelblue4","mediumpurple4","palegreen4")
    ) +
  scale_y_continuous(breaks=seq(0,.25,.05),limits = c(0,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(9,18,25,35,45,55,65,75,85)) +
  ggtitle("C") +
  theme_classic() +
  ylab("Percent of Cases Who Died") + 
  xlab("Age Group Mid-point Age (years)")

p4 <- ggplot(death.plot.data.var[death.plot.data.var$Area == "Michigan", ], 
             aes(x=mid.point.age, 
                 y = value, 
                 group = lineage, 
                 color = lineage,
                 fill=lineage)) + 
  geom_line() +
  geom_ribbon(
    aes(ymin=Upper, ymax=Lower, fill = lineage), alpha=0.5, color = NA
    ) +
  scale_linetype_manual(values=c("solid", "dashed", "dashed")) +
  scale_color_manual(
    name="",values=c("firebrick4","lightsteelblue4","palegreen4"),
    labels = c("Spring 2021:\nAlpha\n","Spring 2021:\nNon Variant\n",
               "Fall 2021:\nDelta\n")
    ) +
  scale_fill_manual(
    name="",
    values=c("firebrick4","lightsteelblue4","palegreen4"),
    labels = c("Spring 2021:\nAlpha\n","Spring 2021:\nNon Variant\n",
               "Fall 2021:\nDelta\n")
    ) +
  scale_y_continuous(breaks=seq(0,.25,.05),limits = c(0,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks=c(9,18,25,35,45,55,65,75,85)) +
  ggtitle("D") +
  theme_classic() +
  ylab("Percent of Cases Who Died") + 
  xlab("Age Group Mid-point Age (years)")

#save figure
pdf("figure4.pdf",
    width = 10, height = 8)
  grid.arrange(p1,p2,p3,p4,nrow=2)
dev.off()
jpeg("figure4.jpg",
    width = 10, height = 8, units="in",res=800)
grid.arrange(p1,p2,p3,p4,nrow=2)
dev.off()
