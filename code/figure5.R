###Project: MI COVID Burden
###Purpose: Figure 5
###Author: Josh Petrie
###Date: 12/23/2021

###Input: "estimated_weekly_region_cases_wide.csv"
###Input: "burden_region_vaccine_adjusted_age_groups.csv"
###Input: "estimated_population_by_age.csv"

###Output: "figure5.pdf"
# =========== Load Libraries ==================
library(data.table)
library(tidyverse)
library(gridExtra)

# =========== Read data ==================
pfp <- "processed/data/filepath"

cases <- fread(
  paste0(pfp,"/estimated_weekly_region_cases_wide.csv"),
  stringsAsFactors = FALSE
  ) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  filter(Week.Ending.Date <= "2021-11-13")

vaccine_adj <- fread(
  paste0(pfp,"/burden_region_vaccine_adjusted_age_groups.csv"),
                     stringsAsFactors = FALSE
  ) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  arrange(PH_Region,Week.Ending.Date) %>% 
  rename(Area = PH_Region)

pop <- fread(
  paste0(pfp,"/estimated_population_by_age.csv"),
  stringsAsFactors = FALSE
  ) %>% 
  rename(Area = PH_Region)

# =========== Get cumulative infections and vaccinations ==================

cumulativeCases <- cases %>%
  arrange(Area,Week.Ending.Date) %>%
  group_by(Area) %>%
  mutate_if(is.numeric,cumsum)

est.long <- cumulativeCases %>%
  pivot_longer(cols = starts_with("est."),
               names_to = "AgeGroup",
               values_to = "estCumCases") %>%
  filter(!grepl("Upper",AgeGroup) & !grepl("Lower",AgeGroup)) %>%
  mutate(AgeGroup = gsub("est.cases.","",AgeGroup))

estLower.long <- cumulativeCases %>%
  pivot_longer(cols = starts_with("est."),
               names_to = "AgeGroup",
               values_to = "estCumCasesLower") %>%
  filter(grepl("Lower",AgeGroup)) %>%
  mutate(AgeGroup = gsub("est.casesLower.","",AgeGroup))

estUpper.long <- cumulativeCases %>%
  pivot_longer(cols = starts_with("est."),
               names_to = "AgeGroup",
               values_to = "estCumCasesUpper") %>%
  filter(grepl("Upper",AgeGroup)) %>%
  mutate(AgeGroup = gsub("est.casesUpper.","",AgeGroup))

cumulativeCases.long <- left_join(est.long,estLower.long,by=c("Area", "Week.Ending.Date","AgeGroup"))
cumulativeCases.long <- left_join(cumulativeCases.long,estUpper.long,
                                  by=c("Area", "Week.Ending.Date","AgeGroup")) %>%
  filter(AgeGroup != "MissingAge")

CasesAndVaccines <- left_join(cumulativeCases.long, 
                              vaccine_adj, 
                              by=c("Area","AgeGroup","Week.Ending.Date"))

CasesAndVaccines[is.na(CasesAndVaccines)] <- 0

CasesAndVaccines <- left_join(CasesAndVaccines, pop, by=c("Area","AgeGroup"))

CasesAndVaccines <- CasesAndVaccines %>%
  mutate(
    perc.infected = estCumCases/population,
    perc.infectedLower = estCumCasesLower/population,
    perc.infectedUpper = estCumCasesUpper/population,
    perc.vaccinated = cumulative.doses/population,
    
    cumulative.immune = population-
      (population*((1-perc.infected)*(1-perc.vaccinated))),
    cumulative.immuneLower = population-
      (population*((1-perc.infectedLower)*(1-perc.vaccinated))),
    cumulative.immuneUpper = population-
      (population*((1-perc.infectedUpper)*(1-perc.vaccinated))),
    
    perc.immune = cumulative.immune/population,
    perc.immuneLower = cumulative.immuneLower/population,
    perc.immuneUpper = cumulative.immuneUpper/population,
    
    SuscPopulation = population-cumulative.immune,
    SuscPopulationLower = population-cumulative.immuneLower,
    SuscPopulationUpper = population-cumulative.immuneUpper,
    
    wave = case_when(
      Week.Ending.Date>=as.Date("2020-10-11") &
        Week.Ending.Date<=as.Date("2021-01-30") ~ "Fall 2020",
      Week.Ending.Date>=as.Date("2021-02-28") &
        Week.Ending.Date<=as.Date("2021-06-19")~ "Spring 2021", 
      Week.Ending.Date>=as.Date("2021-07-24") &
        Week.Ending.Date<=as.Date("2021-11-13")~ "Fall 2021",
      TRUE ~ NA_character_)
    )

CasesAndVaccinesAge <- CasesAndVaccines %>%
  group_by(AgeGroup,Week.Ending.Date) %>%
  summarise(cumulative.cases =  sum(estCumCases),
            cumulative.doses = sum(cumulative.doses),
            cumulative.immune =  sum(cumulative.immune),
            population = sum(population)) %>%
  mutate(perc.infected = cumulative.cases/population,
         perc.immune = cumulative.immune/population,
         perc.vaccinated = cumulative.doses/population) %>%
  filter(Week.Ending.Date < "2021-11-14") 

CasesAndVaccinesRegion <- CasesAndVaccines %>%
  group_by(Area,Week.Ending.Date) %>%
  summarise(cumulative.cases =  sum(estCumCases),
            cumulative.doses = sum(cumulative.doses),
            cumulative.immune =  sum(cumulative.immune),
            population = sum(population)) %>%
  mutate(perc.infected = cumulative.cases/population,
         perc.vaccinated = cumulative.doses/population,
         perc.immune = cumulative.immune/population) %>%
  filter(Week.Ending.Date < "2021-11-14")

###RESULTS text
CasesAndVaccinesAge$perc.infected[
  CasesAndVaccinesAge$Week.Ending.Date=="2020-10-10"
  ]
CasesAndVaccinesAge$perc.infected[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-01-30"
  ]
sum(
  CasesAndVaccinesAge$cumulative.cases[
    CasesAndVaccinesAge$Week.Ending.Date=="2021-01-30"
    ]
  ) /
  sum(
    CasesAndVaccinesAge$population[
      CasesAndVaccinesAge$Week.Ending.Date=="2021-01-30"
      ]
    )

CasesAndVaccinesAge$perc.infected[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-06-19"
  ]
sum(
  CasesAndVaccinesAge$cumulative.cases[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-06-19"
  ]
  ) /
  sum(
    CasesAndVaccinesAge$population[
      CasesAndVaccinesAge$Week.Ending.Date=="2021-06-19"
      ]
    )

CasesAndVaccinesAge$perc.infected[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-11-13"
  ]
sum(
  CasesAndVaccinesAge$cumulative.cases[
    CasesAndVaccinesAge$Week.Ending.Date=="2021-11-13"
    ]
  ) /
  sum(
    CasesAndVaccinesAge$population[
      CasesAndVaccinesAge$Week.Ending.Date=="2021-11-13"
      ]
    )



CasesAndVaccinesAge$perc.vaccinated[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-02-27"
  ]
CasesAndVaccinesAge$perc.vaccinated[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-11-13"
  ]

CasesAndVaccinesAge$perc.immune[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-06-19"
  ]
CasesAndVaccinesAge$perc.immune[
  CasesAndVaccinesAge$Week.Ending.Date=="2021-11-13"
  ]

###Plot
p1 <- ggplot(
  CasesAndVaccinesAge, 
  aes(x=Week.Ending.Date, y=perc.infected,group=AgeGroup, color = AgeGroup)
  ) +
  geom_line(size=1.25) +
  geom_vline(xintercept = as.Date("2020-10-11"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-02-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-24"), linetype = "dotted") + 
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("A") +
  theme_classic() +
  ylab("Percent Infected") + 
  xlab("Week Ending Date") +
  labs(color='Age Group\n(years)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

p2 <- ggplot(
  CasesAndVaccinesRegion, 
  aes(x=Week.Ending.Date, y=perc.infected,group=Area, color = Area)
  ) +
  geom_line(size=1.25) +
  geom_vline(xintercept = as.Date("2020-10-11"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-02-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-24"), linetype = "dotted") + 
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("B") + 
  theme_classic() +
  ylab("Percent Infected") + 
  xlab("Week Ending Date") +
  labs(color='Region') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

p3 <- ggplot(CasesAndVaccinesAge, aes(x=Week.Ending.Date, y=perc.vaccinated,
                                      group=AgeGroup, color = AgeGroup)) +
  geom_line(size=1.25) +
  geom_vline(xintercept = as.Date("2020-10-11"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-02-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-24"), linetype = "dotted") + 
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("C") + 
  theme_classic() +
  ylab("Percent Vaccinated") + 
  xlab("Week Ending Date") +
  labs(color='Age Group\n(years)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

p4 <- ggplot(
  CasesAndVaccinesRegion, 
  aes(x=Week.Ending.Date, y=perc.vaccinated,group=Area, color = Area)
  ) +
  geom_line(size=1.25) +
  geom_vline(xintercept = as.Date("2020-10-11"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-02-28"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-07-24"), linetype = "dotted") + 
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("D") + 
  theme_classic() +
  ylab("Percent Vaccinated") + 
  xlab("Week Ending Date") + 
  labs(color='Region') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

p5 <- ggplot(
  CasesAndVaccinesAge, 
  aes(x=Week.Ending.Date, y=perc.immune,group=AgeGroup, color = AgeGroup)
  ) +
  geom_line(size=1.25) +
  geom_vline(xintercept = as.Date("2020-10-11"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-02-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-24"), linetype = "dotted") + 
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("E") + 
  theme_classic() +
  ylab("Percent Infected or Vaccinated") + 
  xlab("Week Ending Date") +
  labs(color='Age Group\n(years)') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

p6 <- ggplot(
  CasesAndVaccinesRegion, 
  aes(x=Week.Ending.Date, y=perc.immune,group=Area, color = Area)
  ) +
  geom_line(size=1.25) +
  geom_vline(xintercept = as.Date("2020-10-11"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2021-02-28"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-07-24"), linetype = "dotted") + 
  scale_y_continuous(limits = c(0,1.05), breaks = seq(0,1,.25),
                     labels = scales::percent_format(accuracy = 1)) +
  ggtitle("F") + 
  theme_classic() +
  ylab("Percent Infected or Vaccinated") + 
  xlab("Week Ending Date") + 
  labs(color='Region') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1)) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")

#save figure
pdf("figure5.pdf",
    width = 10, height = 12)
  grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)
dev.off() 
jpeg("figure5.jpg",
    width = 10, height = 12, units="in",res=800)
grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)
dev.off() 
