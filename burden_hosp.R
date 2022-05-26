###Project: B.1.1.7 Burden
###Purpose: Prepare Hospitalization Data by Age and Region
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "/conf_withoutMDOC_phregion_ageyear.csv"
###Input: "/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv"
###Input: "EMResource Hospitals by County_07022020.xlsx"
###Output: "burden_region_hosp.csv"

# =========== Load Packages and set filepaths ==================
library(tidyverse)
library(openxlsx)
library(lubridate)
library(reshape2)

lfp<- "original/data/filepath"
ofp<- "processed/data/filepath"

# =========== Read Data ==================
mdss <- read.csv(paste0(lfp,"/conf_withoutMDOC_phregion_ageyear.csv"),
                 stringsAsFactors = FALSE)

hhs <- read.csv(paste0(lfp,"/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv"),
                 stringsAsFactors = FALSE)

map <- read.xlsx(paste0(lfp,"/EMResource Hospitals by County_07022020.xlsx"))

# =========== Reformat HHS hospitalization data and map facilities to counties ==================
#prepare hhs data for merging to county
hhs <- hhs %>%
  #keep only Michigan hospitals
  filter(state == "MI") %>%
  rename(Hospital.Name = hospital_name) %>%
  #remove any non-alphabetic characters from hospital name
  mutate(Hospital.Name = tolower(gsub("[^A-Za-z ]", " ", Hospital.Name))) 

#remove the word hospital from hospital name
hhs$Hospital.Name <- gsub("hospital", "", hhs$Hospital.Name)
#recode and multiple spaces to single and trim any leading or trailing spaces
hhs$Hospital.Name <- gsub("\\s+", " ", str_trim(hhs$Hospital.Name))

#remove second County column
map <- map[,-ncol(map)] %>%
  #remove any non-alphabetic characters from hospital name
  mutate(Hospital.Name = tolower(gsub("[^A-Za-z ]", " ", Hospital.Name))) 

#remove the word hospital from hospital name
map$Hospital.Name <- gsub("hospital", "", map$Hospital.Name)
#recode and multiple spaces to single and trim any leading or trailing spaces
map$Hospital.Name <- gsub("\\s+", " ", str_trim(map$Hospital.Name))

#get unique hospital names
hhs.unique <- hhs %>%
  filter(!duplicated(Hospital.Name))
#merge to map
map2 <- left_join(hhs.unique, map, by="Hospital.Name")

#sum(is.na(map2$Region))
#mapped all but 36 - not bad

#manual recodes: look up matching hospital names and recode based on function below
copy.county <- function(name1,name2){
  map2[map2$Hospital.Name==name1,95:98] <- map[map$Hospital.Name==name2,c(1,3:5)]
  return(map2)
}

map2 <- copy.county("lakeland st joseph","lakeland st joseph adult mh")
map2 <- copy.county("ascension borgess lee","borgess lee memorial")
map2 <- copy.county("ascension macomb oakland hosp warren campus","ascension macomb oakland warren")
map2 <- copy.county("ascension providence","ascension providence southfield")
map2 <- copy.county("aspirus iron river clinics inc","aspirus iron river")
map2 <- copy.county("aspirus ontonagon inc","aspirus ontonagon")
map2 <- copy.county("bell","up health system bell")
map2 <- copy.county("chippewa county war memorial","war memorial")
map2 <- copy.county("covenant medical center","covenant healthcare")
map2 <- copy.county("detroit receiving univ health center","detroit receiving hosp univ health ctr")
map2 <- copy.county("dickinson county memorial","dickinson memorial health care system")
map2 <- copy.county("edward w sparrow","sparrow")
map2 <- copy.county("harper university","harper univ hutzel women s s")
map2 <- copy.county("hills dales general","hills and dales general")
map2 <- copy.county("insight surgical","ascension macomb oakland warren")#not same hospital, but both in Warren
map2 <- copy.county("lake huron medical center","lake huron medical center was sjmph")
map2 <- copy.county("mackinac straits and health center","mackinac straits")
map2 <- copy.county("mckenzie health system","mckenzie memorial")
map2 <- copy.county("mclaren northern michigan","mclaren northern michigan petoskey")
map2 <- copy.county("mercy health muskegon","mercy health mercy campus")
map2 <- copy.county("munson healthcare otsego memorial","otsego memorial")
map2 <- copy.county("north ottawa community health system","north ottawa community hosp")
map2 <- copy.county("promedica charles and virginia hickman","promedica bixby")
map2 <- copy.county("saint joseph mercy livingston","st joseph mercy livingston")
map2 <- copy.county("saint mary s health care","mercy health st mary s main campus")
map2 <- copy.county("select specialty macomb county","ascension macomb oakland warren")#not same hospital, but both in Macomb
map2 <- copy.county("select specialty spectrum health","select specialty grand rapids")
map2 <- copy.county("select specialty wyandotte llc","select specialty downriver")
map2 <- copy.county("sparrow carson","sparrow carson city")
map2 <- copy.county("sparrow specialty","sparrow")
map2 <- copy.county("spectrum health butterworth campus","spectrum health butterworth")
map2 <- copy.county("spectrum health zeeland community","spectrum health zeeland")
map2 <- copy.county("st francis","osf st francis")
map2 <- copy.county("straith for special surgery","straith")
map2 <- copy.county("university of michigan health system","university of mi s health ctr")
map2 <- copy.county("vibra of southeastern mi taylor campus","vibra of south eastern michigan")

#sum(is.na(map2$Region))
#all now mapped

map2 <- map2 %>%
  select(Hospital.Name,Region,County)

hhs <- left_join(hhs, map2, by = "Hospital.Name") %>%
  mutate(collection_week = as.Date(collection_week, format = "%Y/%m/%d"))

#sum(is.na(hhs$Region))
#sum(is.na(hhs$County))
#still all good


hhs <- hhs %>%
  mutate(previous_day_admission_adult_covid_confirmed_7_day_sum = case_when(
    is.na(previous_day_admission_adult_covid_confirmed_7_day_sum) ~ 0,
    previous_day_admission_adult_covid_confirmed_7_day_sum ==-999999 ~ 0,
    TRUE ~ as.double(previous_day_admission_adult_covid_confirmed_7_day_sum)
  ),
  previous_day_admission_pediatric_covid_confirmed_7_day_sum = case_when(
    is.na(previous_day_admission_pediatric_covid_confirmed_7_day_sum) ~ 0,
    previous_day_admission_pediatric_covid_confirmed_7_day_sum ==-999999 ~ 0,
    TRUE ~ as.double(previous_day_admission_pediatric_covid_confirmed_7_day_sum)
  ),
  previous_day_admission_adult_covid_suspected_7_day_sum = case_when(
    is.na(previous_day_admission_adult_covid_suspected_7_day_sum) ~ 0,
    previous_day_admission_adult_covid_suspected_7_day_sum ==-999999 ~ 0,
    TRUE ~ as.double(previous_day_admission_adult_covid_suspected_7_day_sum)
  ),
  previous_day_admission_pediatric_covid_suspected_7_day_sum = case_when(
    is.na(previous_day_admission_pediatric_covid_suspected_7_day_sum) ~ 0,
    previous_day_admission_pediatric_covid_suspected_7_day_sum ==-999999 ~ 0,
    TRUE ~ as.double(previous_day_admission_pediatric_covid_suspected_7_day_sum)
  ),
  previous_day_covid_ED_visits_7_day_sum = case_when(
    is.na(previous_day_covid_ED_visits_7_day_sum) ~ 0,
    previous_day_covid_ED_visits_7_day_sum ==-999999 ~ 0,
    TRUE ~ as.double(previous_day_covid_ED_visits_7_day_sum)
  ))

#group to region

hhs_region <- hhs %>% 
  group_by(collection_week,Region) %>%
  summarise(previous_day_admission_adult_covid_confirmed_7_day_sum = 
              sum(previous_day_admission_adult_covid_confirmed_7_day_sum),                      
            previous_day_admission_pediatric_covid_confirmed_7_day_sum = 
              sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum),
            previous_day_admission_adult_covid_suspected_7_day_sum = 
           sum(previous_day_admission_adult_covid_suspected_7_day_sum),                      
         previous_day_admission_pediatric_covid_suspected_7_day_sum = 
           sum(previous_day_admission_pediatric_covid_suspected_7_day_sum),
         previous_day_covid_ED_visits_7_day_sum = sum(previous_day_covid_ED_visits_7_day_sum))

hhs_region <- as.data.frame(hhs_region) %>% 
  mutate(Area=paste("Region",Region),
         collection_week = collection_week+1)

# =========== Use MDSS hospitalization reports to infer age distribution ==================
#convert dates
mdss <- mdss %>%
  mutate(Date = as.Date(Date))

#filter out deaths and cases; missing regions
mdss <- as.data.frame(mdss) %>%
  filter(Area != 0 & Metric == "Hospitalization") %>%
  select(-Geography,-Metric)

#convert wide to long
long.hosp <- gather(mdss, Age, hosp, 
                     X0:X999, factor_key=TRUE) %>%
  mutate(Age = gsub("X","",Age)) %>%
  filter(Age != 999)

#create aggregation variables
long.hosp <- long.hosp %>%
  mutate(AgeGroup = case_when(Age < 18 ~ "0to17",
                              Age < 20 ~ "18to19",
                              Age < 30 ~ "20to29",
                              Age < 40 ~ "30to39",
                              Age < 50 ~ "40to49",
                              Age < 60 ~ "50to59",
                              Age < 70 ~ "60to69",
                              Age < 80 ~ "70to79",
                              Age >= 80 ~ "80plus"),
         collection_week = ceiling_date(Date, "week")-1)

#get weekly case counts by age for each region
long.hosp.week <- long.hosp %>% 
  group_by(Area, AgeGroup, collection_week) %>% 
  summarise(hosp = sum(hosp))

#convert long to wide
wide.hosp.week <- long.hosp.week %>% 
  pivot_wider(names_from = AgeGroup, values_from = c(hosp))

names(wide.hosp.week)[3:11] <- paste0("X",names(wide.hosp.week)[3:11])

#calculate total adult hospitaliztions
wide.hosp.week <- as.data.frame(wide.hosp.week) %>%
  mutate(
    total.mdss.adult = X18to19+X20to29+X30to39+X40to49+X50to59+X60to69+X70to79+X80plus
    )

#calculate proportion of total hospitalizations that occurred in each age group
wide.hosp.week <- as.data.frame(wide.hosp.week) %>%
  mutate(p.18to19 = ifelse(total.mdss.adult==0, 0, X18to19/total.mdss.adult),
         p.20to29 = ifelse(total.mdss.adult==0, 0, X20to29/total.mdss.adult),
         p.30to39 = ifelse(total.mdss.adult==0, 0, X30to39/total.mdss.adult),
         p.40to49 = ifelse(total.mdss.adult==0, 0, X40to49/total.mdss.adult),
         p.50to59 = ifelse(total.mdss.adult==0, 0, X50to59/total.mdss.adult),
         p.60to69 = ifelse(total.mdss.adult==0, 0, X60to69/total.mdss.adult),
         p.70to79 = ifelse(total.mdss.adult==0, 0, X70to79/total.mdss.adult),
         p.80plus = ifelse(total.mdss.adult==0, 0, X80plus/total.mdss.adult))

#merge mdss hospitalizations and hhs data and calculate total hospitalizations by week, age, and region
wide.hosp.week <- left_join(
  wide.hosp.week, hhs_region, by=c("Area","collection_week")
  ) %>%
  mutate(total_adult = previous_day_admission_adult_covid_confirmed_7_day_sum + 
           previous_day_admission_adult_covid_suspected_7_day_sum,
         total_ped = previous_day_admission_pediatric_covid_confirmed_7_day_sum +
           previous_day_admission_pediatric_covid_suspected_7_day_sum,
         final.0to17 = total_ped,
         final.18to19 = p.18to19 * total_adult,
         final.20to29 = p.20to29 * total_adult,
         final.30to39 = p.30to39 * total_adult,
         final.40to49 = p.40to49 * total_adult,
         final.50to59 = p.50to59 * total_adult,
         final.60to69 = p.60to69 * total_adult,
         final.70to79 = p.70to79 * total_adult,
         final.80plus = p.80plus * total_adult)

#final formatting
wide.hosp.week <- wide.hosp.week %>%
  filter(collection_week>=as.Date("2020-09-01") & Area != 0 & !is.na(Region)) %>%
  select(Area, collection_week,
         final.0to17,final.18to19,final.20to29,final.30to39,final.40to49,
         final.50to59,final.60to69,final.70to79,final.80plus) %>%
  rename(Week.Ending.Date = collection_week,
         hosp.0to17 = final.0to17,
         hosp.18to19 = final.18to19,
         hosp.20to29 = final.20to29,
         hosp.30to39 = final.30to39,
         hosp.40to49 = final.40to49,
         hosp.50to59= final.50to59,
         hosp.60to69 = final.60to69,
         hosp.70to79 = final.70to79,
         hosp.80plus = final.80plus)

# =========== Write ==================
write.csv(wide.hosp.week,paste0(ofp,"/burden_region_hosp.csv"),
          row.names = FALSE,
          na="")