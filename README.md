# MI_COVID_burden
Code used to estimate total SARS-CoV-2 cases, hospitalizations, and deaths by age in Michigan. 

## Order of analysis:
1. burden_sero.R
2. Weibull fit.R
3. MCMC assumed mean.R
4. MCMC summary assumed mean.R
   * .sh scripts to run 3. and 4. on server:
      * mcmc_0_17_assumed_mean.sh
      * mcmc_18_49_assumed_mean.sh
      * mcmc_50_64_assumed_mean.sh
      * mcmc_65plus_assumed_mean.sh
6. burden_cases.R
7. burden_hosp.R
8. burden_deaths.R
9. burden_vaccine.R
10. population.R
11. figure1.R
12. figure2.R
13. figure3.R
14. figure4.R
15. figure5.R
16. figure6.R

## Data Notes:
1. conf_withoutMDOC_phregion_ageyear_aggregated.csv is an aggregated version of the Michigan Disease Surveillance System (MDSS) case, hospitalization, and death data used in this analysis. It contains weekly event counts for approximately 10 year age groups with cells containing <5 individuals masked. The original analysis used more granular data with daily event counts by 1 year age groups. The code has not yet been updated to use the aggregated data. 
2. Nationwide_Commercial_Laboratory_Seroprevalence_Survey.csv was obtained from: https://covid.cdc.gov/covid-data-tracker/#national-lab
3. Covid_Vaccine_Coverage_by_County_718469_7.xlsx was obtained from: https://www.michigan.gov/documents/coronavirus/Covid_Vaccine_Coverage_by_County_718469_7.xlsx
4. Excess_Deaths_Associated_with_COVID-19.csv was obtained from: https://www.cdc.gov/nchs/nvss/vsrr/covid19/excess_deaths.htm
5. Additional, publicly available data used in the analysis but not posted here:
  * COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv available at: https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u
  * SARS-CoV-2 variant proportions by time: https://covariants.org/
