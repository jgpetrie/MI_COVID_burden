# MI_COVID_burden
Code used to estimate SARS-CoV-2 cases, hospitalizations, and deaths by age in Michigan. 

Order of analysis:
1. burden_sero.R
2. Weibull fit.R
3. MCMC assumed mean.R
4. MCMC summary assumed mean.R
   * .sh scripts to run 3. and 4. on server:
      ** mcmc_0_17_assumed_mean.sh
      ** mcmc_18_49_assumed_mean.sh
      ** mcmc_50_64_assumed_mean.sh
      ** mcmc_65plus_assumed_mean.sh
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
