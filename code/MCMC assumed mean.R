################################################################################
#   Code Adapted from Kayoko Shioda et al.Estimating the cumulative incidence of 
#   SARS-CoV-2 infection and the infection fatality ratio in light of waning
#   antibodies. PMID: 33935138. 
#   https://github.com/lopmanlab/SARS-CoV-2_CumInc_WaningAntibodies
#
# 
################################################################################
library(dplyr)
library(tidyr)
library(data.table)

args <- commandArgs()
#------------------------------------------------------------------------------#
# Set up
#------------------------------------------------------------------------------#
# Which age group are you analyzing?
select.age <- args[6] # "0-17", "18-49", "50-64" or "65+"

# What is assumption on mean time from seroconversion to seroreversion in months?
assumed.mean <- as.numeric(args[7])

# What is assumption on standard deviation of time from seroconversion to serorersion in days?
assumed.sd <- as.numeric(args[8])

# Number of iterations to run the MCMC sampler
burn_in_steps <- as.numeric(args[9])
run_steps     <- as.numeric(args[10])
total_steps   <- burn_in_steps + run_steps

# tune sigmas
sigma_rho_1 <- as.numeric(args[11])
sigma_rho_2 <- as.numeric(args[12])
sigma_rho_3 <- as.numeric(args[13])
sigma_rho_4 <- as.numeric(args[14])
sigma_rho_5 <- as.numeric(args[15])

# Do you want to save MCMC outputs?
save.results <- "yes" # "yes" or "no"

# If yes, specify a folder path where you want to save your RData
res.save.folder <- "/save/filepath"

#------------------------------------------------------------------------------#
# Create a function to estimate time between seroconversion and seroreversion
#------------------------------------------------------------------------------#

FQ_dist <- function (t_lag_all, shape1, shape2, scale1, scale2) { 
  pr <- {}
  for (i in 1:length(t_lag_all)) {
    pr[i] <- pweibull(t_lag_all[i], shape1, scale1) - integrate(f = function (y) {
      dweibull(y, shape1, scale1) * (pweibull(t_lag_all[i]-y, shape2, scale2))}, 
      lower=0, upper=t_lag_all[i])$value 
  }
  return(pr)
}

#------------------------------------------------------------------------------#
# Load and reformat the adjusted COVID-19 case data and CDC serosurvey data
#------------------------------------------------------------------------------#

#read estimated daily case counts by region and year of age - long
rfp<- "/read/filepath"

#read CDC seroprevalence data
cdc_seroprev <- read.csv(paste0(rfp,"/MIseroprevalence_full.csv"),
                         stringsAsFactors = FALSE) %>%
  #calculate number seropositive in sample
  mutate(n_pos = round((Estimate/100)*N),
         Date = as.Date(End_Date)) %>%
  #select age group
  filter(!is.na(n_pos) & AgeGroup == select.age)

max_date <- max(cdc_seroprev$Date, na.rm=TRUE)

#Read MDSS Data
mdss <- read.csv(paste0(rfp,"/conf_withoutMDOC_phregion_ageyear.csv"),
                 stringsAsFactors = FALSE)

#convert dates
mdss <- mdss %>%
  mutate(Date = as.Date(Date))

#filter out deaths and hospitalizations; missing regions; and dates after last seroprevalence estimate 
mdss <- as.data.frame(mdss) %>%
  filter(Area != 0 & Metric == "Case Count" & Date <= max_date) %>%
  select(-Geography,-Metric)

#convert wide to long
long.cases <- gather(mdss, Age, cases, 
                     X0:X999, factor_key=TRUE) %>%
  mutate(Age = gsub("X","",Age)) %>%
  filter(Age != 999)

long.cases <- long.cases %>%
  #create age groups matching CDC serosurvey data
  mutate(AgeGroup = case_when(Age<18 ~ "0-17", 
                              Age<50 ~ "18-49", 
                              Age<65 ~ "50-64",
                              Age>=65 ~ "65+"),
         Date = as.Date(Date)) %>%
  #aggregate case counts to day and age group for all of MI
  group_by(Date,AgeGroup) %>%
  summarise(cases = sum(cases)) %>%
  #select age group
  filter(AgeGroup == select.age)

# Load the reported number of COVID-19 associated deaths
rep_counts <- long.cases$cases

num_date <- nrow(long.cases)
cut1 <- which(long.cases$Date=="2020-06-01")
cut2 <- which(long.cases$Date=="2020-10-01")
cut3 <- which(long.cases$Date=="2021-03-01")
cut4 <- which(long.cases$Date=="2021-06-01")

#read population data  
pop <- as.numeric(read.csv(paste0(rfp,"/population_age.csv")) %>%
  #create age groups matching CDC serosurvey data
  mutate(AgeGroup = case_when(age_code < 18 ~ "0-17",
                              age_code < 50 ~ "18-49",
                              age_code < 65 ~ "50-64",
                              age_code >= 65 ~ "65+")) %>%
  #aggregate population by year of age to age groups
  group_by(AgeGroup) %>%
  summarise(population = sum(population)) %>%
  #select age group
  filter(AgeGroup == select.age) %>%
  select(population))


#------------------------------------------------------------------------------#
# Create a log-likelihood function for MCMC
#------------------------------------------------------------------------------#

weibull_hazard_LL <- function(set.rho_1,set.rho_2,set.rho_3,set.rho_4,set.rho_5) {
  
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  # 1. True number of infections
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  
  # Calculate the number of true infections by multiplying the number of cases estimated
  # from reported cases * national adjustment by a scaling factor (set.rho)
  true_inf <- rep(NA, num_date) 
  for (t1 in 1:(num_date)) {
    if(t1<cut1){
      true_inf[t1] <- min(rep_counts[t1]*set.rho_1,pop)# True infections should not exceed the whole population
    } else if(t1<cut2){
      true_inf[t1] <- min(rep_counts[t1]*set.rho_2,pop)
    } else if(t1<cut3){
      true_inf[t1] <- min(rep_counts[t1]*set.rho_3,pop)
    } else if(t1<cut4){
      true_inf[t1] <- min(rep_counts[t1]*set.rho_4,pop)
    } else {
      true_inf[t1] <- min(rep_counts[t1]*set.rho_5,pop)
    }
  }
  
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  # 2. Timeline of seroconversion & seroreversion
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  # 2. Seropositive individuals on Day t
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  
  # Calculate the probability that individuals are seropositive on each day
  FQ_cdf <- FQ_dist(1:num_date, 
                    shape1=shape_conv, scale1=scale_conv, 
                    shape2=shape_rev, scale2=scale_rev) 
  
  # Calculate the number of seropositive individuals on each day
  seropos_cases <- rep(NA, num_date)
  seropos_cases[1] <- 0 # No individuals have seroconverted on Day 1
  for (t2 in 2:num_date) { # On Day 2, 3, ..., last day,
    seropos_cases_day_i <- c() 
    for (k in 1:(t2-1)) {
      seropos_cases_day_i[k] <- true_inf[k] * FQ_cdf[t2-k]
    }
    seropos_cases[t2] <- sum(seropos_cases_day_i)
    seropos_cases[t2] <- min(seropos_cases[t2], pop) # Seropositive cases should not exceed the whole population
  }
  
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  # 3. Log likelihood
  #-----*-----*-----*-----*-----*-----*-----*-----*-----#
  
  log_likl <- c()
  for (round in 1:nrow(cdc_seroprev)) { # For each round of the CDC serosurvey...
    # Calculate the log-likelihood, comparing the simulated seroprevalence and 
    # observed seroprevalence reported by CDC
    log_likl[round] <- dbinom(x = cdc_seroprev$n_pos[round], # Number of seropositives (CDC data)
                              prob = seropos_cases[(long.cases$Date)==cdc_seroprev$Date[round]]/pop, # Simulated seroprevalence
                              size = cdc_seroprev$N[round], # Number of samples collected (CDC data)
                              log=T)
  }
  
  log_LL <- sum(log_likl) # Sum of log-likelihood for all rounds of the CDC serosurvey
  
  return(list(log_LL, seropos_cases))
}

#------------------------------------------------------------------------------#
# Use MCMC to optimize parameters
#------------------------------------------------------------------------------#

# Give starting values for each parameter (Start with national estimates from CDC)
rho_1_0  <- 5   # scaling factor period 1
old_rho_1  <- rho_1_0
rho_2_0  <- 5   # scaling factor period 2
old_rho_2  <- rho_2_0
rho_3_0  <- 5   # scaling factor period 3
old_rho_3  <- rho_3_0
rho_4_0  <- 5   # scaling factor period 4
old_rho_4  <- rho_4_0
rho_5_0 <- 5
old_rho_5 <- rho_5_0
 
# Specify the shape and scale parameters for a Weibull distribution for time
# from symptom onset to seroconversion
shape_conv <- 2.092141 # From Iyer, et al. medRxiv 2020
scale_conv <- 13.00029 # From Iyer, et al. medRxiv 2020

# Calculate the shape and scale parameters for a Weibull distribution for time
# from seroconversion to seroreversion
mean_serorev <- assumed.mean * (365/12) # From PMID: 33935138
sd_serorev <- assumed.sd     # From PMIC: 33935138
sum.r <- (mean_serorev^2 + sd_serorev^2)/mean_serorev^2
fn <- function (x, sum) {lgamma(1+2/x) - 2*lgamma(1+1/x) - log(sum)}
shape_rev <- uniroot(fn, sum=sum.r, c(0.01,100), extendInt = "yes", maxiter = 10000)$root
scale_rev <- mean_serorev/(gamma(1+1/shape_rev))

# Get a starting log_likelihood value
# [[1]] is log_LL. [[2]] is seropos_cases
old_log_LL <- weibull_hazard_LL(old_rho_1, old_rho_2, old_rho_3, old_rho_4, old_rho_5)[[1]] 

# Make an empty vector to store sampled parameters in
sampled_pars  <- matrix(0, total_steps, 5) # 1 column for each parameter
sampled_logLL <- matrix(0, total_steps, 5) # 1 column for each parameter
dt.seropos1   <- matrix(0, total_steps, num_date) # Estimated number of seropositives on each day
dt.seropos2   <- matrix(0, total_steps, num_date) # Estimated number of seropositives on each day
dt.seropos3   <- matrix(0, total_steps, num_date) # Estimated number of seropositives on each day
dt.seropos4   <- matrix(0, total_steps, num_date) # Estimated number of seropositives on each day
dt.seropos5   <- matrix(0, total_steps, num_date) # Estimated number of seropositives on each day
accept_rho_1  <- rep(0, total_steps) # 1 if a new candidate value is accepted. 0 if rejected
accept_rho_2  <- rep(0, total_steps) # 1 if a new candidate value is accepted. 0 if rejected
accept_rho_3  <- rep(0, total_steps) # 1 if a new candidate value is accepted. 0 if rejected
accept_rho_4  <- rep(0, total_steps) # 1 if a new candidate value is accepted. 0 if rejected
accept_rho_5  <- rep(0, total_steps) # 1 if a new candidate value is accepted. 0 if rejected

start_time <- Sys.time()
# Implement MCMC
for (i in 1:total_steps) {
  
  ###############################
  ### rho_1 (scaling factor) ####
  ###############################
  
  # 1. Propose a new value for rho (scaling factor)
  current_rho_1 <- -999
  while(current_rho_1 < 1){  
  current_rho_1  <- rnorm(1,old_rho_1,sigma_rho_1)
  }

  # 2. Proposal ratio (0 because the proposal distribution was symmetric)
  log_proposal_ratio  <- 0.0
  
  # 3. Get the log-likelihood for the newly sampled parameter
  current_log_LL <- weibull_hazard_LL(current_rho_1, old_rho_2, old_rho_3, old_rho_4, old_rho_5)[[1]]
  # This stores the estimated number of seropositives on each day
  ts_seropos     <- weibull_hazard_LL(current_rho_1, old_rho_2, old_rho_3, old_rho_4, old_rho_5)[[2]] 
  
  
  # 4. Log-likelihood ratio for the last and current sampled parameters
  log_likelihood_ratio <- current_log_LL - old_log_LL
  
  # 5. Get the acceptance probability
  acceptance_prob <- exp(log_proposal_ratio + log_likelihood_ratio)
  
  # 6. Draw a random nunber from (0, 1] and if it's less than the acceptance
  # probability, accept the current parameter. Otherwise, stay on the last one.
  if (runif(1) < acceptance_prob & current_rho_1 > 0) { 
    old_rho_1    <- current_rho_1
    old_log_LL <- current_log_LL
    accept_rho_1[i] <- 1 # 1 if accepted. 0 if rejected.
  } 
  
  # 7. Save the current parameter to the trace
  sampled_pars[i,1]  <- old_rho_1
  sampled_logLL[i,1] <- old_log_LL
  dt.seropos1[i,]    <- ts_seropos
  
  ###############################
  ### rho_2 (scaling factor) ####
  ###############################
  
  # 1. Propose a new value for rho (scaling factor)
  current_rho_2 <- -999
  while(current_rho_2 < 1){    
  current_rho_2  <- rnorm(1,old_rho_2,sigma_rho_2)
  }

  # 2. Proposal ratio (0 because the proposal distribution was symmetric)
  log_proposal_ratio  <- 0.0
  
  # 3. Get the log-likelihood for the newly sampled parameter
  current_log_LL <- weibull_hazard_LL(old_rho_1, current_rho_2, old_rho_3, old_rho_4, old_rho_5)[[1]]
  # This stores the estimated number of seropositives on each day
  ts_seropos     <- weibull_hazard_LL(old_rho_1, current_rho_2, old_rho_3, old_rho_4, old_rho_5)[[2]] 
  
  
  # 4. Log-likelihood ratio for the last and current sampled parameters
  log_likelihood_ratio <- current_log_LL - old_log_LL
  
  # 5. Get the acceptance probability
  acceptance_prob <- exp(log_proposal_ratio + log_likelihood_ratio)
  
  # 6. Draw a random nunber from (0, 1] and if it's less than the acceptance
  # probability, accept the current parameter. Otherwise, stay on the last one.
  if (runif(1) < acceptance_prob & current_rho_2 > 0) { 
    old_rho_2    <- current_rho_2
    old_log_LL <- current_log_LL
    accept_rho_2[i] <- 1 # 1 if accepted. 0 if rejected.
  } 
  
  # 7. Save the current parameter to the trace
  sampled_pars[i,2]  <- old_rho_2
  sampled_logLL[i,2] <- old_log_LL
  dt.seropos2[i,]    <- ts_seropos

  ###############################
  ### rho_3 (scaling factor) ####
  ###############################
  
  # 1. Propose a new value for rho (scaling factor)
  current_rho_3 <- -999
  while(current_rho_3 < 1){
  current_rho_3  <- rnorm(1,old_rho_3,sigma_rho_3)
  }

  # 2. Proposal ratio (0 because the proposal distribution was symmetric)
  log_proposal_ratio  <- 0.0
  
  # 3. Get the log-likelihood for the newly sampled parameter
  current_log_LL <- weibull_hazard_LL(old_rho_1, old_rho_2, current_rho_3, old_rho_4, old_rho_5)[[1]]
  # This stores the estimated number of seropositives on each day
  ts_seropos     <- weibull_hazard_LL(old_rho_1, old_rho_2, current_rho_3, old_rho_4, old_rho_5)[[2]] 
  
  
  # 4. Log-likelihood ratio for the last and current sampled parameters
  log_likelihood_ratio <- current_log_LL - old_log_LL
  
  # 5. Get the acceptance probability
  acceptance_prob <- exp(log_proposal_ratio + log_likelihood_ratio)
  
  # 6. Draw a random nunber from (0, 1] and if it's less than the acceptance
  # probability, accept the current parameter. Otherwise, stay on the last one.
  if (runif(1) < acceptance_prob & current_rho_3 > 0) { 
    old_rho_3    <- current_rho_3
    old_log_LL <- current_log_LL
    accept_rho_3[i] <- 1 # 1 if accepted. 0 if rejected.
  } 
  
  # 7. Save the current parameter to the trace
  sampled_pars[i,3]  <- old_rho_3
  sampled_logLL[i,3] <- old_log_LL
  dt.seropos3[i,]    <- ts_seropos  
  
  ###############################
  ### rho_4 (scaling factor) ####
  ###############################
  
  # 1. Propose a new value for rho (scaling factor)
  current_rho_4 <- -999
  while(current_rho_4 < 1){
  current_rho_4  <- rnorm(1,old_rho_4,sigma_rho_4)
  }
  # 2. Proposal ratio (0 because the proposal distribution was symmetric)
  log_proposal_ratio  <- 0.0
  
  # 3. Get the log-likelihood for the newly sampled parameter
  current_log_LL <- weibull_hazard_LL(old_rho_1, old_rho_2, old_rho_3, current_rho_4, old_rho_5)[[1]]
  # This stores the estimated number of seropositives on each day
  ts_seropos     <- weibull_hazard_LL(old_rho_1, old_rho_2, old_rho_3, current_rho_4, old_rho_5)[[2]] 
  
  
  # 4. Log-likelihood ratio for the last and current sampled parameters
  log_likelihood_ratio <- current_log_LL - old_log_LL
  
  # 5. Get the acceptance probability
  acceptance_prob <- exp(log_proposal_ratio + log_likelihood_ratio)
  
  # 6. Draw a random nunber from (0, 1] and if it's less than the acceptance
  # probability, accept the current parameter. Otherwise, stay on the last one.
  if (runif(1) < acceptance_prob & current_rho_4 > 0) { 
    old_rho_4    <- current_rho_4
    old_log_LL <- current_log_LL
    accept_rho_4[i] <- 1 # 1 if accepted. 0 if rejected.
  } 
  
  # 7. Save the current parameter to the trace
  sampled_pars[i,4]  <- old_rho_4
  sampled_logLL[i,4] <- old_log_LL
  dt.seropos4[i,]    <- ts_seropos

  ###############################
  ### rho_5 (scaling factor) ####
  ###############################
  
  # 1. Propose a new value for rho (scaling factor)
  current_rho_5 <- -999
  while(current_rho_5 < 1){
  current_rho_5  <- rnorm(1,old_rho_5,sigma_rho_5)
  }
   # 2. Proposal ratio (0 because the proposal distribution was symmetric)
  log_proposal_ratio  <- 0.0
  
  # 3. Get the log-likelihood for the newly sampled parameter
  current_log_LL <- weibull_hazard_LL(old_rho_1, old_rho_2, old_rho_3, old_rho_4, current_rho_5)[[1]]
  # This stores the estimated number of seropositives on each day
  ts_seropos     <- weibull_hazard_LL(old_rho_1, old_rho_2, old_rho_3, old_rho_4, current_rho_5)[[2]] 
  
  
  # 4. Log-likelihood ratio for the last and current sampled parameters
  log_likelihood_ratio <- current_log_LL - old_log_LL
  
  # 5. Get the acceptance probability
  acceptance_prob <- exp(log_proposal_ratio + log_likelihood_ratio)
  
  # 6. Draw a random nunber from (0, 1] and if it's less than the acceptance
  # probability, accept the current parameter. Otherwise, stay on the last one.
  if (runif(1) < acceptance_prob & current_rho_5 > 0) { 
    old_rho_5    <- current_rho_5
    old_log_LL <- current_log_LL
    accept_rho_5[i] <- 1 # 1 if accepted. 0 if rejected.
  } 
  
  # 7. Save the current parameter to the trace
  sampled_pars[i,5]  <- old_rho_5
  sampled_logLL[i,5] <- old_log_LL
  dt.seropos5[i,]    <- ts_seropos
  
  if(i %in% seq(1,total_steps,ceiling(total_steps/100))){
    end_time <- Sys.time()
    print(round(i/total_steps, 2))
    print(end_time - start_time)
  }
}

# Save MCMC outputs
if (save.results == "yes") {
  accept <- bind_cols(accept_rho_1, accept_rho_2, accept_rho_3, accept_rho_4, accept_rho_5)
  names(accept) <- c("rho_1","rho_2","rho_3","rho_4","rho_5")
  fwrite(accept, file=paste0(res.save.folder,"/","mean_",assumed.mean,"/accept_",select.age,".csv"))
  fwrite(sampled_pars,  file=paste0(res.save.folder,"/","mean_",assumed.mean,"/sampled_pars_",select.age,".csv"))
  fwrite(sampled_logLL, file=paste0(res.save.folder,"/","mean_",assumed.mean,"/sampled_logLL_",select.age,".csv"))
  fwrite(dt.seropos1,    file=paste0(res.save.folder,"/","mean_",assumed.mean,"/dt.seropos1_",select.age,".csv"))
  fwrite(dt.seropos2,    file=paste0(res.save.folder,"/","mean_",assumed.mean,"/dt.seropos2_",select.age,".csv"))
  fwrite(dt.seropos3,    file=paste0(res.save.folder,"/","mean_",assumed.mean,"/dt.seropos3_",select.age,".csv"))
  fwrite(dt.seropos4,    file=paste0(res.save.folder,"/","mean_",assumed.mean,"/dt.seropos4_",select.age,".csv"))
  fwrite(dt.seropos5,    file=paste0(res.save.folder,"/","mean_",assumed.mean,"/dt.seropos5_",select.age,".csv"))
}
