library(dplyr)
library(data.table)
library(ggplot2)

args <- commandArgs()

age_select <- args[6]
assumed.mean <- as.numeric(args[7])
assumed.sd <- as.numeric(args[8])
burn_in <- as.numeric(args[9])
samples <- as.numeric(args[10])
iter <- burn_in + samples

in_dir <- "/mcmc/output/filepath"
in_dir2 <- "/original/data/filepath"
out_dir <- "/save/results/filepath"


sampled_pars <- fread(paste0(in_dir,"/sampled_pars_",age_select,".csv"),stringsAsFactors = FALSE)
accept_list <- fread(paste0(in_dir,"/accept_",age_select,".csv"),stringsAsFactors = FALSE)


estimate <- c(quantile(sampled_pars$V1[(burn_in+1):iter],.5),
              quantile(sampled_pars$V2[(burn_in+1):iter],.5),
              quantile(sampled_pars$V3[(burn_in+1):iter],.5),
              quantile(sampled_pars$V4[(burn_in+1):iter],.5),
              quantile(sampled_pars$V5[(burn_in+1):iter],.5))

lower <- c(quantile(sampled_pars$V1[(burn_in+1):iter],.025),
              quantile(sampled_pars$V2[(burn_in+1):iter],.025),
              quantile(sampled_pars$V3[(burn_in+1):iter],.025),
              quantile(sampled_pars$V4[(burn_in+1):iter],.025),
              quantile(sampled_pars$V5[(burn_in+1):iter],.025))

upper <- c(quantile(sampled_pars$V1[(burn_in+1):iter],.975),
           quantile(sampled_pars$V2[(burn_in+1):iter],.975),
           quantile(sampled_pars$V3[(burn_in+1):iter],.975),
           quantile(sampled_pars$V4[(burn_in+1):iter],.975),
           quantile(sampled_pars$V5[(burn_in+1):iter],.975))

accept <- c(mean(accept_list$rho_1[(burn_in+1):iter]), 
            mean(accept_list$rho_2[(burn_in+1):iter]), 
            mean(accept_list$rho_3[(burn_in+1):iter]), 
            mean(accept_list$rho_4[(burn_in+1):iter]),
            mean(accept_list$rho_5[(burn_in+1):iter]))

pars <- data.frame("par" = c("rho_1","rho_2","rho_3","rho_4","rho_5"),
                   "estimate" = estimate,
                   "lower" = lower,
                   "upper" = upper,
                   "accept" = accept)

fwrite(pars,paste0(out_dir,"/parameter_estimates_",age_select,".csv"),row.names = FALSE, na="")

png(paste0(out_dir,"/trace_",age_select,".png"))
layout(matrix(1:6,ncol=2,byrow=T))
par(mar=c(2,4,1,1))
plot(1:iter,sampled_pars$V1,type="l", ylab="", main="case scaling MAR - MAY 202")
plot(1:iter,sampled_pars$V2,type="l", ylab="", main="case scaling JUN - SEP 2020")
plot(1:iter,sampled_pars$V3,type="l", ylab="", main="case scaling OCT 2020 - FEB 2021")
plot(1:iter,sampled_pars$V4,type="l", ylab="", main="case scaling MAR - MAY 2021")
plot(1:iter,sampled_pars$V5,type="l", ylab="", main="case scaling JUN - NOV 2021")
dev.off()

pars_matrix <- as.matrix(sampled_pars)
png(paste0(out_dir,"/correlation_",age_select,".png"))
layout(matrix(1:(ncol(pars_matrix)^2),ncol=ncol(pars_matrix),byrow=T))
par(mar=rep(2,4))
  for (i in 1:ncol(pars_matrix)){
    for (j in 1:ncol(pars_matrix)){
      if (i==j){
        x=pars_matrix[,i]
        xmin=min(x)
        xmax=max(x)

        hist(x,xlim=c(xmin,xmax),main = NULL, xlab=paste("Parameter",i))
      }
      else {
        x=pars_matrix[,i]
        xmin=min(x)
        xmax=max(x)

        y=pars_matrix[,j]
        ymin=min(y)
        ymax=max(y)

        plot(x,y,xlim=c(xmin,xmax),ylim=c(ymin,ymax), xlab=paste("Parameter",i)
             , ylab=paste("Parameter",j))
      }
    }
  }

dev.off()


dt.seropos <- as.matrix(fread(paste0(in_dir,"/dt.seropos5_",age_select,".csv"),stringsAsFactors = FALSE))
cdc_seroprev <- fread(paste0(in_dir2,"/MIseroprevalence_full.csv"),stringsAsFactors = FALSE) %>%
  #calculate number seropositive in sample
  mutate(n_pos = round((Estimate/100)*N),
         Date = as.Date(End_Date, format="%m/%d/%Y"),
         x = as.numeric(Date-as.Date("2020-03-01"))) %>%
  #select age group
  filter(!is.na(n_pos) & AgeGroup == age_select)

#read population data  
pop <- as.numeric(read.csv(paste0(in_dir2,"/population_age.csv")) %>%
  #create age groups matching CDC serosurvey data
  mutate(AgeGroup = case_when(age_code < 18 ~ "0-17",
                              age_code < 50 ~ "18-49",
                              age_code < 65 ~ "50-64",
                              age_code >= 65 ~ "65+")) %>%
  #aggregate population by year of age to age groups
  group_by(AgeGroup) %>%
  summarise(population = sum(population)) %>%
  #select age group
  filter(AgeGroup == age_select) %>%
  select(population))

days <- ncol(dt.seropos)

avg.seropos <- rep(NA,days)
for(i in 1:days){
  avg.seropos[i] <- quantile(dt.seropos[,i],.5)/pop
}
upper <- rep(NA,days)
for(i in 1:days){
  upper[i] <- quantile(dt.seropos[,i]/pop,.975)
}
lower <- rep(NA,days)
for(i in 1:days){
  lower[i] <- quantile(dt.seropos[,i]/pop,.025)
}

jpeg(file=paste0(out_dir,"/est_seroprevalence_",age_select,".jpg"))
plot <- ggplot() +
  geom_ribbon(aes(x=seq(1:days),
                  ymin=lower,
                  ymax=upper),
              fill="gray",
              alpha = 50) +
  geom_point(aes(x=seq(1:days),
                 y=avg.seropos)) +
  geom_point(aes(x=cdc_seroprev$x,
                 y=cdc_seroprev$Estimate/100, color="CDC")) +
  geom_errorbar(aes(x=cdc_seroprev$x,
                    ymin=cdc_seroprev$Lower/100, 
                    ymax=cdc_seroprev$Upper/100, color="CDC"), 
                width=1,
                position=position_dodge(0.05)) +
  ylab("Proportion Seropositive") +
  xlab("Days from March 1, 2020") +
  scale_color_manual(labels = c("CDC Estimate"), 
                     values = c("CDC"="firebrick3"), 
                     name = "") +
  theme_classic()
print(plot)
dev.off()
