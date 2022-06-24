###Project: MI COVID Burden
###Purpose: Figure 2
###Author: Josh Petrie
###Date: 12/23/2021
###Input: "conf_withoutMDOC_phregion_ageyear.csv"
###Input: "estimated_weekly_region_cases_wide.csv"
###Input: "estimated_daily_region_cases_wide.csv"

###Output: "figure2.pdf"

# =========== Load Libraries ==================
library(tidyverse)
library(data.table)
library(lubridate)
library(jsonlite)
library(Hmisc)
library(EpiEstim)
library(gridExtra)

# =========== Read Data ==================
ofp <- "original/data/filepath"
pfp <- "processed/data/filepath"

original_cases <- fread(paste0(ofp,"/conf_withoutMDOC_phregion_ageyear.csv"),
                        stringsAsFactors = FALSE)

cases <- fread(paste0(pfp,"/estimated_weekly_region_cases_wide.csv"),
               stringsAsFactors = FALSE) %>%
  mutate(Week.Ending.Date = as.Date(Week.Ending.Date)) %>%
  filter(Week.Ending.Date <= "2021-11-13")

cases.daily <- fread(paste0(pfp,"/estimated_daily_region_cases_wide.csv"),
                     stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date <= "2021-11-13")

# =========== Figure 2, panel A: Original Cases ==================
#convert dates
original_cases <- original_cases %>%
  mutate(Date = as.Date(Date))

#filter out deaths and hospitalizations; missing regions 
original_cases <- as.data.frame(original_cases) %>%
  filter(Area != 0 & Metric == "Case Count") %>%
  mutate(total_count = rowSums(.[3:84]),
         Week.Ending.Date = ceiling_date(Date, "week")-1) %>%
  select(-Geography,-Metric,-c(3:84))

original_cases <- original_cases %>%
  group_by(Week.Ending.Date) %>%
  summarise(total_count=sum(total_count))

###RESULTS text
sum(original_cases$total_count[original_cases$Week.Ending.Date>="2020-10-11" & 
                                 original_cases$Week.Ending.Date<="2021-01-30"])

sum(original_cases$total_count[original_cases$Week.Ending.Date>="2021-02-28" & 
                                 original_cases$Week.Ending.Date<="2021-06-19"])

sum(original_cases$total_count[original_cases$Week.Ending.Date>="2021-07-25" & 
                                 original_cases$Week.Ending.Date<="2021-11-13"])

### Plot
p1 <- ggplot(original_cases, aes(x=Week.Ending.Date, y = total_count)) + 
  geom_area() +
  ggtitle("A") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1),
        legend.spacing.y = unit(-0.2, "cm")) +
  ylab("Weekly Reported Cases") +
  xlab("Week Ending Date") + 
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") + 
  scale_y_continuous(expand = c(0,0), 
                     labels = c("0","10,000","20,000","30,000","40,000"), 
                     breaks=c(0,10000,20000,30000,40000)) +
  scale_fill_manual(name = "", values = c(Total="gray20"))

# =========== Figure 2, panel B: Variant proportions ==================

#Emma B. Hodcroft. 2021. "CoVariants: SARS-CoV-2 Mutations and Variants of Interest." https://covariants.org/

url <- "https://raw.githubusercontent.com/hodcroftlab/covariants/master/cluster_tables/USAClusters_data.json"

var_perc <- fromJSON(txt=url)

var_perc <- var_perc[[1]]$Michigan

var_perc <- data.frame(
  "Week.Ending.Date" = as.Date(var_perc$week)+12,
  "total_sequences" = var_perc$total_sequences,
  "Alpha" = var_perc$`20I (Alpha, V1)`,
  "Delta" = var_perc$`21I (Delta)`+var_perc$`21J (Delta)`+var_perc$`21A (Delta)`)

var_perc <- cbind(var_perc,
                  binconf(var_perc$Alpha,
                          var_perc$total_sequences,
                          method = "wilson"), 
                  binconf(var_perc$Delta,
                          var_perc$total_sequences,
                          method = "wilson"))

var_perc_long <- bind_rows(var_perc[,c(1,5,6,7)],var_perc[,c(1,8,9,10)]) 

var_perc_long$variant <- c(rep("Alpha",nrow(var_perc)),
                           rep("Delta",nrow(var_perc)))

var_perc_long <- var_perc_long %>%
  filter(Week.Ending.Date > "2020-12-01") %>%
  filter(variant == "Alpha" | Week.Ending.Date > "2021-04-01" ) %>%
  filter(variant == "Delta" | Week.Ending.Date < "2021-08-22")

names(var_perc) <- c(names(var_perc)[1:4],
                     "p_alpha","p_alpha_l","p_alpha_u",
                     "p_delta","p_delta_l","p_delta_u")


p2 <- ggplot(var_perc_long, 
             aes(x = Week.Ending.Date, y = PointEst, group = variant,
                 color = variant, fill = variant)) +
  geom_line(size=1.5) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper, fill=variant), 
              alpha=0.5, color = NA) +
  theme_classic() +
  ylab("Fraction") +
  xlab("Week Ending Date") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = c(0,0)) +
  scale_color_manual(name = "", values = c("firebrick3","palegreen4")) +
  scale_fill_manual(name = "", values = c("firebrick3","palegreen4")) +
  ggtitle("B") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1), 
        legend.justification = c(1,0.5), 
        legend.position = c(1,0.5))

# =========== Figure 2, panel C: Epi Curve ==================

epicurve <- cases %>%
  mutate(
    cases = est.cases.0to17+est.cases.18to19+est.cases.20to29+est.cases.30to39+
      est.cases.40to49+est.cases.50to59+est.cases.60to69+est.cases.70to79+
      est.cases.80plus
    ) %>%
  group_by(Week.Ending.Date) %>%
  summarise(cases = sum(cases)) 

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

###RESULTS text
sum(epicurve$cases[epicurve$Week.Ending.Date>="2020-10-11" & 
                                 epicurve$Week.Ending.Date<="2021-01-30"])

sum(epicurve$cases[epicurve$Week.Ending.Date>="2021-02-28" & 
                                 epicurve$Week.Ending.Date<="2021-06-19"])

sum(epicurve$cases[epicurve$Week.Ending.Date>="2021-07-25" & 
                                 epicurve$Week.Ending.Date<="2021-11-13"])

sum(epicurve$alpha[epicurve$Week.Ending.Date>="2021-02-28" & 
                     epicurve$Week.Ending.Date<="2021-06-19"])

sum(epicurve$nonVariant[epicurve$Week.Ending.Date>="2021-02-28" & 
                     epicurve$Week.Ending.Date<="2021-06-19"])

sum(epicurve$delta[epicurve$Week.Ending.Date>="2021-02-28" & 
                          epicurve$Week.Ending.Date<="2021-06-19"])


sum(epicurve$nonVariant[epicurve$Week.Ending.Date>="2020-10-11" & 
                     epicurve$Week.Ending.Date<="2021-01-30"])

sum(epicurve$alpha[epicurve$Week.Ending.Date>="2020-10-11" & 
                     epicurve$Week.Ending.Date<="2021-01-30"])

sum(epicurve$delta[epicurve$Week.Ending.Date>="2021-07-25" & 
                     epicurve$Week.Ending.Date<="2021-11-13"])
sum(epicurve$alpha[epicurve$Week.Ending.Date>="2021-07-25" & 
                     epicurve$Week.Ending.Date<="2021-11-13"])

###Plot
p3 <- ggplot(epicurve, aes(x=Week.Ending.Date, y = cases)) + 
  geom_rect(aes(xmin=as.Date("2020-10-11"),
                xmax = as.Date("2021-01-30"),
                ymin = -Inf,
                ymax = Inf), fill = 'gray', alpha = 0.05) +
  geom_rect(aes(xmin=as.Date("2021-02-28"),
                xmax = as.Date("2021-06-19"),
                ymin = -Inf,
                ymax = Inf), fill = 'gray', alpha = 0.05) +
  geom_rect(aes(xmin=as.Date("2021-07-25"),
                xmax = as.Date("2021-11-13"),
                ymin = -Inf,
                ymax = Inf), fill = 'gray', alpha = 0.05) +
  geom_area(aes(fill="Total")) +
  geom_line(aes(x=Week.Ending.Date, y=alpha, color = "Alpha"), size=1.5) +
  geom_line(aes(x=Week.Ending.Date, y=delta, color = "Delta"), size=1.5) +
  geom_line(aes(x=Week.Ending.Date, y=nonVariant, color = "Other"), size=1.5) +
  ggtitle("C") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.1),
        legend.spacing.y = unit(-0.2, "cm")) +
  ylab("Weekly Estimated Cases") +
  xlab("Week Ending Date") + 
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") + 
  scale_y_continuous(expand = c(0,0), 
                     labels = c("0","100,000","200,000","300,000","400,000"), 
                     breaks=c(0,100000,200000,300000,400000),
                     limits = c(0,400001)) +
  scale_color_manual(
    name="",
    values=c(Alpha="firebrick3", Delta="palegreen4",Other="lightsteelblue3")
    )+
  scale_fill_manual(name = "", values = c(Total="gray20")) + 
  theme(legend.justification = c(0.1,1), legend.position = c(0.1,1))

# =========== Figure 2, panel D: Reproduction Number ==================

epicurve.daily <- cases.daily %>%
  select(Date, Area, starts_with("est.cases.")) %>%
  mutate(cases = rowSums(across(where(is.numeric)),na.rm = TRUE)) %>%
  group_by(Date) %>%
  summarise(I = sum(cases)) %>%
  rename(dates = Date)

epicurve.daily <- left_join(
  epicurve.daily,var_perc,by=c("dates" = "Week.Ending.Date")
  ) %>%
  fill(p_alpha, .direction="up") %>%
  fill(p_delta, .direction="up") %>%
  fill(p_alpha, .direction="down") %>%
  fill(p_delta, .direction="down") %>%
  mutate(alpha = ceiling(I*p_alpha),
         delta = ceiling(I*p_delta),
         other = ceiling(I-alpha-delta))

#Reproductive number
Make_Re <- function(variant,cutDate1,cutDate2){
  df <- epicurve.daily %>%
    filter(dates >= as.Date(cutDate1) & dates <= as.Date(cutDate2)) %>%
    select(dates,{{variant}}) %>%
    rename(I = {{variant}})
  
  T <- nrow(df)
  t_start <- seq(2, T-13) # starting at 2 as conditional on the past observations
  t_end <- t_start + 13 
  
  Re <- estimate_R(df,
                   method="parametric_si",
                   config = make_config(list(t_start = t_start,
                                             t_end = t_end,
                                             mean_si = 5.68,
                                             std_si = 4.77))
  )
  
  dates <- epicurve.daily %>%
    filter(dates >= as.Date(cutDate1)  & dates <= as.Date(cutDate2)) %>%
    mutate(t_end = row_number()) %>%
    select(dates,t_end) 
  
  plotdata <- left_join(Re[[1]],dates,by="t_end")
  
  return(plotdata)
}

alpha_plotdata <- Make_Re(alpha,"2020-12-06","2021-08-21")
delta_plotdata <- Make_Re(delta,"2021-04-04","2021-11-13")
other_plotdata <- Make_Re(other,"2020-03-01","2021-08-21")

###RESULTS text
compare_alpha <- left_join(
  select(
    alpha_plotdata,
    c("Median(R)","Quantile.0.025(R)","Quantile.0.975(R)","dates")
    ),
  select(
    other_plotdata,
    c("Median(R)","Quantile.0.025(R)","Quantile.0.975(R)","dates")
    ),
  by = "dates"
  ) %>%
  mutate(ratio = `Median(R).x` / `Median(R).y`,
         ratio_lower = `Quantile.0.025(R).x` / `Quantile.0.025(R).y`,
         ratio_upper = `Quantile.0.975(R).x` / `Quantile.0.975(R).y`)

sum(compare_alpha$ratio)/nrow(compare_alpha)
sum(compare_alpha$ratio_lower)/nrow(compare_alpha)
sum(compare_alpha$ratio_upper)/nrow(compare_alpha)

compare_delta <- left_join(
  select(
    delta_plotdata,
    c("Median(R)","Quantile.0.025(R)","Quantile.0.975(R)","dates")
    ),
  select(
    alpha_plotdata,
    c("Median(R)","Quantile.0.025(R)","Quantile.0.975(R)","dates")
    ),
  by = "dates"
  ) %>%
  mutate(ratio = `Median(R).x` / `Median(R).y`,
         ratio_lower = `Quantile.0.025(R).x` / `Quantile.0.025(R).y`,
         ratio_upper = `Quantile.0.975(R).x` / `Quantile.0.975(R).y`) %>%
  filter(!is.na(ratio))

sum(compare_delta$ratio)/nrow(compare_delta)
sum(compare_delta$ratio_lower)/nrow(compare_delta)
sum(compare_delta$ratio_upper)/nrow(compare_delta)

###Plot
p4 <- ggplot() +
  geom_line(
    data=alpha_plotdata,
    aes(x=dates, y=`Mean(R)`, color = "Alpha"), 
    size=1.5
    ) +
  geom_ribbon(
    data=alpha_plotdata, 
    aes(x=dates, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`), 
    alpha=0.5, 
    color = NA, 
    fill = "firebrick3"
    ) +
  geom_line(
    data=delta_plotdata,
    aes(x=dates, y=`Mean(R)`, 
        color = "Delta"), 
    size=1.5
    ) +
  geom_ribbon(
    data=delta_plotdata, 
    aes(x=dates, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`),
    alpha=0.5, 
    color = NA, 
    fill = "palegreen4"
    ) +
  geom_line(
    data=other_plotdata,
    aes(x=dates, y=`Mean(R)`, color = "Other"), 
    size=1.5
    ) +
  geom_ribbon(
    data=other_plotdata, 
    aes(x=dates, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`),
    alpha=0.5, 
    color = NA, 
    fill = "lightsteelblue3"
    ) +
  scale_color_manual(
    name = "", 
    values = c(Alpha="firebrick3", Delta="palegreen4",Other="lightsteelblue3")
    ) +
  ggtitle("D") +
  ylab(expression(paste("Reproduction Number (",R[e],")"))) +
  xlab("Date") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  theme_classic() + 
  theme(legend.justification = c(1,0.9), legend.position = c(1,0.9))

# =========== Put 4 panels together ==================
plots <- list(p1,p2,p3,p4)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)

for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

#save figure
pdf("figure2.pdf",
    width = 10, height = 10)
  do.call("grid.arrange", c(grobs, ncol = 2))
dev.off() 
jpeg("figure2.jpg",
    width = 10, height = 10, units="in",res=800)
do.call("grid.arrange", c(grobs, ncol = 2))
dev.off() 

 
