library(ggplot2)
library(gridExtra)

# Calculates a likelihood for fitting the mean & sd of a weibull
# Assuming weighted least squares (normally distributed measurement error with known sd's for the data)
#  - actually just doing ordinary least squares for now but if you update the variance lines below (lines 30 & 43) with the data variances it will do weighted!
# Converted this function from Josh's plot function
likelihood = function(params){
  mean <- params[1]
  sd <- params[2]
  
  # Calculate the shape and scale parameters for a Weibull distribution for time 
  # from seroconversion to seroreversion
  sum.r <- (mean^2 + sd^2)/mean^2
  fn <- function (x, sum) {lgamma(1+2/x) - 2*lgamma(1+1/x) - log(sum)}
  shape <- uniroot(fn, sum=sum.r, c(0.01,100), extendInt = "yes", maxiter = 10000)$root
  scale <- mean/(gamma(1+1/shape))
  
  #get cumulative probability of seroreversion for 2-week periods from weibull distribution
  p_undetectable <- pweibull(seq(0,365,(365/26)), shape = shape, scale = scale)[-1]
  
  #convert to probability of remaining detectable
  p_detectable <- 1-p_undetectable
  
  data <- data.frame("days" = seq(14,365,14), 
                          "p_detectable" = p_detectable,
                          "Kahre" = c(rep(NA,10),0.59,rep(NA,11),0.24,rep(NA,3)),
                          "Kahre.var" = c(rep(NA,10),(.59*.41)/109, 
                                          rep(NA,11), (.24*.76)/109, rep(NA,3)),

                          "Maine" = c(NA,.996,.994,.972,.971,.923,rep(NA,20)),
                          "Maine.var" = c(NA,
                                          (.996*.004)/248,
                                          (.994*.006)/179,
                                          (.972*.028)/71,
                                          (.971*.029)/34,
                                          (.923*.077)/13,
                                          rep(NA,20))
                          )
  
  likelihood = sum( (data$p_detectable - data$Kahre)^2/data$Kahre.var, na.rm = TRUE) + 
    sum( (data$p_detectable - data$Maine)^2/data$Maine.var, na.rm = TRUE)
  
  return(likelihood)
}

# Starting parameter estimates
startpars = c("m" = 8*365/12, "s" = 100)

# Fit to the data!
fit = optim(par = startpars, fn = likelihood)



plot.weibull <- function(m,s){
  mean <- m 
  sd <- s
  
  # Calculate the shape and scale parameters for a Weibull distribution for time 
  # from seroconversion to seroreversion
  sum.r <- (mean^2 + sd^2)/mean^2
  fn <- function (x, sum) {lgamma(1+2/x) - 2*lgamma(1+1/x) - log(sum)}
  shape <- uniroot(fn, sum=sum.r, c(0.01,100), extendInt = "yes", maxiter = 10000)$root
  scale <- mean/(gamma(1+1/shape))
  
  #get cumulative probability of seroreversion for 2-week periods from weibull distribution
  p_undetectable <- pweibull(seq(0,365,(365/26)), shape = shape, scale = scale)[-1]
  
  #convert to probability of remaining detectable
  p_detectable <- 1-p_undetectable
  
  plot.data <- data.frame("days" = seq(14,365,14), 
                          "p_detectable" = p_detectable,
                          "Kahre" = c(rep(NA,10),0.59,rep(NA,11),0.24,rep(NA,3)),
                          "Kahre.upper" = c(rep(NA,10),
                                            0.59 + 1.96*sqrt((.59*.41)/109),
                                            rep(NA,11),
                                            0.24 + 1.96*sqrt((.24*.76)/109),
                                            rep(NA,3)),
                          "Kahre.lower" = c(rep(NA,10),
                                            0.59 - 1.96*sqrt((.59*.41)/109),
                                            rep(NA,11),
                                            0.24 - 1.96*sqrt((.24*.76)/109),
                                            rep(NA,3)),
                          "Maine" = c(NA,.996,.994,.972,.971,.923,rep(NA,20)),
                          "Maine.upper" = c(NA,1,1,1,1,1,rep(NA,20)),
                          "Maine.lower" = c(NA,.988,.983,.933,.914,.778,rep(NA,20)))
  
  plot <- ggplot(data = plot.data, aes(x=days,y=p_detectable)) +
    geom_line(size=1.5) +
    geom_point(aes(y=Maine, color="Maine"), size=2) +
    geom_errorbar(aes(ymin=Maine.lower, ymax=Maine.upper, color="Maine"), width=1,
                  position=position_dodge(0.05)) +
    geom_point(aes(y=Kahre, color="Kahre"), size=2) +
    geom_errorbar(aes(ymin=Kahre.lower, ymax=Kahre.upper, color="Kahre"), width=1,
                  position=position_dodge(0.05)) +
    ggtitle(paste0("Mean: ",round(m,1),"; SD: ",round(s, 1)," Days")) +
    ylab("Proportion Detectable") +
    xlab("Days Post-Infection") +
    scale_color_manual(labels = c("Kahre","Maine"), 
                       values = c("Maine"="firebrick3","Kahre"="lightsteelblue3"), 
                       name = "") +
    theme_classic()
  
  #curve(pweibull(x, shape = shape, scale = scale), from = 0, to = 365)
  return(plot)
}

# Plot the resulting fitted parameters
pdf("S:/Monto_Ohmit/Sam Harrison/Burden - B117/documents/results/weibull_fit.pdf",
    width = 8, height = 6)
  plot.weibull(fit$par[1],fit$par[2])
dev.off() 

jpeg("S:/Monto_Ohmit/Sam Harrison/Burden - B117/documents/results/weibull_fit.jpg",
    width = 8, height = 6, units = "in", res = 800)
plot.weibull(fit$par[1],fit$par[2])
dev.off() 

