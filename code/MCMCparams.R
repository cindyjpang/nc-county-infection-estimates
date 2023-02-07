## ---------------------------
##
## Script name: MCMCparams.R
##
## Purpose of script: compute infection from deaths 
##
## Author: Cindy J. Pang
##
## Date Created: 2023-01-09
##
## Copyright (c) Cindy Pang, 2023
## Email: pangcind@live.unc.edu
##
## ---------------------------
##
## Notes: (1) Get rid of counties with 0 deaths reported -> before this file, 
##        (2) Read County-specific data, output County-specific simulations   
##        
##
## ---------------------------
library(data.table)

set.seed(1234)

## Read parameter data
ifr_age_dat <- read.csv("./outputs/IFR_by_Age.csv")
incubation_dat <- readxl::read_xlsx("./data/incubation_params.xlsx")

age_param_tbl <- merge(ifr_age_dat, 
                       incubation_dat, 
                       by = "age_cat", 
                       all = TRUE)

age_param_tbl <- age_param_tbl %>%
  mutate(cov_incubation = incubation_sd/incubation_mean,
         age_cat = gsub("Age", "Ages", age_cat))
age_param_tbl$age_cat[6]<- "Ages 75p"
# Define Independent Parameters 
n_monte = 10000
missing_value = -1.0e34
dt_onset_min = 12.8
dt_onset_max = 19.2
cov1 = 0.86
cov2 = 0.45 # from Flaxman et al 

# ; gauss_cvf(P) = qnorm(1-P, mean = 0, sd = 1)
# The GAUSS_CVF function computes the cutoff value V in a standard Gaussian (normal) distribution with a mean of 0.0 and a variance of 1.0 such that the probability that a random variable X is greater than V is equal to a user-supplied probability P
gauss_cvf <- function(p) {
  return(qnorm(1-p, mean = 0, sd=1))
}

## save gaussian constants we don't need to call the memory 
gauss_cvf025 <- gauss_cvf(0.025)
gauss_cvf975 <- gauss_cvf(0.975)

monte.sim.age <- data.frame()
for(i in unique(age_param_tbl$age_cat)){
  # Subset by Age Category
  age_df <- filter(age_param_tbl, age_cat == i)
  
  # ; Derive the implied means and standard deviwations of the parameter
  # ; distributions, assuming that the specified uncertainty ranges for each
  # ; paramter represent the 95% confidence intervals of a normal distribution
  
  ifr_mean = age_df$ifr_mean
  dt_incubation_mean = age_df$incubation_mean
  dt_onset_mean = 0.5 * (dt_onset_min + dt_onset_max)
  
  
  # ; Derive the implied means and standard deviwations of the parameter
  # ; distributions, assuming that the specified uncertainty ranges for each
  # ; paramter represent the 95% confidence intervals of a normal distribution
  
  ifr_sd = age_df$ifr_sd
  dt_incubation_sd = age_df$incubation_sd
  dt_onset_sd = (dt_onset_max - dt_onset_min) / (gauss_cvf025 - gauss_cvf975)
  
  
  monte.sim <- data.frame()
  
 for(monte in 0:n_monte){
    # Derive a random value for ifr
    x1 = rnorm(1)
    ifr = ifr_mean + x1*ifr_sd
    while(abs(x1) > gauss_cvf(0.025) | ifr < 0){
      x1 = rnorm(1)
      ifr = ifr_mean + x1*ifr_sd
    } 
    
    
    # Derive a random value for dt_incubation
    x2 = rnorm(1)
    while(abs(x2) > gauss_cvf(0.025)){
      x2 = rnorm(1)
    }
    dt_incubation = dt_incubation_mean + x1*dt_incubation_sd
    
    #; Derive a random value for dt_onset
    x3 = rnorm(1)
    while (abs(x3) > gauss_cvf(0.025)){
      x3 = rnorm(1)
    }
    dt_onset = dt_onset_mean + x3 * dt_onset_sd
    
    #; Derive the parameters for the Gamma distribution
    dt_death = dt_incubation + dt_onset
    sd1 = cov1 * dt_incubation
    sd2 = cov2 * dt_onset
    sd = sqrt(sd1^2 + sd2^2)
    cov = sd / dt_death
    alpha = 1.0 / cov^2
    beta = dt_death * cov^2
    denominator = beta^alpha * gamma(alpha)
    
    monte.sim <- rbind(monte.sim, c(monte, x1, ifr, x2, dt_incubation, x3, dt_onset, dt_death, sd, alpha, beta, denominator))
    
  }
  
  setnames(monte.sim, new = c('monte','x1','ifr','x2','dt_incubation', 'x3', 'dt_onset', 'dt_death', 'sd', 'alpha', 'beta', 'denominator'), 
           old = names(monte.sim))
  monte.sim$age_group <- i
  monte.sim.age <- rbind(monte.sim.age, monte.sim)
  print(paste0("Finished Age Group: ", i))
  
}



write.csv(monte.sim.age, "./outputs/mcmc_params.csv", row.names = FALSE)


