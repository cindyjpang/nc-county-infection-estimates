## ---------------------------
##
## Script name: MCMCrun.R
##
## Purpose of script: feeds into the supercomputer to code
##
## Author: Cindy J. Pang
##
## Date Created: 2023-01-24
##
## Copyright (c) Cindy Pang, 2023
## Email: pangcind@live.unc.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
library(tidyverse)

monte.sim <- read.csv("./outputs/mcmc_params.csv")
monte.dat <- read.csv("./outputs/mcmc_CtyAgeDate_input.csv")

counties <- unique(monte.dat$County)
for(cty in counties){ ## counties
  cty_temp <- unique(filter(monte.dat, County == cty)) # gets rid of duplicates
  age_groups <- unique(cty_temp$AgeGrp)
  
  county_age_temp <- data.frame()
  for(age_group in age_groups){ ## county and age group
    monte.age.dat <- filter(monte.sim, age_group == age_group)
    
    # get parameters
    alpha = monte.age.dat$alpha[1] # simulation number
    beta = monte.age.dat$beta[1]
    ifr = monte.age.dat$ifr[1]
    denominator = monte.age.dat$denominator[1]
    
    
    
    # construct matrix with the correct dimensions
    
    cty_data <- filter(cty_temp, AgeGrp == age_group)
    n_obs <- count(cty_data)$n
    
    n_mtx <- matrix(data = NA, nrow = n_obs, ncol = n_obs)
    s <- c()
    days <- cty_data$day
    
    for(t_prime in 1:n_obs){
      t_prime_row <- c()
      for(t in 1:n_obs){
        x = days[t]-days[t_prime]
        if(x <= 0){
          t_prime_row[t]<- 0
        }else{
          pdf <- (x*(alpha-1)*exp(-x/beta))/denominator
          t_prime_row[t]<- (cty_data$Count[t]*dgamma(x,shape= alpha, scale = 1/beta))/ifr
        }
        
      }
      n_mtx[t_prime, ]<- t_prime_row
      s[t_prime]<- sum(t_prime_row)
    }
    
    scaled_inf <- c()
    t0 <- max(days)
    
    for(i in 1:length(s)){
      scaled_inf[i]<- s[i]/pgamma(max(days)-i, shape = alpha, scale = 1/beta)
    }
    cty_data$new_inf <- scaled_inf
    county_age_temp <- rbind(county_age_temp, cty_data)
    
    
  }
  
  
  cty_sim1 <- county_age_temp %>%
    select(-Count)%>%
    spread(AgeGrp, new_inf)%>%
    mutate(total_ninf = rowSums(across(age_groups)))
  
  cty_plot <- ggplot(cty_sim1, aes(day, total_ninf))+
    geom_line()+
    ggtitle(cty)+
    ylab("Total Infections")
  
  print(cty_plot)
  
}
























