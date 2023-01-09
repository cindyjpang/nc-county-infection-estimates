## ---------------------------
##
## Script name: summarizeIFR.R
##
## Purpose of script: generate mean and standard deviations for IFR stratified by age
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
## Notes:
##   
##
## ---------------------------

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

ihme_dat <- read_csv("data/IHME_COVID_19_IFR_2020_2021_BY_AGE_Y2022M02D23.csv")

assign_age_cat <- function(age){
  age_bin = ""
  if(age <= 17){ ## 0-17
    age_bin = "Age 0-17"
  }else if(age > 17 && age <= 24){
    age_bin = "Age 18-24"
  }else if(age > 24 && age <= 49){
    age_bin = "Age 25-49"
  }else if(age > 49 && age <=64){
    age_bin = "Age 50-64"
  }else if(age > 64 && age <=74){
    age_bin = "Age 65-74"
  }else{
    age_bin = "Age 75+"
  }
  return(age_bin)
}

ihme_dat <- ihme_dat %>%
  mutate(age_cat = lapply(age_years, assign_age_cat))

ihme_summary <- ihme_dat %>%
  group_by(age_cat)%>%
  summarize(mean = mean(ifr_agespecific_mean)/100, 
            sd = sd(ifr_agespecific_mean)/100)





