## ---------------------------
##
## Script name: MCMCsort_days.R
##
## Purpose of script:
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
## Notes: I just want to check something
##   
##
## ---------------------------

library(dplyr)
library(ggplot2)

#nc_age_dat <- read.csv("./outputs/NC_AgeDateCat_ceil.csv")
monte.dat <- read.csv("./outputs/mcmc_counties.csv")

nc_age_dat$Date <- as.Date(nc_age_dat$Date)

## summarize when the start and end dates are
# temp_summary <- nc_age_dat %>%
#   group_by(County)%>%
#   summarize(min_date = min(Date), 
#             max_date = max(Date), 
#             N = n())

temp_summary2 <- monte.dat %>%
  group_by(County)%>%
  summarise(min_date = min(Date), 
            max_date = max(Date))

## produce table by days from start
day_dt <- monte.dat %>%
  merge(temp_summary2, 
        by = "County", 
        all = TRUE)%>%
  arrange(County, Date)%>%
  mutate(day = as.integer(difftime(Date, min_date, units = "days"))+1)

## summarize observations PER age group PER county
summary_obs <- day_dt %>%
  group_by(County, AgeGrp)%>%
  summarise(N = n())
summary_obs2 <- summary_obs%>%
  group_by(County)%>%
  summarize(obs_mean = mean(N), 
            obs_sd = sd(N))

## filter table to include important vars
temp_tbl <- day_dt %>%
  select(County, Count, AgeGrp, day)

## Produce summary of number of days vs # of rows --> inconsistent but that should not be an issue? 
## return to this point later
day_summary <- temp_tbl %>%
  group_by(County, AgeGrp)%>%
  summarize(ndays = max(day))%>%
  merge(summary_obs2, 
        by = "County", 
        all = TRUE)%>%
  select(-obs_sd)%>%
  mutate(gt_only = (ndays>obs_mean), 
         equal = (ndays == obs_mean),
         lt_only = (ndays < obs_mean))

temp <- filter(temp_tbl, County == "Orange", AgeGrp == "Ages 50-64")


ggplot(temp, aes(day, Count))+geom_line()

write.csv(temp_tbl, "./outputs/mcmc_CtyAgeDate_input.csv", row.names = FALSE)



