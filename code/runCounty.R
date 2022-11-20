## ---------------------------
##
## Script name: runCounty
##
## Purpose of script: **For Now** Set up analysis for inputing county level data into the MCMC Simulation
##
## Author: Cindy J. Pang
##
## Date Created: 2022-11-07
##
## Copyright (c) Cindy Pang, 2022
## Email: pangcind@live.unc.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

library(tidyverse)
library(lubridate)
library(dplyr)

nc_count <- read.csv("./data/nc_county_jhu_counts.csv")
nc_age_demo <- readxl::read_excel("data/CasesDemographics_Age_121421.xlsx")

# Subset study period from beginning of pandemic to January 1, 2021
nc_count <- filter(nc_count, Date <= as.Date("2021-01-01"))

###### data cleaning and manipulation to get deaths by age, convert weeks to days ####
# get rid of rows with cases, delete the rows that have the names 
nc_age_death <- nc_age_demo[-c(1), -c(1,4,6,8,10,12,14,16,18)]
names(nc_age_death)<- c("week_start","County", "Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "Deaths_Supp", "Deaths_Miss")
nc_age_death$week_start <- as.Date(nc_age_death$week_start, "%m/%d/%Y")
nc_age_death <- nc_age_death %>%
  filter(week_start <= as.Date("2021-01-01"))%>%
  mutate(across(.cols = c("Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "Deaths_Supp", "Deaths_Miss"), .fns = as.numeric),
         week_total = rowSums(across(.cols = c("Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "Deaths_Supp", "Deaths_Miss")), na.rm = TRUE))%>%
  replace(is.na(.), 0)

### compare jhu to ncdhhs data ###
jhu_deaths <- nc_count%>%
  group_by(County)%>%
  mutate(sum_deaths_by_day = cumsum(daily_deaths))
ncdhhs_deaths <- nc_age_death %>%
  group_by(County)%>%
  mutate(sum_deaths_by_week = cumsum(week_total))

jhu_ncdhhs <- merge(jhu_deaths[, c("Date", "County", "sum_deaths_by_day")], 
                    ncdhhs_deaths[, c("week_start", "County", "sum_deaths_by_week")], 
                    by.x = c("Date"), 
                    by.y = c("week_start"), 
                    all = TRUE)


# nc_long <- nc_count %>%
#   select(County, day, daily_deaths)%>%
#   spread(County, daily_deaths)

