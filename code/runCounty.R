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

# CONVERT DATA TO LONG FORM to input into Phipps, Grafton, Kompas 
nc_count <- read.csv("./data/nc_county_jhu_counts.csv")

nc_long <- nc_count %>%
  select(County, day, daily_deaths)%>%
  spread(County, daily_deaths)

