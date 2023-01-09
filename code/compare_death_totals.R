## ---------------------------
##
## Script name: compare_death_totals.R
##
## Purpose of script: 
## Author: Cindy J. Pang
##
## Date Created: 2022-12-30
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

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(tmap)
library(sf)

age_dat <- read.csv("./outputs/NC_AgeDateCat.csv")
age_dat_ceil <- read.csv("./outputs/NC_AgeDateCat_ceil.csv")

# produce summary for each county with raw age dat counts, unrounded
nc_death_summary <- age_dat %>%
  group_by(County)%>%
  summarize(total_deaths0.17 = sum(Deaths0.17), 
            total_deaths18.24 = sum(Deaths18.24), 
            total_deaths25.49 = sum(Deaths25.49), 
            total_deaths50.64 = sum(Deaths50.64), 
            total_deaths65.74 = sum(Deaths65.74), 
            total_deaths75p = sum(Deaths75p), 
            total_deathsSM = sum(DeathsSM))%>%
  rowwise(County)%>%
  mutate(total_deaths_all = sum(c_across(total_deaths0.17:total_deathsSM)))

# produce summary for each county with integer death counts, rounded
nc_death_ceil_summary <- age_dat_ceil %>%
  group_by(County)%>%
  summarize(total_deaths0.17 = sum(Deaths0.17), 
            total_deaths18.24 = sum(Deaths18.24), 
            total_deaths25.49 = sum(Deaths25.49), 
            total_deaths50.64 = sum(Deaths50.64), 
            total_deaths65.74 = sum(Deaths65.74), 
            total_deaths75p = sum(Deaths75p), 
            total_deathsSM = sum(DeathsSM))%>%
  rowwise(County)%>%
  mutate(total_deaths_all = sum(c_across(total_deaths0.17:total_deathsSM)))


# compare to ncdhhs data
nc_age_demo <- readxl::read_excel("data/CasesDemographics_Age_121421.xlsx")
nc_age_death <- nc_age_demo[-c(1), -c(1,4,6,8,10,12,14,16,18)]
names(nc_age_death)<- c("week_start","County", "Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "Deaths_Supp", "Deaths_Miss")
nc_age_death$week_start <- as.Date(nc_age_death$week_start, "%m/%d/%Y")
nc_age_death <- nc_age_death %>%
  filter(week_start <= as.Date("2021-01-01"))%>%
  mutate(across(.cols = c("Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "Deaths_Supp", "Deaths_Miss"), .fns = as.numeric),
         week_total = rowSums(across(.cols = c("Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "Deaths_Supp", "Deaths_Miss")), na.rm = TRUE),
         weekday = wday(week_start, label = TRUE),
         Deaths_SM = Deaths_Supp + Deaths_Miss)%>%
  replace(is.na(.), 0)

# generate summary by age group
ncdhhs_dat_summary <- nc_age_death %>%
  group_by(County)%>%
  summarize(total_deaths0.17 = sum(Deaths0.17), 
            total_deaths18.24 = sum(Deaths18.24), 
            total_deaths25.49 = sum(Deaths25.49), 
            total_deaths50.64 = sum(Deaths50.64), 
            total_deaths65.74 = sum(Deaths65.74), 
            total_deaths75p = sum(Deaths75p), 
            total_deathsSM = sum(Deaths_SM))%>%
  rowwise(County)%>%
  mutate(total_deaths_all = sum(c_across(total_deaths0.17:total_deathsSM)))


######################
#                    #
#     MAP SUMMARY    #
#                    #
######################

## MAP DATA ##
shp <- st_read("data/cartographic_boundaries/cb_2018_nc_county_5m.shp")

## Merge Files
ncdhhs_summary_shp <- merge(shp, 
                            ncdhhs_dat_summary, 
                            by.x = "NAME", 
                            by.y = "County",
                            all = TRUE)

nc_death_summary_shp <- merge(shp, 
                            nc_death_summary, 
                            by.x = "NAME", 
                            by.y = "County",
                            all = TRUE)

nc_death_ceil_summary_shp <- merge(shp, 
                              nc_death_ceil_summary, 
                              by.x = "NAME", 
                              by.y = "County",
                              all = TRUE)

## Generate Maps for All Ages 
breaks = c(0, 25, 50, 100, 200, Inf)

all_death_ncdhhs_map <- tm_shape(ncdhhs_summary_shp)+
  tm_polygons("total_deaths_all", 
              breaks = breaks, 
              title = "Total Deaths, NCDHHS")

all_death_nc_map <- tm_shape(nc_death_summary_shp)+
  tm_polygons("total_deaths_all", 
              breaks = breaks, 
              title = "Total Deaths, Imputed - Raw ")

all_death_nc_ceil_map <- tm_shape(nc_death_ceil_summary_shp)+
  tm_polygons("total_deaths_all", 
              breaks = breaks, 
              title = "Total Deaths, Imputed - Upper")
tmap_arrange(all_death_ncdhhs_map, all_death_nc_map, all_death_nc_ceil_map)

