## ---------------------------
##
## Script name: MCMCcounties.R
##
## Purpose of script: Sort through all counties which have cases and get rid of counties which don't have cases;
##                    Feeds less data, takes less memory
##
## Author: Cindy J. Pang
##
## Date Created: 2023-01-12
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

age_dat_ceil <- read.csv("./outputs/NC_AgeDateCat_ceil.csv")
nc_death_ceil_summary <- read.csv("./outputs/nc_death_ceil_summary.csv")

nc_death_eliminate <- nc_death_ceil_summary %>%
  mutate_if(is.numeric, funs(.>0))%>%
  select(-X)

## first get rid of all of the counties which have 0 deaths total for all age categories
complete_deaths <- filter(nc_death_ceil_summary, total_deaths_all > 0)%>%
  select(County)

## construct new dataframe with all counties EXCEPT the ones filtered out
complete_deaths_obs <- age_dat_ceil %>%
  merge(complete_deaths, 
        by = "County", 
        all = FALSE)

## construct dataframes for all 7 age categories using the complete death obs
deaths0.17 <- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deaths0.17 > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, Deaths0.17)%>%
  mutate(AgeGrp = "Ages 0-17")%>%
  rename(Count = Deaths0.17)

deaths18.24 <- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deaths18.24 > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, Deaths18.24)%>%
  mutate(AgeGrp = "Ages 0-18")%>%
  rename(Count = Deaths18.24)

deaths25.49 <- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deaths25.49 > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, Deaths25.49)%>%
  mutate(AgeGrp = "Ages 25-49")%>%
  rename(Count = Deaths25.49)

deaths50.64 <- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deaths50.64 > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, Deaths50.64)%>%
  mutate(AgeGrp = "Ages 50-64")%>%
  rename(Count = Deaths50.64)

deaths65.74 <- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deaths65.74 > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, Deaths65.74)%>%
  mutate(AgeGrp = "Ages 65-74")%>%
  rename(Count = Deaths65.74)

deaths75p <- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deaths75p > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, Deaths75p)%>%
  mutate(AgeGrp = "Ages 75p")%>%
  rename(Count = Deaths75p)

deathsSM<- age_dat_ceil %>%
  merge(filter(nc_death_ceil_summary, total_deathsSM > 0)%>%select(County), 
        by = "County", 
        all = FALSE)%>%
  select(County, Date, DeathsSM)%>%
  mutate(AgeGrp = "Ages SM")%>%
  rename(Count = DeathsSM)

# combine all into one masterframe

all_obs <- rbind(deaths0.17, deaths18.24, deaths25.49, deaths50.64, deaths65.74, deaths75p, deathsSM)
all_obs <- arrange(all_obs, County)

# export files 
write.csv(all_obs, "./outputs/mcmc_counties.csv")

