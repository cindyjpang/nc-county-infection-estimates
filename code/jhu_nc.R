## ---------------------------
##
## Script name: JHU_NC
##
## Purpose of script: read data from Hopkins and aggregate county cases and deaths
## Output: cleaned county cases and deaths file
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
## Notes: Copied and pasted old code from revised_estimates folder
##   
##
## ---------------------------

# load libraries
library(readr)
library(httr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)


# read hopkins data 
hopkins_death <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
hopkins_cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

# filter to North Carolina
nc_daily_death <- hopkins_death %>%
  filter(Province_State == "North Carolina")%>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Province_State, -Country_Region, -Lat, -Long_, -Combined_Key, -Population)%>%
  gather(key = "Date", value = "cum_deaths", -Admin2)

nc_daily_cases <- hopkins_cases %>%
  filter(Province_State == "North Carolina")%>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Province_State, -Country_Region, -Lat, -Long_, -Combined_Key)%>%
  gather(key = "Date", value = "cum_cases", -Admin2)


nc_case_death <- merge(nc_daily_death, 
                       nc_daily_cases, 
                       by = c("Admin2", "Date"), 
                       all = TRUE)

parse_time <- function(date, choice){
  date <- str_remove(date, "X")
  parsed_date <- str_split_fixed(date, "\\.", 3)
  
  day <- as.integer(parsed_date[1,2])
  month <- as.integer(parsed_date[1,1])
  year <- as.integer(parsed_date[1,3])+2000
  
  if(choice == 1){
    return(year)
  }else if(choice == 2){
    return(month)
  }else{
    return(day)
  }
  
}

hopkins_nc_daily <- nc_case_death %>%
  mutate(year = lapply(Date, parse_time, choice = 1),
         month = lapply(Date, parse_time, choice = 2), 
         day = lapply(Date, parse_time, choice = 3),
         newDate = make_datetime(year, month, day))%>%
  select(Admin2, cum_deaths, cum_cases, newDate)

nc_daily <- hopkins_nc_daily %>%
  rename(County = Admin2, 
         Date = newDate)%>%
  relocate(Date, .after = County)%>%
  group_by(County)%>%
  arrange(Date, .by_group = TRUE)%>%
  mutate(daily_deaths = cum_deaths - lag(cum_deaths, default = first(cum_deaths)),
         daily_cases = cum_cases - lag(cum_cases, default = first(cum_cases)))%>%
  subset(daily_cases >= 0 & daily_deaths >= 0) # get rid of any negative values and recount 

nc_daily_DATE <- nc_daily %>%
  select(County, Date, daily_deaths, daily_cases)

# get start and end dates, results of this table show that start/end dates are the same across all counties
nc_county_start <- nc_daily_DATE %>%
  group_by(County)%>%
  summarize(start_date = min(Date), 
            end_date = max(Date))


