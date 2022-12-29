## ---------------------------
##
## Script name: partitionCountyAgeDate.R
##
## Purpose of script:  Set up inputs for county level data into the MCMC Simulation
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
library(ggplot2)
library(tidyr)


nc_count <- read.csv("./data/nc_county_jhu_counts.csv")
nc_age_demo <- readxl::read_excel("data/CasesDemographics_Age_121421.xlsx")
co_est2021_pop_37 <- readxl:: read_excel("data/co-est2021-pop-37.xlsx", sheet = "NC_POP21")%>%
  mutate(County = str_remove(County, " County, North Carolina"),
         County = str_remove(County, "\\."))
# Subset study period from beginning of pandemic to January 1, 2021
nc_count <- filter(nc_count, Date <= as.Date("2021-01-01"))

###### data cleaning and manipulation to get deaths by age, convert weeks to days ####
# get rid of rows with cases, delete the rows that have the names 

################
# NCDHHS Data  #
################

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

min_start <- nc_age_death %>%
  group_by(County)%>%
  summarize(min_week_start = min(week_start), max_week_start = max(week_start))

################
# HOPKINS Data #
################
jhu_week_dist <- nc_count[, -c(1, 4, 6)]%>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
    weekday = wday(Date, label = TRUE), 
    week_start = floor_date(Date, unit = "weeks"))%>%
  merge(min_start, 
        by = "County", 
        all = TRUE)%>%
  mutate(in_time_bound = (week_start >= min_week_start & week_start <= max_week_start))%>%
  filter(in_time_bound == TRUE)%>%
  select(County, Date, daily_deaths, weekday, week_start)%>%
  arrange(County, Date)




counties <- unique(jhu_week_dist$County)
nc_cty_scaled <- data.frame()
for(county in counties){
  weeks <- unique(filter(jhu_week_dist, County == county)$week_start)
  for(week in weeks){
    temp<- jhu_week_dist%>%
      filter(County == county, week_start == week)
    scale_denominator = sum(temp$daily_deaths)

    if(length(temp$Date) < 7){
      print(paste0("ERROR in county = ", county, " , week = ", as.Date(week, origin = '1970-01-01')))
      temp <- temp %>%
        complete(Date = seq.Date(as.Date(week, origin = '1970-01-01'), as.Date(week, origin = '1970-01-01')+ 6, by = "day"))%>%
        fill(`daily_deaths`, 0)%>%
        fill(County)
    }
      day_mtx <- matrix(rep(temp$daily_deaths, 7), nrow = 7, ncol = 7)
      temp2<- nc_age_death %>%
        filter(County == county, week_start == week)%>%
        select(Deaths0.17, Deaths18.24, Deaths25.49, Deaths50.64, Deaths65.74, Deaths75p, Deaths_SM)
      age_mtx <- t(matrix(rep(as.numeric(temp2), 7), nrow = 7, ncol = 7))
      
      if(scale_denominator == 0){
        age_day_mtx <- (age_mtx*day_mtx)
      }else{
        age_day_mtx <- (age_mtx*day_mtx)*(1/scale_denominator)
      }

      age_day_tbl <-as.data.frame(age_day_mtx)
      age_day_tbl <- cbind(temp$County, temp$Date, age_day_tbl)
      colnames(age_day_tbl) <- c("County","Date","Deaths0.17", "Deaths18.24", "Deaths25.49", "Deaths50.64", "Deaths65.74", "Deaths75p", "DeathsSM")
      print(age_day_tbl)
      nc_cty_scaled <- rbind(nc_cty_scaled, age_day_tbl)

  }
}

# fill all NAs with 0s






# temp<- jhu_week_dist%>%
#   filter(County == "Alamance", week_start == as.Date("2020-05-17"))%>%
#   complete(Date = seq.Date(as.Date("2020-05-17"), as.Date("2020-05-17")+6, by = "day"))%>%
#   fill('daily_deaths',0)%>%
#   fill(County)



# merge by week data 

# temp <- merge(jhu_week_dist, 
#               nc_age_death, 
#               by.x = c("Date", "County"), 
#               by.y = c("week_start", "County"), 
#               all = TRUE)

# nc_long <- nc_count %>%
#   select(County, day, daily_deaths)%>%
#   spread(County, daily_deaths)



### compare jhu to ncdhhs data ###
# jhu_deaths <- nc_count[, -c(1)]%>%
#   group_by(County)%>%
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
#          sum_deaths_by_day = cumsum(daily_deaths))
# ncdhhs_deaths <- nc_age_death %>%
#   group_by(County)%>%
#   mutate(sum_deaths_by_week = cumsum(week_total))
# 
# jhu_ncdhhs <- merge(jhu_deaths[, c("Date", "County", "daily_deaths")], 
#                     ncdhhs_deaths[, c("week_start", "County", "week_total")], 
#                     by.x = c("Date", "County"), 
#                     by.y = c("week_start", "County"), 
#                     all = TRUE)%>%
#   group_by(County)%>%
#   arrange(County, Date)%>%
#   replace(is.na(.), 0)%>%
#   mutate(cumulative_jhu = cumsum(daily_deaths), 
#          cumulative_ncdhhs = cumsum(week_total), 
#          discrepancy = cumulative_ncdhhs - cumulative_jhu)
# 
# jhu_ncdhhs$County <- as.factor(jhu_ncdhhs$County)
# 
# jhu_ncdhhs <- merge(jhu_ncdhhs, 
#                     co_est2021_pop_37, 
#                     by = "County", 
#                     all = FALSE)



### Plot comparison for Supplementary Appendix 
# p <- ggplot(data = jhu_ncdhhs, aes(Date))+
#   geom_line(aes(y = cumulative_jhu/POP_2021, color = "JHU"))+
#   geom_line(aes(y = cumulative_ncdhhs/POP_2021, color = "NCDHHS"))+
#   scale_color_manual("",
#                      breaks = c("JHU", "NCDHHS"), 
#                      values = c("JHU" = "blue", "NCDHHS" = "red"))+
#   xlab("Date")+
#   ylab("Cumulative Infections per Person")+
#   facet_wrap("County")
# p
# 
# d <- ggplot(data = jhu_ncdhhs, aes(Date, discrepancy/POP_2021))+
#   geom_line()+
#   xlab("Date")+
#   ylab("Discrepancy = NCDHHS - JHU")+
#   facet_wrap("County")
# d
# 
# 
# 
# ggsave(plot = p, 
#        filename = "JHUvNCDHHS_Supplement.png", 
#        path = "figures", 
#        device = "png")
# ggsave(plot = d, 
#        filename = "JHUvNCDHHS_displacement_Supplement.png",
#        path = "figures", 
#        device = "png")
