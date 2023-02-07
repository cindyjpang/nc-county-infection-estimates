# **North Carolina COVID-19 Infection Estimates**
Backsolve for true infections in North Carolina counties during the COVID-19 pandemic 

### **_Code_**
-```jhu_nc.R```  outputs the data from the COVID-19 Data Repository by [the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19), filtered and subsetted to include a time series of daily reported cases and deaths for each county in the state of North Carolina   
-```partitionCountyAgeDate.R``` outputs the inputs for the MCMC Simulation  
- ```summarizeIFR.R``` produces a summary of IFR values for the data-table was produced from [data from the IHME](https://ghdx.healthdata.org/record/ihme-data/covid_19_infection_fatality_ratio) with means and standard deviations for each age-group 
  
  
**MCMC Files** are used for sorting and tidying data before it runs on UNC Longleaf  

1.1 ```MCMCcounties.R``` sorts through all age groups and counties with total deaths greater than 1  
1.2 ```MCMCparams.R``` generates the Monte-Carlo simulation parameters for 10,000 simulations. This also allows for reproducibility.  
2. ```MCMCsort_days.R``` must be completed after 1.1, which parses the dates (yyyy-mm-dd) into integer days  
3. ```MCMCrun.R``` file to RUN on Longleaf 






### **_Data_**
-```nc_county_jhu_counts.csv``` contains fields: County, Date, days, daily_deaths, daily_cases as columns 

