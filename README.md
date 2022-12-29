# **North Carolina COVID-19 Infection Estimates**
Backsolve for true infections in North Carolina counties during the COVID-19 pandemic 

### **_Code_**
-```jhu_nc.R```  outputs the data from the COVID-19 Data Repository by [the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19), filtered and subsetted to include a time series of daily reported cases and deaths for each county in the state of North Carolina   
-```partitionCountyAgeDate.R``` outputs the inputs for the MCMC Simulation 


### **_Data_**
-```nc_county_jhu_counts.csv``` contains fields: County, Date, days, daily_deaths, daily_cases as columns 

