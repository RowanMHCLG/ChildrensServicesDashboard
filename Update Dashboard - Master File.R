############ Master CS Dashboard file - Update Dashboard ##################
#################### Rowan Morris 15/05/2026 ###############################

#libraries 
library(tidyverse)
library(progress)

##############filepaths##################

####revenue path - needs updating after updated to the RO - Ed's team in steves team (Edward.Thomas1@communities.gov.uk) produce this and the filepath will need updating #######
revenue_path <- "Q:/ADD Directorate/Local Policy Analysis/LGF/LA data/Revenue/Revenue_R/Final Output/complete_data_2425_second.csv"
outturn_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")
all_outturn_years <- c("2010/11", "2011/12", "2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")

current_year = "2024/25"

#get gdp deflators 
fosteR::get_latest_gdp_deflator(c(current_year, "2015/16"), covid = TRUE)

#get rora, core authorities and years
source("DataProcess/process_RO - master.R")



#s251 data
#s251 education data 

#ofsted
#ofsted data
#ofsted SEND 

#pop0to25
#pop0to17
#pop16to17


#ffp

#htst data 
#htst data england 
#ruralurban class

#ehcp and sen 
#ehcp numbers data
#england ehcp pc 
#la ehcp pc 
#send intervention 

#dsg allocations
#dsg deficits 
#dsg deficits pivot 
#finances data 
#send expenditure data 




