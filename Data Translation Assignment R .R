##Lynna Tran
##OSMBA 5300 - Winter 2021
##Data Translation Assignment 

library(tidyverse)
library(jtools)
library(car)
library(readr)
library(purrr)
library(lubridate)

#### READ DATA ####
Scorecard <- read.csv("Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")

name_id <- read_csv("Lab3_Rawdata/id_name_link.csv") %>%
  distinct(schname, .keep_all = TRUE) %>%
  rename(OPEID = opeid) %>%
  rename(UNITID = unitid)

prepend <- function(fname) {
    paste('Lab3_Rawdata/', fname, sep = '')
  }
  
trends <- list.files(path = 'Lab3_Rawdata', pattern = 'trends_up_to_') %>%
  map(prepend) %>%
  map(read_csv) %>%
  reduce(rbind)


#### MERGE DATA ####

name_scorecard           <- inner_join(name_id, Scorecard, by = 'OPEID', 'UNITID')
name_trends_scorecard    <- inner_join(trends, name_scorecard, by = 'schname')

##RESEARCH Question 
# Among colleges that predominantly grant bachelor’s degrees,
# did it result in more student interest in high-earnings colleges relative to low-earnings ones 
# (as proxied by Google searches for keywords associated with those colleges)?


#Colleges that predominantly grant bachelors degrees 


bachelors_colleges <- name_trends_scorecard %>%
  filter(PREDDEG == 3) %>%
  select(OPEID, UNITID.x, schname, keyword , keynum, monthorweek , index,  md_earn_wne_p10.REPORTED.EARNINGS)

##Earnings of colleges 

distinct_earnings <- distinct(bachelors_colleges, md_earn_wne_p10.REPORTED.EARNINGS)

distinct_earnings <- bachelors_colleges %>%
  filter(md_earn_wne_p10.REPORTED.EARNINGS != 'NULL') %>%
  filter(md_earn_wne_p10.REPORTED.EARNINGS != 'PrivacySuppressed') %>%
  mutate(median_earnings = as.numeric(md_earn_wne_p10.REPORTED.EARNINGS))

## checking median earnings values 
overall_median_earnings <- median(distinct_earnings$median_earnings)


##adds t/f for high earning and low earning 
interested_colleges <- distinct_earnings %>%
  mutate(high_earning = median_earnings > 41800) %>%
  mutate(low_earning  = median_earnings < 41799)


sd_index_colleges <- interested_colleges %>%
  group_by(schname, keynum) %>%
  summarise(sd_index = (index - mean(index)) / sd(index), schname,
            keyword, monthorweek, keynum, median_earnings, sd_index, high_earning, low_earning)


  
