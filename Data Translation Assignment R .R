##Lynna Tran
##OSMBA 5300 - Winter 2021
##Data Translation Assignment 

###LOAD LIBRARY###
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

name_trends_scorecard    <- inner_join(name_id, Scorecard, by = 'OPEID', 'UNITID') %>%
  inner_join(trends, by = 'schname')

##RESEARCH Question 
# Among colleges that predominantly grant bachelor’s degrees,
# did it result in more student interest in high-earnings colleges relative to low-earnings ones 
# (as proxied by Google searches for keywords associated with those colleges)?

##FILTERING COLLEGES 

#Colleges that predominantly grant bachelors degrees 

name_trends_scorecard <- distinct(name_trends_scorecard, OPEID, UNITID.x, schname, keyword , keynum, monthorweek , index,  md_earn_wne_p10.REPORTED.EARNINGS)

bachelors_colleges <- name_trends_scorecard %>%
  filter(PREDDEG == 3) %>%
  select(OPEID, UNITID.x, schname, keyword , keynum, monthorweek , index,  md_earn_wne_p10.REPORTED.EARNINGS) %>%
  distinct(schname, md_earn_wne_p10.REPORTED.EARNINGS)  ##running a distinct on college name vs median cost because we know that there's a one to one match already between college name/median earnings

##Earnings of colleges, adding median earnings columns after filtering 
college_earnings <- bachelors_colleges %>%
  filter(md_earn_wne_p10.REPORTED.EARNINGS != 'NULL') %>%
  filter(md_earn_wne_p10.REPORTED.EARNINGS != 'PrivacySuppressed') %>%
  mutate(median_earnings = as.numeric(md_earn_wne_p10.REPORTED.EARNINGS))

##Checking median earnings values for overall college median earnings 
overall_median_earnings <- median(college_earnings$median_earnings)

##Adding dependent variables to be tested such as High Earning Dummy Variable and SD index based off of schname and index 
##adds t/f for high earning and low earning 
interested_colleges <- college_earnings %>%
  mutate(high_earning = median_earnings > 41800) %>%
  mutate(low_earning  = median_earnings < 41799) %>%
  filter(!is.na(index)) ##filtering out NA indexes because if there's no index value, we can't use it anyways 

##adding sd index
sd_index_colleges <- interested_colleges %>%
  group_by(schname, keynum) %>%
  summarise(sd_index = (index - mean(index)) / sd(index), schname,
            keyword, monthorweek, keynum, median_earnings, sd_index, high_earning, low_earning)

####GROUP BY DATE ####
#so the data level can be by year. summing up the sd_index by year helps shows the pattern of the changing index, aggrated into a year 

college_information <- interested_colleges %>% 
  select(schname ,median_earnings, high_earning) %>%
  distinct(schname ,median_earnings, high_earning)

college_sdindex_year2013 <- sd_index_colleges %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE))  %>%
  select(schname, keynum, sd_index, DATE, median_earnings, high_earning) %>%
  filter(between(DATE, as.Date("2013-03-24"), as.Date("2013-12-31"))) %>%
  group_by(schname) %>%
  summarise(sd_index2013 = sum(sd_index))

college_sdindex_year2014 <- sd_index_colleges %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE))  %>%
  select(schname, keynum, sd_index, DATE, median_earnings, high_earning) %>%
  filter(between(DATE, as.Date("2014-01-01"), as.Date("2014-12-31"))) %>%
  group_by(schname) %>%
  summarise(sd_index2014 = sum(sd_index))

college_sdindex_year2015 <- sd_index_colleges %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE))  %>%
  select(schname, keynum, sd_index, DATE, median_earnings, high_earning) %>%
  filter(between(DATE, as.Date("2015-01-01"), as.Date("2015-12-31"))) %>%
  group_by(schname) %>%
  summarise(sd_index2015 = sum(sd_index))

college_sdindex_year2016 <- sd_index_colleges %>% 
  mutate(DATE = substr(monthorweek, 1, 10)) %>%
  mutate(DATE = as.Date(DATE))  %>%
  select(schname, keynum, sd_index, DATE, median_earnings, high_earning) %>%
  filter(between(DATE, as.Date("2016-01-01"), as.Date("2016-12-31"))) %>%
  group_by(schname) %>%
  summarise(sd_index2016 = sum(sd_index))

college_sdindex_year <- left_join(college_sdindex_year2013, college_sdindex_year2014, by = 'schname') %>%
  left_join(college_sdindex_year2015, by = 'schname') %>%
  left_join(college_sdindex_year2016, by = 'schname') %>%
  left_join(college_information, by = 'schname')
  
###2013 MODEL
sd_index_model2013 <- lm(data = college_sdindex_year, median_earnings ~ sd_index2013)
export_summs(sd_index_model2013)

ggplot(data = sd_index_model2013, aes(x = sd_index2013, y = log(median_earnings))) +
  geom_point() + ggtitle("All Schools 2013")

hist(rstandard(( sd_index_model2013 )), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of All Schools 2013' )

sd_index_model2013HE <- lm(data = college_sdindex_year, median_earnings ~ sd_index2013  + high_earning)
export_summs(sd_index_model2013HE)

ggplot(data = sd_index_model2013HE, aes(sd_index2013, log(median_earnings))) +
  geom_point() + ggtitle("All Schools 2013 with High-Earning Dummy Variable")

###2014 MODEL
sd_index_model2014 <- lm(data = college_sdindex_year, median_earnings ~ sd_index2014)
export_summs(sd_index_model2014)

ggplot(data = sd_index_model2013, aes(sd_index2014, median_earnings)) +
  geom_point() + ggtitle("All Schools 2014")

hist(rstandard(( sd_index_model2014 )), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of All Schools 2014' )

sd_index_model2014HE <- lm(data = college_sdindex_year, median_earnings ~ sd_index2014  + high_earning)
export_summs(sd_index_model2014HE)

ggplot(data = sd_index_model2014HE, aes(sd_index2014, median_earnings)) +
  geom_point() + ggtitle("All Schools 2014 with High-Earning Dummy Variable")

###2015 MODEL
sd_index_model2015 <- lm(data = college_sdindex_year, median_earnings ~ sd_index2015)
export_summs(sd_index_model2015)

ggplot(data = sd_index_model2015, aes(sd_index2015, median_earnings)) +
  geom_point() + ggtitle("All Schools 2015")

hist(rstandard(( sd_index_model2015 )), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of All Schools 2015' )

sd_index_model2015HE <- lm(data = college_sdindex_year, median_earnings ~ sd_index2015  + high_earning)
export_summs(sd_index_model2015HE)

ggplot(data = sd_index_model2015HE, aes(sd_index2015, median_earnings)) +
  geom_point() + ggtitle("All Schools 2015 with High-Earning Dummy Variable")

###2016 MODEL
sd_index_model2016 <- lm(data = college_sdindex_year, median_earnings ~ sd_index2016)
export_summs(sd_index_model2016)

ggplot(data = sd_index_model2016, aes(sd_index2016, median_earnings)) +
  geom_point() + ggtitle("All Schools 2016")

hist(rstandard(( sd_index_model2016 )), 
     xlab = "Standardized residuals", main = 'Standardized Residuals of All Schools 2016' )

sd_index_model2016HE <- lm(data = college_sdindex_year, median_earnings ~ sd_index2016  + high_earning)
export_summs(sd_index_model2016HE)

ggplot(data = sd_index_model2016HE, aes(sd_index2016, median_earnings)) +
  geom_point() + ggtitle("All Schools 2016 with High-Earning Dummy Variable")
