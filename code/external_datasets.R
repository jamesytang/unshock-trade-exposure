# --------------------------------------------------------------------------------------------------------------------------------
# External Dataseets 
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file formats external datasets for future joins 

# Libraries 
library(tidyverse)
library(dplyr)
library(haven)
library(broom)
library(naniar)
library(ggplot2)
library(data.table)
library(R.utils)
library(data.table)


rm(list=ls()) 


# SEER Population Data - County
# --------------------------------------------------------------------------------------------------------------------------------

# County level population estimates from SEER

# https://seer.cancer.gov/popdata/popdic.html
seer_data <- fread("/Users/tangj18/Documents/Honors Research /downloaded_data/SEER_population_data/us.1990_2019.19ages.adjusted.txt.gz", col.names = "data")

county_pop <- data.frame(matrix(ncol = 10, nrow = nrow(seer_data)))

colnames(county_pop) <- c('year',         'state',     'state_fips',      
                          'county_fips',  'registry',  'race',      
                          'origin',       'sex',       'age',       
                          'population')

county_pop$year <- as.numeric(substr(seer_data$data, 1, 4))
county_pop$state <- substr(seer_data$data, 5, 6)
county_pop$state_fips <- as.numeric(substr(seer_data$data, 7, 8)) 
county_pop$county_fips <- as.numeric(substr(seer_data$data, 9, 11))
county_pop$registry <- substr(seer_data$data, 12, 13)
county_pop$race <- substr(seer_data$data, 14, 14)
county_pop$origin <- substr(seer_data$data, 15, 15)
county_pop$sex <- substr(seer_data$data, 16, 16)
county_pop$age <- substr(seer_data$data, 17, 18)
county_pop$population <- as.numeric(substr(seer_data$data, 19, 26))

county_pop_1990_2019 <- county_pop %>%
  filter(year >= 1990) %>%
  mutate(fipscounty = state_fips*1000 + county_fips) %>%
  group_by(year, fipscounty) %>%
  summarise(total_pop = sum(population),
            white_pop = sum(ifelse(race=='1', population, 0)),
            black_pop = sum(ifelse(race=='2', population, 0)),
            native_american_pop = sum(ifelse(race=='3', population, 0)),
            asian_pop = sum(ifelse(race=='4', population, 0)),
            male_pop = sum(ifelse(sex=='1', population, 0)),
            female_pop = sum(ifelse(sex=='2', population, 0)),
            age_01_04_pop = sum(ifelse(age=='01', population, 0)),
            age_05_09_pop = sum(ifelse(age=='02', population, 0)),
            age_10_14_pop = sum(ifelse(age=='03', population, 0)),
            age_15_19_pop = sum(ifelse(age=='04', population, 0)),
            age_20_24_pop = sum(ifelse(age=='05', population, 0)),
            age_25_29_pop = sum(ifelse(age=='06', population, 0)),
            age_30_34_pop = sum(ifelse(age=='07', population, 0)),
            age_35_39_pop = sum(ifelse(age=='08', population, 0)),
            age_40_44_pop = sum(ifelse(age=='09', population, 0)),
            age_45_49_pop = sum(ifelse(age=='10', population, 0)),
            age_50_54_pop = sum(ifelse(age=='11', population, 0)),
            age_55_59_pop = sum(ifelse(age=='12', population, 0)),
            age_60_64_pop = sum(ifelse(age=='13', population, 0)),
            age_65_69_pop = sum(ifelse(age=='14', population, 0)),
            age_70_74_pop = sum(ifelse(age=='15', population, 0)),
            age_75_79_pop = sum(ifelse(age=='16', population, 0)),
            age_80_84_pop = sum(ifelse(age=='17', population, 0)),
            age_85_plus_pop = sum(ifelse(age=='18', population, 0))
  ) 

write.csv(county_pop_1990_2019, "/Users/tangj18/Documents/Honors Research /interim/county_pop_1990_2019.csv", row.names = F)



# BLS Unemployment Data - County
# --------------------------------------------------------------------------------------------------------------------------------

# County level unemployment rate

la.data.64.County <- read.delim("~/Documents/Honors Research /downloaded_data/BLS/la.data.64.County.txt")
unemployment_county <- subset(la.data.64.County, substr(la.data.64.County$series_id, 20, 20) == '3')
unemployment_county$value <- as.numeric(unemployment_county$value)
unemployment_county$fipscounty <- substr(unemployment_county$series_id, 6, 10)

unemployment_county <- unemployment_county %>% 
  filter(year >= 1990 & year <= 2019) %>%
  select(fipscounty, year, period, value) %>%
  mutate(fipscounty = as.numeric(fipscounty))

write.csv(unemployment_county, "/Users/tangj18/Documents/Honors Research /interim/unemployment_county.csv", row.names = F)



# Split into quarters 
unemployment_county_avg_month <- unemployment_county %>% 
  filter(year >= 2014 & year <= 2019) %>%
  mutate(date = as.Date(paste(year, substr(period, 2, 3), '01', sep='-'))) %>%
  drop_na() 

# install.packages('zoo')
library(zoo)
unemployment_county_avg_quarter <- unemployment_county %>% 
  filter(year >= 2014 & year <= 2019) %>%
  mutate(date = as.Date(paste(year, substr(period, 2, 3), '01', sep='-'))) %>%
  mutate(quarter = as.yearqtr(date, format = "%Y-%m-%d")) %>%
  drop_na() %>%
  group_by(fipscounty, quarter) %>%
  summarise(avg_unemp_rate = mean(value, na.rm=T)) %>%
  mutate(time_counter = row_number())





# Labor Participation Data - County
# --------------------------------------------------------------------------------------------------------------------------------

# County level labor force participation rates

county_pop_1990_2019 <- read_csv("/Users/tangj18/Documents/Honors Research /interim/county_pop_1990_2019.csv") %>%
  select(fipscounty, year, total_pop, age_01_04_pop, age_05_09_pop, age_10_14_pop, age_15_19_pop) %>%
  mutate(nicivpop = total_pop - (age_01_04_pop + age_05_09_pop + age_10_14_pop + (age_15_19_pop)*0.6)) %>%
  select(fipscounty, year, nicivpop)

# From Pierce Schott (2020)
labor_1990_2015 <- read_dta("/Users/tangj18/Documents/Honors Research /extra/Pierce_Schott/laucnty_1990_2013_countyid.dta") %>%
  select(countyid, year, laborforce) 

# https://www.bls.gov/lau/#cntyaa
# Download and reformat files
laucnty16 <- read_csv("/Users/tangj18/Documents/Honors Research /downloaded_data/labor_force/laucnty16.csv")
laucnty17 <- read_csv("/Users/tangj18/Documents/Honors Research /downloaded_data/labor_force/laucnty17.csv")
laucnty18 <- read_csv("/Users/tangj18/Documents/Honors Research /downloaded_data/labor_force/laucnty18.csv")
laucnty19 <- read_csv("/Users/tangj18/Documents/Honors Research /downloaded_data/labor_force/laucnty19.csv")

labor_2016_2019 <- rbind(
  laucnty16,
  laucnty17,
  laucnty18,
  laucnty19
)

labor_2016_2019 <- labor_2016_2019 %>%
  mutate(countyid = as.numeric(state_fips)*1000 + as.numeric(county_fips)) %>%
  select(countyid, Year, labor_force) %>%
  rename(year = Year,
         laborforce = labor_force) %>%
  arrange(countyid, year)

labor_1990_2019 <- rbind(
  labor_1990_2015,
  labor_2016_2019
)
  
# Normally distributed
lfpr_1990_2019 <- labor_1990_2019 %>%
  select(countyid, year, laborforce) %>%
  left_join(county_pop_1990_2019, by=c("countyid"="fipscounty", "year"="year")) %>%
  mutate(lfpr = (laborforce/nicivpop)*100) %>%
  arrange(countyid, year)

  
write.csv(lfpr_1990_2019, "/Users/tangj18/Documents/Honors Research /interim/lfpr_1990_2019.csv", row.names = F)


# END ------------------------------------
















