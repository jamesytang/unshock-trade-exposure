# --------------------------------------------------------------------------------------------------------------------------------
# Preparing AAR PNTR dataset for visualization and regression 
# --------------------------------------------------------------------------------------------------------------------------------

# Libraries
library(tidyverse)
library(dplyr)
library(readr)

rm(list=ls()) 

# --------------------------------------------------------------------------------------------------------------------------------

# Load and reformat AAR pntr county dataset
AAR_pntr_county <- read_csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_pntr_county.csv")

# Separate AAR scores into deciles and quartiles - and an unaffected category 
AAR_pntr_county_edit <- AAR_pntr_county %>%
  mutate(decile = ntile(AAR_county, 10)) %>%
  mutate(bottom_decile = as.factor(ifelse(decile==1, 1, 0)),
         upper_decile = as.factor(ifelse(decile==10, 1, 0))) %>%
  mutate(negative_shock = ifelse(AAR_county < -0.15, 1, 0),
         positive_shock = ifelse(AAR_county > 0.15, 1, 0),
         unaffected = as.factor(ifelse(dplyr::between(AAR_pntr_county$AAR_county, -0.1, 0.1)==TRUE, 1, 0))) %>%
  mutate(aar_category = as.factor(ifelse(negative_shock==1,0,
                                         ifelse(unaffected==1,1,
                                                ifelse(positive_shock==1,2,3))))) %>%
  mutate(quartile = ntile(AAR_county, 4)) %>%
  mutate(bottom_quartile = as.factor(ifelse(quartile==1, 1, 0)),
         upper_quartile = as.factor(ifelse(quartile==4, 1, 0)))

write.csv(AAR_pntr_county_edit, "/Users/tangj18/Documents/Honors Research /interim/AAR_pntr_county_edit.csv", row.names = F)


# AAR PNTR Estimates for unemployment
# Load and clean BLS unemployment data
unemployment_county <- read_csv("/Users/tangj18/Documents/Honors Research /interim/unemployment_county.csv")

unemployment_county_avg_1990_2007 <- unemployment_county %>% 
  filter(year >= 1990 & year <= 2013) %>%
  group_by(fipscounty, year) %>%
  summarise(avg_unemp_rate = mean(value, na.rm = T)) %>%
  mutate(fipscounty = as.numeric(fipscounty)) 

# Not normally distributed
# unemployment_county_avg_1990_2007 %>%
#   filter(year == 2000) %>%
#   ggplot(., aes(x=avg_unemp_rate)) + geom_density()

# SEER Population Data
county_pop <- read_csv("/Users/tangj18/Documents/Honors Research /interim/county_pop_1990_2019.csv")
county_pop <- county_pop %>%
  select(fipscounty, year, total_pop) %>%
  filter(year >= 1990 & year <= 2013) 


# Join BLS unemployment data with SEER population data and county level AAR scores
# Decile plots
AAR_unemp_1990_2007 <- AAR_pntr_county_edit %>%
  left_join(unemployment_county_avg_1990_2007) %>%
  mutate(post = ifelse(year <= 2000, 0, 1)) %>%
  left_join(county_pop, by=c('fipscounty'='fipscounty', 'year'='year'))
year_aar_pop <- AAR_unemp_1990_2007 %>%
  group_by(aar_category, year) %>%
  summarise(aar_category_total_pop = sum(total_pop, na.rm = T)) %>%
  filter(!is.na(year))
AAR_unemp_1990_2007 <- AAR_unemp_1990_2007 %>%
  left_join(year_aar_pop, by=c('aar_category' = 'aar_category', 'year'='year')) %>%
  mutate(weighted_unemp = (total_pop/aar_category_total_pop)*avg_unemp_rate)

write.csv(AAR_unemp_1990_2007, "/Users/tangj18/Documents/Honors Research /interim/AAR_unemp_1990_2007.csv", row.names = F)



# END ---------------------------------------------------------------------
