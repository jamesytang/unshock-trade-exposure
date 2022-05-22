# --------------------------------------------------------------------------------------------------------------------------------
# "Unshocking" regression analysis
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file compares PNTR negative counties that are trade war positive vs trade war negative 
# As such, we use AAR to see how the trade war impacted negatively exposed counties


# Libraries
library(tidyverse)
library(dplyr)
library(haven)
library(broom)
library(naniar)
library(ggplot2)
library(data.table)
library(R.utils)
library(zoo)
library(fixest)


rm(list=ls()) 


# -------------------------------------------------------------------------------------------
# Unshock - Industry Method

# Industry Level Dataset construction
# -------------------------------------------------------------------------------------------

# Filter and label counties that were unaffected vs. "unshocked"
AAR_pntr_industry <- read.csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_pntr_industry.csv")
AAR_tw_industry <- read.csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_industry.csv")
AAR_pntr_county <- read.csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_pntr_county.csv")
AAR_tw_county <- read.csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_county.csv")

AAR_industry <- AAR_pntr_industry %>%
  left_join(AAR_tw_industry, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  rename(aar_pntr = abnormal_returns_ev_avg.x,
         aar_tw = abnormal_returns_ev_avg.y) %>%
  mutate(negative_pntr = ifelse(aar_pntr < 0, 1, 0),
         positive_pntr = ifelse(aar_pntr > 0, 1, 0),
         negative_tw = ifelse(aar_tw < 0, 1, 0),
         positive_tw = ifelse(aar_tw > 0, 1, 0),
         negative_tw1 = ifelse(abnormal_returns_ev_avg1 < 0, 1, 0),
         positive_tw1 = ifelse(abnormal_returns_ev_avg1 > 0, 1, 0),
         negative_tw2 = ifelse(abnormal_returns_ev_avg2 < 0, 1, 0),
         positive_tw2 = ifelse(abnormal_returns_ev_avg2 > 0, 1, 0),
         negative_tw3 = ifelse(abnormal_returns_ev_avg3 < 0, 1, 0),
         positive_tw3 = ifelse(abnormal_returns_ev_avg3 > 0, 1, 0),
         negative_tw4 = ifelse(abnormal_returns_ev_avg4 < 0, 1, 0),
         positive_tw4 = ifelse(abnormal_returns_ev_avg4 > 0, 1, 0),
         negative_tw5 = ifelse(abnormal_returns_ev_avg5 < 0, 1, 0),
         positive_tw5 = ifelse(abnormal_returns_ev_avg5 > 0, 1, 0),
         negative_tw6 = ifelse(abnormal_returns_ev_avg6 < 0, 1, 0),
         positive_tw6 = ifelse(abnormal_returns_ev_avg6 > 0, 1, 0),
         negative_tw7 = ifelse(abnormal_returns_ev_avg7 < 0, 1, 0),
         positive_tw7 = ifelse(abnormal_returns_ev_avg7 > 0, 1, 0)) %>%
  mutate(control = ifelse(negative_pntr == 1 & negative_tw == 1, 1, 0),
         unshocked = ifelse(negative_pntr == 1 & positive_tw == 1, 1, 0),
         control1 = ifelse(negative_pntr == 1 & negative_tw1 == 1, 1, 0),
         unshocked1 = ifelse(negative_pntr == 1 & positive_tw1 == 1, 1, 0),
         control2 = ifelse(negative_pntr == 1 & negative_tw2 == 1, 1, 0),
         unshocked2 = ifelse(negative_pntr == 1 & positive_tw2 == 1, 1, 0),
         control3 = ifelse(negative_pntr == 1 & negative_tw3 == 1, 1, 0),
         unshocked3 = ifelse(negative_pntr == 1 & positive_tw3 == 1, 1, 0),
         control4 = ifelse(negative_pntr == 1 & negative_tw4 == 1, 1, 0),
         unshocked4 = ifelse(negative_pntr == 1 & positive_tw4 == 1, 1, 0),
         control5 = ifelse(negative_pntr == 1 & negative_tw5 == 1, 1, 0),
         unshocked5 = ifelse(negative_pntr == 1 & positive_tw5 == 1, 1, 0),
         control6 = ifelse(negative_pntr == 1 & negative_tw6 == 1, 1, 0),
         unshocked6 = ifelse(negative_pntr == 1 & positive_tw6 == 1, 1, 0),
         control7 = ifelse(negative_pntr == 1 & negative_tw7 == 1, 1, 0),
         unshocked7 = ifelse(negative_pntr == 1 & positive_tw7 == 1, 1, 0))

unshock <- AAR_industry %>%
  filter(control == 1 | unshocked == 1)


# Filter counties w/ threshold of jobs 
county_industry_emp <- read.csv("/Users/tangj18/Documents/Honors Research /interim/county_industry_emp.csv") %>%
  mutate(pct_emp = (sum_emp/total_emp)*100)

control_industries <- unique(unshock$FF_48[unshock$control==1])
unshock_industries <- unique(unshock$FF_48[unshock$unshocked==1])

unshock_county_dat <- county_industry_emp %>%
  mutate(control_ind = ifelse(FF_48 %in% control_industries & pct_emp >= 20, 1, 0),
         unshock_ind = ifelse(FF_48 %in% unshock_industries & pct_emp >= 20, 1, 0)) %>%
  filter(control_ind == 1 | unshock_ind == 1) %>%
  select(fipscounty, control_ind, unshock_ind) %>%
  distinct() %>%
  group_by(fipscounty) %>%
  summarise(control_ind = sum(control_ind),
            unshock_ind = sum(unshock_ind)) %>%
  filter(!(control_ind==1 & unshock_ind==1)) %>%
  left_join(AAR_pntr_county) %>%
  left_join(AAR_tw_county, by = c("fipscounty")) %>%
  rename(aar_pntr = AAR_county.x,
         aar_tw = AAR_county.y) %>%
  select(-AAR_norm) %>%
  drop_na()

write.csv(unshock_county_dat, "/Users/tangj18/Documents/Honors Research /interim/unshock_county_dat.csv", row.names = F)



# Stage 1: PNTR
# ---------------------------------------------------------------------------------------------------

# Add unemployment and labor force data
unemployment_county <- read.csv("/Users/tangj18/Documents/Honors Research /interim/unemployment_county.csv")

unemployment_county_avg_1990_2007 <- unemployment_county %>% 
  filter(year >= 1990 & year <= 2013) %>%
  group_by(fipscounty, year) %>%
  summarise(avg_unemp_rate = mean(value, na.rm = T)) %>%
  mutate(fipscounty = as.numeric(fipscounty)) 

# SEER Population Data
county_pop <- read.csv("/Users/tangj18/Documents/Honors Research /interim/county_pop_1990_2019.csv")
county_pop <- county_pop %>%
  select(fipscounty, year, total_pop) %>%
  filter(year >= 1990 & year <= 2013) 

# BLS Labor Force Participation Data
lfpr_1990_2013 <- read.csv("/Users/tangj18/Documents/Honors Research /interim/lfpr_1990_2019.csv") %>%
  filter(year < 2014)



# Regression
# ---------------------------------------------------------------------------------------------------

# Regression data set 
ushock_1990_2013 <- unshock_county_dat %>%
  left_join(unemployment_county_avg_1990_2007) %>%
  mutate(post = ifelse(year <= 2000, 0, 1),
         treat = ifelse(unshock_ind==1, 1, 0)) %>%
  left_join(county_pop, by=c('fipscounty'='fipscounty', 'year'='year')) %>%
  left_join(lfpr_1990_2013, by=c("fipscounty"="countyid", "year"="year")) %>%
  drop_na() %>%
  filter(year > 1994 & year < 2008)


# Regression
# Unemployment
twfe_reg = feols(avg_unemp_rate ~ i(year, treat, ref=2000) + aar_pntr | 
                   fipscounty + year,                             
                 cluster = ~fipscounty,
                 weights = log(ushock_1990_2013$total_pop),
                 data = ushock_1990_2013)
iplot(twfe_reg, 
      xlab = 'Year',
      main = '"Unshocked" vs Control (PNTR) - Unemployment',
      ylab = "% Difference in unemployment rate",
      drop = '"log(total_pop)"')
summary(twfe_reg)

reg_unemp_ushock_pntr <- lm(avg_unemp_rate ~ treat*post + treat + post + fipscounty + year, 
                       weights = log(ushock_1990_2013$total_pop),
                       data = ushock_1990_2013)
summary(reg_unemp_ushock_pntr)


weighted_unemp <- ushock_1990_2013 %>%
  group_by(fipscounty) %>%
  summarise(mean_pop = mean(total_pop),
            avg_unemp = mean(avg_unemp_rate)) %>%
  mutate(weighted_unemp = (mean_pop)*avg_unemp) 
sum(weighted_unemp$weighted_unemp)/(sum(weighted_unemp$mean_pop))




# Labor Force Participation
twfe_reg = feols(lfpr ~ i(year, treat, ref=2000) + aar_pntr | 
                   fipscounty + year,                             ## FEs
                 cluster = ~fipscounty, 
                 weights = log(ushock_1990_2013$total_pop),
                 data = ushock_1990_2013)
iplot(twfe_reg, 
      xlab = 'Time to treatment',
      main = '"Unshockedr" vs Controlr (PNTR) - Unemployment',
      drop = '"log(total_pop)"')
summary(twfe_reg)



# Stage 2: Trade War
# ---------------------------------------------------------------------------------------------------

unemployment_county_avg_2014_2019 <- unemployment_county %>% 
  filter(year >= 2014) %>%
  mutate(date = as.Date(paste(year, substr(period, 2, 3), '01', sep='-'))) %>%
  mutate(quarter = as.yearqtr(date, format = "%Y-%m-%d")) %>%
  drop_na() %>%
  group_by(fipscounty, year, quarter) %>%
  summarise(avg_unemp_rate = mean(value, na.rm=T))

# years
# unemployment_county_avg_2014_2019 <- unemployment_county %>% 
#   filter(year >= 2014) %>%
#   group_by(fipscounty, year) %>%
#   summarise(avg_unemp_rate = mean(value, na.rm = T)) %>%
#   mutate(fipscounty = as.numeric(fipscounty)) 

county_pop <- read.csv("/Users/tangj18/Documents/Honors Research /interim/county_pop_1990_2019.csv")
county_pop <- county_pop %>%
  select(fipscounty, year, total_pop) %>%
  filter(year >= 2014) 

# BLS Labor Force Participation Data
lfpr_2014_2019 <- read.csv("/Users/tangj18/Documents/Honors Research /interim/lfpr_1990_2019.csv") %>%
  filter(year >= 2014)



# Regression - Trade War
# ---------------------------------------------------------------------------------------------------

# Regression Dataset 
ushock_2014_2019 <- unshock_county_dat %>%
  left_join(unemployment_county_avg_2014_2019) %>%
  mutate(post = ifelse(year <= 2018, 0, 1),
         treat = ifelse(unshock_ind==1, 1, 0)) %>%
  left_join(county_pop, by=c('fipscounty'='fipscounty', 'year'='year')) %>%
  left_join(lfpr_2014_2019, by=c("fipscounty"="countyid", "year"="year")) %>%
  drop_na() %>%
  filter(year >= 2016) %>%
  group_by(fipscounty) %>%
  mutate(time_period = row_number())

write.csv(ushock_2014_2019, "/Users/tangj18/Documents/Honors Research /interim/ushock_2014_2019.csv", row.names = F)

# Regression
twfe_reg = feols(avg_unemp_rate ~ i(quarter, treat, ref = "2018 Q1") + aar_tw | 
                   fipscounty + quarter,                             ## FEs
                 cluster = ~fipscounty,
                 weights = log(ushock_2014_2019$total_pop),
                 data = ushock_2014_2019)

iplot(twfe_reg, 
      xlab = 'Time to treatment (quarterly: 2016-2019)',
      ylab = '% Difference in unemployment rate',
      main = ' "Unshocked" vs Control (Trade War) - Unemployment')


reg_unemp_ushock <- lm(avg_unemp_rate ~ treat*post + treat + post + fipscounty + quarter, 
                       weights = log(ushock_2014_2019$total_pop),
                       data = ushock_2014_2019)
summary(reg_unemp_ushock)

weighted_unemp <- ushock_2014_2019 %>%
  group_by(fipscounty) %>%
  summarise(mean_pop = mean(total_pop),
            avg_unemp = mean(avg_unemp_rate)) %>%
  mutate(weighted_unemp = (mean_pop)*avg_unemp) 
sum(weighted_unemp$weighted_unemp)/(sum(weighted_unemp$mean_pop))

stargazer(reg_unemp_ushock_pntr, reg_unemp_ushock)






# Manufacturing only? 
# Which industries are most/least responsive







# # County level
# # -------------------------------------------------------------------------------------------
# AAR_pntr_county <- read_csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_pntr_county.csv")
# AAR_tw_county <- read_csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_county.csv")
# 
# AAR_county <- AAR_pntr_county %>%
#   left_join(AAR_tw_county, by = c("fipscounty"="fipscounty")) %>%
#   select(-AAR_norm) %>%
#   rename(aar_pntr = AAR_county.x,
#          aar_tw = AAR_county.y) %>%
#   #mutate(aar_tw = aar_tw*5) %>%
#   mutate(negative_pntr = ifelse(aar_pntr < 0, 1, 0),
#          positive_pntr = ifelse(aar_pntr > 0, 1, 0),
#          negative_tw = ifelse(aar_tw < 0, 1, 0),
#          positive_tw = ifelse(aar_tw > 0, 1, 0)) %>%
#   mutate(control = ifelse(negative_pntr == 1 & negative_tw == 1, 1, 0),
#          unshocked = ifelse(negative_pntr == 1 & positive_tw == 1, 1, 0)) %>%
#   #mutate(controlr = ifelse((aar_pntr < -0.15 & aar_pntr > -0.4) & abs(aar_pntr-aar_tw) < 0.05, 1, 0),
#   #       unshockedr = ifelse((aar_pntr < -0.15 & aar_pntr > -0.4) & aar_tw-aar_pntr > 0.3, 1, 0)) %>%
#   filter(control == 1 | unshocked == 1) %>%
#   arrange(unshocked, aar_pntr, aar_tw) 
# 
# # mean(AAR_county$aar_pntr[AAR_county$controlr==1])
# # table(AAR_county$controlr)
# # mean(AAR_county$aar_pntr[AAR_county$unshockedr==1])
# # table(AAR_county$unshockedr)
# 
# # mean(AAR_county$aar_tw - AAR_county$aar_pntr, na.rm=T)
# # sd(AAR_county$aar_tw - AAR_county$aar_pntr, na.rm = T)
# # ggplot(AAR_county, aes(x=(aar_tw-aar_pntr))) + geom_density()
# # 
# # sum((AAR_county$negative_pntr==1) & (abs(AAR_county$aar_tw - AAR_county$aar_pntr) < 0.02), na.rm=T)
# # sum((AAR_county$negative_pntr==1) & (AAR_county$aar_tw - AAR_county$aar_pntr > 0.25), na.rm=T)
# # 
# # AAR_county %>%
# #   filter(control == 1) %>%
# #   ggplot(., aes(x=aar_tw)) + geom_density()
# # 
# # AAR_county %>%
# #   filter(unshocked == 1) %>%
# #   ggplot(., aes(x=aar_tw)) + geom_density()
# # 
# # sd(AAR_county$aar_tw[AAR_county$unshocked==1])
# # sd(AAR_county$aar_tw[AAR_county$control==1])
# # sd(AAR_county$aar_pntr[AAR_county$unshocked==1])
# # sd(AAR_county$aar_pntr[AAR_county$control==1])
# 
# 
# ushock_dat <- AAR_county %>%
#   filter((aar_pntr < -0.1 & aar_pntr > -0.5) & (aar_tw < -0.055 | aar_tw > 0.04)) %>%
#   select(-c(negative_pntr, positive_pntr, negative_tw, positive_tw))
# 
# mean(ushock_dat$aar_pntr[ushock_dat$control==1])
# mean(ushock_dat$aar_pntr[ushock_dat$unshocked==1])
# 
# 
# 
# 
# 
# # Stage 1 - PNTR
# # -------------------------------------------------------------------------------------
# unemployment_county <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/unemployment_county.csv")
# 
# unemployment_county_avg_1990_2007 <- unemployment_county %>% 
#   filter(year >= 1990 & year <= 2013) %>%
#   group_by(fipscounty, year) %>%
#   summarise(avg_unemp_rate = mean(value, na.rm = T)) %>%
#   mutate(fipscounty = as.numeric(fipscounty)) 
# 
# # SEER Population Data
# county_pop <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/county_pop_1990_2019.csv")
# county_pop <- county_pop %>%
#   select(fipscounty, year, total_pop) %>%
#   filter(year >= 1990 & year <= 2013) 
# 
# # BLS Labor Force Participation Data
# lfpr_1990_2013 <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/lfpr_1990_2019.csv") %>%
#   filter(year < 2014)
# 
# 
# # Join BLS unemployment data with SEER population data and county level AAR scores
# # Decile plots
# ushock_1990_2013 <- ushock_dat %>%
#   left_join(unemployment_county_avg_1990_2007) %>%
#   mutate(post = ifelse(year <= 2000, 0, 1),
#          treat = ifelse(unshocked==1, 1, 0)) %>%
#   left_join(county_pop, by=c('fipscounty'='fipscounty', 'year'='year')) %>%
#   left_join(lfpr_1990_2013, by=c("fipscounty"="countyid", "year"="year")) %>%
#   drop_na()
# 
# # Regression
# twfe_reg = feols(avg_unemp_rate ~ i(year, treat, ref = 1999) + log(total_pop) | 
#                    fipscounty + year,                             ## FEs
#                  cluster = ~fipscounty,                          ## Clustered SEs
#                  data = ushock_1990_2013)
# 
# iplot(twfe_reg, 
#       xlab = 'Time to treatment',
#       main = '"Unshockedr" vs Controlr (PNTR) - Unemployment')
# 
# 
# summary(twfe_reg)
# 
# 
# ushock_pop <- ushock_1990_2013 %>%
#   group_by(treat, year) %>%
#   summarise(avg_pop = mean(total_pop)) 
# 
# 
# 
# 
# 
# # Stage 2 - Trade War
# # -------------------------------------------------------------------------------------
# # Split to quarters
# unemployment_county_avg_2014_2019 <- unemployment_county %>% 
#   filter(year >= 2014) %>%
#   mutate(date = as.Date(paste(year, substr(period, 2, 3), '01', sep='-'))) %>%
#   mutate(quarter = as.yearqtr(date, format = "%Y-%m-%d")) %>%
#   drop_na() %>%
#   group_by(fipscounty, year, quarter) %>%
#   summarise(avg_unemp_rate = mean(value, na.rm=T))
# 
# # years
# # unemployment_county_avg_2014_2019 <- unemployment_county %>% 
# #   filter(year >= 2014) %>%
# #   group_by(fipscounty, year) %>%
# #   summarise(avg_unemp_rate = mean(value, na.rm = T)) %>%
# #   mutate(fipscounty = as.numeric(fipscounty)) 
# 
# county_pop <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/county_pop_1990_2019.csv")
# county_pop <- county_pop %>%
#   select(fipscounty, year, total_pop) %>%
#   filter(year >= 2014) 
# 
# # BLS Labor Force Participation Data
# lfpr_2014_2019 <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/lfpr_1990_2019.csv") %>%
#   filter(year >= 2014)
# 
# 
# 
# ushock_2014_2019 <- ushock_dat %>%
#   left_join(unemployment_county_avg_2014_2019) %>%
#   mutate(post = ifelse(year <= 2018, 0, 1),
#          treat = ifelse(unshocked==1, 1, 0)) %>%
#   left_join(county_pop, by=c('fipscounty'='fipscounty', 'year'='year')) %>%
#   left_join(lfpr_2014_2019, by=c("fipscounty"="countyid", "year"="year")) %>%
#   drop_na()
# 
# # Regression
# twfe_reg = feols(avg_unemp_rate ~ i(quarter, treat, ref = "2018 Q1") + log(total_pop)| 
#                    fipscounty + quarter,                             ## FEs
#                  cluster = ~fipscounty,                          ## Clustered SEs
#                  data = ushock_2014_2019)
# 
# iplot(twfe_reg, 
#       xlab = 'Time to treatment',
#       main = ' "Unshocked" vs Control (Trade War) - Unemployment')
# 
# 
# summary(twfe_reg)






