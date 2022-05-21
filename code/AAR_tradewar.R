# --------------------------------------------------------------------------------------------------------------------------------
# Trade War AAR Estimate 
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file computes AAR to study the effect of Trump's trade war with China in 2018. 

# Datasets: 
#    - Fama French 49 Industry Daily Stock Returns
#    - Fama French Daily Market Returns 
#    - EFSY 2013 Imputed County Business Patterns
#    - SIC to FF industry xwalk
#    - SIC to NAICS industry xwalk 
#    - SIC 2012 to 2017 xwalk 
#    - FIPS code county name xwalk 


# Libraries 
library(tidyverse)
library(dplyr)
library(haven)
library(broom)
library(naniar)
library(ggplot2)
library(data.table)
library(R.utils)



rm(list=ls()) 


# --------------------------------------------------------------------------------------------------------------------------------

# Data Prep 
# Loading in data
# Fama French 49 industry returns (daily)
industry_returns_49 <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/CRSP/industry_return_49.csv')
names(industry_returns_49)[1] <- "date_id"

# Fama French market returns (daily)
market_return_daily <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/CRSP/market_return_daily.csv')
names(market_return_daily)[1] <- "date_id"
market_return_daily <- transform(market_return_daily, date = as.Date(as.character(date_id), "%Y%m%d"))
market_return_daily$date_id <- NULL


# --------------------------------------------------------------------------------------------------------------------------------

# Calculate Industry Abnormal Returns 
# Estimating alpha and beta
combined_returns <- as.data.frame(cbind(industry_returns_49, market_return_daily))
combined_returns[combined_returns == -99.99] <- NA
combined_returns[combined_returns == -999] <- NA

# Subset for available dates in 2017 - year prior to the trade war
# Used to create estimates for alpha and beta
combined_returns_2017 <- subset(combined_returns, date > '2016-12-31' & date < '2018-01-01')

# Initialize vectors for alpha and beta
alpha <- vector()
beta <- vector()

# Regress market portfolio return against every industry j
# Only regress columns with complete observations
for (j in 2:50){
  y <- combined_returns_2017[j]
  vars <- cbind(combined_returns_2017$Mkt.RF, y)
  vars <- vars[complete.cases(vars), ]
  names(vars)[1] <- "Rm"
  lm_Rm <- lm(as.matrix(vars[2])~as.matrix(vars[1]), data = vars)
  summary_lm_Rm <- summary(lm_Rm)
  alpha[j-1] <- summary_lm_Rm$coefficients[1, 1] 
  beta[j-1] <- summary_lm_Rm$coefficients[2, 1] 
}


# Expected returns for 2018 using estimated alpha and beta
# Market portfolio return is the only factor 
combined_returns_2018 <- subset(combined_returns, date > '2017-12-31' & date < '2019-01-01')

# Initialize empty df for expected returns by date
expected_returns_2018 <- data.frame(matrix(
  vector(), 251, 50, dimnames=list(c(), names(combined_returns_2018[1:50]))),
  stringsAsFactors=F)
expected_returns_2018[1] <- combined_returns_2018$date

# Linear model to calculate expected returns for industry j using alpha_j beta_j
for (j in 1:49){
  expected_returns_2018[j+1] <- alpha[j] + beta[j]*combined_returns_2018$Mkt.RF
}


# Abnormal returns for 2018
# Initialize empty df for abnormal returns by date
abnormal_returns <- data.frame(matrix(
  vector(), 251, 50, dimnames=list(c(), names(combined_returns_2018[1:50]))),
  stringsAsFactors=F)
abnormal_returns[1] <- combined_returns_2018$date

# Abnormal returns = Realized returns - expected returns 
for (j in 2:50){
  abnormal_returns[j] <- combined_returns_2018[j] - expected_returns_2018[j]
}


# Event study windows: Pick important dates to base AAR pntr estimates 
# Dates: 
#    - March 1, 2018: Announcement of steel and aluminum tariffs
#    - March 22, 2018: Announcement that the U.S. was proposing tariffs on a large fraction of Chinese imports
#    - April 2, 2018: Announcement of Chinese retaliation on 128 categories of U.S. exports
#    - June 15, 2018: Announcement that China was going to retaliate against $50 billion of U.S. exports
#    - July 5, 2018: Mexican announcement that they were going to retaliate in response to the steel and aluminum tariffs; 
#    - July 25, 2018: Announcement that the European Union was preparing retaliatory tariffs on $20 billion of U.S. exports;
#    - September 17, 2018: Announcement of tariffs on $200 billion of Chinese imports 

abnormal_returns_war_windows <- abnormal_returns %>% 
  filter((date_id >= '2018-02-27' & date_id <= '2018-03-05') | #  
           (date_id >= '2018-03-20' & date_id <= '2018-03-26') | # 
           (date_id >= '2018-03-28' & date_id <= '2018-04-04') | # 
           (date_id >= '2018-06-13' & date_id <= '2018-06-19') | # 
           (date_id >= '2018-07-02' & date_id <= '2018-07-09') | # 
           (date_id >= '2018-07-23' & date_id <= '2018-07-27') | 
           (date_id >= '2018-09-13' & date_id <= '2018-09-19')) %>% 
  select(-date_id) 


# Separate scores per event
abnormal_returns_war_windows1 <- abnormal_returns %>% 
  filter(date_id >= '2018-02-27' & date_id <= '2018-03-05') %>% 
  select(-date_id) 

abnormal_returns_war_windows2 <- abnormal_returns %>% 
  filter(date_id >= '2018-03-20' & date_id <= '2018-03-26') %>% 
  select(-date_id) 

abnormal_returns_war_windows3 <- abnormal_returns %>% 
  filter(date_id >= '2018-03-28' & date_id <= '2018-04-04') %>% 
  select(-date_id) 

abnormal_returns_war_windows4 <- abnormal_returns %>% 
  filter(date_id >= '2018-06-13' & date_id <= '2018-06-19') %>% 
  select(-date_id) 

abnormal_returns_war_windows5 <- abnormal_returns %>% 
  filter(date_id >= '2018-07-02' & date_id <= '2018-07-09') %>% 
  select(-date_id)

abnormal_returns_war_windows6 <- abnormal_returns %>% 
  filter(date_id >= '2018-07-23' & date_id <= '2018-07-27') %>% 
  select(-date_id) 

abnormal_returns_war_windows7 <- abnormal_returns %>% 
  filter(date_id >= '2018-09-13' & date_id <= '2018-09-19') %>% 
  select(-date_id) 



# Combine and finalize AAR industry dataset 
abnormal_returns_ev_avg <- data.frame(cbind(colnames(abnormal_returns_war_windows), 
                                            colMeans(abnormal_returns_war_windows)))
colnames(abnormal_returns_ev_avg) <- c('industry', 'abnormal_returns_ev_avg')
abnormal_returns_ev_avg$abnormal_returns_ev_avg <- as.numeric(abnormal_returns_ev_avg$abnormal_returns_ev_avg)
abnormal_returns_ev_avg$FF_48 <- 1:nrow(abnormal_returns_ev_avg)

abnormal_returns_ev_avg1 <- data.frame(cbind(colnames(abnormal_returns_war_windows1), 
                                            colMeans(abnormal_returns_war_windows1)))
colnames(abnormal_returns_ev_avg1) <- c('industry', 'abnormal_returns_ev_avg1')
abnormal_returns_ev_avg1$abnormal_returns_ev_avg1 <- as.numeric(abnormal_returns_ev_avg1$abnormal_returns_ev_avg1)
abnormal_returns_ev_avg1$FF_48 <- 1:nrow(abnormal_returns_ev_avg1)

abnormal_returns_ev_avg2 <- data.frame(cbind(colnames(abnormal_returns_war_windows2), 
                                             colMeans(abnormal_returns_war_windows2)))
colnames(abnormal_returns_ev_avg2) <- c('industry', 'abnormal_returns_ev_avg2')
abnormal_returns_ev_avg2$abnormal_returns_ev_avg2 <- as.numeric(abnormal_returns_ev_avg2$abnormal_returns_ev_avg2)
abnormal_returns_ev_avg2$FF_48 <- 1:nrow(abnormal_returns_ev_avg2)

abnormal_returns_ev_avg3 <- data.frame(cbind(colnames(abnormal_returns_war_windows3), 
                                             colMeans(abnormal_returns_war_windows3)))
colnames(abnormal_returns_ev_avg3) <- c('industry', 'abnormal_returns_ev_avg3')
abnormal_returns_ev_avg3$abnormal_returns_ev_avg3 <- as.numeric(abnormal_returns_ev_avg3$abnormal_returns_ev_avg3)
abnormal_returns_ev_avg3$FF_48 <- 1:nrow(abnormal_returns_ev_avg3)

abnormal_returns_ev_avg4 <- data.frame(cbind(colnames(abnormal_returns_war_windows4), 
                                             colMeans(abnormal_returns_war_windows4)))
colnames(abnormal_returns_ev_avg4) <- c('industry', 'abnormal_returns_ev_avg4')
abnormal_returns_ev_avg4$abnormal_returns_ev_avg4 <- as.numeric(abnormal_returns_ev_avg4$abnormal_returns_ev_avg4)
abnormal_returns_ev_avg4$FF_48 <- 1:nrow(abnormal_returns_ev_avg4)

abnormal_returns_ev_avg5 <- data.frame(cbind(colnames(abnormal_returns_war_windows5), 
                                             colMeans(abnormal_returns_war_windows5)))
colnames(abnormal_returns_ev_avg5) <- c('industry', 'abnormal_returns_ev_avg5')
abnormal_returns_ev_avg5$abnormal_returns_ev_avg5 <- as.numeric(abnormal_returns_ev_avg5$abnormal_returns_ev_avg5)
abnormal_returns_ev_avg5$FF_48 <- 1:nrow(abnormal_returns_ev_avg5)

abnormal_returns_ev_avg6 <- data.frame(cbind(colnames(abnormal_returns_war_windows6), 
                                             colMeans(abnormal_returns_war_windows6)))
colnames(abnormal_returns_ev_avg6) <- c('industry', 'abnormal_returns_ev_avg6')
abnormal_returns_ev_avg6$abnormal_returns_ev_avg6 <- as.numeric(abnormal_returns_ev_avg6$abnormal_returns_ev_avg6)
abnormal_returns_ev_avg6$FF_48 <- 1:nrow(abnormal_returns_ev_avg6)

abnormal_returns_ev_avg7 <- data.frame(cbind(colnames(abnormal_returns_war_windows7), 
                                             colMeans(abnormal_returns_war_windows7)))
colnames(abnormal_returns_ev_avg7) <- c('industry', 'abnormal_returns_ev_avg7')
abnormal_returns_ev_avg7$abnormal_returns_ev_avg7 <- as.numeric(abnormal_returns_ev_avg7$abnormal_returns_ev_avg7)
abnormal_returns_ev_avg7$FF_48 <- 1:nrow(abnormal_returns_ev_avg7)

AAR_tradewar_industry <- abnormal_returns_ev_avg %>%
  left_join(abnormal_returns_ev_avg1, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  left_join(abnormal_returns_ev_avg2, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  left_join(abnormal_returns_ev_avg3, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  left_join(abnormal_returns_ev_avg4, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  left_join(abnormal_returns_ev_avg5, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  left_join(abnormal_returns_ev_avg6, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  left_join(abnormal_returns_ev_avg7, by = c("industry"="industry", "FF_48"="FF_48")) %>%
  relocate(FF_48, .after = industry)


# **** Output file to csv **** 
write.csv(AAR_tradewar_industry, "/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_industry.csv", row.names = F)


# Remove unnecessary dfs
rm(abnormal_returns,
   abnormal_returns_ev_avg,
   abnormal_returns_war_windows,
   combined_returns,
   combined_returns_2017,
   combined_returns_2018,
   expected_returns_2018, 
   industry_returns_49,
   lm_Rm,
   market_return_daily, 
   summary_lm_Rm,
   vars,
   y,
   alpha,
   beta,
   j)


# --------------------------------------------------------------------------------------------------------------------------------

# Aggregating AAR industry scores to the county level 
# Loading in data 
cbp_13 <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/county_business_patterns/efsy_cbp_2013.csv')
cbp_13$avg_emp <- cbp_13$lb
cbp_13$lb <- NULL
cbp_13$ub <- NULL

fips_county_xwalk <- read_csv('/Users/tangj18/Documents/Honors Research /xwalks/fips_county2014.csv')

naics_2012_2017 <- read_csv('/Users/tangj18/Documents/Honors Research /xwalks/2012_to_2017_NAICS.csv')
naics_2012_2017$X5 <- NULL
naics_2012_2017$X6 <- NULL
naics_2012_2017$X7 <- NULL
naics_2012_2017$X8 <- NULL
naics_2012_2017$X9 <- NULL

sic_naics_xwalk_2017 <- read_csv('/Users/tangj18/Documents/Honors Research /xwalks/sic_naics_xwalk_2017_2022.csv')

sic_naics_xwalk_2017 <- sic_naics_xwalk_2017[!duplicated(sic_naics_xwalk_2017), ]

sic_ffindustry_xwalk <- read_csv('/Users/tangj18/Documents/Honors Research /xwalks/SIC_to_Fama_French_industry.csv')
sic_ffindustry_xwalk$SIC0 <- NULL


# Code conversion CBP --> Fama French Industries
# Fips code
cbp_13$fipscounty <- cbp_13$fipstate*1000 + cbp_13$fipscty

# Join fips code county name xwalk
cbp_13 <- cbp_13 %>%
  left_join(fips_county_xwalk)

# Filter for naics 6 code only
cbp_13_naics6 <- cbp_13[!grepl("/$", cbp_13$naics),, drop = FALSE]
cbp_13_naics6 <- cbp_13_naics6[!grepl("-$", cbp_13_naics6$naics),, drop = FALSE]
cbp_13_naics6$naics <- as.integer(cbp_13_naics6$naics)

# Conversion from naics6 (2012) to naics6 (2017)
cbp_13_naics6_2017 <- cbp_13_naics6 %>%
  left_join(naics_2012_2017, by=c("naics"="NAICS_2012")) %>% 
  select(fipscounty, NAICS_2017, avg_emp) %>%
  distinct(fipscounty, NAICS_2017, avg_emp, .keep_all = T) %>%
  filter(avg_emp != 0)

# Conversion from naics6 2017 --> SIC 2017 --> Fama French 48
cbp_13_naics_sic_ff <- cbp_13_naics6_2017 %>%
  left_join(sic_naics_xwalk_2017, by=c("NAICS_2017"="NAICS")) %>%
  mutate(SIC = as.numeric(SIC)) %>%
  left_join(sic_ffindustry_xwalk) %>%
  select(fipscounty, NAICS_2017, SIC, FF_48, avg_emp) %>%
  distinct(fipscounty, NAICS_2017, FF_48, avg_emp, .keep_all = T)

# Fixing codes that get duplicated through conversion process
dupes <- cbp_13_naics_sic_ff %>%
  select(fipscounty, NAICS_2017, avg_emp) %>%
  group_by(fipscounty, NAICS_2017, avg_emp) %>%
  summarise(count_dupe = n())

county_industry_emp <- cbp_13_naics_sic_ff %>%
  left_join(dupes) %>%
  mutate(adj_emp = avg_emp/count_dupe) %>%
  select(fipscounty, FF_48, adj_emp) 

options(scipen = 999)
county_industry_emp <- county_industry_emp %>%
  group_by(fipscounty, FF_48) %>%
  summarise(sum_emp = sum(adj_emp))
county_total_emp <- county_industry_emp %>%
  group_by(fipscounty) %>%
  summarise(total_emp = sum(sum_emp))
county_industry_emp <- county_industry_emp %>%
  left_join(county_total_emp)
county_industry_emp$FF_48[is.na(county_industry_emp$FF_48)] <- 49

write.csv(county_industry_emp, "/Users/tangj18/Documents/Honors Research /interim/county_industry_emp.csv", row.names = F)


county_AAR <- county_industry_emp %>%
  left_join(AAR_tradewar_industry) %>%
  mutate(abnormal_returns_ev_avg = as.numeric(abnormal_returns_ev_avg)) %>%
  mutate(industry_score = (sum_emp/total_emp)*abnormal_returns_ev_avg,
         industry_score1 = (sum_emp/total_emp)*abnormal_returns_ev_avg1,
         industry_score2 = (sum_emp/total_emp)*abnormal_returns_ev_avg2,
         industry_score3 = (sum_emp/total_emp)*abnormal_returns_ev_avg3,
         industry_score4 = (sum_emp/total_emp)*abnormal_returns_ev_avg4,
         industry_score5 = (sum_emp/total_emp)*abnormal_returns_ev_avg5,
         industry_score6 = (sum_emp/total_emp)*abnormal_returns_ev_avg6,
         industry_score7 = (sum_emp/total_emp)*abnormal_returns_ev_avg7) %>%
  group_by(fipscounty) %>%
  summarise(AAR_county = sum(industry_score),
            AAR_county1 = sum(industry_score1),
            AAR_county2 = sum(industry_score2),
            AAR_county3 = sum(industry_score3),
            AAR_county4 = sum(industry_score4),
            AAR_county5 = sum(industry_score5),
            AAR_county6 = sum(industry_score6),
            AAR_county7 = sum(industry_score7))

AAR_tradewar_county <- county_AAR


# **** Output file to csv **** 
write.csv(AAR_tradewar_county, "/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_county.csv", row.names = F)


# Remove unnecessary dfs
rm(cbp_13,
   cbp_13_naics6,
   cbp_13_naics6_2017,
   county_AAR,
   county_industry_emp,
   dupes,
   fips_county_xwalk,
   naics_2012_2017, 
   sic_ffindustry_xwalk,
   sic_naics_xwalk_2017,
   cbp_13_naics_sic_ff,
   county_total_emp)



# END ----------------------------





















