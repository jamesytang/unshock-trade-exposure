# --------------------------------------------------------------------------------------------------------------------------------
# PNTR AAR Estimate 
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file computes AAR to study the effect of PNTR with China in 2000. 

# Datasets: 
#    - Fama French 49 Industry Daily Stock Returns
#    - Fama French Daily Market Returns 
#    - Dorn 1990 Imputed County Business Patterns
#    - SIC to FF industry xwalk
#    - FIPS code county name xwalk 


# Libraries -----------------------
library(tidyverse)
library(dplyr)
library(haven)
library(broom)
library(naniar)
library(ggplot2)
library(data.table)
library(R.utils)
# ---------------------------------


rm(list=ls()) 


# Data prep -------------------------------
# Load in daily industry returns data
industry_returns_49 <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/CRSP/industry_return_49.csv')
names(industry_returns_49)[1] <- "date_id"

# Load in daily maret returns data 
market_return_daily <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/CRSP/market_return_daily.csv')
names(market_return_daily)[1] <- "date_id"
market_return_daily <- transform(market_return_daily, date = as.Date(as.character(date_id), "%Y%m%d"))
market_return_daily$date_id <- NULL

# Combine and filter datasets
combined_returns <- as.data.frame(cbind(industry_returns_49, market_return_daily))
combined_returns[combined_returns == -99.99] <- NA
combined_returns_1999 <- subset(combined_returns, date > '1998-12-31' & date < '2000-01-01')

# -----------------------------------------


# Calculate industry abnormal returns ---------------------------------------------------------------------------
# Initialize empty vectors for alpha and beta
alpha <- vector()
beta <- vector()

# Estimating alpha and beta - See Greenland et al 2020 for details
# Each of the 49 FF industries are assigned their own values 
for (j in 2:50){
  y <- combined_returns_1999[j]
  vars <- cbind(combined_returns_1999$Mkt.RF, y)
  vars <- vars[complete.cases(vars), ]
  names(vars)[1] <- "Rm"
  lm_Rm <- lm(as.matrix(vars[2])~as.matrix(vars[1]), data = vars)
  summary_lm_Rm <- summary(lm_Rm)
  alpha[j-1] <- summary_lm_Rm$coefficients[1, 1] 
  beta[j-1] <- summary_lm_Rm$coefficients[2, 1] 
}


combined_returns_2000 <- subset(combined_returns, date > '1999-12-31' & date < '2001-01-01')


# Expected returns
expected_returns_2000 <- data.frame(matrix(
  vector(), 252, 50, dimnames=list(c(), names(combined_returns_2000[1:50]))),
  stringsAsFactors=F)
expected_returns_2000[1] <- combined_returns_2000$date
for (j in 1:49){
  expected_returns_2000[j+1] <- alpha[j] + beta[j]*combined_returns_2000$Mkt.RF
}


# Abnormal Returns
abnormal_returns <- data.frame(matrix(
  vector(), 252, 50, dimnames=list(c(), names(combined_returns_2000[1:50]))),
  stringsAsFactors=F)
abnormal_returns[1] <- combined_returns_2000$date
for (j in 2:50){
  abnormal_returns[j] <- combined_returns_2000[j] - expected_returns_2000[j]
}


# Event study windows: Pick important dates to base AAR pntr estimates 
# Dates: 
#    - May 15, 2000: Introduction of the PNTR bill in the US House of Representatives on
#    - May 24, 2000: Vote to approve PNTR in the House
#    - July 27, 2000: Successful cloture motion to proceed with a vote on PNTR in the US Senate
#    - September 19, 2000: Vote to approve PNTR by the Senate;
#    - October 10, 2000: Signature of PNTR into law by President Clinton;

abnormal_returns_pntr_windows <- abnormal_returns %>% 
  filter((date_id >= '2000-05-11' & date_id <= '2000-05-17') | 
           (date_id >= '2000-05-22' & date_id <= '2000-05-26') |   
           (date_id >= '2000-07-25' & date_id <= '2000-07-31') | 
           (date_id >= '2000-09-15' & date_id <= '2000-09-21') | 
           (date_id >= '2000-10-06' & date_id <= '2000-10-12')) %>% 
  select(-date_id) 


# Combine and finalize AAR industry dataset -----------------------------------------------------------------
abnormal_returns_ev_avg <- data.frame(cbind(colnames(abnormal_returns_pntr_windows), 
                                            colMeans(abnormal_returns_pntr_windows)))
colnames(abnormal_returns_ev_avg) <- c('industry', 'abnormal_returns_ev_avg')
abnormal_returns_ev_avg$FF_48 <- 1:nrow(abnormal_returns_ev_avg)
AAR_pntr_industry <- abnormal_returns_ev_avg


# **** Output file to csv **** 
write.csv(AAR_pntr_industry, "/Users/tangj18/Documents/Honors Research /interim_data/AAR_pntr_industry.csv", row.names = F)


# Remove unnecessary dfs
rm(abnormal_returns,
   abnormal_returns_ev_avg,
   abnormal_returns_pntr_windows,
   combined_returns,
   combined_returns_1999,
   combined_returns_2000,
   expected_returns_2000, 
   industry_returns_49,
   lm_Rm,
   market_return_daily,
   summary_lm_Rm,
   vars,
   y,
   alpha,
   beta,
   j)




# Aggregating AAR industry scores to the county level ------------------------------------
# Loading in data --------------------------
cbp_dorn <- read_dta('/Users/tangj18/Documents/Honors Research /downloaded_data/Extra Data/Pierce_schott/AERI2018-0396_data/20180396_input/CBP_1990adj_dorn.dta')
cbp_dorn <- cbp_dorn[complete.cases(cbp_dorn$code4), ]
cbp_dorn_90 <- cbp_dorn %>% 
  mutate(sic = code4) %>%
  mutate(fipscounty = countyid) %>%
  select(fipscounty, sic, imp_emp)

sic_ffindustry_xwalk <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/xwalks/SIC_to_Fama_French_industry.csv')
sic_ffindustry_xwalk$SIC0 <- NULL

fips_county_xwalk <- read_csv('/Users/tangj18/Documents/Honors Research /downloaded_data/xwalks/fips_county1990.csv')
fips_county_xwalk  <- fips_county_xwalk %>%
  filter(str_sub(fips,-3,-1)!="000")
# ------------------------------------------


# Code conversion CBP --> Fama French Industries 
# Conversion from SIC --> Fama French 48
cbp_dorn_90_sic_ff <- cbp_dorn_90 %>%
  left_join(sic_ffindustry_xwalk, by=c("sic"="SIC")) %>%
  select(fipscounty, sic, FF_48, imp_emp) %>%
  distinct(fipscounty, sic, FF_48, imp_emp, .keep_all = T) %>%
  filter(!is.na(FF_48))

options(scipen = 999)

# Aggregate employment counts - to help with weighted averages
county_industry_emp <- cbp_dorn_90_sic_ff %>%
  group_by(fipscounty, FF_48) %>%
  summarise(sum_emp = sum(imp_emp))
county_total_emp <- county_industry_emp %>%
  group_by(fipscounty) %>%
  summarise(total_emp = sum(sum_emp))
county_industry_emp <- county_industry_emp %>%
  left_join(county_total_emp)


# AAR at the county level 
AAR_county_1990 <- county_industry_emp %>%
  left_join(AAR_pntr_industry) %>%
  mutate(abnormal_returns_ev_avg = as.numeric(abnormal_returns_ev_avg)) %>%
  mutate(industry_score = (sum_emp/total_emp)*abnormal_returns_ev_avg) %>%
  group_by(fipscounty) %>%
  summarise(AAR_county = sum(industry_score)) %>%
  mutate(fipscounty = as.character(fipscounty)) %>%
  mutate(AAR_norm = scale(AAR_county))


AAR_pntr_county <- AAR_county_1990


# **** Output file to csv **** 
write.csv(AAR_pntr_county, "/Users/tangj18/Documents/Honors Research /interim_data/AAR_pntr_county.csv", row.names = F)


# Remove unnecessary dfs
rm(cbp_dorn,
   cbp_dorn_90,
   cbp_dorn_90_sic_ff,
   county_industry_emp,
   county_total_emp,
   sic_ffindustry_xwalk,
   fips_county_xwalk)







# END ---------------------------------------------------------------------





















