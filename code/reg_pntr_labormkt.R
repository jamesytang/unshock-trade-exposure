# --------------------------------------------------------------------------------------------------------------------------------
# PNTR Regressions - Labor Market
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file provides difference in differences regression estimates for labor market outcomes during PNTR


# Libraries
library(data.table) ## For some minor data wrangling
library(fixest)     ## NB: Requires version >=0.9.0
library(jtools)
library(dplyr)
library(haven)
library(tidyr)


rm(list=ls()) 


AAR_unemp_1990_2007 <- read.csv("/Users/tangj18/Documents/Honors Research /interim/AAR_unemp_1990_2007.csv")


# Controls 
# ------------------------------------------------------
# MFA exposure measure
mfa <- read_dta("/Users/tangj18/Documents/Honors Research /downloaded_data/Pierce_schott/NTR_1990w_countyid.dta") %>%
  select(countyid, R_a1997_row_90w, R_a2001_all_90w, R_a2004_all_90w)

# Time invariant variables - Chinese subsidies and tariff rates + manufacturing share
time_invariant <- read_dta("/Users/tangj18/Documents/Honors Research /downloaded_data/Pierce_schott/NTR_1990w_countyid.dta") %>%
  select(countyid, R_dsub_90w, R_dr_90w, emp_scratch, empman) %>%
  rename(chn_subsidies = R_dsub_90w,
         chn_tariffs = R_dr_90w,
         total_emp = emp_scratch,
         manuf_emp = empman) %>%
  mutate(manuf_share = (manuf_emp/total_emp)*100,
         chn_subsidies = chn_subsidies*100,
         chn_tariffs = chn_tariffs*100) %>%
  select(-c(total_emp, manuf_emp))

# NTR rates 
ntr_rates <- read_dta("/Users/tangj18/Documents/Honors Research /downloaded_data/Pierce_schott/NTR_ntr_1990w_year_countyid.dta") %>%
  mutate(R_ntr_90w = R_ntr_90w*100) %>%
  rename(ntr_ratees = R_ntr_90w)

# Census vars 
census_1990 <- read_dta("/Users/tangj18/Documents/Honors Research /downloaded_data/Pierce_schott/census_vars_1990_countyid.dta") %>%
  select(countyid, medianhhinc1990, pctnocollege1990, pctveteran1990, pctforeignborn1990, pctmanufacturing1990)

# Labor force participation rate
lfpr_1990_2013 <- read.csv("/Users/tangj18/Documents/Honors Research /interim/lfpr_1990_2019.csv") %>%
  filter(year < 2014)


# Combine control variables into one dataset 
control_variables <- AAR_unemp_1990_2007 %>% select(fipscounty, year) %>%
  left_join(mfa, by = c("fipscounty" = "countyid")) %>%
  mutate(mfa_exposure = ifelse(year < 1997, 0, 
                               ifelse(year >= 1997 & year < 2001, R_a1997_row_90w,
                                      ifelse(year >= 2001 & year < 2004, R_a2001_all_90w,
                                             ifelse(year >= 2004 & year <= 2005, R_a2004_all_90w, 0))))) %>%
  select(-c(R_a1997_row_90w, R_a2001_all_90w, R_a2004_all_90w)) %>%
  left_join(time_invariant, by = c("fipscounty" = "countyid")) %>%
  left_join(ntr_rates, by = c("fipscounty" = "countyid", "year"="year")) %>%
  left_join(census_1990, by = c("fipscounty" = "countyid"))
  


# Dataset for regression
# ----------------------------------------------------
reg_dat_pntr_labor <- AAR_unemp_1990_2007 %>%
  select(fipscounty, year, AAR_county, aar_category, quartile, post, avg_unemp_rate, total_pop) %>%
  filter(aar_category %in% c(0, 2)) %>%
  mutate(time_to_treat = year-2000,
         treat = ifelse(aar_category == 0, 1, 0),
         statefips = as.factor(fipscounty %/% 1000),
         adj1 = ifelse(year < 2001, year, 2001),
         adj2 = ifelse(year < 2001, 0, 2001)) %>%
  left_join(control_variables, by = c("fipscounty" = "fipscounty", "year"="year")) %>%
  left_join(lfpr_1990_2013, by=c("fipscounty"="countyid", "year"="year")) %>%
  filter(year <= 2012) %>%
  drop_na()

write.csv(reg_dat_pntr_labor, "/Users/tangj18/Documents/Honors Research /interim/reg_dat_pntr_labor.csv", row.names = F)

  

# Regression 
# ---------------------------------------------------
# reg_dat_pntr_labor <- read.csv("/Users/tangj18/Documents/Honors Research /interim/reg_dat_pntr_labor.csv")

# Unemployment
twfe_reg = feols(avg_unemp_rate ~  i(year, treat, ref = 2000) | 
                   fipscounty + year,                             ## FEs
                 cluster = ~fipscounty,                          ## Clustered SEs
                 weights  = log(reg_dat_pntr_labor$total_pop),
                 data = reg_dat_pntr_labor)
iplot(twfe_reg, 
      xlab = 'Year',
      ylab = '% Difference in Unemployment Rate',
      main = 'Negative vs Positive (PNTR) - Unemployment Rate'
      )
summary(twfe_reg)

reg_dat_unemp <- reg_dat_pntr_labor %>% filter(year <= 2007)
reg_unemp_pntr <- lm(avg_unemp_rate ~ treat*post + treat + post + fipscounty + year, 
                     weights = log(reg_dat_unemp$total_pop),
                     data = reg_dat_unemp)
stargazer(reg_unemp_pntr)

weighted_unemp <- reg_dat_pntr_labor %>%
  group_by(fipscounty) %>%
  summarise(mean_pop = mean(total_pop),
            avg_unemp = mean(avg_unemp_rate)) %>%
  mutate(weighted_unemp = (mean_pop)*avg_unemp) 

sum(weighted_unemp$weighted_unemp)/(sum(weighted_unemp$mean_pop))



twfe_reg = feols(lfpr ~  i(year, treat, ref = 2000) | 
                   fipscounty + year,                             ## FEs
                 cluster = ~fipscounty,                          ## Clustered SEs
                 weights  = log(reg_dat_pntr_labor$total_pop),
                 data = reg_dat_pntr_labor)
iplot(twfe_reg, 
      xlab = 'Year',
      ylab = "% Difference in LFPR",
      main = 'Negative vs Positive (PNTR) - Labor Force Participation'
)
summary(twfe_reg)


reg_lfpr_pntr <- lm(lfpr ~ treat*post + treat + post + fipscounty + year, 
                     weights = log(reg_dat_pntr_labor$total_pop),
                     data = reg_dat_pntr_labor)
stargazer(reg_unemp_pntr, reg_lfpr_pntr)

weighted_lfpr <- reg_dat_pntr_labor %>%
  group_by(fipscounty) %>%
  summarise(mean_pop = mean(total_pop),
            avg_lfpr = mean(lfpr)) %>%
  mutate(weighted_lfpr = (mean_pop)*avg_lfpr) 

sum(weighted_lfpr$weighted_lfpr)/(sum(weighted_lfpr$mean_pop))


export_summs(reg_unemp_pntr, 
             reg_lfpr_pntr,
             scale = TRUE, 
             model.names = c("Unemployment (1990-2007)", "Labor Force Participation (1990-2012)"))



# END ---------------------------------------------------------------------------------





