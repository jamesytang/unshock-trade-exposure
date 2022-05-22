# --------------------------------------------------------------------------------------------------------------------------------
# PNTR Regressions - Deaths of despair
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file provides difference in differences regression estimates for death outcomes during PNTR


# Libraries 
library(zoo)
library(dplyr)
library(ggplot2)
library(fixest)
library(tidyr)
library(stargazer)


rm(list=ls()) 



# PNTR
# ---------------------------------------------------------------------------------------------
AAR_pntr_county <- read.csv("C:/Users/tangjy/Documents/final_datasets/AAR_pntr_county.csv")
age_adjusted_deaths_pntr <- read.csv("C:/Users/tangjy/Documents/final_datasets/age_adjusted_deaths_pntr.csv")

AAR_pntr_county <- AAR_pntr_county %>%
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


AAR_death_pntr <- AAR_pntr_county %>%
  left_join(age_adjusted_deaths_pntr) %>%
  mutate(post = ifelse(year <= 2000, 0, 1))

reg_dat0 <- AAR_death_pntr %>%
  filter(aar_category %in% c(0, 1)) %>%
  mutate(time_to_treat = year-2000,
         treat = ifelse(aar_category == 0, 1, 0),
         statefips = as.factor(fipscounty %/% 1000),
         adj1 = ifelse(year < 2001, year, 2001),
         adj2 = ifelse(year < 2001, 0, 2001)) %>%
  drop_na()

reg_dat2 <- AAR_death_pntr %>%
  filter(aar_category %in% c(2, 1)) %>%
  mutate(time_to_treat = year-2000,
         treat = ifelse(aar_category == 2, 1, 0),
         statefips = as.factor(fipscounty %/% 1000),
         adj1 = ifelse(year < 2001, year, 2001),
         adj2 = ifelse(year < 2001, 0, 2001)) %>%
  drop_na()

reg_dat <- AAR_death_pntr %>%
  filter(aar_category %in% c(0, 2)) %>%
  mutate(time_to_treat = year-2000,
         treat = ifelse(aar_category == 2, 1, 0),
         statefips = as.factor(fipscounty %/% 1000),
         adj1 = ifelse(year < 2001, year, 2001),
         adj2 = ifelse(year < 2001, 0, 2001)) %>%
  drop_na()


# Drug OD
reg_drug_od_pos = feols(drug_od_aa ~  i(year, treat, ref = 2000) | 
                          fipscounty + year,                             ## FEs
                        cluster = ~fipscounty,                          ## Clustered SEs
                        weights  = log(reg_dat2$county_pop),
                        data = reg_dat2)
iplot(reg_drug_od_pos, 
      xlab = 'Year',
      ylab = 'Difference in Drug Overdose Deaths/100,000'
)

reg_drug_od_neg = feols(drug_od_aa ~  i(year, treat, ref = 2000) | 
                          fipscounty + year,                             ## FEs
                        cluster = ~fipscounty,                          ## Clustered SEs
                        weights  = log(reg_dat0$county_pop),
                        data = reg_dat0)
iplot(reg_drug_od_neg, 
      xlab = 'Year',
      ylab = 'Difference in Drug Overdose Deaths/100,000'
)


# Suicide
reg_suicide = feols(suicide_aa ~  i(year, treat, ref = 2000) | 
                   fipscounty + year,                             ## FEs
                 cluster = ~fipscounty,                          ## Clustered SEs
                 weights  = log(reg_dat$county_pop),
                 data = reg_dat)
iplot(reg_suicide, 
      xlab = 'Year',
      ylab = 'Difference in Suicide Deaths/100,000'
)
summary(twfe_reg)


# Alcohol Related
reg_arld = feols(arld_calc ~  i(year, treat, ref = 2000) | 
                   fipscounty + year,                             ## FEs
                 cluster = ~fipscounty,                          ## Clustered SEs
                 weights  = log(reg_dat$county_pop),
                 data = reg_dat)
iplot(reg_arld,
      xlab = 'Year',
      ylab = 'Difference in ARLD Deaths/100,000'
)
summary(twfe_reg)


reg_drug_od_neg <- lm(drug_od_aa ~ treat*post + treat + post + fipscounty + year, 
                    weights = log(reg_dat0$county_pop),
                    data = reg_dat0)
summary(reg_drug_od_neg)
reg_drug_od_pos <- lm(drug_od_aa ~ treat*post + treat + post + fipscounty + year, 
                      weights = log(reg_dat2$county_pop),
                      data = reg_dat2)
reg_suicide <- lm(suicide_aa ~ treat*post + treat + post + fipscounty + year,
                  weights = log(reg_dat$county_pop),
                  data = reg_dat)
reg_arld <- lm(arld_calc ~ treat*post + treat + post + fipscounty + year,
               weights = log(reg_dat$county_pop),
               data = reg_dat)

mean(reg_dat0$drug_od_aa)
mean(reg_dat2$drug_od_aa)
mean(reg_dat$suicide_aa)
mean(reg_dat$arld_calc)

stargazer(reg_drug_od_neg, 
          reg_drug_od_pos,
          reg_suicide,
          reg_arld)





# # Trade War - Unshock
# # ---------------------------------------------------------------------------------------------
# 
# unshock_county_dat <- read.csv("C:/Users/tangjy/Documents/imported_data/unshock_county_dat.csv")
# age_adjusted_deaths_pntr <- read.csv("C:/Users/tangjy/Documents/final_datasets/age_adjusted_deaths_pntr.csv")
# age_adjusted_deaths_tw <- read.csv("C:/Users/tangjy/Documents/final_datasets/age_adjusted_deaths_tw.csv")
# 
# 
# ushock_pntr <- unshock_county_dat %>%
#   left_join(age_adjusted_deaths_pntr) %>%
#   mutate(post = ifelse(year <= 2000, 0, 1),
#          treat = ifelse(unshock_ind==1, 1, 0)) %>%
#   drop_na() 
# 
# twfe_reg = feols(drug_od_aa ~ i(year, treat, ref=2000) | 
#                    fipscounty + year,                             ## FEs
#                  cluster = ~fipscounty,
#                  weights = log(ushock_pntr$county_pop),
#                  data = ushock_pntr)
# 
# iplot(twfe_reg, 
#       xlab = 'Time to treatment',
#       main = '"Unshocked" vs Control (PNTR) - Drug Overdose')
# 
# 
# summary(twfe_reg)
# 
# 
# ushock_tw <- unshock_county_dat %>%
#   left_join(age_adjusted_deaths_tw) %>%
#   mutate(post = ifelse(quarter <= "2018 Q1", 0, 1),
#          treat = ifelse(unshock_ind==1, 1, 0)) %>%
#   drop_na() %>%
#   group_by(fipscounty) %>%
#   mutate(time_period = row_number())
# 
# 
# 
# twfe_reg = feols(drug_od_aa ~ i(time_period, treat, ref=21) | 
#                    fipscounty + time_period,                             ## FEs
#                  cluster = ~fipscounty,
#                  weights = log(ushock_tw$county_pop),
#                  data = ushock_tw)
# 
# iplot(twfe_reg, 
#       xlab = 'Time to treatment - (quarterly: 2013-2019)',
#       main = '"Unshocked" vs Control (Trade War) - Drug Overdose')
# 
# 
# reg_drug_od_ushock <- lm(drug_od_aa ~ treat*post + treat + post + fipscounty + quarter, 
#                        weights = log(ushock_tw$county_pop),
#                        data = ushock_tw)
# summary(reg_drug_od_ushock)
# 
# 
# ushock_2014_2019 <- read.csv("C:/Users/tangjy/Documents/imported_data/ushock_2014_2019.csv")
# reg_unemp_ushock <- lm(avg_unemp_rate ~ treat*post + treat + post + fipscounty + quarter, 
#                        weights = log(ushock_2014_2019$total_pop),
#                        data = ushock_2014_2019)
# summary(reg_unemp_ushock)
# 
# 
# stargazer(reg_unemp_ushock, reg_drug_od_ushock)






