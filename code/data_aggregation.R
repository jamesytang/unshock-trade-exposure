# ---------------------------------------------------------------------------------------------------------
# Aggregate and join data to calculate age-adjusted death rates 
# ---------------------------------------------------------------------------------------------------------

# R Libraries 
library(dplyr)
library(ggplot2)
library(reshape2)
setwd("~")
setwd("C:/Users/tangjy/Documents/interim")

death_agg_90_95 <- function(mort_year){
  county_mort <- mort_year %>%
    mutate(year = 1900 + data_year,
           fipscounty = residence_fipsstate*1000 + residence_fipscounty,
           suicide_death = ifelse(cause_recode_282 >= 33700 & cause_recode_282 <=34400, 1, 0),
           drug_od_death = ifelse(cause_recode_282 %in% c(31700, 35300), 1, 0),
           arld_death = ifelse(cause_recode_282 == 24200, 1, 0),
           male = ifelse(sex == 1, 1, 0),
           female = ifelse(sex == 2, 1, 0),
           white = ifelse(substr(race, 1, 1) == 1, 1, 0),
           black = ifelse(substr(race, 1, 1) == 2, 1, 0),
           amer_ind = ifelse(substr(race, 1, 1) == 3, 1, 0),
           asian = ifelse(substr(race, 1, 1) %in% c(4, 5, 6, 7, 8), 1, 0),
           other_race = ifelse(substr(race, 1, 1) == 9, 1, 0),
           age_00_01 = ifelse(substr(age, 5, 6) %in% c("01","02"), 1, 0),
           age_01_04 = ifelse(substr(age, 5, 6) %in% c("03","04","05","06"), 1, 0),
           age_05_09 = ifelse(substr(age, 5, 6) == "07", 1, 0),
           age_10_14 = ifelse(substr(age, 5, 6) == "08", 1, 0),
           age_15_19 = ifelse(substr(age, 5, 6) == "09", 1, 0),
           age_20_24 = ifelse(substr(age, 5, 6) == "10", 1, 0),
           age_25_29 = ifelse(substr(age, 5, 6) == "11", 1, 0),
           age_30_34 = ifelse(substr(age, 5, 6) == "12", 1, 0),
           age_35_39 = ifelse(substr(age, 5, 6) == "13", 1, 0),
           age_40_44 = ifelse(substr(age, 5, 6) == "14", 1, 0),
           age_45_49 = ifelse(substr(age, 5, 6) == "15", 1, 0),
           age_50_54 = ifelse(substr(age, 5, 6) == "16", 1, 0),
           age_55_59 = ifelse(substr(age, 5, 6) == "17", 1, 0),
           age_60_64 = ifelse(substr(age, 5, 6) == "18", 1, 0),
           age_65_69 = ifelse(substr(age, 5, 6) == "19", 1, 0),
           age_70_74 = ifelse(substr(age, 5, 6) == "20", 1, 0),
           age_75_79 = ifelse(substr(age, 5, 6) == "21", 1, 0),
           age_80_84 = ifelse(substr(age, 5, 6) == "22", 1, 0),
           age_85_over = ifelse(substr(age, 5, 6) %in% c("23","24","25","26"), 1, 0)
    ) %>%
    group_by(fipscounty, year) %>%
    summarise(total_deaths = n(),
              dod_deaths = sum(suicide_death, drug_od_death, arld_death),
              suicide_deaths = sum(suicide_death),
              drug_od_deaths = sum(drug_od_death),
              arld_deaths = sum(arld_death),
              dod_deaths_00_01 = sum(suicide_death[age_00_01==1], drug_od_death[age_00_01==1], arld_death[age_00_01==1]),
              dod_deaths_01_04 = sum(suicide_death[age_01_04==1], drug_od_death[age_01_04==1], arld_death[age_01_04==1]),
              dod_deaths_05_09 = sum(suicide_death[age_05_09==1], drug_od_death[age_05_09==1], arld_death[age_05_09==1]),
              dod_deaths_10_14 = sum(suicide_death[age_10_14==1], drug_od_death[age_10_14==1], arld_death[age_10_14==1]),
              dod_deaths_15_19 = sum(suicide_death[age_15_19==1], drug_od_death[age_15_19==1], arld_death[age_15_19==1]),
              dod_deaths_20_24 = sum(suicide_death[age_20_24==1], drug_od_death[age_20_24==1], arld_death[age_20_24==1]),
              dod_deaths_25_29 = sum(suicide_death[age_25_29==1], drug_od_death[age_25_29==1], arld_death[age_25_29==1]),
              dod_deaths_30_34 = sum(suicide_death[age_30_34==1], drug_od_death[age_30_34==1], arld_death[age_30_34==1]),
              dod_deaths_35_39 = sum(suicide_death[age_35_39==1], drug_od_death[age_35_39==1], arld_death[age_35_39==1]),
              dod_deaths_40_44 = sum(suicide_death[age_40_44==1], drug_od_death[age_40_44==1], arld_death[age_40_44==1]),
              dod_deaths_45_49 = sum(suicide_death[age_45_49==1], drug_od_death[age_45_49==1], arld_death[age_45_49==1]),
              dod_deaths_50_54 = sum(suicide_death[age_50_54==1], drug_od_death[age_50_54==1], arld_death[age_50_54==1]),
              dod_deaths_55_59 = sum(suicide_death[age_55_59==1], drug_od_death[age_55_59==1], arld_death[age_55_59==1]),
              dod_deaths_60_64 = sum(suicide_death[age_60_64==1], drug_od_death[age_60_64==1], arld_death[age_60_64==1]),
              dod_deaths_65_69 = sum(suicide_death[age_65_69==1], drug_od_death[age_65_69==1], arld_death[age_65_69==1]),
              dod_deaths_70_74 = sum(suicide_death[age_70_74==1], drug_od_death[age_70_74==1], arld_death[age_70_74==1]),
              dod_deaths_75_79 = sum(suicide_death[age_75_79==1], drug_od_death[age_75_79==1], arld_death[age_75_79==1]),
              dod_deaths_80_84 = sum(suicide_death[age_80_84==1], drug_od_death[age_80_84==1], arld_death[age_80_84==1]),
              dod_deaths_85_over = sum(suicide_death[age_85_over==1], drug_od_death[age_85_over==1], arld_death[age_85_over==1]),
              suicide_deaths_00_01 = sum(suicide_death[age_00_01==1]),
              suicide_deaths_01_04 = sum(suicide_death[age_01_04==1]),
              suicide_deaths_05_09 = sum(suicide_death[age_05_09==1]),
              suicide_deaths_10_14 = sum(suicide_death[age_10_14==1]),
              suicide_deaths_15_19 = sum(suicide_death[age_15_19==1]),
              suicide_deaths_20_24 = sum(suicide_death[age_20_24==1]),
              suicide_deaths_25_29 = sum(suicide_death[age_25_29==1]),
              suicide_deaths_30_34 = sum(suicide_death[age_30_34==1]),
              suicide_deaths_35_39 = sum(suicide_death[age_35_39==1]),
              suicide_deaths_40_44 = sum(suicide_death[age_40_44==1]),
              suicide_deaths_45_49 = sum(suicide_death[age_45_49==1]),
              suicide_deaths_50_54 = sum(suicide_death[age_50_54==1]),
              suicide_deaths_55_59 = sum(suicide_death[age_55_59==1]),
              suicide_deaths_60_64 = sum(suicide_death[age_60_64==1]),
              suicide_deaths_65_69 = sum(suicide_death[age_65_69==1]),
              suicide_deaths_70_74 = sum(suicide_death[age_70_74==1]),
              suicide_deaths_75_79 = sum(suicide_death[age_75_79==1]),
              suicide_deaths_80_84 = sum(suicide_death[age_80_84==1]),
              suicide_deaths_85_over = sum(suicide_death[age_85_over==1]),
              drug_od_deaths_00_01 = sum(drug_od_death[age_00_01==1]),
              drug_od_deaths_01_04 = sum(drug_od_death[age_01_04==1]),
              drug_od_deaths_05_09 = sum(drug_od_death[age_05_09==1]),
              drug_od_deaths_10_14 = sum(drug_od_death[age_10_14==1]),
              drug_od_deaths_15_19 = sum(drug_od_death[age_15_19==1]),
              drug_od_deaths_20_24 = sum(drug_od_death[age_20_24==1]),
              drug_od_deaths_25_29 = sum(drug_od_death[age_25_29==1]),
              drug_od_deaths_30_34 = sum(drug_od_death[age_30_34==1]),
              drug_od_deaths_35_39 = sum(drug_od_death[age_35_39==1]),
              drug_od_deaths_40_44 = sum(drug_od_death[age_40_44==1]),
              drug_od_deaths_45_49 = sum(drug_od_death[age_45_49==1]),
              drug_od_deaths_50_54 = sum(drug_od_death[age_50_54==1]),
              drug_od_deaths_55_59 = sum(drug_od_death[age_55_59==1]),
              drug_od_deaths_60_64 = sum(drug_od_death[age_60_64==1]),
              drug_od_deaths_65_69 = sum(drug_od_death[age_65_69==1]),
              drug_od_deaths_70_74 = sum(drug_od_death[age_70_74==1]),
              drug_od_deaths_75_79 = sum(drug_od_death[age_75_79==1]),
              drug_od_deaths_80_84 = sum(drug_od_death[age_80_84==1]),
              drug_od_deaths_85_over = sum(drug_od_death[age_85_over==1]),
              arld_deaths_00_01 = sum(arld_death[age_00_01==1]),
              arld_deaths_01_04 = sum(arld_death[age_01_04==1]),
              arld_deaths_05_09 = sum(arld_death[age_05_09==1]),
              arld_deaths_10_14 = sum(arld_death[age_10_14==1]),
              arld_deaths_15_19 = sum(arld_death[age_15_19==1]),
              arld_deaths_20_24 = sum(arld_death[age_20_24==1]),
              arld_deaths_25_29 = sum(arld_death[age_25_29==1]),
              arld_deaths_30_34 = sum(arld_death[age_30_34==1]),
              arld_deaths_35_39 = sum(arld_death[age_35_39==1]),
              arld_deaths_40_44 = sum(arld_death[age_40_44==1]),
              arld_deaths_45_49 = sum(arld_death[age_45_49==1]),
              arld_deaths_50_54 = sum(arld_death[age_50_54==1]),
              arld_deaths_55_59 = sum(arld_death[age_55_59==1]),
              arld_deaths_60_64 = sum(arld_death[age_60_64==1]),
              arld_deaths_65_69 = sum(arld_death[age_65_69==1]),
              arld_deaths_70_74 = sum(arld_death[age_70_74==1]),
              arld_deaths_75_79 = sum(arld_death[age_75_79==1]),
              arld_deaths_80_84 = sum(arld_death[age_80_84==1]),
              arld_deaths_85_over = sum(arld_death[age_85_over==1])
    )
  
  year <- county_mort$year[1]
  mort_year_name <- paste("county_mort", year, sep="_")
  assign(mort_year_name, county_mort, env=.GlobalEnv)
}

unzip("mort_1990.zip")
mort_1990 <- read.csv("mort_1990.csv")
death_agg_90_95(mort_1990)
rm(mort_1990)
gc()

unzip("mort_1991.zip")
mort_1991 <- read.csv("mort_1991.csv")
death_agg_90_95(mort_1991)
rm(mort_1991)
gc()

unzip("mort_1992.zip")
mort_1992 <- read.csv("mort_1992.csv")
death_agg_90_95(mort_1992)
rm(mort_1992)
gc()

unzip("mort_1993.zip")
mort_1993 <- read.csv("mort_1993.csv")
death_agg_90_95(mort_1993)
rm(mort_1993)
gc()

unzip("mort_1994.zip")
mort_1994 <- read.csv("mort_1994.csv")
death_agg_90_95(mort_1994)
rm(mort_1994)
gc()

unzip("mort_1995.zip")
mort_1995 <- read.csv("mort_1995.csv")
death_agg_90_95(mort_1995)
rm(mort_1995)
gc()



death_agg_96_98 <- function(mort_year){
  county_mort <- mort_year %>%
    mutate(year = data_year,
           fipscounty = residence_fipsstate*1000 + residence_fipscounty,
           suicide_death = ifelse(cause_recode_282 >= 33700 & cause_recode_282 <=34400, 1, 0),
           drug_od_death = ifelse(cause_recode_282 %in% c(31700, 35300), 1, 0),
           arld_death = ifelse(cause_recode_282 == 24200, 1, 0),
           male = ifelse(sex == 1, 1, 0),
           female = ifelse(sex == 2, 1, 0),
           white = ifelse(substr(race, 1, 1) == 1, 1, 0),
           black = ifelse(substr(race, 1, 1) == 2, 1, 0),
           amer_ind = ifelse(substr(race, 1, 1) == 3, 1, 0),
           asian = ifelse(substr(race, 1, 1) %in% c(4, 5, 6, 7, 8), 1, 0),
           other_race = ifelse(substr(race, 1, 1) == 9, 1, 0),
           age_00_01 = ifelse(substr(age, 5, 6) %in% c("01","02"), 1, 0),
           age_01_04 = ifelse(substr(age, 5, 6) %in% c("03","04","05","06"), 1, 0),
           age_05_09 = ifelse(substr(age, 5, 6) == "07", 1, 0),
           age_10_14 = ifelse(substr(age, 5, 6) == "08", 1, 0),
           age_15_19 = ifelse(substr(age, 5, 6) == "09", 1, 0),
           age_20_24 = ifelse(substr(age, 5, 6) == "10", 1, 0),
           age_25_29 = ifelse(substr(age, 5, 6) == "11", 1, 0),
           age_30_34 = ifelse(substr(age, 5, 6) == "12", 1, 0),
           age_35_39 = ifelse(substr(age, 5, 6) == "13", 1, 0),
           age_40_44 = ifelse(substr(age, 5, 6) == "14", 1, 0),
           age_45_49 = ifelse(substr(age, 5, 6) == "15", 1, 0),
           age_50_54 = ifelse(substr(age, 5, 6) == "16", 1, 0),
           age_55_59 = ifelse(substr(age, 5, 6) == "17", 1, 0),
           age_60_64 = ifelse(substr(age, 5, 6) == "18", 1, 0),
           age_65_69 = ifelse(substr(age, 5, 6) == "19", 1, 0),
           age_70_74 = ifelse(substr(age, 5, 6) == "20", 1, 0),
           age_75_79 = ifelse(substr(age, 5, 6) == "21", 1, 0),
           age_80_84 = ifelse(substr(age, 5, 6) == "22", 1, 0),
           age_85_over = ifelse(substr(age, 5, 6) %in% c("23","24","25","26"), 1, 0)
    ) %>%
    group_by(fipscounty, year) %>%
    summarise(total_deaths = n(),
              dod_deaths = sum(suicide_death, drug_od_death, arld_death),
              suicide_deaths = sum(suicide_death),
              drug_od_deaths = sum(drug_od_death),
              arld_deaths = sum(arld_death),
              dod_deaths_00_01 = sum(suicide_death[age_00_01==1], drug_od_death[age_00_01==1], arld_death[age_00_01==1]),
              dod_deaths_01_04 = sum(suicide_death[age_01_04==1], drug_od_death[age_01_04==1], arld_death[age_01_04==1]),
              dod_deaths_05_09 = sum(suicide_death[age_05_09==1], drug_od_death[age_05_09==1], arld_death[age_05_09==1]),
              dod_deaths_10_14 = sum(suicide_death[age_10_14==1], drug_od_death[age_10_14==1], arld_death[age_10_14==1]),
              dod_deaths_15_19 = sum(suicide_death[age_15_19==1], drug_od_death[age_15_19==1], arld_death[age_15_19==1]),
              dod_deaths_20_24 = sum(suicide_death[age_20_24==1], drug_od_death[age_20_24==1], arld_death[age_20_24==1]),
              dod_deaths_25_29 = sum(suicide_death[age_25_29==1], drug_od_death[age_25_29==1], arld_death[age_25_29==1]),
              dod_deaths_30_34 = sum(suicide_death[age_30_34==1], drug_od_death[age_30_34==1], arld_death[age_30_34==1]),
              dod_deaths_35_39 = sum(suicide_death[age_35_39==1], drug_od_death[age_35_39==1], arld_death[age_35_39==1]),
              dod_deaths_40_44 = sum(suicide_death[age_40_44==1], drug_od_death[age_40_44==1], arld_death[age_40_44==1]),
              dod_deaths_45_49 = sum(suicide_death[age_45_49==1], drug_od_death[age_45_49==1], arld_death[age_45_49==1]),
              dod_deaths_50_54 = sum(suicide_death[age_50_54==1], drug_od_death[age_50_54==1], arld_death[age_50_54==1]),
              dod_deaths_55_59 = sum(suicide_death[age_55_59==1], drug_od_death[age_55_59==1], arld_death[age_55_59==1]),
              dod_deaths_60_64 = sum(suicide_death[age_60_64==1], drug_od_death[age_60_64==1], arld_death[age_60_64==1]),
              dod_deaths_65_69 = sum(suicide_death[age_65_69==1], drug_od_death[age_65_69==1], arld_death[age_65_69==1]),
              dod_deaths_70_74 = sum(suicide_death[age_70_74==1], drug_od_death[age_70_74==1], arld_death[age_70_74==1]),
              dod_deaths_75_79 = sum(suicide_death[age_75_79==1], drug_od_death[age_75_79==1], arld_death[age_75_79==1]),
              dod_deaths_80_84 = sum(suicide_death[age_80_84==1], drug_od_death[age_80_84==1], arld_death[age_80_84==1]),
              dod_deaths_85_over = sum(suicide_death[age_85_over==1], drug_od_death[age_85_over==1], arld_death[age_85_over==1]),
              suicide_deaths_00_01 = sum(suicide_death[age_00_01==1]),
              suicide_deaths_01_04 = sum(suicide_death[age_01_04==1]),
              suicide_deaths_05_09 = sum(suicide_death[age_05_09==1]),
              suicide_deaths_10_14 = sum(suicide_death[age_10_14==1]),
              suicide_deaths_15_19 = sum(suicide_death[age_15_19==1]),
              suicide_deaths_20_24 = sum(suicide_death[age_20_24==1]),
              suicide_deaths_25_29 = sum(suicide_death[age_25_29==1]),
              suicide_deaths_30_34 = sum(suicide_death[age_30_34==1]),
              suicide_deaths_35_39 = sum(suicide_death[age_35_39==1]),
              suicide_deaths_40_44 = sum(suicide_death[age_40_44==1]),
              suicide_deaths_45_49 = sum(suicide_death[age_45_49==1]),
              suicide_deaths_50_54 = sum(suicide_death[age_50_54==1]),
              suicide_deaths_55_59 = sum(suicide_death[age_55_59==1]),
              suicide_deaths_60_64 = sum(suicide_death[age_60_64==1]),
              suicide_deaths_65_69 = sum(suicide_death[age_65_69==1]),
              suicide_deaths_70_74 = sum(suicide_death[age_70_74==1]),
              suicide_deaths_75_79 = sum(suicide_death[age_75_79==1]),
              suicide_deaths_80_84 = sum(suicide_death[age_80_84==1]),
              suicide_deaths_85_over = sum(suicide_death[age_85_over==1]),
              drug_od_deaths_00_01 = sum(drug_od_death[age_00_01==1]),
              drug_od_deaths_01_04 = sum(drug_od_death[age_01_04==1]),
              drug_od_deaths_05_09 = sum(drug_od_death[age_05_09==1]),
              drug_od_deaths_10_14 = sum(drug_od_death[age_10_14==1]),
              drug_od_deaths_15_19 = sum(drug_od_death[age_15_19==1]),
              drug_od_deaths_20_24 = sum(drug_od_death[age_20_24==1]),
              drug_od_deaths_25_29 = sum(drug_od_death[age_25_29==1]),
              drug_od_deaths_30_34 = sum(drug_od_death[age_30_34==1]),
              drug_od_deaths_35_39 = sum(drug_od_death[age_35_39==1]),
              drug_od_deaths_40_44 = sum(drug_od_death[age_40_44==1]),
              drug_od_deaths_45_49 = sum(drug_od_death[age_45_49==1]),
              drug_od_deaths_50_54 = sum(drug_od_death[age_50_54==1]),
              drug_od_deaths_55_59 = sum(drug_od_death[age_55_59==1]),
              drug_od_deaths_60_64 = sum(drug_od_death[age_60_64==1]),
              drug_od_deaths_65_69 = sum(drug_od_death[age_65_69==1]),
              drug_od_deaths_70_74 = sum(drug_od_death[age_70_74==1]),
              drug_od_deaths_75_79 = sum(drug_od_death[age_75_79==1]),
              drug_od_deaths_80_84 = sum(drug_od_death[age_80_84==1]),
              drug_od_deaths_85_over = sum(drug_od_death[age_85_over==1]),
              arld_deaths_00_01 = sum(arld_death[age_00_01==1]),
              arld_deaths_01_04 = sum(arld_death[age_01_04==1]),
              arld_deaths_05_09 = sum(arld_death[age_05_09==1]),
              arld_deaths_10_14 = sum(arld_death[age_10_14==1]),
              arld_deaths_15_19 = sum(arld_death[age_15_19==1]),
              arld_deaths_20_24 = sum(arld_death[age_20_24==1]),
              arld_deaths_25_29 = sum(arld_death[age_25_29==1]),
              arld_deaths_30_34 = sum(arld_death[age_30_34==1]),
              arld_deaths_35_39 = sum(arld_death[age_35_39==1]),
              arld_deaths_40_44 = sum(arld_death[age_40_44==1]),
              arld_deaths_45_49 = sum(arld_death[age_45_49==1]),
              arld_deaths_50_54 = sum(arld_death[age_50_54==1]),
              arld_deaths_55_59 = sum(arld_death[age_55_59==1]),
              arld_deaths_60_64 = sum(arld_death[age_60_64==1]),
              arld_deaths_65_69 = sum(arld_death[age_65_69==1]),
              arld_deaths_70_74 = sum(arld_death[age_70_74==1]),
              arld_deaths_75_79 = sum(arld_death[age_75_79==1]),
              arld_deaths_80_84 = sum(arld_death[age_80_84==1]),
              arld_deaths_85_over = sum(arld_death[age_85_over==1])
    )
  
  year <- county_mort$year[1]
  mort_year_name <- paste("county_mort", year, sep="_")
  assign(mort_year_name, county_mort, env=.GlobalEnv)
}


unzip("mort_1996.zip")
mort_1996 <- read.csv("mort_1996.csv")
death_agg_96_98(mort_1996)
rm(mort_1996)
gc()

unzip("mort_1997.zip")
mort_1997 <- read.csv("mort_1997.csv")
death_agg_96_98(mort_1997)
rm(mort_1997)
gc()

unzip("mort_1998.zip")
mort_1998 <- read.csv("mort_1998.csv")
death_agg_96_98(mort_1998)
rm(mort_1998)
gc()



county_mort_90_98 <- bind_rows(
  county_mort_1990,
  county_mort_1991,
  county_mort_1992,
  county_mort_1993,
  county_mort_1994,
  county_mort_1995,
  county_mort_1996,
  county_mort_1997,
  county_mort_1998
)

write.csv(county_mort_90_98, "C:/Users/tangjy/Documents/interim/county_mort_90_98.csv", row.names = F)





death_agg_99_02 <- function(mort_year){
  county_mort <- mort_year %>%
    mutate(year = data_year,
           fipscounty = residence_fipsstate*1000 + residence_fipscounty,
           suicide_death = ifelse(cause_recode_358 >= 424 & cause_recode_358 <= 431, 1, 0),
           drug_od_death = ifelse(cause_recode_358 %in% c(420, 443), 1, 0),
           arld_death = ifelse(cause_recode_358 == 298, 1, 0),
           male = ifelse(sex == 1, 1, 0),
           female = ifelse(sex == 2, 1, 0),
           white = ifelse(substr(race, 1, 1) == 1, 1, 0),
           black = ifelse(substr(race, 1, 1) == 2, 1, 0),
           amer_ind = ifelse(substr(race, 1, 1) == 3, 1, 0),
           asian = ifelse(substr(race, 1, 1) %in% c(4, 5, 6, 7, 8), 1, 0),
           other_race = ifelse(substr(race, 1, 1) == 9, 1, 0),
           age_00_01 = ifelse(substr(age, 5, 6) %in% c("01","02"), 1, 0),
           age_01_04 = ifelse(substr(age, 5, 6) %in% c("03","04","05","06"), 1, 0),
           age_05_09 = ifelse(substr(age, 5, 6) == "07", 1, 0),
           age_10_14 = ifelse(substr(age, 5, 6) == "08", 1, 0),
           age_15_19 = ifelse(substr(age, 5, 6) == "09", 1, 0),
           age_20_24 = ifelse(substr(age, 5, 6) == "10", 1, 0),
           age_25_29 = ifelse(substr(age, 5, 6) == "11", 1, 0),
           age_30_34 = ifelse(substr(age, 5, 6) == "12", 1, 0),
           age_35_39 = ifelse(substr(age, 5, 6) == "13", 1, 0),
           age_40_44 = ifelse(substr(age, 5, 6) == "14", 1, 0),
           age_45_49 = ifelse(substr(age, 5, 6) == "15", 1, 0),
           age_50_54 = ifelse(substr(age, 5, 6) == "16", 1, 0),
           age_55_59 = ifelse(substr(age, 5, 6) == "17", 1, 0),
           age_60_64 = ifelse(substr(age, 5, 6) == "18", 1, 0),
           age_65_69 = ifelse(substr(age, 5, 6) == "19", 1, 0),
           age_70_74 = ifelse(substr(age, 5, 6) == "20", 1, 0),
           age_75_79 = ifelse(substr(age, 5, 6) == "21", 1, 0),
           age_80_84 = ifelse(substr(age, 5, 6) == "22", 1, 0),
           age_85_over = ifelse(substr(age, 5, 6) %in% c("23","24","25","26"), 1, 0)
    ) %>%
    group_by(fipscounty, year) %>%
    summarise(total_deaths = n(),
              dod_deaths = sum(suicide_death, drug_od_death, arld_death),
              suicide_deaths = sum(suicide_death),
              drug_od_deaths = sum(drug_od_death),
              arld_deaths = sum(arld_death),
              dod_deaths_00_01 = sum(suicide_death[age_00_01==1], drug_od_death[age_00_01==1], arld_death[age_00_01==1]),
              dod_deaths_01_04 = sum(suicide_death[age_01_04==1], drug_od_death[age_01_04==1], arld_death[age_01_04==1]),
              dod_deaths_05_09 = sum(suicide_death[age_05_09==1], drug_od_death[age_05_09==1], arld_death[age_05_09==1]),
              dod_deaths_10_14 = sum(suicide_death[age_10_14==1], drug_od_death[age_10_14==1], arld_death[age_10_14==1]),
              dod_deaths_15_19 = sum(suicide_death[age_15_19==1], drug_od_death[age_15_19==1], arld_death[age_15_19==1]),
              dod_deaths_20_24 = sum(suicide_death[age_20_24==1], drug_od_death[age_20_24==1], arld_death[age_20_24==1]),
              dod_deaths_25_29 = sum(suicide_death[age_25_29==1], drug_od_death[age_25_29==1], arld_death[age_25_29==1]),
              dod_deaths_30_34 = sum(suicide_death[age_30_34==1], drug_od_death[age_30_34==1], arld_death[age_30_34==1]),
              dod_deaths_35_39 = sum(suicide_death[age_35_39==1], drug_od_death[age_35_39==1], arld_death[age_35_39==1]),
              dod_deaths_40_44 = sum(suicide_death[age_40_44==1], drug_od_death[age_40_44==1], arld_death[age_40_44==1]),
              dod_deaths_45_49 = sum(suicide_death[age_45_49==1], drug_od_death[age_45_49==1], arld_death[age_45_49==1]),
              dod_deaths_50_54 = sum(suicide_death[age_50_54==1], drug_od_death[age_50_54==1], arld_death[age_50_54==1]),
              dod_deaths_55_59 = sum(suicide_death[age_55_59==1], drug_od_death[age_55_59==1], arld_death[age_55_59==1]),
              dod_deaths_60_64 = sum(suicide_death[age_60_64==1], drug_od_death[age_60_64==1], arld_death[age_60_64==1]),
              dod_deaths_65_69 = sum(suicide_death[age_65_69==1], drug_od_death[age_65_69==1], arld_death[age_65_69==1]),
              dod_deaths_70_74 = sum(suicide_death[age_70_74==1], drug_od_death[age_70_74==1], arld_death[age_70_74==1]),
              dod_deaths_75_79 = sum(suicide_death[age_75_79==1], drug_od_death[age_75_79==1], arld_death[age_75_79==1]),
              dod_deaths_80_84 = sum(suicide_death[age_80_84==1], drug_od_death[age_80_84==1], arld_death[age_80_84==1]),
              dod_deaths_85_over = sum(suicide_death[age_85_over==1], drug_od_death[age_85_over==1], arld_death[age_85_over==1]),
              suicide_deaths_00_01 = sum(suicide_death[age_00_01==1]),
              suicide_deaths_01_04 = sum(suicide_death[age_01_04==1]),
              suicide_deaths_05_09 = sum(suicide_death[age_05_09==1]),
              suicide_deaths_10_14 = sum(suicide_death[age_10_14==1]),
              suicide_deaths_15_19 = sum(suicide_death[age_15_19==1]),
              suicide_deaths_20_24 = sum(suicide_death[age_20_24==1]),
              suicide_deaths_25_29 = sum(suicide_death[age_25_29==1]),
              suicide_deaths_30_34 = sum(suicide_death[age_30_34==1]),
              suicide_deaths_35_39 = sum(suicide_death[age_35_39==1]),
              suicide_deaths_40_44 = sum(suicide_death[age_40_44==1]),
              suicide_deaths_45_49 = sum(suicide_death[age_45_49==1]),
              suicide_deaths_50_54 = sum(suicide_death[age_50_54==1]),
              suicide_deaths_55_59 = sum(suicide_death[age_55_59==1]),
              suicide_deaths_60_64 = sum(suicide_death[age_60_64==1]),
              suicide_deaths_65_69 = sum(suicide_death[age_65_69==1]),
              suicide_deaths_70_74 = sum(suicide_death[age_70_74==1]),
              suicide_deaths_75_79 = sum(suicide_death[age_75_79==1]),
              suicide_deaths_80_84 = sum(suicide_death[age_80_84==1]),
              suicide_deaths_85_over = sum(suicide_death[age_85_over==1]),
              drug_od_deaths_00_01 = sum(drug_od_death[age_00_01==1]),
              drug_od_deaths_01_04 = sum(drug_od_death[age_01_04==1]),
              drug_od_deaths_05_09 = sum(drug_od_death[age_05_09==1]),
              drug_od_deaths_10_14 = sum(drug_od_death[age_10_14==1]),
              drug_od_deaths_15_19 = sum(drug_od_death[age_15_19==1]),
              drug_od_deaths_20_24 = sum(drug_od_death[age_20_24==1]),
              drug_od_deaths_25_29 = sum(drug_od_death[age_25_29==1]),
              drug_od_deaths_30_34 = sum(drug_od_death[age_30_34==1]),
              drug_od_deaths_35_39 = sum(drug_od_death[age_35_39==1]),
              drug_od_deaths_40_44 = sum(drug_od_death[age_40_44==1]),
              drug_od_deaths_45_49 = sum(drug_od_death[age_45_49==1]),
              drug_od_deaths_50_54 = sum(drug_od_death[age_50_54==1]),
              drug_od_deaths_55_59 = sum(drug_od_death[age_55_59==1]),
              drug_od_deaths_60_64 = sum(drug_od_death[age_60_64==1]),
              drug_od_deaths_65_69 = sum(drug_od_death[age_65_69==1]),
              drug_od_deaths_70_74 = sum(drug_od_death[age_70_74==1]),
              drug_od_deaths_75_79 = sum(drug_od_death[age_75_79==1]),
              drug_od_deaths_80_84 = sum(drug_od_death[age_80_84==1]),
              drug_od_deaths_85_over = sum(drug_od_death[age_85_over==1]),
              arld_deaths_00_01 = sum(arld_death[age_00_01==1]),
              arld_deaths_01_04 = sum(arld_death[age_01_04==1]),
              arld_deaths_05_09 = sum(arld_death[age_05_09==1]),
              arld_deaths_10_14 = sum(arld_death[age_10_14==1]),
              arld_deaths_15_19 = sum(arld_death[age_15_19==1]),
              arld_deaths_20_24 = sum(arld_death[age_20_24==1]),
              arld_deaths_25_29 = sum(arld_death[age_25_29==1]),
              arld_deaths_30_34 = sum(arld_death[age_30_34==1]),
              arld_deaths_35_39 = sum(arld_death[age_35_39==1]),
              arld_deaths_40_44 = sum(arld_death[age_40_44==1]),
              arld_deaths_45_49 = sum(arld_death[age_45_49==1]),
              arld_deaths_50_54 = sum(arld_death[age_50_54==1]),
              arld_deaths_55_59 = sum(arld_death[age_55_59==1]),
              arld_deaths_60_64 = sum(arld_death[age_60_64==1]),
              arld_deaths_65_69 = sum(arld_death[age_65_69==1]),
              arld_deaths_70_74 = sum(arld_death[age_70_74==1]),
              arld_deaths_75_79 = sum(arld_death[age_75_79==1]),
              arld_deaths_80_84 = sum(arld_death[age_80_84==1]),
              arld_deaths_85_over = sum(arld_death[age_85_over==1])
    )
  
  year <- county_mort$year[1]
  mort_year_name <- paste("county_mort", year, sep="_")
  assign(mort_year_name, county_mort, env=.GlobalEnv)
}

unzip("mort_1999.zip")
mort_1999 <- read.csv("mort_1999.csv")
death_agg_99_02(mort_1999)
rm(mort_1999)
gc()

unzip("mort_2000.zip")
mort_2000 <- read.csv("mort_2000.csv")
death_agg_99_02(mort_2000)
rm(mort_2000)
gc()

unzip("mort_2001.zip")
mort_2001 <- read.csv("mort_2001.csv")
death_agg_99_02(mort_2001)
rm(mort_2001)
gc()

unzip("mort_2002.zip")
mort_2002 <- read.csv("mort_2002.csv")
death_agg_99_02(mort_2002)
rm(mort_2002)
gc()







state_fips_xwalk <- read.csv("C:/Users/tangjy/Documents/xwalks/state_fips_xwalk.csv")

death_agg_03_04 <- function(mort_year){
  county_mort <- mort_year %>%
    left_join(state_fips_xwalk, by=c("residence_state"="state_abbrev")) %>%
    mutate(year= data_year,
           fipscounty = state_fips*1000 + residence_county,
           suicide_death = ifelse(cause_recode_358 >= 424 & cause_recode_358 <= 431, 1, 0),
           drug_od_death = ifelse(cause_recode_358 %in% c(420, 443), 1, 0),
           arld_death = ifelse(cause_recode_358 == 298, 1, 0),
           age_00_01 = ifelse(substr(age, 8, 9) %in% c("01","02"), 1, 0),
           age_01_04 = ifelse(substr(age, 8, 9) %in% c("03","04","05","06"), 1, 0),
           age_05_09 = ifelse(substr(age, 8, 9) == "07", 1, 0),
           age_10_14 = ifelse(substr(age, 8, 9) == "08", 1, 0),
           age_15_19 = ifelse(substr(age, 8, 9) == "09", 1, 0),
           age_20_24 = ifelse(substr(age, 8, 9) == "10", 1, 0),
           age_25_29 = ifelse(substr(age, 8, 9) == "11", 1, 0),
           age_30_34 = ifelse(substr(age, 8, 9) == "12", 1, 0),
           age_35_39 = ifelse(substr(age, 8, 9) == "13", 1, 0),
           age_40_44 = ifelse(substr(age, 8, 9) == "14", 1, 0),
           age_45_49 = ifelse(substr(age, 8, 9) == "15", 1, 0),
           age_50_54 = ifelse(substr(age, 8, 9) == "16", 1, 0),
           age_55_59 = ifelse(substr(age, 8, 9) == "17", 1, 0),
           age_60_64 = ifelse(substr(age, 8, 9) == "18", 1, 0),
           age_65_69 = ifelse(substr(age, 8, 9) == "19", 1, 0),
           age_70_74 = ifelse(substr(age, 8, 9) == "20", 1, 0),
           age_75_79 = ifelse(substr(age, 8, 9) == "21", 1, 0),
           age_80_84 = ifelse(substr(age, 8, 9) == "22", 1, 0),
           age_85_over = ifelse(substr(age, 8, 9) %in% c("23","24","25","26"), 1, 0)) %>%
    group_by(fipscounty, year) %>%
    summarise(total_deaths = n(),
              dod_deaths = sum(suicide_death, drug_od_death, arld_death),
              suicide_deaths = sum(suicide_death),
              drug_od_deaths = sum(drug_od_death),
              arld_deaths = sum(arld_death),
              dod_deaths_00_01 = sum(suicide_death[age_00_01==1], drug_od_death[age_00_01==1], arld_death[age_00_01==1]),
              dod_deaths_01_04 = sum(suicide_death[age_01_04==1], drug_od_death[age_01_04==1], arld_death[age_01_04==1]),
              dod_deaths_05_09 = sum(suicide_death[age_05_09==1], drug_od_death[age_05_09==1], arld_death[age_05_09==1]),
              dod_deaths_10_14 = sum(suicide_death[age_10_14==1], drug_od_death[age_10_14==1], arld_death[age_10_14==1]),
              dod_deaths_15_19 = sum(suicide_death[age_15_19==1], drug_od_death[age_15_19==1], arld_death[age_15_19==1]),
              dod_deaths_20_24 = sum(suicide_death[age_20_24==1], drug_od_death[age_20_24==1], arld_death[age_20_24==1]),
              dod_deaths_25_29 = sum(suicide_death[age_25_29==1], drug_od_death[age_25_29==1], arld_death[age_25_29==1]),
              dod_deaths_30_34 = sum(suicide_death[age_30_34==1], drug_od_death[age_30_34==1], arld_death[age_30_34==1]),
              dod_deaths_35_39 = sum(suicide_death[age_35_39==1], drug_od_death[age_35_39==1], arld_death[age_35_39==1]),
              dod_deaths_40_44 = sum(suicide_death[age_40_44==1], drug_od_death[age_40_44==1], arld_death[age_40_44==1]),
              dod_deaths_45_49 = sum(suicide_death[age_45_49==1], drug_od_death[age_45_49==1], arld_death[age_45_49==1]),
              dod_deaths_50_54 = sum(suicide_death[age_50_54==1], drug_od_death[age_50_54==1], arld_death[age_50_54==1]),
              dod_deaths_55_59 = sum(suicide_death[age_55_59==1], drug_od_death[age_55_59==1], arld_death[age_55_59==1]),
              dod_deaths_60_64 = sum(suicide_death[age_60_64==1], drug_od_death[age_60_64==1], arld_death[age_60_64==1]),
              dod_deaths_65_69 = sum(suicide_death[age_65_69==1], drug_od_death[age_65_69==1], arld_death[age_65_69==1]),
              dod_deaths_70_74 = sum(suicide_death[age_70_74==1], drug_od_death[age_70_74==1], arld_death[age_70_74==1]),
              dod_deaths_75_79 = sum(suicide_death[age_75_79==1], drug_od_death[age_75_79==1], arld_death[age_75_79==1]),
              dod_deaths_80_84 = sum(suicide_death[age_80_84==1], drug_od_death[age_80_84==1], arld_death[age_80_84==1]),
              dod_deaths_85_over = sum(suicide_death[age_85_over==1], drug_od_death[age_85_over==1], arld_death[age_85_over==1]),
              suicide_deaths_00_01 = sum(suicide_death[age_00_01==1]),
              suicide_deaths_01_04 = sum(suicide_death[age_01_04==1]),
              suicide_deaths_05_09 = sum(suicide_death[age_05_09==1]),
              suicide_deaths_10_14 = sum(suicide_death[age_10_14==1]),
              suicide_deaths_15_19 = sum(suicide_death[age_15_19==1]),
              suicide_deaths_20_24 = sum(suicide_death[age_20_24==1]),
              suicide_deaths_25_29 = sum(suicide_death[age_25_29==1]),
              suicide_deaths_30_34 = sum(suicide_death[age_30_34==1]),
              suicide_deaths_35_39 = sum(suicide_death[age_35_39==1]),
              suicide_deaths_40_44 = sum(suicide_death[age_40_44==1]),
              suicide_deaths_45_49 = sum(suicide_death[age_45_49==1]),
              suicide_deaths_50_54 = sum(suicide_death[age_50_54==1]),
              suicide_deaths_55_59 = sum(suicide_death[age_55_59==1]),
              suicide_deaths_60_64 = sum(suicide_death[age_60_64==1]),
              suicide_deaths_65_69 = sum(suicide_death[age_65_69==1]),
              suicide_deaths_70_74 = sum(suicide_death[age_70_74==1]),
              suicide_deaths_75_79 = sum(suicide_death[age_75_79==1]),
              suicide_deaths_80_84 = sum(suicide_death[age_80_84==1]),
              suicide_deaths_85_over = sum(suicide_death[age_85_over==1]),
              drug_od_deaths_00_01 = sum(drug_od_death[age_00_01==1]),
              drug_od_deaths_01_04 = sum(drug_od_death[age_01_04==1]),
              drug_od_deaths_05_09 = sum(drug_od_death[age_05_09==1]),
              drug_od_deaths_10_14 = sum(drug_od_death[age_10_14==1]),
              drug_od_deaths_15_19 = sum(drug_od_death[age_15_19==1]),
              drug_od_deaths_20_24 = sum(drug_od_death[age_20_24==1]),
              drug_od_deaths_25_29 = sum(drug_od_death[age_25_29==1]),
              drug_od_deaths_30_34 = sum(drug_od_death[age_30_34==1]),
              drug_od_deaths_35_39 = sum(drug_od_death[age_35_39==1]),
              drug_od_deaths_40_44 = sum(drug_od_death[age_40_44==1]),
              drug_od_deaths_45_49 = sum(drug_od_death[age_45_49==1]),
              drug_od_deaths_50_54 = sum(drug_od_death[age_50_54==1]),
              drug_od_deaths_55_59 = sum(drug_od_death[age_55_59==1]),
              drug_od_deaths_60_64 = sum(drug_od_death[age_60_64==1]),
              drug_od_deaths_65_69 = sum(drug_od_death[age_65_69==1]),
              drug_od_deaths_70_74 = sum(drug_od_death[age_70_74==1]),
              drug_od_deaths_75_79 = sum(drug_od_death[age_75_79==1]),
              drug_od_deaths_80_84 = sum(drug_od_death[age_80_84==1]),
              drug_od_deaths_85_over = sum(drug_od_death[age_85_over==1]),
              arld_deaths_00_01 = sum(arld_death[age_00_01==1]),
              arld_deaths_01_04 = sum(arld_death[age_01_04==1]),
              arld_deaths_05_09 = sum(arld_death[age_05_09==1]),
              arld_deaths_10_14 = sum(arld_death[age_10_14==1]),
              arld_deaths_15_19 = sum(arld_death[age_15_19==1]),
              arld_deaths_20_24 = sum(arld_death[age_20_24==1]),
              arld_deaths_25_29 = sum(arld_death[age_25_29==1]),
              arld_deaths_30_34 = sum(arld_death[age_30_34==1]),
              arld_deaths_35_39 = sum(arld_death[age_35_39==1]),
              arld_deaths_40_44 = sum(arld_death[age_40_44==1]),
              arld_deaths_45_49 = sum(arld_death[age_45_49==1]),
              arld_deaths_50_54 = sum(arld_death[age_50_54==1]),
              arld_deaths_55_59 = sum(arld_death[age_55_59==1]),
              arld_deaths_60_64 = sum(arld_death[age_60_64==1]),
              arld_deaths_65_69 = sum(arld_death[age_65_69==1]),
              arld_deaths_70_74 = sum(arld_death[age_70_74==1]),
              arld_deaths_75_79 = sum(arld_death[age_75_79==1]),
              arld_deaths_80_84 = sum(arld_death[age_80_84==1]),
              arld_deaths_85_over = sum(arld_death[age_85_over==1]))
  
  year <- county_mort$year[1]
  mort_year_name <- paste("county_mort", year, sep="_")
  assign(mort_year_name, county_mort, env=.GlobalEnv)
}


unzip("mort_2003.zip")
mort_2003 <- read.csv("mort_2003.csv")
death_agg_03_04(mort_2003)
rm(mort_2003)
gc()

unzip("mort_2004.zip")
mort_2004 <- read.csv("mort_2004.csv")
death_agg_03_04(mort_2004)
rm(mort_2004)
gc()

county_mort_99_04 <- bind_rows(
  county_mort_1999,
  county_mort_2000,
  county_mort_2001,
  county_mort_2002,
  county_mort_2003,
  county_mort_2004
)

write.csv(county_mort_99_04, "C:/Users/tangjy/Documents/interim/county_mort_99_04.csv", row.names = F)





death_agg_05_12 <- function(mort_year){
  county_mort <- mort_year %>%
    left_join(state_fips_xwalk, by=c("state_residence"="state_abbrev")) %>%
    mutate(year= current_data_year,
           fipscounty = state_fips*1000 + county_residence,
           suicide_death = ifelse(cause_recode_358 >= 424 & cause_recode_358 <= 431, 1, 0),
           drug_od_death = ifelse(cause_recode_358 %in% c(420, 443), 1, 0),
           arld_death = ifelse(cause_recode_358 == 298, 1, 0),
           age_00_01 = ifelse(substr(age_recode_27, 1, 2) %in% c("01","02"), 1, 0),
           age_01_04 = ifelse(substr(age_recode_27, 1, 2) %in% c("03","04","05","06"), 1, 0),
           age_05_09 = ifelse(substr(age_recode_27, 1, 2) == "07", 1, 0),
           age_10_14 = ifelse(substr(age_recode_27, 1, 2) == "08", 1, 0),
           age_15_19 = ifelse(substr(age_recode_27, 1, 2) == "09", 1, 0),
           age_20_24 = ifelse(substr(age_recode_27, 1, 2) == "10", 1, 0),
           age_25_29 = ifelse(substr(age_recode_27, 1, 2) == "11", 1, 0),
           age_30_34 = ifelse(substr(age_recode_27, 1, 2) == "12", 1, 0),
           age_35_39 = ifelse(substr(age_recode_27, 1, 2) == "13", 1, 0),
           age_40_44 = ifelse(substr(age_recode_27, 1, 2) == "14", 1, 0),
           age_45_49 = ifelse(substr(age_recode_27, 1, 2) == "15", 1, 0),
           age_50_54 = ifelse(substr(age_recode_27, 1, 2) == "16", 1, 0),
           age_55_59 = ifelse(substr(age_recode_27, 1, 2) == "17", 1, 0),
           age_60_64 = ifelse(substr(age_recode_27, 1, 2) == "18", 1, 0),
           age_65_69 = ifelse(substr(age_recode_27, 1, 2) == "19", 1, 0),
           age_70_74 = ifelse(substr(age_recode_27, 1, 2) == "20", 1, 0),
           age_75_79 = ifelse(substr(age_recode_27, 1, 2) == "21", 1, 0),
           age_80_84 = ifelse(substr(age_recode_27, 1, 2) == "22", 1, 0),
           age_85_over = ifelse(substr(age_recode_27, 1, 2) %in% c("23","24","25","26"), 1, 0)) %>%
    group_by(fipscounty, year) %>%
    summarise(total_deaths = n(),
              dod_deaths = sum(suicide_death, drug_od_death, arld_death),
              suicide_deaths = sum(suicide_death),
              drug_od_deaths = sum(drug_od_death),
              arld_deaths = sum(arld_death),
              dod_deaths_00_01 = sum(suicide_death[age_00_01==1], drug_od_death[age_00_01==1], arld_death[age_00_01==1]),
              dod_deaths_01_04 = sum(suicide_death[age_01_04==1], drug_od_death[age_01_04==1], arld_death[age_01_04==1]),
              dod_deaths_05_09 = sum(suicide_death[age_05_09==1], drug_od_death[age_05_09==1], arld_death[age_05_09==1]),
              dod_deaths_10_14 = sum(suicide_death[age_10_14==1], drug_od_death[age_10_14==1], arld_death[age_10_14==1]),
              dod_deaths_15_19 = sum(suicide_death[age_15_19==1], drug_od_death[age_15_19==1], arld_death[age_15_19==1]),
              dod_deaths_20_24 = sum(suicide_death[age_20_24==1], drug_od_death[age_20_24==1], arld_death[age_20_24==1]),
              dod_deaths_25_29 = sum(suicide_death[age_25_29==1], drug_od_death[age_25_29==1], arld_death[age_25_29==1]),
              dod_deaths_30_34 = sum(suicide_death[age_30_34==1], drug_od_death[age_30_34==1], arld_death[age_30_34==1]),
              dod_deaths_35_39 = sum(suicide_death[age_35_39==1], drug_od_death[age_35_39==1], arld_death[age_35_39==1]),
              dod_deaths_40_44 = sum(suicide_death[age_40_44==1], drug_od_death[age_40_44==1], arld_death[age_40_44==1]),
              dod_deaths_45_49 = sum(suicide_death[age_45_49==1], drug_od_death[age_45_49==1], arld_death[age_45_49==1]),
              dod_deaths_50_54 = sum(suicide_death[age_50_54==1], drug_od_death[age_50_54==1], arld_death[age_50_54==1]),
              dod_deaths_55_59 = sum(suicide_death[age_55_59==1], drug_od_death[age_55_59==1], arld_death[age_55_59==1]),
              dod_deaths_60_64 = sum(suicide_death[age_60_64==1], drug_od_death[age_60_64==1], arld_death[age_60_64==1]),
              dod_deaths_65_69 = sum(suicide_death[age_65_69==1], drug_od_death[age_65_69==1], arld_death[age_65_69==1]),
              dod_deaths_70_74 = sum(suicide_death[age_70_74==1], drug_od_death[age_70_74==1], arld_death[age_70_74==1]),
              dod_deaths_75_79 = sum(suicide_death[age_75_79==1], drug_od_death[age_75_79==1], arld_death[age_75_79==1]),
              dod_deaths_80_84 = sum(suicide_death[age_80_84==1], drug_od_death[age_80_84==1], arld_death[age_80_84==1]),
              dod_deaths_85_over = sum(suicide_death[age_85_over==1], drug_od_death[age_85_over==1], arld_death[age_85_over==1]),
              suicide_deaths_00_01 = sum(suicide_death[age_00_01==1]),
              suicide_deaths_01_04 = sum(suicide_death[age_01_04==1]),
              suicide_deaths_05_09 = sum(suicide_death[age_05_09==1]),
              suicide_deaths_10_14 = sum(suicide_death[age_10_14==1]),
              suicide_deaths_15_19 = sum(suicide_death[age_15_19==1]),
              suicide_deaths_20_24 = sum(suicide_death[age_20_24==1]),
              suicide_deaths_25_29 = sum(suicide_death[age_25_29==1]),
              suicide_deaths_30_34 = sum(suicide_death[age_30_34==1]),
              suicide_deaths_35_39 = sum(suicide_death[age_35_39==1]),
              suicide_deaths_40_44 = sum(suicide_death[age_40_44==1]),
              suicide_deaths_45_49 = sum(suicide_death[age_45_49==1]),
              suicide_deaths_50_54 = sum(suicide_death[age_50_54==1]),
              suicide_deaths_55_59 = sum(suicide_death[age_55_59==1]),
              suicide_deaths_60_64 = sum(suicide_death[age_60_64==1]),
              suicide_deaths_65_69 = sum(suicide_death[age_65_69==1]),
              suicide_deaths_70_74 = sum(suicide_death[age_70_74==1]),
              suicide_deaths_75_79 = sum(suicide_death[age_75_79==1]),
              suicide_deaths_80_84 = sum(suicide_death[age_80_84==1]),
              suicide_deaths_85_over = sum(suicide_death[age_85_over==1]),
              drug_od_deaths_00_01 = sum(drug_od_death[age_00_01==1]),
              drug_od_deaths_01_04 = sum(drug_od_death[age_01_04==1]),
              drug_od_deaths_05_09 = sum(drug_od_death[age_05_09==1]),
              drug_od_deaths_10_14 = sum(drug_od_death[age_10_14==1]),
              drug_od_deaths_15_19 = sum(drug_od_death[age_15_19==1]),
              drug_od_deaths_20_24 = sum(drug_od_death[age_20_24==1]),
              drug_od_deaths_25_29 = sum(drug_od_death[age_25_29==1]),
              drug_od_deaths_30_34 = sum(drug_od_death[age_30_34==1]),
              drug_od_deaths_35_39 = sum(drug_od_death[age_35_39==1]),
              drug_od_deaths_40_44 = sum(drug_od_death[age_40_44==1]),
              drug_od_deaths_45_49 = sum(drug_od_death[age_45_49==1]),
              drug_od_deaths_50_54 = sum(drug_od_death[age_50_54==1]),
              drug_od_deaths_55_59 = sum(drug_od_death[age_55_59==1]),
              drug_od_deaths_60_64 = sum(drug_od_death[age_60_64==1]),
              drug_od_deaths_65_69 = sum(drug_od_death[age_65_69==1]),
              drug_od_deaths_70_74 = sum(drug_od_death[age_70_74==1]),
              drug_od_deaths_75_79 = sum(drug_od_death[age_75_79==1]),
              drug_od_deaths_80_84 = sum(drug_od_death[age_80_84==1]),
              drug_od_deaths_85_over = sum(drug_od_death[age_85_over==1]),
              arld_deaths_00_01 = sum(arld_death[age_00_01==1]),
              arld_deaths_01_04 = sum(arld_death[age_01_04==1]),
              arld_deaths_05_09 = sum(arld_death[age_05_09==1]),
              arld_deaths_10_14 = sum(arld_death[age_10_14==1]),
              arld_deaths_15_19 = sum(arld_death[age_15_19==1]),
              arld_deaths_20_24 = sum(arld_death[age_20_24==1]),
              arld_deaths_25_29 = sum(arld_death[age_25_29==1]),
              arld_deaths_30_34 = sum(arld_death[age_30_34==1]),
              arld_deaths_35_39 = sum(arld_death[age_35_39==1]),
              arld_deaths_40_44 = sum(arld_death[age_40_44==1]),
              arld_deaths_45_49 = sum(arld_death[age_45_49==1]),
              arld_deaths_50_54 = sum(arld_death[age_50_54==1]),
              arld_deaths_55_59 = sum(arld_death[age_55_59==1]),
              arld_deaths_60_64 = sum(arld_death[age_60_64==1]),
              arld_deaths_65_69 = sum(arld_death[age_65_69==1]),
              arld_deaths_70_74 = sum(arld_death[age_70_74==1]),
              arld_deaths_75_79 = sum(arld_death[age_75_79==1]),
              arld_deaths_80_84 = sum(arld_death[age_80_84==1]),
              arld_deaths_85_over = sum(arld_death[age_85_over==1]))
  
  year <- county_mort$year[1]
  mort_year_name <- paste("county_mort", year, sep="_")
  assign(mort_year_name, county_mort, env=.GlobalEnv)
}

unzip("mort_2005.zip")
mort_2005 <- read.csv("mort_2005.csv")
death_agg_05_12(mort_2005)
rm(mort_2005)
gc()

unzip("mort_2006.zip")
mort_2006 <- read.csv("mort_2006.csv")
death_agg_05_12(mort_2006)
rm(mort_2006)
gc()

unzip("mort_2007.zip")
mort_2007 <- read.csv("mort_2007.csv")
death_agg_05_12(mort_2007)
rm(mort_2007)
gc()

unzip("mort_2008.zip")
mort_2008 <- read.csv("mort_2008.csv")
death_agg_05_12(mort_2008)
rm(mort_2008)
gc()

unzip("mort_2009.zip")
mort_2009 <- read.csv("mort_2009.csv")
death_agg_05_12(mort_2009)
rm(mort_2009)
gc()

unzip("mort_2010.zip")
mort_2010 <- read.csv("mort_2010.csv")
death_agg_05_12(mort_2010)
rm(mort_2010)
gc()

unzip("mort_2011.zip")
mort_2011 <- read.csv("mort_2011.csv")
death_agg_05_12(mort_2011)
rm(mort_2011)
gc()

unzip("mort_2012.zip")
mort_2012 <- read.csv("mort_2012.csv")
death_agg_05_12(mort_2012)
rm(mort_2012)
gc()


county_mort_05_12 <- bind_rows(
  county_mort_2005,
  county_mort_2006,
  county_mort_2007,
  county_mort_2008,
  county_mort_2009,
  county_mort_2010,
  county_mort_2011,
  county_mort_2012
)

write.csv(county_mort_05_12, "C:/Users/tangjy/Documents/interim/county_mort_05_12.csv", row.names = F)






death_agg_13_19 <- function(mort_year){
  county_mort <- mort_year %>%
    left_join(state_fips_xwalk, by=c("state_residence"="state_abbrev")) %>%
    mutate(year= current_data_year,
           fipscounty = state_fips*1000 + county_residence,
           suicide_death = ifelse(cause_recode_358 >= 424 & cause_recode_358 <= 431, 1, 0),
           drug_od_death = ifelse(cause_recode_358 %in% c(420, 443), 1, 0),
           arld_death = ifelse(cause_recode_358 == 298, 1, 0),
           age_00_01 = ifelse(substr(age_recode_27, 1, 2) %in% c("01","02"), 1, 0),
           age_01_04 = ifelse(substr(age_recode_27, 1, 2) %in% c("03","04","05","06"), 1, 0),
           age_05_09 = ifelse(substr(age_recode_27, 1, 2) == "07", 1, 0),
           age_10_14 = ifelse(substr(age_recode_27, 1, 2) == "08", 1, 0),
           age_15_19 = ifelse(substr(age_recode_27, 1, 2) == "09", 1, 0),
           age_20_24 = ifelse(substr(age_recode_27, 1, 2) == "10", 1, 0),
           age_25_29 = ifelse(substr(age_recode_27, 1, 2) == "11", 1, 0),
           age_30_34 = ifelse(substr(age_recode_27, 1, 2) == "12", 1, 0),
           age_35_39 = ifelse(substr(age_recode_27, 1, 2) == "13", 1, 0),
           age_40_44 = ifelse(substr(age_recode_27, 1, 2) == "14", 1, 0),
           age_45_49 = ifelse(substr(age_recode_27, 1, 2) == "15", 1, 0),
           age_50_54 = ifelse(substr(age_recode_27, 1, 2) == "16", 1, 0),
           age_55_59 = ifelse(substr(age_recode_27, 1, 2) == "17", 1, 0),
           age_60_64 = ifelse(substr(age_recode_27, 1, 2) == "18", 1, 0),
           age_65_69 = ifelse(substr(age_recode_27, 1, 2) == "19", 1, 0),
           age_70_74 = ifelse(substr(age_recode_27, 1, 2) == "20", 1, 0),
           age_75_79 = ifelse(substr(age_recode_27, 1, 2) == "21", 1, 0),
           age_80_84 = ifelse(substr(age_recode_27, 1, 2) == "22", 1, 0),
           age_85_over = ifelse(substr(age_recode_27, 1, 2) %in% c("23","24","25","26"), 1, 0)) %>%
    group_by(fipscounty, year, death_month) %>%
    summarise(total_deaths = n(),
              dod_deaths = sum(suicide_death, drug_od_death, arld_death),
              suicide_deaths = sum(suicide_death),
              drug_od_deaths = sum(drug_od_death),
              arld_deaths = sum(arld_death),
              dod_deaths_00_01 = sum(suicide_death[age_00_01==1], drug_od_death[age_00_01==1], arld_death[age_00_01==1]),
              dod_deaths_01_04 = sum(suicide_death[age_01_04==1], drug_od_death[age_01_04==1], arld_death[age_01_04==1]),
              dod_deaths_05_09 = sum(suicide_death[age_05_09==1], drug_od_death[age_05_09==1], arld_death[age_05_09==1]),
              dod_deaths_10_14 = sum(suicide_death[age_10_14==1], drug_od_death[age_10_14==1], arld_death[age_10_14==1]),
              dod_deaths_15_19 = sum(suicide_death[age_15_19==1], drug_od_death[age_15_19==1], arld_death[age_15_19==1]),
              dod_deaths_20_24 = sum(suicide_death[age_20_24==1], drug_od_death[age_20_24==1], arld_death[age_20_24==1]),
              dod_deaths_25_29 = sum(suicide_death[age_25_29==1], drug_od_death[age_25_29==1], arld_death[age_25_29==1]),
              dod_deaths_30_34 = sum(suicide_death[age_30_34==1], drug_od_death[age_30_34==1], arld_death[age_30_34==1]),
              dod_deaths_35_39 = sum(suicide_death[age_35_39==1], drug_od_death[age_35_39==1], arld_death[age_35_39==1]),
              dod_deaths_40_44 = sum(suicide_death[age_40_44==1], drug_od_death[age_40_44==1], arld_death[age_40_44==1]),
              dod_deaths_45_49 = sum(suicide_death[age_45_49==1], drug_od_death[age_45_49==1], arld_death[age_45_49==1]),
              dod_deaths_50_54 = sum(suicide_death[age_50_54==1], drug_od_death[age_50_54==1], arld_death[age_50_54==1]),
              dod_deaths_55_59 = sum(suicide_death[age_55_59==1], drug_od_death[age_55_59==1], arld_death[age_55_59==1]),
              dod_deaths_60_64 = sum(suicide_death[age_60_64==1], drug_od_death[age_60_64==1], arld_death[age_60_64==1]),
              dod_deaths_65_69 = sum(suicide_death[age_65_69==1], drug_od_death[age_65_69==1], arld_death[age_65_69==1]),
              dod_deaths_70_74 = sum(suicide_death[age_70_74==1], drug_od_death[age_70_74==1], arld_death[age_70_74==1]),
              dod_deaths_75_79 = sum(suicide_death[age_75_79==1], drug_od_death[age_75_79==1], arld_death[age_75_79==1]),
              dod_deaths_80_84 = sum(suicide_death[age_80_84==1], drug_od_death[age_80_84==1], arld_death[age_80_84==1]),
              dod_deaths_85_over = sum(suicide_death[age_85_over==1], drug_od_death[age_85_over==1], arld_death[age_85_over==1]),
              suicide_deaths_00_01 = sum(suicide_death[age_00_01==1]),
              suicide_deaths_01_04 = sum(suicide_death[age_01_04==1]),
              suicide_deaths_05_09 = sum(suicide_death[age_05_09==1]),
              suicide_deaths_10_14 = sum(suicide_death[age_10_14==1]),
              suicide_deaths_15_19 = sum(suicide_death[age_15_19==1]),
              suicide_deaths_20_24 = sum(suicide_death[age_20_24==1]),
              suicide_deaths_25_29 = sum(suicide_death[age_25_29==1]),
              suicide_deaths_30_34 = sum(suicide_death[age_30_34==1]),
              suicide_deaths_35_39 = sum(suicide_death[age_35_39==1]),
              suicide_deaths_40_44 = sum(suicide_death[age_40_44==1]),
              suicide_deaths_45_49 = sum(suicide_death[age_45_49==1]),
              suicide_deaths_50_54 = sum(suicide_death[age_50_54==1]),
              suicide_deaths_55_59 = sum(suicide_death[age_55_59==1]),
              suicide_deaths_60_64 = sum(suicide_death[age_60_64==1]),
              suicide_deaths_65_69 = sum(suicide_death[age_65_69==1]),
              suicide_deaths_70_74 = sum(suicide_death[age_70_74==1]),
              suicide_deaths_75_79 = sum(suicide_death[age_75_79==1]),
              suicide_deaths_80_84 = sum(suicide_death[age_80_84==1]),
              suicide_deaths_85_over = sum(suicide_death[age_85_over==1]),
              drug_od_deaths_00_01 = sum(drug_od_death[age_00_01==1]),
              drug_od_deaths_01_04 = sum(drug_od_death[age_01_04==1]),
              drug_od_deaths_05_09 = sum(drug_od_death[age_05_09==1]),
              drug_od_deaths_10_14 = sum(drug_od_death[age_10_14==1]),
              drug_od_deaths_15_19 = sum(drug_od_death[age_15_19==1]),
              drug_od_deaths_20_24 = sum(drug_od_death[age_20_24==1]),
              drug_od_deaths_25_29 = sum(drug_od_death[age_25_29==1]),
              drug_od_deaths_30_34 = sum(drug_od_death[age_30_34==1]),
              drug_od_deaths_35_39 = sum(drug_od_death[age_35_39==1]),
              drug_od_deaths_40_44 = sum(drug_od_death[age_40_44==1]),
              drug_od_deaths_45_49 = sum(drug_od_death[age_45_49==1]),
              drug_od_deaths_50_54 = sum(drug_od_death[age_50_54==1]),
              drug_od_deaths_55_59 = sum(drug_od_death[age_55_59==1]),
              drug_od_deaths_60_64 = sum(drug_od_death[age_60_64==1]),
              drug_od_deaths_65_69 = sum(drug_od_death[age_65_69==1]),
              drug_od_deaths_70_74 = sum(drug_od_death[age_70_74==1]),
              drug_od_deaths_75_79 = sum(drug_od_death[age_75_79==1]),
              drug_od_deaths_80_84 = sum(drug_od_death[age_80_84==1]),
              drug_od_deaths_85_over = sum(drug_od_death[age_85_over==1]),
              arld_deaths_00_01 = sum(arld_death[age_00_01==1]),
              arld_deaths_01_04 = sum(arld_death[age_01_04==1]),
              arld_deaths_05_09 = sum(arld_death[age_05_09==1]),
              arld_deaths_10_14 = sum(arld_death[age_10_14==1]),
              arld_deaths_15_19 = sum(arld_death[age_15_19==1]),
              arld_deaths_20_24 = sum(arld_death[age_20_24==1]),
              arld_deaths_25_29 = sum(arld_death[age_25_29==1]),
              arld_deaths_30_34 = sum(arld_death[age_30_34==1]),
              arld_deaths_35_39 = sum(arld_death[age_35_39==1]),
              arld_deaths_40_44 = sum(arld_death[age_40_44==1]),
              arld_deaths_45_49 = sum(arld_death[age_45_49==1]),
              arld_deaths_50_54 = sum(arld_death[age_50_54==1]),
              arld_deaths_55_59 = sum(arld_death[age_55_59==1]),
              arld_deaths_60_64 = sum(arld_death[age_60_64==1]),
              arld_deaths_65_69 = sum(arld_death[age_65_69==1]),
              arld_deaths_70_74 = sum(arld_death[age_70_74==1]),
              arld_deaths_75_79 = sum(arld_death[age_75_79==1]),
              arld_deaths_80_84 = sum(arld_death[age_80_84==1]),
              arld_deaths_85_over = sum(arld_death[age_85_over==1]))
  
  year <- county_mort$year[1]
  mort_year_name <- paste("county_mort", year, sep="_")
  assign(mort_year_name, county_mort, env=.GlobalEnv)
}



unzip("mort_2013.zip")
mort_2013 <- read.csv("mort_2013.csv")
death_agg_13_19(mort_2013)
rm(mort_2013)
gc()

unzip("mort_2014.zip")
mort_2014 <- read.csv("mort_2014.csv")
death_agg_13_19(mort_2014)
rm(mort_2014)
gc()

unzip("mort_2015.zip")
mort_2015 <- read.csv("mort_2015.csv")
death_agg_13_19(mort_2015)
rm(mort_2015)
gc()


unzip("mort_2016.zip")
mort_2016 <- read.csv("mort_2016.csv")
death_agg_13_19(mort_2016)
rm(mort_2016)
gc()

unzip("mort_2017.zip")
mort_2017 <- read.csv("mort_2017.csv")
death_agg_13_19(mort_2017)
rm(mort_2017)
gc()

unzip("mort_2018.zip")
mort_2018 <- read.csv("mort_2018.csv")
death_agg_13_19(mort_2018)
rm(mort_2018)
gc()

unzip("mort_2019.zip")
mort_2019 <- read.csv("mort_2019.csv")
death_agg_13_19(mort_2019)
rm(mort_2019)
gc()


county_mort_tw <- bind_rows(
  county_mort_2013,
  county_mort_2014,
  county_mort_2015,
  county_mort_2016,
  county_mort_2017,
  county_mort_2018,
  county_mort_2019
)

write.csv(county_mort_tw, "C:/Users/tangjy/Documents/interim/county_mort_tw.csv", row.names = F)

county_mort_90_98 <- read.csv("C:/Users/tangjy/Documents/interim/county_mort_90_98.csv")
county_mort_99_04 <- read.csv("C:/Users/tangjy/Documents/interim/county_mort_99_04.csv")
county_mort_05_12 <- read.csv("C:/Users/tangjy/Documents/interim/county_mort_05_12.csv")

county_mort_pntr <- bind_rows(
  county_mort_90_98,
  county_mort_99_04,
  county_mort_05_12
)

rm(
  county_mort_90_98,
  county_mort_99_04,
  county_mort_05_12
)

write.csv(county_mort_pntr, "C:/Users/tangjy/Documents/interim/county_mort_pntr.csv", row.names = F)









# Age Adjusted Death Rates
# ----------------------------------------------------------------------------------------------------

county_mort_pntr <- read.csv("C:/Users/tangjy/Documents/interim/county_mort_pntr.csv")

county_pop_90_12 <- read.csv("C:/Users/tangjy/Documents/interim/county_pop_1990_2019.csv") %>%
  filter(year <= 2012) %>%
  select(
    year,
    fipscounty,
    total_pop,
    age_01_04_pop,
    age_05_09_pop,
    age_10_14_pop,
    age_15_19_pop,
    age_20_24_pop,
    age_25_29_pop,
    age_30_34_pop,
    age_35_39_pop,
    age_40_44_pop,
    age_45_49_pop,
    age_50_54_pop,
    age_55_59_pop,
    age_60_64_pop,
    age_65_69_pop,
    age_70_74_pop,
    age_75_79_pop,
    age_80_84_pop,
    age_85_plus_pop,
    )
  

county_mort_pntr_rates <- county_mort_pntr %>%
  left_join(county_pop_90_12) %>%
  filter(fipscounty != 0) 

#%>%
  #mutate(
    #crude_total_rate = (total_deaths/total_pop)*100,
    #crude_dod_rate = (dod_death/total_pop)*100,
    #crude_suicide_rate = (suicide_deaths/total_pop)*100,
    #crude_drug_od_rate = (drug_od_deaths/total_pop)*100, 
    #crude_arld_rate = (arld_death/total_pop)*100,
  #)


#agg_deaths_melt <- melt(county_mort_pntr_rates,
#                        id.vars = c(
#                          "fipscounty", 
#                          "year"
#                        ), 
#                        measure.vars = c(
#                          "total_deaths", 
#                          "dod_deaths",
#                          "suicide_deaths",
#                          "drug_od_deaths",
#                          "arld_deaths"
#                          )
#                        ) %>%
#  arrange(fipscounty, year)


age_xwalk <- read.csv("C:/Users/tangjy/Documents/xwalks/age_xwalk.csv") %>%
  rename(age_bracket = ï..age_bracket)

aged_dod_deaths_melt <- melt(county_mort_pntr_rates,
                             id.vars = c(
                               "fipscounty", 
                               "year"
                             ), 
                             measure.vars = c(
                               "dod_deaths_01_04",
                               "dod_deaths_05_09",
                               "dod_deaths_10_14",
                               "dod_deaths_15_19",
                               "dod_deaths_20_24",
                               "dod_deaths_25_29",
                               "dod_deaths_30_34",
                               "dod_deaths_35_39",
                               "dod_deaths_40_44",
                               "dod_deaths_45_49",
                               "dod_deaths_50_54",
                               "dod_deaths_55_59",
                               "dod_deaths_60_64",
                               "dod_deaths_65_69",
                               "dod_deaths_70_74",
                               "dod_deaths_75_79",
                               "dod_deaths_80_84",
                               "dod_deaths_85_over"
                               )
                             ) %>%
  rename(
    age_dod = variable,
    dod_deaths = value
    ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_suicide, age_drug_od, age_arld, age_pop))


aged_suicide_deaths_melt <- melt(county_mort_pntr_rates,
                                 id.vars = c(
                                   "fipscounty", 
                                   "year"
                                 ), 
                                 measure.vars = c(
                                   "suicide_deaths_01_04",
                                   "suicide_deaths_05_09",
                                   "suicide_deaths_10_14",
                                   "suicide_deaths_15_19",
                                   "suicide_deaths_20_24",
                                   "suicide_deaths_25_29",
                                   "suicide_deaths_30_34",
                                   "suicide_deaths_35_39",
                                   "suicide_deaths_40_44",
                                   "suicide_deaths_45_49",
                                   "suicide_deaths_50_54",
                                   "suicide_deaths_55_59",
                                   "suicide_deaths_60_64",
                                   "suicide_deaths_65_69",
                                   "suicide_deaths_70_74",
                                   "suicide_deaths_75_79",
                                   "suicide_deaths_80_84",
                                   "suicide_deaths_85_over"
                                   )
                                 ) %>%
  rename(
    age_suicide = variable,
    suicide_deaths = value
    ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_drug_od, age_arld, age_pop))


aged_drug_od_deaths_melt <- melt(county_mort_pntr_rates,
                                 id.vars = c(
                                   "fipscounty", 
                                   "year"
                                 ), 
                                 measure.vars = c(
                                   "drug_od_deaths_01_04",
                                   "drug_od_deaths_05_09",
                                   "drug_od_deaths_10_14",
                                   "drug_od_deaths_15_19",
                                   "drug_od_deaths_20_24",
                                   "drug_od_deaths_25_29",
                                   "drug_od_deaths_30_34",
                                   "drug_od_deaths_35_39",
                                   "drug_od_deaths_40_44",
                                   "drug_od_deaths_45_49",
                                   "drug_od_deaths_50_54",
                                   "drug_od_deaths_55_59",
                                   "drug_od_deaths_60_64",
                                   "drug_od_deaths_65_69",
                                   "drug_od_deaths_70_74",
                                   "drug_od_deaths_75_79",
                                   "drug_od_deaths_80_84",
                                   "drug_od_deaths_85_over"
                                   )
                                 ) %>%
  rename(
    age_drug_od = variable,
    drug_od_deaths = value
    ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_suicide, age_arld, age_pop))

aged_arld_deaths_melt <- melt(county_mort_pntr_rates,
                              id.vars = c(
                                "fipscounty", 
                                "year"
                              ), 
                              measure.vars = c(
                                "arld_deaths_01_04",
                                "arld_deaths_05_09",
                                "arld_deaths_10_14",
                                "arld_deaths_15_19",
                                "arld_deaths_20_24",
                                "arld_deaths_25_29",
                                "arld_deaths_30_34",
                                "arld_deaths_35_39",
                                "arld_deaths_40_44",
                                "arld_deaths_45_49",
                                "arld_deaths_50_54",
                                "arld_deaths_55_59",
                                "arld_deaths_60_64",
                                "arld_deaths_65_69",
                                "arld_deaths_70_74",
                                "arld_deaths_75_79",
                                "arld_deaths_80_84",
                                "arld_deaths_85_over"
                                )
                              ) %>%
  rename(
    age_arld = variable,
    arld_deaths = value
    ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_suicide, age_drug_od, age_pop))


aged_county_pop_melt <- melt(county_mort_pntr_rates,
                             id.vars = c(
                               "fipscounty", 
                               "year"
                             ), 
                             measure.vars = c(
                               "age_01_04_pop",
                               "age_05_09_pop",
                               "age_10_14_pop",
                               "age_15_19_pop",
                               "age_20_24_pop",
                               "age_25_29_pop",
                               "age_30_34_pop",
                               "age_35_39_pop",
                               "age_40_44_pop",
                               "age_45_49_pop",
                               "age_50_54_pop",
                               "age_55_59_pop",
                               "age_60_64_pop",
                               "age_65_69_pop",
                               "age_70_74_pop",
                               "age_75_79_pop",
                               "age_80_84_pop",
                               "age_85_plus_pop"
                               )
                             ) %>%
  rename(
    age_pop = variable,
    age_count = value
    ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_suicide, age_drug_od, age_arld))


age_dist <- read.csv("C:/Users/tangjy/Documents/xwalks/age_dist.csv") %>%
  rename(age_bracket = ï..â..All.AgesÂ.,
         us_age_per_mm = age_per_mm_2) %>%
  select(age_bracket, us_age_per_mm) %>%
  filter(age_bracket != "total")


age_adjusted_deaths_pntr <- aged_county_pop_melt %>%
  relocate(age_bracket, .after = year) %>%
  left_join(aged_dod_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "age_bracket"="age_bracket")) %>%
  left_join(aged_suicide_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "age_bracket"="age_bracket")) %>%
  left_join(aged_drug_od_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "age_bracket"="age_bracket")) %>%
  left_join(aged_arld_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "age_bracket"="age_bracket")) %>%
  select(-c(age_pop, age_dod, age_suicide, age_drug_od, age_arld)) %>%
  left_join(age_dist) %>%
  relocate(us_age_per_mm, .after = age_bracket) %>%
  mutate(
    dod_calc = (us_age_per_mm*dod_deaths)/age_count,
    suicide_calc = (us_age_per_mm*suicide_deaths)/age_count,
    drug_od_calc = (us_age_per_mm*drug_od_deaths)/age_count,
    arld_calc = (us_age_per_mm*arld_deaths)/age_count
    ) %>%
  group_by(fipscounty, year) %>%
  summarise(
    county_pop = sum(age_count),
    dod_crude = sum(dod_deaths)/county_pop,
    suicide_crude = sum(suicide_deaths)/county_pop,
    drug_od_crude = sum(drug_od_deaths)/county_pop,
    arld_crude = sum(arld_deaths)/county_pop,
    dod_aa = sum(dod_calc)/10,
    suicide_aa = sum(suicide_calc)/10,
    drug_od_aa = sum(drug_od_calc)/10,
    arld_aa = sum(arld_calc)/10
  )


write.csv(age_adjusted_deaths_pntr, "C:/Users/tangjy/Documents/final_datasets/age_adjusted_deaths_pntr.csv", row.names = F)

rm(list=ls())


# -------------------------------------------------------------------------------------

county_mort_tw <- read.csv("C:/Users/tangjy/Documents/interim/county_mort_tw.csv")

county_pop_13_19 <- read.csv("C:/Users/tangjy/Documents/interim/county_pop_1990_2019.csv") %>%
  filter(year > 2012) %>%
  select(
    year,
    fipscounty,
    total_pop,
    age_01_04_pop,
    age_05_09_pop,
    age_10_14_pop,
    age_15_19_pop,
    age_20_24_pop,
    age_25_29_pop,
    age_30_34_pop,
    age_35_39_pop,
    age_40_44_pop,
    age_45_49_pop,
    age_50_54_pop,
    age_55_59_pop,
    age_60_64_pop,
    age_65_69_pop,
    age_70_74_pop,
    age_75_79_pop,
    age_80_84_pop,
    age_85_plus_pop,
  )


county_mort_tw_rates <- county_mort_tw %>%
  left_join(county_pop_13_19) %>%
  filter(fipscounty != 0) 


age_xwalk <- read.csv("C:/Users/tangjy/Documents/xwalks/age_xwalk.csv") %>%
  rename(age_bracket = ï..age_bracket)

aged_dod_deaths_melt <- melt(county_mort_tw_rates,
                             id.vars = c(
                               "fipscounty", 
                               "year",
                               "death_month"
                             ), 
                             measure.vars = c(
                               "dod_deaths_01_04",
                               "dod_deaths_05_09",
                               "dod_deaths_10_14",
                               "dod_deaths_15_19",
                               "dod_deaths_20_24",
                               "dod_deaths_25_29",
                               "dod_deaths_30_34",
                               "dod_deaths_35_39",
                               "dod_deaths_40_44",
                               "dod_deaths_45_49",
                               "dod_deaths_50_54",
                               "dod_deaths_55_59",
                               "dod_deaths_60_64",
                               "dod_deaths_65_69",
                               "dod_deaths_70_74",
                               "dod_deaths_75_79",
                               "dod_deaths_80_84",
                               "dod_deaths_85_over"
                               )
                             ) %>%
  rename(
    age_dod = variable,
    dod_deaths = value
  ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_suicide, age_drug_od, age_arld, age_pop))


aged_suicide_deaths_melt <- melt(county_mort_tw_rates,
                                 id.vars = c(
                                   "fipscounty", 
                                   "year",
                                   "death_month"
                                 ), 
                                 measure.vars = c(
                                   "suicide_deaths_01_04",
                                   "suicide_deaths_05_09",
                                   "suicide_deaths_10_14",
                                   "suicide_deaths_15_19",
                                   "suicide_deaths_20_24",
                                   "suicide_deaths_25_29",
                                   "suicide_deaths_30_34",
                                   "suicide_deaths_35_39",
                                   "suicide_deaths_40_44",
                                   "suicide_deaths_45_49",
                                   "suicide_deaths_50_54",
                                   "suicide_deaths_55_59",
                                   "suicide_deaths_60_64",
                                   "suicide_deaths_65_69",
                                   "suicide_deaths_70_74",
                                   "suicide_deaths_75_79",
                                   "suicide_deaths_80_84",
                                   "suicide_deaths_85_over"
                                 )
) %>%
  rename(
    age_suicide = variable,
    suicide_deaths = value
  ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_drug_od, age_arld, age_pop))


aged_drug_od_deaths_melt <- melt(county_mort_tw_rates,
                                 id.vars = c(
                                   "fipscounty", 
                                   "year",
                                   "death_month"
                                 ), 
                                 measure.vars = c(
                                   "drug_od_deaths_01_04",
                                   "drug_od_deaths_05_09",
                                   "drug_od_deaths_10_14",
                                   "drug_od_deaths_15_19",
                                   "drug_od_deaths_20_24",
                                   "drug_od_deaths_25_29",
                                   "drug_od_deaths_30_34",
                                   "drug_od_deaths_35_39",
                                   "drug_od_deaths_40_44",
                                   "drug_od_deaths_45_49",
                                   "drug_od_deaths_50_54",
                                   "drug_od_deaths_55_59",
                                   "drug_od_deaths_60_64",
                                   "drug_od_deaths_65_69",
                                   "drug_od_deaths_70_74",
                                   "drug_od_deaths_75_79",
                                   "drug_od_deaths_80_84",
                                   "drug_od_deaths_85_over"
                                 )
) %>%
  rename(
    age_drug_od = variable,
    drug_od_deaths = value
  ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_suicide, age_arld, age_pop))

aged_arld_deaths_melt <- melt(county_mort_tw_rates,
                              id.vars = c(
                                "fipscounty", 
                                "year",
                                "death_month"
                              ), 
                              measure.vars = c(
                                "arld_deaths_01_04",
                                "arld_deaths_05_09",
                                "arld_deaths_10_14",
                                "arld_deaths_15_19",
                                "arld_deaths_20_24",
                                "arld_deaths_25_29",
                                "arld_deaths_30_34",
                                "arld_deaths_35_39",
                                "arld_deaths_40_44",
                                "arld_deaths_45_49",
                                "arld_deaths_50_54",
                                "arld_deaths_55_59",
                                "arld_deaths_60_64",
                                "arld_deaths_65_69",
                                "arld_deaths_70_74",
                                "arld_deaths_75_79",
                                "arld_deaths_80_84",
                                "arld_deaths_85_over"
                              )
) %>%
  rename(
    age_arld = variable,
    arld_deaths = value
  ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_suicide, age_drug_od, age_pop))


aged_county_pop_melt <- melt(county_mort_tw_rates,
                             id.vars = c(
                               "fipscounty", 
                               "year",
                               "death_month"
                             ), 
                             measure.vars = c(
                               "age_01_04_pop",
                               "age_05_09_pop",
                               "age_10_14_pop",
                               "age_15_19_pop",
                               "age_20_24_pop",
                               "age_25_29_pop",
                               "age_30_34_pop",
                               "age_35_39_pop",
                               "age_40_44_pop",
                               "age_45_49_pop",
                               "age_50_54_pop",
                               "age_55_59_pop",
                               "age_60_64_pop",
                               "age_65_69_pop",
                               "age_70_74_pop",
                               "age_75_79_pop",
                               "age_80_84_pop",
                               "age_85_plus_pop"
                             )
) %>%
  rename(
    age_pop = variable,
    age_count = value
  ) %>%
  arrange(fipscounty, year) %>%
  left_join(age_xwalk) %>%
  select(-c(age_dod, age_suicide, age_drug_od, age_arld))


age_dist <- read.csv("C:/Users/tangjy/Documents/xwalks/age_dist.csv") %>%
  rename(age_bracket = ï..â..All.AgesÂ.,
         us_age_per_mm = age_per_mm_2) %>%
  select(age_bracket, us_age_per_mm) %>%
  filter(age_bracket != "total")

gc()

age_adjusted_deaths_tw <- aged_county_pop_melt %>%
  relocate(age_bracket, .after = year) %>%
  left_join(aged_dod_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "death_month"="death_month", "age_bracket"="age_bracket")) %>%
  left_join(aged_suicide_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "death_month"="death_month", "age_bracket"="age_bracket")) %>%
  left_join(aged_drug_od_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "death_month"="death_month", "age_bracket"="age_bracket")) %>%
  left_join(aged_arld_deaths_melt, by=c("fipscounty"="fipscounty", "year"="year", "death_month"="death_month", "age_bracket"="age_bracket")) %>%
  select(-c(age_pop, age_dod, age_suicide, age_drug_od, age_arld)) %>%
  left_join(age_dist) %>%
  relocate(us_age_per_mm, .after = age_bracket) %>%
  mutate(
    dod_calc = (us_age_per_mm*dod_deaths)/age_count,
    suicide_calc = (us_age_per_mm*suicide_deaths)/age_count,
    drug_od_calc = (us_age_per_mm*drug_od_deaths)/age_count,
    arld_calc = (us_age_per_mm*arld_deaths)/age_count,
    date = as.Date(paste(year, death_month, '01', sep='-')),
    quarter = as.yearqtr(date, format = "%Y-%m-%d")
  ) %>%
  group_by(fipscounty, quarter) %>%
  summarise(
    county_pop = sum(age_count)/3,
    dod_crude = sum(dod_deaths)/county_pop,
    suicide_crude = sum(suicide_deaths)/county_pop,
    drug_od_crude = sum(drug_od_deaths)/county_pop,
    arld_crude = sum(arld_deaths)/county_pop,
    dod_aa = sum(dod_calc)/10,
    suicide_aa = sum(suicide_calc)/10,
    drug_od_aa = sum(drug_od_calc)/10,
    arld_calc = sum(arld_calc)/10
  )

write.csv(age_adjusted_deaths_tw, "C:/Users/tangjy/Documents/final_datasets/age_adjusted_deaths_tw.csv", row.names = F)

rm(list=ls())







