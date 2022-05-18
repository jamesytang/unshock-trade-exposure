# ---------------------------------------------------------------------------------------------------------
# Load and reformat death data 
# ---------------------------------------------------------------------------------------------------------

# R Libraries 
library(dplyr)
library(ggplot2)

# State name/abbr <--> state fips cd conversion 
state_fips_xwalk <- read.csv("C:/Users/tangjy/Documents/xwalks/state_fips_xwalk.csv")


# Functions are grouped by years based on data dictionary similarities 
# - Note: Raw .txt files are one string per death with information coded by string position 
# - Following code converts strings to separate columns using substrings
# - The following years are grouped together:
#   - 1990-1992
#   - 1993-1994
#   - 1995
#   - 1996-1998
#   - 1999
#   - 2000-2002
#   - 2003
#   - 2004
#   - 2005-2019


# Load raw mortality file - Function applies to data years 1990-1992
load_mortality_file_90_92 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 50, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'reporting_area',
                           'record_type',
                           'resident_status',
                           'death_month',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'occurrence_region',
                           'occurrence_division',
                           'occurrence_fipsstate',
                           'occurrence_fipscounty',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_region',
                           'residence_division',
                           'residence_psma',
                           'residence_psma_pop',
                           'residence_fipsstate',
                           'residence_fipscounty',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic1',
                           'hispanic2',
                           'industry1',
                           'industry2',
                           'occupation1',
                           'occupation2',
                           'education1',
                           'education2',
                           'icd9',
                           'cause_recode_282',
                           'cause_recode_72',
                           'cause_recode_61',
                           'cause_recode_52',
                           'cause_recode_34',
                           'autopsy',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 1, 2)
  mort_year$reporting_area <- substr(file_name$V1, 3, 3)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_month <- substr(file_name$V1, 55, 56)
  mort_year$death_day_of_week <- substr(file_name$V1, 83, 83) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 29, 30)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 49, 49)
  mort_year$occurrence_region <- substr(file_name$V1, 26, 26)
  mort_year$occurrence_division <- substr(file_name$V1, 27, 27)
  mort_year$occurrence_fipsstate <- substr(file_name$V1, 119, 120)
  mort_year$occurrence_fipscounty <- substr(file_name$V1, 121, 123)
  
  mort_year$residence_state <- substr(file_name$V1, 31, 32)
  mort_year$residence_exp_state <- substr(file_name$V1, 44, 45) 
  mort_year$residence_county <- substr(file_name$V1, 33, 35)
  mort_year$residence_county_pop <- substr(file_name$V1, 50, 50)
  mort_year$residence_city <- substr(file_name$V1, 36, 38)
  mort_year$residence_city_pop <- substr(file_name$V1, 39, 39)
  mort_year$residence_met <- substr(file_name$V1, 40, 40)
  mort_year$residence_region <- substr(file_name$V1, 41, 41)
  mort_year$residence_division <- substr(file_name$V1, 42, 42)
  mort_year$residence_psma <- substr(file_name$V1, 46, 48)
  mort_year$residence_psma_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_fipsstate <- substr(file_name$V1, 124, 125)
  mort_year$residence_fipscounty <- substr(file_name$V1,126, 128)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 129, 132)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 134, 135)
  
  mort_year$sex <- substr(file_name$V1, 59, 59)
  mort_year$race <- substr(file_name$V1, 60, 63)
  mort_year$age <- substr(file_name$V1, 64, 74)
  mort_year$marital_status <- substr(file_name$V1, 77, 77)
  mort_year$birth_state <- substr(file_name$V1, 78, 79)
  mort_year$hispanic1 <- substr(file_name$V1, 80, 81)
  mort_year$hispanic2 <- substr(file_name$V1, 82, 82)
  mort_year$industry1 <- substr(file_name$V1, 85, 87)
  mort_year$industry2 <- substr(file_name$V1, 94, 97)
  mort_year$occupation1 <- substr(file_name$V1, 88, 90)
  mort_year$occupation2 <- substr(file_name$V1, 98, 101)
  mort_year$education1 <- substr(file_name$V1, 52, 53)
  mort_year$education2 <- substr(file_name$V1, 54, 54)
  
  mort_year$icd9 <- substr(file_name$V1, 142, 145)
  mort_year$cause_recode_282 <- substr(file_name$V1, 146, 150)
  mort_year$cause_recode_72 <- substr(file_name$V1, 151, 153)
  mort_year$cause_recode_61 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_52 <- substr(file_name$V1, 91, 93)
  mort_year$cause_recode_34 <- substr(file_name$V1, 157, 159)
  
  mort_year$autopsy <- substr(file_name$V1, 84, 84)
  mort_year$place_accident <- substr(file_name$V1, 141, 141)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


# Load raw mortality file - Function applies to data years 1993-1994
load_mortality_file_93_94 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 48, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'reporting_area',
                           'record_type',
                           'resident_status',
                           'death_month',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'occurrence_region',
                           'occurrence_division',
                           'occurrence_fipsstate',
                           'occurrence_fipscounty',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_region',
                           'residence_division',
                           'residence_psma',
                           'residence_psma_pop',
                           'residence_fipsstate',
                           'residence_fipscounty',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic1',
                           'hispanic2',
                           'industry1',
                           'occupation1',
                           'education1',
                           'education2',
                           'icd9',
                           'cause_recode_282',
                           'cause_recode_72',
                           'cause_recode_61',
                           'cause_recode_52',
                           'cause_recode_34',
                           'autopsy',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 1, 2)
  mort_year$reporting_area <- substr(file_name$V1, 3, 3)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_month <- substr(file_name$V1, 55, 56)
  mort_year$death_day_of_week <- substr(file_name$V1, 83, 83) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 29, 30)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 49, 49)
  mort_year$occurrence_region <- substr(file_name$V1, 26, 26)
  mort_year$occurrence_division <- substr(file_name$V1, 27, 27)
  mort_year$occurrence_fipsstate <- substr(file_name$V1, 119, 120)
  mort_year$occurrence_fipscounty <- substr(file_name$V1, 121, 123)
  
  mort_year$residence_state <- substr(file_name$V1, 31, 32)
  mort_year$residence_exp_state <- substr(file_name$V1, 44, 45) 
  mort_year$residence_county <- substr(file_name$V1, 33, 35)
  mort_year$residence_county_pop <- substr(file_name$V1, 50, 50)
  mort_year$residence_city <- substr(file_name$V1, 36, 38)
  mort_year$residence_city_pop <- substr(file_name$V1, 39, 39)
  mort_year$residence_met <- substr(file_name$V1, 40, 40)
  mort_year$residence_region <- substr(file_name$V1, 41, 41)
  mort_year$residence_division <- substr(file_name$V1, 42, 42)
  mort_year$residence_psma <- substr(file_name$V1, 46, 48)
  mort_year$residence_psma_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_fipsstate <- substr(file_name$V1, 124, 125)
  mort_year$residence_fipscounty <- substr(file_name$V1,126, 128)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 129, 132)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 134, 135)
  
  mort_year$sex <- substr(file_name$V1, 59, 59)
  mort_year$race <- substr(file_name$V1, 60, 63)
  mort_year$age <- substr(file_name$V1, 64, 74)
  mort_year$marital_status <- substr(file_name$V1, 77, 77)
  mort_year$birth_state <- substr(file_name$V1, 78, 79)
  mort_year$hispanic1 <- substr(file_name$V1, 80, 81)
  mort_year$hispanic2 <- substr(file_name$V1, 82, 82)
  mort_year$industry1 <- substr(file_name$V1, 85, 87)
  mort_year$occupation1 <- substr(file_name$V1, 88, 90)
  mort_year$education1 <- substr(file_name$V1, 52, 53)
  mort_year$education2 <- substr(file_name$V1, 54, 54)
  
  mort_year$icd9 <- substr(file_name$V1, 142, 145)
  mort_year$cause_recode_282 <- substr(file_name$V1, 146, 150)
  mort_year$cause_recode_72 <- substr(file_name$V1, 151, 153)
  mort_year$cause_recode_61 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_52 <- substr(file_name$V1, 91, 93)
  mort_year$cause_recode_34 <- substr(file_name$V1, 157, 159)
  
  mort_year$autopsy <- substr(file_name$V1, 84, 84)
  mort_year$place_accident <- substr(file_name$V1, 141, 141)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


# Load raw mortality file - Function applies to data year 1995
load_mortality_file_95 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 47, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'reporting_area',
                           'record_type',
                           'resident_status',
                           'death_month',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'occurrence_region',
                           'occurrence_division',
                           'occurrence_fipsstate',
                           'occurrence_fipscounty',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_region',
                           'residence_division',
                           'residence_psma',
                           'residence_psma_pop',
                           'residence_fipsstate',
                           'residence_fipscounty',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic1',
                           'hispanic2',
                           'industry1',
                           'occupation1',
                           'education1',
                           'education2',
                           'icd9',
                           'cause_recode_282',
                           'cause_recode_72',
                           'cause_recode_61',
                           'cause_recode_52',
                           'cause_recode_34',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 1, 2)
  mort_year$reporting_area <- substr(file_name$V1, 3, 3)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_month <- substr(file_name$V1, 55, 56)
  mort_year$death_day_of_week <- substr(file_name$V1, 83, 83) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 29, 30)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 49, 49)
  mort_year$occurrence_region <- substr(file_name$V1, 26, 26)
  mort_year$occurrence_division <- substr(file_name$V1, 27, 27)
  mort_year$occurrence_fipsstate <- substr(file_name$V1, 119, 120)
  mort_year$occurrence_fipscounty <- substr(file_name$V1, 121, 123)
  
  mort_year$residence_state <- substr(file_name$V1, 31, 32)
  mort_year$residence_exp_state <- substr(file_name$V1, 44, 45) 
  mort_year$residence_county <- substr(file_name$V1, 33, 35)
  mort_year$residence_county_pop <- substr(file_name$V1, 50, 50)
  mort_year$residence_city <- substr(file_name$V1, 36, 38)
  mort_year$residence_city_pop <- substr(file_name$V1, 39, 39)
  mort_year$residence_met <- substr(file_name$V1, 40, 40)
  mort_year$residence_region <- substr(file_name$V1, 41, 41)
  mort_year$residence_division <- substr(file_name$V1, 42, 42)
  mort_year$residence_psma <- substr(file_name$V1, 46, 48)
  mort_year$residence_psma_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_fipsstate <- substr(file_name$V1, 124, 125)
  mort_year$residence_fipscounty <- substr(file_name$V1,126, 128)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 129, 132)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 134, 135)
  
  mort_year$sex <- substr(file_name$V1, 59, 59)
  mort_year$race <- substr(file_name$V1, 60, 63)
  mort_year$age <- substr(file_name$V1, 64, 74)
  mort_year$marital_status <- substr(file_name$V1, 77, 77)
  mort_year$birth_state <- substr(file_name$V1, 78, 79)
  mort_year$hispanic1 <- substr(file_name$V1, 80, 81)
  mort_year$hispanic2 <- substr(file_name$V1, 82, 82)
  mort_year$industry1 <- substr(file_name$V1, 85, 87)
  mort_year$occupation1 <- substr(file_name$V1, 88, 90)
  mort_year$education1 <- substr(file_name$V1, 52, 53)
  mort_year$education2 <- substr(file_name$V1, 54, 54)
  
  mort_year$icd9 <- substr(file_name$V1, 142, 145)
  mort_year$cause_recode_282 <- substr(file_name$V1, 146, 150)
  mort_year$cause_recode_72 <- substr(file_name$V1, 151, 153)
  mort_year$cause_recode_61 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_52 <- substr(file_name$V1, 91, 93)
  mort_year$cause_recode_34 <- substr(file_name$V1, 157, 159)
  
  mort_year$place_accident <- substr(file_name$V1, 141, 141)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


# Load raw mortality file - Function applies to data years 1996-1998
load_mortality_file_96_98 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 46, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'record_type',
                           'resident_status',
                           'death_month',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'occurrence_region',
                           'occurrence_division',
                           'occurrence_fipsstate',
                           'occurrence_fipscounty',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_region',
                           'residence_division',
                           'residence_psma',
                           'residence_psma_pop',
                           'residence_fipsstate',
                           'residence_fipscounty',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic1',
                           'hispanic2',
                           'industry1',
                           'occupation1',
                           'education1',
                           'education2',
                           'icd9',
                           'cause_recode_282',
                           'cause_recode_72',
                           'cause_recode_61',
                           'cause_recode_52',
                           'cause_recode_34',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 115, 118)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_month <- substr(file_name$V1, 55, 56)
  mort_year$death_day_of_week <- substr(file_name$V1, 83, 83) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 29, 30)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 49, 49)
  mort_year$occurrence_region <- substr(file_name$V1, 26, 26)
  mort_year$occurrence_division <- substr(file_name$V1, 27, 27)
  mort_year$occurrence_fipsstate <- substr(file_name$V1, 119, 120)
  mort_year$occurrence_fipscounty <- substr(file_name$V1, 121, 123)
  
  mort_year$residence_state <- substr(file_name$V1, 31, 32)
  mort_year$residence_exp_state <- substr(file_name$V1, 44, 45) 
  mort_year$residence_county <- substr(file_name$V1, 33, 35)
  mort_year$residence_county_pop <- substr(file_name$V1, 50, 50)
  mort_year$residence_city <- substr(file_name$V1, 36, 38)
  mort_year$residence_city_pop <- substr(file_name$V1, 39, 39)
  mort_year$residence_met <- substr(file_name$V1, 40, 40)
  mort_year$residence_region <- substr(file_name$V1, 41, 41)
  mort_year$residence_division <- substr(file_name$V1, 42, 42)
  mort_year$residence_psma <- substr(file_name$V1, 46, 48)
  mort_year$residence_psma_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_fipsstate <- substr(file_name$V1, 124, 125)
  mort_year$residence_fipscounty <- substr(file_name$V1,126, 128)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 129, 132)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 134, 135)
  
  mort_year$sex <- substr(file_name$V1, 59, 59)
  mort_year$race <- substr(file_name$V1, 60, 63)
  mort_year$age <- substr(file_name$V1, 64, 74)
  mort_year$marital_status <- substr(file_name$V1, 77, 77)
  mort_year$birth_state <- substr(file_name$V1, 78, 79)
  mort_year$hispanic1 <- substr(file_name$V1, 80, 81)
  mort_year$hispanic2 <- substr(file_name$V1, 82, 82)
  mort_year$industry1 <- substr(file_name$V1, 85, 87)
  mort_year$occupation1 <- substr(file_name$V1, 88, 90)
  mort_year$education1 <- substr(file_name$V1, 52, 53)
  mort_year$education2 <- substr(file_name$V1, 54, 54)
  
  mort_year$icd9 <- substr(file_name$V1, 142, 145)
  mort_year$cause_recode_282 <- substr(file_name$V1, 146, 150)
  mort_year$cause_recode_72 <- substr(file_name$V1, 151, 153)
  mort_year$cause_recode_61 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_52 <- substr(file_name$V1, 91, 93)
  mort_year$cause_recode_34 <- substr(file_name$V1, 157, 159)
  
  mort_year$place_accident <- substr(file_name$V1, 141, 141)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


load_mortality_file_99 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 45, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'record_type',
                           'resident_status',
                           'death_month',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'occurrence_region',
                           'occurrence_division',
                           'occurrence_fipsstate',
                           'occurrence_fipscounty',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_region',
                           'residence_division',
                           'residence_psma',
                           'residence_psma_pop',
                           'residence_fipsstate',
                           'residence_fipscounty',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic1',
                           'hispanic2',
                           'industry1',
                           'occupation1',
                           'education1',
                           'education2',
                           'icd10',
                           'cause_recode_358',
                           'cause_recode_113',
                           'cause_recode_130',
                           'cause_recode_39',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 115, 118)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_month <- substr(file_name$V1, 55, 56)
  mort_year$death_day_of_week <- substr(file_name$V1, 83, 83) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 29, 30)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 49, 49)
  mort_year$occurrence_region <- substr(file_name$V1, 26, 26)
  mort_year$occurrence_division <- substr(file_name$V1, 27, 27)
  mort_year$occurrence_fipsstate <- substr(file_name$V1, 119, 120)
  mort_year$occurrence_fipscounty <- substr(file_name$V1, 121, 123)
  
  mort_year$residence_state <- substr(file_name$V1, 31, 32)
  mort_year$residence_exp_state <- substr(file_name$V1, 44, 45) 
  mort_year$residence_county <- substr(file_name$V1, 33, 35)
  mort_year$residence_county_pop <- substr(file_name$V1, 50, 50)
  mort_year$residence_city <- substr(file_name$V1, 36, 38)
  mort_year$residence_city_pop <- substr(file_name$V1, 39, 39)
  mort_year$residence_met <- substr(file_name$V1, 40, 40)
  mort_year$residence_region <- substr(file_name$V1, 41, 41)
  mort_year$residence_division <- substr(file_name$V1, 42, 42)
  mort_year$residence_psma <- substr(file_name$V1, 46, 48)
  mort_year$residence_psma_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_fipsstate <- substr(file_name$V1, 124, 125)
  mort_year$residence_fipscounty <- substr(file_name$V1,126, 128)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 129, 132)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 134, 135)
  
  mort_year$sex <- substr(file_name$V1, 59, 59)
  mort_year$race <- substr(file_name$V1, 60, 63)
  mort_year$age <- substr(file_name$V1, 64, 74)
  mort_year$marital_status <- substr(file_name$V1, 77, 77)
  mort_year$birth_state <- substr(file_name$V1, 78, 79)
  mort_year$hispanic1 <- substr(file_name$V1, 80, 81)
  mort_year$hispanic2 <- substr(file_name$V1, 82, 82)
  mort_year$industry1 <- substr(file_name$V1, 85, 87)
  mort_year$occupation1 <- substr(file_name$V1, 88, 90)
  mort_year$education1 <- substr(file_name$V1, 52, 53)
  mort_year$education2 <- substr(file_name$V1, 54, 54)
  
  mort_year$icd10 <- substr(file_name$V1, 142, 145)
  mort_year$cause_recode_358 <- substr(file_name$V1, 146, 148)
  mort_year$cause_recode_113 <- substr(file_name$V1, 151, 153)
  mort_year$cause_recode_130 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_39 <- substr(file_name$V1, 157, 158)

  mort_year$place_accident <- substr(file_name$V1, 141, 141)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


load_mortality_file_00_02 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 44, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'record_type',
                           'resident_status',
                           'death_date',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'occurrence_region',
                           'occurrence_division',
                           'occurrence_fipsstate',
                           'occurrence_fipscounty',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_region',
                           'residence_division',
                           'residence_psma',
                           'residence_psma_pop',
                           'residence_fipsstate',
                           'residence_fipscounty',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic1',
                           'hispanic2',
                           'education1',
                           'education2',
                           'date_of_birth',
                           'icd10',
                           'cause_recode_358',
                           'cause_recode_113',
                           'cause_recode_130',
                           'cause_recode_39',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 115, 118)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_date <- substr(file_name$V1, 55, 58)
  mort_year$death_day_of_week <- substr(file_name$V1, 83, 83) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 29, 30)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 49, 49)
  mort_year$occurrence_region <- substr(file_name$V1, 26, 26)
  mort_year$occurrence_division <- substr(file_name$V1, 27, 27)
  mort_year$occurrence_fipsstate <- substr(file_name$V1, 119, 120)
  mort_year$occurrence_fipscounty <- substr(file_name$V1, 121, 123)
  
  mort_year$residence_state <- substr(file_name$V1, 31, 32)
  mort_year$residence_exp_state <- substr(file_name$V1, 44, 45) 
  mort_year$residence_county <- substr(file_name$V1, 33, 35)
  mort_year$residence_county_pop <- substr(file_name$V1, 50, 50)
  mort_year$residence_city <- substr(file_name$V1, 36, 38)
  mort_year$residence_city_pop <- substr(file_name$V1, 39, 39)
  mort_year$residence_met <- substr(file_name$V1, 40, 40)
  mort_year$residence_region <- substr(file_name$V1, 41, 41)
  mort_year$residence_division <- substr(file_name$V1, 42, 42)
  mort_year$residence_psma <- substr(file_name$V1, 46, 48)
  mort_year$residence_psma_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_fipsstate <- substr(file_name$V1, 124, 125)
  mort_year$residence_fipscounty <- substr(file_name$V1,126, 128)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 129, 132)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 134, 135)
  
  mort_year$sex <- substr(file_name$V1, 59, 59)
  mort_year$race <- substr(file_name$V1, 60, 63)
  mort_year$age <- substr(file_name$V1, 64, 74)
  mort_year$marital_status <- substr(file_name$V1, 77, 77)
  mort_year$birth_state <- substr(file_name$V1, 78, 79)
  mort_year$hispanic1 <- substr(file_name$V1, 80, 81)
  mort_year$hispanic2 <- substr(file_name$V1, 82, 82)
  mort_year$education1 <- substr(file_name$V1, 52, 53)
  mort_year$education2 <- substr(file_name$V1, 54, 54)
  mort_year$date_of_birth <- substr(file_name$V1, 102, 109)
  
  mort_year$icd10 <- substr(file_name$V1, 142, 145)
  mort_year$cause_recode_358 <- substr(file_name$V1, 146, 148)
  mort_year$cause_recode_113 <- substr(file_name$V1, 151, 153)
  mort_year$cause_recode_130 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_39 <- substr(file_name$V1, 157, 158)
  
  mort_year$place_accident <- substr(file_name$V1, 141, 141)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


load_mortality_file_03 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 33, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'record_type',
                           'resident_status',
                           'death_date',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_psma_pop',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic',
                           'education',
                           'date_of_birth',
                           'icd10',
                           'cause_recode_358',
                           'cause_recode_113',
                           'cause_recode_130',
                           'cause_recode_39',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 102, 105)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_date <- substr(file_name$V1, 65, 68)
  mort_year$death_day_of_week <- substr(file_name$V1, 85, 85) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 26, 27)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 28, 28)
  
  mort_year$residence_state <- substr(file_name$V1, 29, 30)
  mort_year$residence_exp_state <- substr(file_name$V1, 45, 46) 
  mort_year$residence_county <- substr(file_name$V1, 35, 37)
  mort_year$residence_county_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_city <- substr(file_name$V1, 38, 42)
  mort_year$residence_city_pop <- substr(file_name$V1, 43, 43)
  mort_year$residence_met <- substr(file_name$V1, 44, 44)
  mort_year$residence_psma_pop <- substr(file_name$V1, 52, 52)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 47, 50)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 53, 54)
  
  mort_year$sex <- substr(file_name$V1, 69, 69)
  mort_year$race <- substr(file_name$V1, 445, 450)
  mort_year$age <- substr(file_name$V1, 70, 82)
  mort_year$marital_status <- substr(file_name$V1, 84, 84)
  mort_year$birth_state <- substr(file_name$V1, 55, 60)
  mort_year$hispanic <- substr(file_name$V1, 484, 488)
  mort_year$education <- substr(file_name$V1, 61, 64)
  mort_year$date_of_birth <- substr(file_name$V1, 86, 93)
  
  mort_year$icd10 <- substr(file_name$V1, 146, 149)
  mort_year$cause_recode_358 <- substr(file_name$V1, 150, 152)
  mort_year$cause_recode_113 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_130 <- substr(file_name$V1, 157, 159)
  mort_year$cause_recode_39 <- substr(file_name$V1, 160, 161)
  
  mort_year$place_accident <- substr(file_name$V1, 145, 145)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


load_mortality_file_04 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 32, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('data_year',
                           'record_type',
                           'resident_status',
                           'death_date',
                           'death_day_of_week',
                           'occurrence_state',
                           'occurrence_exp_state',
                           'occurrence_county',
                           'occurrence_county_pop',
                           'residence_state',
                           'residence_exp_state',
                           'residence_county',
                           'residence_county_pop',
                           'residence_city',
                           'residence_city_pop',
                           'residence_met',
                           'residence_psma_pop',
                           'residence_fips_pmsa',
                           'residence_fips_cmsa',
                           'sex',
                           'race',
                           'age',
                           'marital_status',
                           'birth_state',
                           'hispanic',
                           'education',
                           'icd10',
                           'cause_recode_358',
                           'cause_recode_113',
                           'cause_recode_130',
                           'cause_recode_39',
                           'place_accident')
  
  mort_year$data_year <- substr(file_name$V1, 102, 105)
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$death_date <- substr(file_name$V1, 65, 68)
  mort_year$death_day_of_week <- substr(file_name$V1, 85, 85) 
  
  mort_year$occurrence_state <- substr(file_name$V1, 21, 22)
  mort_year$occurrence_exp_state <- substr(file_name$V1, 26, 27)
  mort_year$occurrence_county <- substr(file_name$V1, 23, 25)
  mort_year$occurrence_county_pop <- substr(file_name$V1, 28, 28)
  
  mort_year$residence_state <- substr(file_name$V1, 29, 30)
  mort_year$residence_exp_state <- substr(file_name$V1, 45, 46) 
  mort_year$residence_county <- substr(file_name$V1, 35, 37)
  mort_year$residence_county_pop <- substr(file_name$V1, 51, 51)
  mort_year$residence_city <- substr(file_name$V1, 38, 42)
  mort_year$residence_city_pop <- substr(file_name$V1, 43, 43)
  mort_year$residence_met <- substr(file_name$V1, 44, 44)
  mort_year$residence_psma_pop <- substr(file_name$V1, 52, 52)
  mort_year$residence_fips_pmsa <- substr(file_name$V1, 47, 50)
  mort_year$residence_fips_cmsa <- substr(file_name$V1, 53, 54)
  
  mort_year$sex <- substr(file_name$V1, 69, 69)
  mort_year$race <- substr(file_name$V1, 445, 450)
  mort_year$age <- substr(file_name$V1, 70, 82)
  mort_year$marital_status <- substr(file_name$V1, 84, 84)
  mort_year$birth_state <- substr(file_name$V1, 55, 60)
  mort_year$hispanic <- substr(file_name$V1, 484, 488)
  mort_year$education <- substr(file_name$V1, 61, 64)
  
  mort_year$icd10 <- substr(file_name$V1, 146, 149)
  mort_year$cause_recode_358 <- substr(file_name$V1, 150, 152)
  mort_year$cause_recode_113 <- substr(file_name$V1, 154, 156)
  mort_year$cause_recode_130 <- substr(file_name$V1, 157, 159)
  mort_year$cause_recode_39 <- substr(file_name$V1, 160, 161)
  
  mort_year$place_accident <- substr(file_name$V1, 145, 145)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


# Run each year separately since data is very large
# Save to interim folder
# Would recommend zipping files to save space 

load_mortality_file_90_92('1990')
write.csv(mort_1990, "C:/Users/tangjy/Documents/interim/mort_1990.csv", row.names = F)
gc()

load_mortality_file_90_92('1991')
write.csv(mort_1991, "C:/Users/tangjy/Documents/interim/mort_1991.csv", row.names = F)
gc()

load_mortality_file_90_92('1992')
write.csv(mort_1992, "C:/Users/tangjy/Documents/interim/mort_1992.csv", row.names = F)
gc()

load_mortality_file_93_94('1993')
write.csv(mort_1993, "C:/Users/tangjy/Documents/interim/mort_1993.csv", row.names = F)
gc()

load_mortality_file_93_94('1994')
write.csv(mort_1994, "C:/Users/tangjy/Documents/interim/mort_1994.csv", row.names = F)
gc()

load_mortality_file_95('1995')
write.csv(mort_1995, "C:/Users/tangjy/Documents/interim/mort_1995.csv", row.names = F)
gc()

load_mortality_file_96_98('1996')
write.csv(mort_1996, "C:/Users/tangjy/Documents/interim/mort_1996.csv", row.names = F)
gc()

load_mortality_file_96_98('1997')
write.csv(mort_1997, "C:/Users/tangjy/Documents/interim/mort_1997.csv", row.names = F)
gc()

load_mortality_file_96_98('1998')
write.csv(mort_1998, "C:/Users/tangjy/Documents/interim/mort_1998.csv", row.names = F)
gc()

load_mortality_file_99('1999')
write.csv(mort_1999, "C:/Users/tangjy/Documents/interim/mort_1999.csv", row.names = F)
gc()

load_mortality_file_00_02('2000')
write.csv(mort_2000, "C:/Users/tangjy/Documents/interim/mort_2000.csv", row.names = F)
gc()

load_mortality_file_00_02('2001')
write.csv(mort_2001, "C:/Users/tangjy/Documents/interim/mort_2001.csv", row.names = F)
gc()

load_mortality_file_00_02('2002')
write.csv(mort_2002, "C:/Users/tangjy/Documents/interim/mort_2002.csv", row.names = F)
gc()

load_mortality_file_03('2003')
write.csv(mort_2003, "C:/Users/tangjy/Documents/interim/mort_2003.csv", row.names = F)
gc()

load_mortality_file_04('2004')
write.csv(mort_2004, "C:/Users/tangjy/Documents/interim/mort_2004.csv", row.names = F)
gc()















# Load raw mortality file - Function applies to data years 2005-2019
# Note: Raw .txt files are one string per death with information coded by string position 
# Following code converts strings to separate columns using substrings
load_mortality_file_05_19 <- function(year){
  file_path <- paste("C:/Users/tangjy/Documents/MCD_files/MULT", year, "US.AllCnty.txt", sep = '')
  file_name <- read.csv(file_path, header=FALSE)
  
  mort_year <- data.frame(matrix(ncol = 94, nrow = nrow(file_name)))
  
  colnames(mort_year) <- c('record_type',                 'resident_status',           'state_occurrence', 
                           'county_occurrence',           'state_occurrence_expanded', 'population_size_occurence', 
                           'state_residence',             'state_residence_recode',    'county_residence', 
                           'city_residence',              'population_size_residence', 'metro_non_metro', 
                           'state_residence_expanded',    'state_birth',               'state_birth_recode', 
                           'education_1989_cd',           'education_2003_cd',         'education_report_flag', 
                           'death_month',                 'sex',                       'detail_age', 
                           'age_sub_flag',                'age_recode_52',             'age_recode_27', 
                           'age_recode_19',               'infant_age_recode_22',      'place_of_death_and_status', 
                           'marital_status',              'death_day_of_week',         'current_data_year', 
                           'injury_at_work',              'manner_of_death',           'method_of_disposition', 
                           'autopsy_flag',                'certifier',                 'tobacco_use_contr_death_flag', 
                           'death_pregnancy_status',      'death_activity_code',       'place_of_injury', 
                           'icd_code',                    'cause_recode_358',          'cause_recode_113', 
                           'infant_cause_recode_130',     'cause_recode_39',           'number_of_entity_conditions', 
                           'entity_condition_1',          'entity_condition_2',        'entity_condition_3',
                           'entity_condition_4',          'entity_condition_5',        'entity_condition_6', 
                           'entity_condition_7',          'entity_condition_8',        'entity_condition_9', 
                           'entity_condition_10',         'entity_condition_11',       'entity_condition_12', 
                           'entity_condition_13',         'entity_condition_14',       'entity_condition_15', 
                           'entity_condition_16',         'entity_condition_17',       'entity_condition_18', 
                           'entity_condition_19',         'entity_condition_20',       'number_of_record_conditions',
                           'record_condition_1',          'record_condition_2',        'record_condition_3',
                           'record_condition_4',          'record_condition_5',        'record_condition_6', 
                           'record_condition_7',          'record_condition_8',        'record_condition_9', 
                           'record_condition_10',         'record_condition_11',       'record_condition_12', 
                           'record_condition_13',         'record_condition_14',       'record_condition_15', 
                           'record_condition_16',         'record_condition_17',       'record_condition_18', 
                           'record_condition_19',         'record_condition_20',       'race',                        
                           'bridged_race_flag',           'race_imputation_flag',      'race_recode_3', 
                           'race_recode_5',               'hispanic_origin',           'hispanic_origin_recode',
                           'race_recode_40')
  
  mort_year$record_type <- substr(file_name$V1, 19, 19)
  mort_year$resident_status <- substr(file_name$V1, 20, 20)
  mort_year$state_occurrence <- substr(file_name$V1, 21, 22)
  mort_year$county_occurrence <- substr(file_name$V1, 23, 25)
  mort_year$state_occurrence_expanded <- substr(file_name$V1, 26, 27)
  mort_year$population_size_occurence <- substr(file_name$V1, 28, 28) 
  mort_year$state_residence <- substr(file_name$V1, 29, 30)
  mort_year$state_residence_recode <- substr(file_name$V1, 33, 34)
  mort_year$county_residence <- substr(file_name$V1, 35, 37)
  mort_year$city_residence <- substr(file_name$V1, 38, 42)
  mort_year$population_size_residence <- substr(file_name$V1, 43, 43)
  mort_year$metro_non_metro <- substr(file_name$V1, 44, 44)
  mort_year$state_residence_expanded <- substr(file_name$V1, 45, 46)
  mort_year$state_birth <- substr(file_name$V1, 55, 56)
  mort_year$state_birth_recode <- substr(file_name$V1, 59, 60)
  
  mort_year$education_1989_cd <- substr(file_name$V1, 61, 62) 
  mort_year$education_2003_cd <- substr(file_name$V1, 63, 63)
  mort_year$education_report_flag <- substr(file_name$V1, 64, 64)
  
  mort_year$death_month <- substr(file_name$V1, 65, 66)
  mort_year$sex <- substr(file_name$V1, 69, 69)
  mort_year$detail_age <- substr(file_name$V1, 70, 73)
  mort_year$age_sub_flag <- substr(file_name$V1, 74, 74)
  mort_year$age_recode_52 <- substr(file_name$V1, 75, 76)
  mort_year$age_recode_27 <- substr(file_name$V1, 77, 78)
  mort_year$age_recode_19 <- substr(file_name$V1, 79, 80)
  mort_year$infant_age_recode_22 <- substr(file_name$V1, 81, 82)
  mort_year$place_of_death_and_status <- substr(file_name$V1, 83, 83)
  mort_year$marital_status <- substr(file_name$V1, 84, 84)
  mort_year$death_day_of_week <- substr(file_name$V1, 85, 85)
  mort_year$current_data_year <- substr(file_name$V1, 102, 105)
  mort_year$injury_at_work <- substr(file_name$V1, 106, 106)
  mort_year$manner_of_death <- substr(file_name$V1, 107, 107)
  mort_year$method_of_disposition <- substr(file_name$V1, 108, 108)
  mort_year$autopsy_flag <- substr(file_name$V1, 109, 109)
  mort_year$certifier <- substr(file_name$V1, 110, 110)
  mort_year$tobacco_use_contr_death_flag <- substr(file_name$V1, 142, 142)
  mort_year$death_pregnancy_status <- substr(file_name$V1, 143, 143)
  mort_year$death_activity_code <- substr(file_name$V1, 144, 144)
  mort_year$place_of_injury <- substr(file_name$V1, 145, 145)
  
  mort_year$icd_code <- substr(file_name$V1, 146, 149)
  mort_year$cause_recode_358 <- substr(file_name$V1, 150, 152)
  mort_year$cause_recode_113 <- substr(file_name$V1, 154, 156)
  mort_year$infant_cause_recode_130 <- substr(file_name$V1, 157, 159)
  mort_year$cause_recode_39 <- substr(file_name$V1, 160, 161)
  
  mort_year$number_of_entity_conditions <- substr(file_name$V1, 163, 164)
  mort_year$entity_condition_1 <- substr(file_name$V1, 165, 171)
  mort_year$entity_condition_2 <- substr(file_name$V1, 172, 178)
  mort_year$entity_condition_3 <- substr(file_name$V1, 179, 185)
  mort_year$entity_condition_4 <- substr(file_name$V1, 186, 192)
  mort_year$entity_condition_5 <- substr(file_name$V1, 193, 199)
  mort_year$entity_condition_6 <- substr(file_name$V1, 200, 206)
  mort_year$entity_condition_7 <- substr(file_name$V1, 207, 213)
  mort_year$entity_condition_8 <- substr(file_name$V1, 214, 220)
  mort_year$entity_condition_9 <- substr(file_name$V1, 221, 227)
  mort_year$entity_condition_10 <- substr(file_name$V1, 228, 234)
  mort_year$entity_condition_11 <- substr(file_name$V1, 235, 241)
  mort_year$entity_condition_12 <- substr(file_name$V1, 242, 248)
  mort_year$entity_condition_13 <- substr(file_name$V1, 249, 255)
  mort_year$entity_condition_14 <- substr(file_name$V1, 256, 262)
  mort_year$entity_condition_15 <- substr(file_name$V1, 263, 269)
  mort_year$entity_condition_16 <- substr(file_name$V1, 270, 276)
  mort_year$entity_condition_17 <- substr(file_name$V1, 277, 283)
  mort_year$entity_condition_18 <- substr(file_name$V1, 284, 290)
  mort_year$entity_condition_19 <- substr(file_name$V1, 291, 297)
  mort_year$entity_condition_20 <- substr(file_name$V1, 298, 304)
  
  mort_year$number_of_record_conditions <- substr(file_name$V1, 341, 342)
  mort_year$record_condition_1 <- substr(file_name$V1, 344, 348)
  mort_year$record_condition_2 <- substr(file_name$V1, 349, 353)
  mort_year$record_condition_3 <- substr(file_name$V1, 354, 358)
  mort_year$record_condition_4 <- substr(file_name$V1, 359, 363)
  mort_year$record_condition_5 <- substr(file_name$V1, 364, 368)
  mort_year$record_condition_6 <- substr(file_name$V1, 369, 373)
  mort_year$record_condition_7 <- substr(file_name$V1, 374, 378)
  mort_year$record_condition_8 <- substr(file_name$V1, 379, 383)
  mort_year$record_condition_9 <- substr(file_name$V1, 384, 388)
  mort_year$record_condition_10 <- substr(file_name$V1, 389, 393)
  mort_year$record_condition_11 <- substr(file_name$V1, 394, 398)
  mort_year$record_condition_12 <- substr(file_name$V1, 399, 403)
  mort_year$record_condition_13 <- substr(file_name$V1, 404, 408)
  mort_year$record_condition_14 <- substr(file_name$V1, 409, 413)
  mort_year$record_condition_15 <- substr(file_name$V1, 414, 418)
  mort_year$record_condition_16 <- substr(file_name$V1, 419, 423)
  mort_year$record_condition_17 <- substr(file_name$V1, 424, 428)
  mort_year$record_condition_18 <- substr(file_name$V1, 429, 433)
  mort_year$record_condition_19 <- substr(file_name$V1, 434, 438)
  mort_year$record_condition_20 <- substr(file_name$V1, 439, 443)
  
  mort_year$race <- substr(file_name$V1, 445, 446)
  mort_year$bridged_race_flag <- substr(file_name$V1, 447, 447)
  mort_year$race_imputation_flag <- substr(file_name$V1, 448, 448)
  mort_year$race_recode_3 <- substr(file_name$V1, 449, 449)
  mort_year$race_recode_5 <- substr(file_name$V1, 450, 450)
  mort_year$hispanic_origin <- substr(file_name$V1, 484, 486)
  mort_year$hispanic_origin_recode <- substr(file_name$V1, 488, 488)
  mort_year$race_recode_40 <- substr(file_name$V1, 489, 490)
  
  mort_year_name <- paste("mort", year, sep="_")
  assign(mort_year_name, mort_year, env=.GlobalEnv)
  gc()
}


# Function to load and format complete mortality files
# Save as csv files to interim directory
# Files are large so clear R environment between runs 
load_mortality_file_05_19('2005')
write.csv(mort_2005, "C:/Users/tangjy/Documents/interim/mort_2005.csv", row.names = F)
gc()
load_mortality_file_05_19('2006')
write.csv(mort_2006, "C:/Users/tangjy/Documents/interim/mort_2006.csv", row.names = F)
gc()
load_mortality_file_05_19('2007')
write.csv(mort_2007, "C:/Users/tangjy/Documents/interim/mort_2007.csv", row.names = F)
gc()
load_mortality_file_05_19('2008')
write.csv(mort_2008, "C:/Users/tangjy/Documents/interim/mort_2008.csv", row.names = F)
gc()
load_mortality_file_05_19('2009')
write.csv(mort_2009, "C:/Users/tangjy/Documents/interim/mort_2009.csv", row.names = F)
gc()
load_mortality_file_05_19('2010')
write.csv(mort_2010, "C:/Users/tangjy/Documents/interim/mort_2010.csv", row.names = F)
gc()
load_mortality_file_05_19('2011')
write.csv(mort_2011, "C:/Users/tangjy/Documents/interim/mort_2011.csv", row.names = F)
gc()
load_mortality_file_05_19('2012')
write.csv(mort_2012, "C:/Users/tangjy/Documents/interim/mort_2012.csv", row.names = F)
gc()
load_mortality_file_05_19('2013')
write.csv(mort_2013, "C:/Users/tangjy/Documents/interim/mort_2013.csv", row.names = F)
gc()
load_mortality_file_05_19('2014')
write.csv(mort_2014, "C:/Users/tangjy/Documents/interim/mort_2014.csv", row.names = F)
gc()
load_mortality_file_05_19('2015')
write.csv(mort_2015, "C:/Users/tangjy/Documents/interim/mort_2015.csv", row.names = F)
gc()
load_mortality_file_05_19('2016')
write.csv(mort_2016, "C:/Users/tangjy/Documents/interim/mort_2016.csv", row.names = F)
gc()
load_mortality_file_05_19('2017')
write.csv(mort_2017, "C:/Users/tangjy/Documents/interim/mort_2017.csv", row.names = F)
gc()
load_mortality_file_05_19('2018')
write.csv(mort_2018, "C:/Users/tangjy/Documents/interim/mort_2018.csv", row.names = F)
gc()
load_mortality_file_05_19('2019')
write.csv(mort_2019, "C:/Users/tangjy/Documents/interim/mort_2019.csv", row.names = F)
gc()



# Function to select relevant columns and filter for deaths of despair conditions
cleaned_mortality_file <- function(loaded_file, year){
  cleaned_file <- loaded_file %>%
    select(current_data_year,
           death_month,
           state_residence, 
           county_residence, 
           education_2003_cd,
           sex,
           age_recode_27,
           marital_status,
           manner_of_death,
           icd_code,
           cause_recode_358,
           race,
           hispanic_origin_recode) %>%
    filter(cause_recode_358 %in% c('424', '425', '426', '427',          # Suicide
                                   '428', '429', '430', '431',
                                   '420', '443',                        # Drug Overdose
                                   '298')) %>%                          # ARLD
    left_join(state_fips_xwalk, by = c('state_residence'='state_abbrev')) %>%
    select(-?..state)
  
  mort_year_name <- paste("cleaned_mort", year, sep="_")
  assign(mort_year_name, cleaned_file, env=.GlobalEnv)
}

cleaned_mortality_file(mort_2013, '2013')
#rm(mort_2013)
gc()
cleaned_mortality_file(mort_2014, '2014')
rm(mort_2014)
gc()
cleaned_mortality_file(mort_2015, '2015')
rm(mort_2015)
gc()
cleaned_mortality_file(mort_2016, '2016')
rm(mort_2016)
gc()
cleaned_mortality_file(mort_2017, '2017')
rm(mort_2017)
gc()
cleaned_mortality_file(mort_2018, '2018')
rm(mort_2018)
gc()
cleaned_mortality_file(mort_2019, '2019')
rm(mort_2019)
gc()

cleaned_mort_2013_2019 <- bind_rows(
  cleaned_mort_2013,
  cleaned_mort_2014, 
  cleaned_mort_2015,
  cleaned_mort_2016,
  cleaned_mort_2017,
  cleaned_mort_2018,
  cleaned_mort_2019
)


write.csv(cleaned_mort_2013_2019, "C:/Users/tangjy/Documents/interim/cleaned_mort_2013_2019.csv", row.names = F)


# Start here 
# cleaned_mort_2013_2019 <- read.csv("C:/Users/tangjy/Documents/interim/cleaned_mort_2013_2019.csv")

# one hot encoding
cleaned_mort_2013_2019 <- cleaned_mort_2013_2019 %>%
  mutate(suicide = ifelse(cause_recode_358 %in% c('424', '425', '426', '427', '428', '429', '430', '431'), 1, 0),
         drug_overdose = ifelse(cause_recode_358 %in% c('420', '443'), 1, 0),
         arld = ifelse(cause_recode_358 == '298', 1, 0)) %>%
  mutate(age_00_19 = ifelse(age_recode_27 %in% c(1,2,3,4,5,6,7,8,9), 1, 0),
         age_20_24 = ifelse(age_recode_27 == 10, 1, 0),
         age_25_29 = ifelse(age_recode_27 == 11, 1, 0), 
         age_30_34 = ifelse(age_recode_27 == 12, 1, 0),
         age_35_39 = ifelse(age_recode_27 == 13, 1, 0),
         age_40_44 = ifelse(age_recode_27 == 14, 1, 0),
         age_45_49 = ifelse(age_recode_27 == 15, 1, 0),
         age_50_54 = ifelse(age_recode_27 == 16, 1, 0),
         age_55_59 = ifelse(age_recode_27 == 17, 1, 0),
         age_60_64 = ifelse(age_recode_27 == 18, 1, 0),
         age_65_100 = ifelse(age_recode_27 %in% c(19,20,21,22,23,24,25,26), 1, 0)) %>%
  mutate(race_white = ifelse(race == 1, 1, 0),
         race_black = ifelse(race == 2, 1, 0),
         race_amerin = ifelse(race == 3, 1, 0),
         race_asian = ifelse(race %in% c(4,5,6,7,18,28,38,48,58,68,78), 1, 0)) %>%
  mutate(gender_male = ifelse(sex == 'M', 1, 0),
         gender_female = ifelse(sex == 'F', 1, 0)) %>%
  mutate(middle_school = ifelse(education_2003_cd == 1, 1, 0),
         part_high_school = ifelse(education_2003_cd == 2, 1, 0),
         high_school_grad_ged = ifelse(education_2003_cd == 3, 1, 0),
         part_college = ifelse(education_2003_cd == 4, 1, 0),
         associate_degree = ifelse(education_2003_cd == 5, 1, 0),
         bachelor_degree = ifelse(education_2003_cd == 6, 1, 0),
         master_degree = ifelse(education_2003_cd == 7, 1, 0),
         doctorate_degree = ifelse(education_2003_cd == 8, 1, 0))


# ----------------------------------------------
# Aggregate data at the county level 
county_year_mort <- cleaned_mort_2013_2019 %>%
  mutate(fips_cd = state_fips*1000 + county_residence) %>%
  group_by(current_data_year, fips_cd) %>%
  summarise(
    total_deaths_of_despair = n(),
    suicide_deaths = sum(ifelse(cause_recode_358 %in% c('424', '425', '426', '427', '428', '429', '430', '431'), 1, 0)),
    drug_overdose_deaths = sum(ifelse(cause_recode_358 %in% c('420', '443'), 1, 0)),
    arld_deaths = sum(ifelse(cause_recode_358=='298', 1, 0))
  ) 









# Importing cleaned SEER Population data by county 2014-2019
county_pop_2013_2019 <- read.csv("C:/Users/tangjy/Documents/imported_data/county_pop_2014_2019.csv")



# Join death data with county population data to create death rates
# Use total death rates for now
county_outcomes_2014_2019 <- county_year_mort %>%
  left_join(county_pop_2014_2019, by=c('current_data_year'='year', 'fips_cd'='fips_cd')) %>%
  select(current_data_year, 
         fips_cd,
         total_pop, 
         total_deaths_of_despair,
         suicide_deaths,
         drug_overdose_deaths,
         arld_deaths) %>%
  mutate(deaths_of_despair_rate = total_deaths_of_despair/total_pop,
         suicide_rate = suicide_deaths/total_pop,
         drug_overdose_rate = drug_overdose_deaths/total_pop,
         arld_rate = arld_deaths/total_pop) 


county_outcomes_1 <- county_outcomes_2014_2019 %>%
  select(current_data_year, 
         fips_cd,
         deaths_of_despair_rate,
         suicide_rate,
         drug_overdose_rate,
         arld_rate)

write.csv(county_outcomes_1, "C:/Users/tangjy/Documents/interim/county_outcomes_1.csv", row.names = F)











