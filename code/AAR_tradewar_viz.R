# --------------------------------------------------------------------------------------------------------------------------------
# Trade War AAR Visualizations   
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file creates visualizations used for Trade War AAR 

# Libraries
library(tidyverse)
library(dplyr)
library(haven)
library(broom)
library(naniar)
library(ggplot2)
library(stargazer)
library(data.table)
library(R.utils)
library(devtools)
library(urbnmapr)
library(paletteer)
library(viridis)
library(zoo)


rm(list=ls()) 


# --------------------------------------------------------------------------------------------------------------------------------

# Load in datasets
AAR_tradewar_county <- read_csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_county.csv")

# Separate AAR scores into deciles and quartiles - and an unaffected category (n observations closest to zero where
# n = roughly the # of observations per decile)
AAR_tradewar_county_edit <- AAR_tradewar_county %>%
  mutate(decile = ntile(AAR_county, 10)) %>%
  mutate(bottom_decile = as.factor(ifelse(decile==1, 1, 0)),
         upper_decile = as.factor(ifelse(decile==10, 1, 0)),
         unaffected = as.factor(ifelse(between(AAR_tradewar_county$AAR_county, -0.0025, 0.0025)==TRUE, 1, 0))) %>%
  mutate(aar_category = as.factor(ifelse(bottom_decile==1,0,
                                         ifelse(unaffected==1,1,
                                                ifelse(upper_decile==1,2,3))))) %>%
  mutate(quartile = ntile(AAR_county, 4)) %>%
  mutate(bottom_quartile = as.factor(ifelse(quartile==1, 1, 0)),
         upper_quartile = as.factor(ifelse(quartile==4, 1, 0)))


# --------------------------------------------------------------------------------------------------------------------------------

# AAR Plot - Top 5 and bottom 5 performing industries
AAR_tradewar_industry <- read_csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_tradewar_industry.csv")

AAR_plot <- data.frame(matrix(vector(), 49, 3,
                              dimnames=list(c(), c("industry", "ff_cd", "AAR"))),
                       stringsAsFactors=F)

AAR_plot$industry <- AAR_tradewar_industry$industry
AAR_plot$ff_cd <- AAR_tradewar_industry$FF_48
AAR_plot$AAR <- as.numeric(AAR_tradewar_industry$abnormal_returns_ev_avg)

AAR_plot <-AAR_plot %>%
  arrange(desc(AAR)) %>%
  mutate(index = row_number()) %>%
  filter(index <= 10 | index >= 40) %>%
  mutate(positive = as.character(ifelse(AAR>0,1,0)))

level_order <- c("Txtls","Cnstr","Mach","Paper","Autos","BldMt","Books","FabPr","Meals","Aero",
                 "Banks","Ships","Fun","ElcEq","Rubbr","Insur","Gold","Hlth","Oil","Toys")

plot <- ggplot(AAR_plot, aes(x=factor(industry, level=level_order),y=AAR, fill = positive)) +
  geom_bar(position = 'identity', 
           stat='identity', 
           show.legend = F) + 
  labs(title = "Industries most affected by the trade war",
       x = "Fama French Industry Definition",
       y = "AAR Industry Score") + 
  scale_x_discrete(labels=c("Txtls" = "Textiles", 
                            "Cnstr" = "Construction",
                            "Mach" = "Machinery",
                            "Paper" = "Business Supplies", 
                            "Autos" = "Automobiles and Trucks",
                            "BldMt" = "Construction Materials",
                            "Books" = "Printing and Publishing", 
                            "FabPr" = "Fabricated Products",
                            "Meals" = "Restaurants, Hotels, Motels",
                            "Aero" = "Aircraft", 
                            "Banks" = "Banking",
                            "Ships" = "Shipbuilding, Railroads",
                            "Fun" = "Entertainment", 
                            "ElcEq" = "Electrical Equipment",
                            "Rubbr" = "Rubber and Plastic Products",
                            "Insur" = "Insurance", 
                            "Gold" = "Precious Metals",
                            "Hlth" = "Healthcare",
                            "Oil" = "Petroleum and Natural Gas", 
                            "Toys" = "Recreation")) + 
  scale_fill_manual("legend", values = c("1" = "midnightblue", "0" = "firebrick4")) + 
  coord_flip() + 
  theme_minimal()
show(plot)


# --------------------------------------------------------------------------------------------------------------------------------

# AAR trade war distribution plot
ggplot(AAR_tradewar_county_edit, aes(x=AAR_county)) + 
  geom_density() + 
  geom_vline(xintercept=mean(AAR_tradewar_county_edit$AAR_county), color='red') + 
  geom_vline(xintercept=mean(AAR_tradewar_county_edit$AAR_county) + sd(AAR_tradewar_county_edit$AAR_county), linetype='dotted') + 
  geom_vline(xintercept=mean(AAR_tradewar_county_edit$AAR_county) - sd(AAR_tradewar_county_edit$AAR_county), linetype='dotted') + 
  labs(title="County-level AAR Distribution",
       x = "AAR County Scores",
       y = "Density") + 
  theme_minimal()



# --------------------------------------------------------------------------------------------------------------------------------

# Mapping AAR by county
county_AAR_mapper <- AAR_tradewar_county_edit %>%
  mutate(fipscounty = as.character(fipscounty)) %>%
  mutate(fipscounty = str_pad(fipscounty, 5, pad='0')) %>%
  mutate(AAR_decile = as.factor(ntile(AAR_county, 11))) %>%
  mutate(decile_bounds = ifelse(AAR_decile==1, "-min : -0.024",
                                ifelse(AAR_decile==2, "-0.024 : -0.013",
                                       ifelse(AAR_decile==3, "-0.013 : -0.007",
                                              ifelse(AAR_decile==4, "-0.007 : -0.003",
                                                     ifelse(AAR_decile==5, "-0.003 : 0.002",
                                                            ifelse(AAR_decile==6, "0.002 : 0.006",
                                                                   ifelse(AAR_decile==7, "0.006 : 0.009",
                                                                          ifelse(AAR_decile==8, "0.009 : 0.014",
                                                                                 ifelse(AAR_decile==9, "0.014 : 0.020",
                                                                                        ifelse(AAR_decile==10, "0.020 : 0.031", "0.031 : max")))))))))))
AAR_mapper <- left_join(county_AAR_mapper, counties, by = c("fipscounty"="county_fips")) 

min(subset(county_AAR_mapper, AAR_decile==1)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==1)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==2)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==2)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==3)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==3)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==4)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==4)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==5)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==5)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==6)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==6)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==7)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==7)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==8)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==8)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==9)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==9)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==10)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==10)$AAR_county)
min(subset(county_AAR_mapper, AAR_decile==11)$AAR_county)
max(subset(county_AAR_mapper, AAR_decile==11)$AAR_county)

level_order <- c("-min : -0.024",
                 "-0.024 : -0.013",
                 "-0.013 : -0.007",
                 "-0.007 : -0.003",
                 "-0.003 : 0.002",
                 "0.002 : 0.006",
                 "0.006 : 0.009",
                 "0.009 : 0.014",
                 "0.014 : 0.020",
                 "0.020 : 0.031",
                 "0.031 : max")

AAR_mapper %>%
  ggplot(aes(long, lat, group = group, fill = factor(decile_bounds, level=level_order))) +
  geom_polygon(color = NA) +
  scale_fill_brewer(palette = 'RdBu') + 
  #paletteer::scale_fill_paletteer_c("scico::tokyo") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "AAR distribution by county", 
       fill = "AAR") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


# --------------------------------------------------------------------------------------------------------------------------------

# # AAR Tradewar Estimates for unemployment
# # Load and clean BLS unemployment data
# unemployment_county <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/unemployment_county.csv",)
# 
# # Unemployment by year
# unemployment_county_year <- unemployment_county %>% 
#   filter(year >= 2013 & year <= 2019) %>%
#   group_by(fipscounty, year) %>%
#   summarise(avg_unemp_rate = mean(value, na.rm = T))
# # Unemployment by quarter
# unemployment_county_quarter <- unemployment_county %>% 
#   filter(year >= 2013 & year <= 2019) %>%
#   mutate(date = as.Date(paste(year, substr(period, 2, 3), '01', sep='-'))) %>%
#   mutate(quarter = as.yearqtr(date, format = "%Y-%m-%d")) %>%
#   drop_na() %>%
#   group_by(fipscounty, quarter) %>%
#   summarise(avg_unemp_rate = mean(value, na.rm=T))
# 
# rm(unemployment_county)
# 
# 
# # SEER Population Data
# county_pop <- read_csv("/Users/tangj18/Documents/Honors Research /interim_data/county_pop_1990_2019.csv")
# county_pop <- county_pop %>%
#   select(fipscounty, year, total_pop) %>%
#   filter(year >= 2013 & year <= 2019) 
# 
# 
# # Join BLS unemployment data with SEER population data and county level AAR scores
# # Decile plots
# AAR_unemp_2013_2019 <- AAR_tradewar_county_edit %>%
#   left_join(unemployment_county_year) %>%
#   mutate(post = ifelse(year <= 2018, 0, 1)) %>%
#   left_join(county_pop, by=c('fipscounty'='fipscounty', 'year'='year'))
# year_aar_pop <- AAR_unemp_2013_2019 %>%
#   group_by(aar_category, year) %>%
#   summarise(aar_category_total_pop = sum(total_pop, na.rm = T)) %>%
#   filter(!is.na(year))
# AAR_unemp_2013_2019 <- AAR_unemp_2013_2019 %>%
#   left_join(year_aar_pop, by=c('aar_category' = 'aar_category', 'year'='year')) %>%
#   select(fipscounty,
#          year,
#          AAR_county,
#          decile,
#          aar_category,
#          avg_unemp_rate,
#          total_pop,
#          aar_category_total_pop,
#          post) %>%
#   mutate(weighted_unemp = (total_pop/aar_category_total_pop)*avg_unemp_rate) %>%
#   filter(aar_category %in% c(0,1,2))
# 
# 
# # Weighted average unemployment 
# AAR_unemp_2013_2019 %>%
#   filter(aar_category %in% c(0,1,2)) %>%
#   group_by(year, aar_category) %>%
#   summarise(weighted_avg_unemp = sum(weighted_unemp, na.rm=T)) %>%
#   ggplot(., aes(x=year, y=weighted_avg_unemp, group=aar_category, color=aar_category)) + 
#   geom_line() +
#   scale_color_manual(labels = c("Bottom Decile", "Unaffected", "Upper Decile"), 
#                      values = c("blue","red","green"))
# 
# # No weights 
# AAR_unemp_2013_2019 %>%
#   filter(aar_category %in% c(0,1,2)) %>%
#   group_by(year, aar_category, post) %>%
#   summarise(avg_unemp = mean(avg_unemp_rate, na.rm=T)) %>%
#   ggplot(., aes(x=year, y=avg_unemp, group=aar_category, color=aar_category)) + 
#   geom_line() +
#   scale_color_manual(labels = c("Bottom Decile", "Unaffected", "Upper Decile"), 
#                      values = c("blue","red","green"))


# END --------------------------------------------------------------







