# --------------------------------------------------------------------------------------------------------------------------------
# PNTR AAR Visualizations   
# --------------------------------------------------------------------------------------------------------------------------------

# Description
# This file creates visualizations used for PNTR AAR 


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
library(grid)



rm(list=ls()) 


#  --------------------------------------------------------------------------------------------------------

# Compare AAR with NTR Gap

AAR_pntr_county_edit <- read_csv("/Users/tangj18/Documents/Honors Research /interim/AAR_pntr_county_edit.csv")

# Pierce Schott (2020) NTR Gap 
NTR_gap_2000 <- read_dta('/Users/tangj18/Documents/Honors Research /downloaded_data/Pierce_schott/NTR_1990w_countyid.dta')
NTR_gap_2000 <- NTR_gap_2000 %>%
  select(countyid, NTR_scratch_90w)  %>%
  mutate(NTR_gap = NTR_scratch_90w) %>%
  select(-NTR_scratch_90w)

aar_vs_ntr <- AAR_pntr_county_edit %>%
  left_join(NTR_gap_2000, by=c("fipscounty"="countyid")) %>%
  filter(!is.na(NTR_gap)) %>%
  filter(!is.na(AAR_county)) %>%
  mutate(AAR_std = (AAR_county - mean(AAR_county))/sd(AAR_county),
         NTR_std = (NTR_gap - mean(NTR_gap))/sd(NTR_gap)) %>%
  mutate(AAR_pos = ifelse(AAR_std >= 0, 1, 0))

ggplot(aar_vs_ntr, aes(x=NTR_std)) +
  geom_density()

ggplot(aar_vs_ntr, aes(x=AAR_county)) + geom_density() + 
  geom_vline(xintercept=mean(AAR_pntr_county_edit$AAR_county), color='red') + 
  geom_vline(xintercept=mean(AAR_pntr_county_edit$AAR_county) + sd(AAR_pntr_county_edit$AAR_county), linetype='dotted') + 
  geom_vline(xintercept=mean(AAR_pntr_county_edit$AAR_county) - sd(AAR_pntr_county_edit$AAR_county), linetype='dotted') + 
  labs(title="County-level AAR Distribution",
       x = "AAR County Scores",
       y = "Density") + 
  theme_minimal()


cor(aar_vs_ntr$AAR_std, aar_vs_ntr$NTR_std)
ggplot(aar_vs_ntr, aes(x=NTR_std, y=AAR_std)) + geom_point()

cor(aar_vs_ntr$AAR_std[aar_vs_ntr$AAR_pos == 1], aar_vs_ntr$NTR_std[aar_vs_ntr$AAR_pos == 1])
ggplot(aar_vs_ntr %>%
         filter(AAR_pos == 1), aes(x=NTR_std, y=AAR_std)) + geom_point()

cor(aar_vs_ntr$AAR_std[aar_vs_ntr$AAR_pos == 0], aar_vs_ntr$NTR_std[aar_vs_ntr$AAR_pos == 0])
ggplot(aar_vs_ntr %>%
         filter(AAR_pos == 0), aes(x=NTR_std, y=AAR_std)) + geom_point()



# Linear regression 
check_AAR <- lm(AAR_std ~ NTR_std, data=aar_vs_ntr)
summary(check_AAR)

# Output coeffecient estimate results in tabular form 
stargazer(check_AAR)


#  --------------------------------------------------------------------------------------------------------

# AAR pntr distribution plot
ggplot(AAR_pntr_county_edit, aes(x=AAR_county)) + 
  geom_density() + 
  geom_vline(xintercept=mean(AAR_pntr_county_edit$AAR_county), color='red') + 
  geom_vline(xintercept=mean(AAR_pntr_county_edit$AAR_county) + sd(AAR_pntr_county_edit$AAR_county), linetype='dotted') + 
  geom_vline(xintercept=mean(AAR_pntr_county_edit$AAR_county) - sd(AAR_pntr_county_edit$AAR_county), linetype='dotted') + 
  labs(title="County level Employment Weighted Average AAR Distribution",
       x = "AAR") + 
  theme_minimal()

mean(AAR_pntr_county_edit$AAR_county)
mean(AAR_pntr_county_edit$AAR_county)


#  --------------------------------------------------------------------------------------------------------

# Mapping AAR by county
county_AAR_mapper <- AAR_pntr_county_edit %>%
  select(fipscounty, AAR_county) %>%
  mutate(fipscounty = as.character(fipscounty)) %>%
  mutate(fipscounty = str_pad(fipscounty, 5, pad='0')) %>%
  mutate(AAR_decile = as.factor(ntile(AAR_county, 11))) %>%
  mutate(decile_bounds = ifelse(AAR_decile==1, "-0.50 : -0.195",
                                ifelse(AAR_decile==2, "-0.195 : -0.153",
                                ifelse(AAR_decile==3, "-0.153 : -0.125",
                                ifelse(AAR_decile==4, "-0.125 : -0.103",
                                ifelse(AAR_decile==5, "-0.103 : -0.084",
                                ifelse(AAR_decile==6, "-0.084 : -0.065",
                                ifelse(AAR_decile==7, "-0.065 : -0.044",
                                ifelse(AAR_decile==8, "-0.044 : -0.024",
                                ifelse(AAR_decile==9, "-0.024 : 0.005",
                                ifelse(AAR_decile==10, "0.005 : 0.065", "0.065 : 0.837")))))))))))
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

level_order <- c("-0.50 : -0.195",
                 "-0.195 : -0.153",
                 "-0.153 : -0.125",
                 "-0.125 : -0.103",
                 "-0.103 : -0.084",
                 "-0.084 : -0.065",
                 "-0.065 : -0.044",
                 "-0.044 : -0.024",
                 "-0.024 : 0.005",
                 "0.005 : 0.065",
                 "0.065 : 0.837")

AAR_mapper %>%
  ggplot(aes(long, lat, group = group, fill = factor(decile_bounds, level=level_order))) +
  geom_polygon(color = NA) +
  scale_fill_brewer(palette = 'RdBu') + 
  #paletteer::scale_fill_paletteer_c("scico::tokyo") + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "AAR distribution by county (split into categories)", 
       fill = "AAR") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



#  --------------------------------------------------------------------------------------------------------

# AAR Plot - Top 5 and bottom 5 performing industries
AAR_pntr_industry <- read_csv("/Users/tangj18/Documents/Honors Research /final_datasets/AAR_pntr_industry.csv")

AAR_plot <- data.frame(matrix(vector(), 49, 3,
                              dimnames=list(c(), c("industry", "ff_cd", "AAR"))),
                       stringsAsFactors=F)

AAR_plot$industry <- AAR_pntr_industry$industry
AAR_plot$ff_cd <- AAR_pntr_industry$FF_48
AAR_plot$AAR <- as.numeric(AAR_pntr_industry$abnormal_returns_ev_avg)

AAR_plot <-AAR_plot %>%
  arrange(desc(AAR)) %>%
  mutate(index = row_number()) %>%
  filter(index <= 10 | index >= 40) %>%
  mutate(positive = as.character(ifelse(AAR>0,1,0)))

level_order <- c("Steel","FabPr","LabEq","Autos","Fun","Gold","BldMt","ElcEq","Fin","Telcm",
                 "Ships","Hlth","Insur","Oil","Drugs","Soda","Food","Guns","Coal","Smoke")

plot <- ggplot(AAR_plot, aes(x=factor(industry, level=level_order),y=AAR, fill = positive)) +
  geom_bar(position = 'identity', 
           stat='identity', 
           show.legend = F) + 
  labs(title = "Industries most affected by the trade war",
       x = "Fama French Industry Definition",
       y = "AAR Industry Score") + 
  scale_x_discrete(labels=c("Steel" = "Steel Works", 
                            "FabPr" = "Fabricated Products",
                            "LabEq" = "Laboratory Equipment",
                            "Autos" = "Automobiles and Trucks", 
                            "Fun" = "Entertainment",
                            "Gold" = "Precious Metals",
                            "BldMt" = "Construction Materials", 
                            "ElcEq" = "Electrical Equipment",
                            "Fin" = "Trading",
                            "Telcm" = "Communication", 
                            "Ships" = "Shipbuilding, Railroads",
                            "Hlth" = "Healthcare",
                            "Insur" = "Insurance", 
                            "Oil" = "Petroleum and Natural Gas",
                            "Drugs" = "Pharmaceutical",
                            "Soda" = "Candy and Soda", 
                            "Food" = "Food Products",
                            "Guns" = "Defense",
                            "Coal" = "Coal", 
                            "Smoke" = "Tobacco Products")) + 
  scale_fill_manual("legend", values = c("1" = "midnightblue", "0" = "firebrick4")) + 
  coord_flip() + 
  theme_minimal()
show(plot)



#  --------------------------------------------------------------------------------------------------------

# # Relationship between unemployment and aar over time 
# slopes = data.frame(matrix(vector(), 24, 3,
#                        dimnames=list(c(), c("year", "slopes", "corr"))),
#                 stringsAsFactors=F)
# 
# for (t in 1990:2013){
#   dat <- AAR_unemp_1990_2007 %>%
#     filter(year == t) %>%
#     group_by(fipscounty, AAR_county, avg_unemp_rate) 
#   
#   fit <- lm(AAR_county ~ avg_unemp_rate, dat)
#   sum <- summary(fit)
#   
#   slopes$year[t-1989] <- t
#   slopes$slopes[t-1989] <- sum$coefficients[2,1]
#   slopes$corr[t-1989] <- cor(dat$avg_unemp_rate, dat$AAR_county)
# }
# 
# # Slopes over time
# ggplot(slopes, aes(x=year,y=slopes)) + 
#   geom_point() + geom_vline(xintercept = 2000) + 
#   labs(title = "AAR vs Unemployment Slope over time",
#        x = "Slope") 
# 
# # Scatter plots
# plot <- AAR_unemp_1990_2007 %>%
#   group_by(fipscounty, AAR_county, avg_unemp_rate)
# 
# plot_1995 <- plot %>%
#   filter(year==1995) %>% 
#   ggplot(., aes(x=AAR_county, y=avg_unemp_rate)) +
#   geom_point() + geom_smooth(method=lm) + ylim(0,25) + 
#   labs(title = "1995") 
# plot_1999 <- plot %>%
#   filter(year==1999) %>% 
#   ggplot(., aes(x=AAR_county, y=avg_unemp_rate)) +
#   geom_point() + geom_smooth(method=lm) + ylim(0,25) +
#   labs(title = "1999")  + 
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# plot_2000 <- plot %>%
#   filter(year==2000) %>% 
#   ggplot(., aes(x=AAR_county, y=avg_unemp_rate)) +
#   geom_point() + geom_smooth(method=lm) + ylim(0,25) + 
#   labs(title = "2000")  + 
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# plot_2001 <- plot %>%
#   filter(year==2001) %>% 
#   ggplot(., aes(x=AAR_county, y=avg_unemp_rate)) +
#   geom_point() + geom_smooth(method=lm) + ylim(0,25) + 
#   labs(title = "2001") 
# plot_2007 <- plot %>%
#   filter(year==2007) %>% 
#   ggplot(., aes(x=AAR_county, y=avg_unemp_rate)) +
#   geom_point() + geom_smooth(method=lm) + ylim(0,25) + 
#   labs(title = "2007")  + 
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank())
# 
# grid.arrange(plot_1995, plot_1999, plot_2000, plot_2001, plot_2007, nrow=2)
# 
# 
# # Weighted average unemployment 
# AAR_unemp_1990_2007 %>%
#   filter(aar_category %in% c(0,1,2)) %>%
#   group_by(year, aar_category) %>%
#   summarise(avg_unemp = sum(weighted_unemp, na.rm=T)) %>%
#   ggplot(., aes(x=year, y=avg_unemp, group=aar_category, color=aar_category)) + 
#   geom_line() +
#   ylim(4, 9) + 
#   scale_color_manual(labels = c("Bottom Decile", "Unaffected", "Upper Decile"), 
#                      values = c("blue","red","green"))
# 
# # No weights 
# AAR_unemp_1990_2007 %>%
#   filter(aar_category %in% c(0,1,2)) %>%
#   group_by(year, aar_category, post) %>%
#   summarise(avg_unemp = mean(avg_unemp_rate, na.rm=T)) %>%
#   ggplot(., aes(x=year, y=avg_unemp, group=aar_category, color=aar_category)) + 
#   geom_line() +
#   scale_color_manual(labels = c("Bottom Decile", "Unaffected", "Upper Decile"), 
#                      values = c("blue","red","green"))
# 
# # Quartiles 
# AAR_quartile_reg <- AAR_unemp_1990_2007 %>% 
#   filter(bottom_quartile==1 | upper_quartile==1) %>%
#   mutate(treated = ifelse(upper_quartile==1,1,0)) %>%
#   group_by(year, fipscounty, treated, post) %>%
#   summarise(avg_unemp = mean(avg_unemp_rate, na.rm = T))
# 
# ggplot(AAR_unemp_1990_2007, aes(x=total_pop, y=avg_unemp_rate)) +
#   geom_point()


# END ------------------------------------------------


