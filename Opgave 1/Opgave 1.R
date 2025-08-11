library(tidyverse)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Return <- read_excel("~/Library/CloudStorage/OneDrive-KøbenhavnsUniversitet/Uni/6. År/PFO2025/Return.xlsx",col_types = c("date", "text", "numeric", "numeric"))

Return = Return %>% select(c(Date, Asset, Return)) %>% filter(Asset %in% c("Jyske Portefølje Vækst Akk KL","Jyske Portefølje Balanceret Akk KL","Jyske Portefølje Stabil Akk KL","Jyske Portefølje Dæmpet akk KL"))

# 3 år tilbage-------


## Mean return------

### Mean return by compounding indiviual weekly return------
individual_yearly_avg_ret_3 = Return %>%
  filter(year(Date) >= 2022 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(mean_week_ret = prod(1+Return)-1) %>%
  ungroup()


### Mean return by compound the average weekly returns------
week_avg_ret_3 = Return %>%
  filter(year(Date) >= 2022 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(mean_week_ret = mean(Return)) %>%
  ungroup()

n_weeks = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(n_weeks = n()) %>%
  select(Asset, Year, n_weeks) %>%
  unique()

year_avg_ret_3_sus = left_join(week_avg_ret_3, n_weeks, by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(mean_ret_year = (mean_week_ret+1)**n_weeks - 1) %>%
  select(c(Asset, Year, mean_ret_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(avg_ret = mean(mean_ret_year)) %>%
  select(Asset, avg_ret) %>%
  unique()
  
## Std------

week_st_3 = Return %>%
  filter(year(Date) >= 2022 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(sd = sqrt(mean((Return - mean(Return))**2)))

year_svg_sd_3 = left_join(week_st_3, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(sd_year = sd*sqrt(n_weeks)) %>%
  select(c(Asset, Year, sd_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(sd_avg = mean(sd_year)) %>%
  select(Asset, sd_avg) %>%
  unique()


## CVaR (CVaR på ugentlig data og skaler herefter op til et år. Her lader man altså DEN VÆRSTE UGE gå igen hele året!)------

week_cvar_3 = Return %>%
  filter(year(Date) >= 2022 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = CVaR(Return, p = 0.99, method = "historical"))

year_svg_cvar_3 = left_join(week_cvar_3, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = (1+CVaR)**n_weeks - 1) %>%
  select(c(Asset, Year, CVaR)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(CVaR = mean(CVaR)) %>%
  select(Asset, CVaR) %>%
  unique()  

year_svg_cvar_3








# 5 år------

## Mean return------

### Mean return by compounding indiviual weekly return------
individual_yearly_avg_ret_5 = Return %>%
  filter(year(Date) >= 2020 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(mean_week_ret = prod(1+Return)-1) %>%
  ungroup()


### Mean return by compound the average weekly returns------
week_avg_ret_5 = Return %>%
  filter(year(Date) >= 2020 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(mean_week_ret = mean(Return)) %>%
  ungroup()

year_avg_ret_5_sus = left_join(week_avg_ret_5, n_weeks, by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(mean_ret_year = (mean_week_ret+1)**n_weeks - 1) %>%
  select(c(Asset, Year, mean_ret_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(avg_ret = mean(mean_ret_year)) %>%
  select(Asset, avg_ret) %>%
  unique()

## Std------

week_st_5 = Return %>%
  filter(year(Date) >= 2020 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(sd = sqrt(mean((Return - mean(Return))**2)))

year_svg_sd_5 = left_join(week_st_5, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(sd_year = sd*sqrt(n_weeks)) %>%
  select(c(Asset, Year, sd_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(sd_avg = mean(sd_year)) %>%
  select(Asset, sd_avg) %>%
  unique()

year_svg_sd_5

## CVaR (CVaR på ugentlig data og skaler herefter op til et år. Her lader man altså DEN VÆRSTE UGE gå igen hele året!)------

week_cvar_5 = Return %>%
  filter(year(Date) >= 2020 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = CVaR(Return, p = 0.99, method = "historical"))

year_svg_cvar_5 = left_join(week_cvar_5, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = (1+CVaR)**n_weeks - 1) %>%
  select(c(Asset, Year, CVaR)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(CVaR = mean(CVaR)) %>%
  select(Asset, CVaR) %>%
  unique()  

year_svg_cvar_5



# 10 år------

## Mean return------

### Mean return by compounding indiviual weekly return------
individual_yearly_avg_ret_10 = Return %>%
  filter(year(Date) >= 2015 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(mean_week_ret = prod(1+Return)-1) %>%
  ungroup()


### Mean return by compound the average weekly returns------
week_avg_ret_10 = Return %>%
  filter(year(Date) >= 2015 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(mean_week_ret = mean(Return)) %>%
  ungroup()

year_avg_ret_10_sus = left_join(week_avg_ret_10, n_weeks, by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(mean_ret_year = (mean_week_ret+1)**n_weeks - 1) %>%
  select(c(Asset, Year, mean_ret_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(avg_ret = mean(mean_ret_year)) %>%
  select(Asset, avg_ret) %>%
  unique()

## Std------

week_st_10 = Return %>%
  filter(year(Date) >= 2015 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(sd = sqrt(mean((Return - mean(Return))**2)))

year_svg_sd_10 = left_join(week_st_10, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(sd_year = sd*sqrt(n_weeks)) %>%
  select(c(Asset, Year, sd_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(sd_avg = mean(sd_year)) %>%
  select(Asset, sd_avg) %>%
  unique()

year_svg_sd_10

## CVaR (CVaR på ugentlig data og skaler herefter op til et år. Her lader man altså DEN VÆRSTE UGE gå igen hele året!)------

week_cvar_10 = Return %>%
  filter(year(Date) >= 2015 & year(Date) <= 2024) %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = CVaR(Return, p = 0.99, method = "historical"))

year_svg_cvar_10 = left_join(week_cvar_10, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = (1+CVaR)**n_weeks - 1) %>%
  select(c(Asset, Year, CVaR)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(CVaR = mean(CVaR)) %>%
  select(Asset, CVaR) %>%
  unique()  



# SAMLET --------

year_avg_ret_3_sus
year_avg_ret_5_sus
year_avg_ret_10_sus
year_svg_sd_3
year_svg_sd_5
year_svg_sd_10
year_svg_cvar_3
year_svg_cvar_5
year_svg_cvar_10
