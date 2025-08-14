library(tidyverse)
library(PerformanceAnalytics)
library(readxl)
library(lubridate)
library(writexl)
library(xtable)

Return <- read_excel("~/Library/CloudStorage/OneDrive-KøbenhavnsUniversitet/Uni/6. År/PFO2025/Opgave 6/Weekly_returns_6.5.xlsx", col_types = c("date", "numeric", "numeric", "numeric")) %>%
  mutate(Year = year(Date)) %>%
  pivot_longer(
    cols = starts_with("Week"),
    names_to = "Portfolio",
    values_to = "Return"
  ) 

n_weeks = Return %>%
  group_by(Portfolio, Year) %>%
  mutate(n_weeks = n()) %>%
  select(Portfolio, Year, n_weeks) %>%
  unique()


# 4-weeks return for problem 5 ------
ret_4_w = Return %>%
  group_by(Portfolio) %>%
  mutate(mean_ret = mean(Return)) %>%
  ungroup() %>%
  select(-c(Date, Return, Year)) %>%
  group_by(Portfolio) %>%
  unique() %>%
  mutate(mean_ret = (1+mean_ret)**4 - 1)




# Avg annual return --------

avg_ret = Return %>%
  group_by(Portfolio, Year) %>%
  mutate(mean_ret = mean(Return)) %>%
  ungroup() %>%
  select(-c(Date, Return)) %>%
  group_by(Portfolio) %>%
  unique() %>%
  left_join(n_weeks, by = c("Year", "Portfolio")) %>%
  mutate(mean_ret = (1+mean_ret)**n_weeks - 1) %>%
  group_by(Portfolio) %>%
  mutate(mean_ret = mean(mean_ret)) %>%
  select(-c(n_weeks,Year)) %>%
  unique()


# SD --------

avg_sd = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Portfolio, Year) %>%
  mutate(sd = sqrt(mean((Return - mean(Return))**2)))

avg_sd = left_join(avg_sd, n_weeks,by = c("Year", "Portfolio")) %>%
  group_by(Portfolio, Year) %>%
  mutate(sd_year = sd*sqrt(n_weeks)) %>%
  select(c(Portfolio, Year, sd_year)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Portfolio) %>%
  mutate(sd_avg = mean(sd_year)) %>%
  select(Portfolio, sd_avg) %>%
  unique()

# VaR ------

VaR_u = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Portfolio, Year) %>%
  mutate(VaR = VaR(Return, p = 0.95, method = "historical"))

VaR_y = left_join(VaR_u, n_weeks,by = c("Year", "Portfolio")) %>%
  group_by(Portfolio, Year) %>%
  mutate(VaR = VaR*sqrt(n_weeks)) %>%
  select(c(Portfolio, Year, VaR)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Portfolio) %>%
  mutate(VaR = mean(VaR)) %>%
  select(Portfolio, VaR) %>%
  unique()

VaR_y

# CVaR ugentligt (CVaR på ugentlig data og skaler herefter op til et år. Her lader man altså DEN VÆRSTE UGE gå igen hele året!)------

CVaR_u = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Portfolio, Year) %>%
  mutate(CVaR = CVaR(Return, p = 0.95, method = "historical"))

CVaR_y = left_join(CVaR_u, n_weeks,by = c("Year", "Portfolio")) %>%
  group_by(Portfolio, Year) %>%
  mutate(CVaR = CVaR*sqrt(n_weeks)) %>%
  select(c(Portfolio, Year, CVaR)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Portfolio) %>%
  mutate(CVaR = mean(CVaR)) %>%
  select(Portfolio, CVaR) %>%
  unique()

CVaR_y


# samlet --------
tabel = avg_ret %>%
  left_join(avg_sd, by = "Portfolio") %>%
  mutate(avg_sr = mean_ret / sd_avg) %>%
  left_join(VaR_y, by = "Portfolio") %>%
  left_join(CVaR_y, by = "Portfolio")

latex_table <- xtable(tabel, 
                      caption = "Summary statistics for each portfolio", 
                      label = "tab:portfolio_stats_4.3",
                      digits = c(0, 0, 3, 3, 3, 3, 3))

# Print LaTeX code
print(latex_table, type = "latex", include.rownames = FALSE)

