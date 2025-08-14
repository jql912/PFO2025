library(tidyverse)
library(PerformanceAnalytics)
library(readxl)
library(lubridate)
library(writexl)


Return <- read_excel("~/Library/CloudStorage/OneDrive-KøbenhavnsUniversitet/Uni/6. År/PFO2025/Opgave 3/Return.xlsx",col_types = c("date", "text", "numeric", "numeric"))

Assets <- read_excel("~/Library/CloudStorage/OneDrive-KøbenhavnsUniversitet/Uni/6. År/PFO2025/Opgave 3/Porteføljer.xlsx",col_types = c("text","text"))

Return = right_join(Return, Assets, by = c("Asset" = "Name")) %>% select(c(Date, Asset, Return)) %>% filter(year(Date) >= 2013 & year(Date) <= 2019)

n_weeks = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(n_weeks = n()) %>%
  select(Asset, Year, n_weeks) %>%
  unique()
  

## CVaR ugentligt (CVaR på ugentlig data og skaler herefter op til et år. Her lader man altså DEN VÆRSTE UGE gå igen hele året!)------

CVaR_m = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = CVaR(Return, p = 0.95, method = "historical"))

CVaR_y = left_join(CVaR_m, n_weeks,by = c("Year", "Asset")) %>%
  group_by(Asset, Year) %>%
  mutate(CVaR = CVaR*sqrt(n_weeks)) %>%
  select(c(Asset, Year, CVaR)) %>%
  unique() %>%
  ungroup() %>%
  group_by(Asset) %>%
  mutate(CVaR = mean(CVaR)) %>%
  select(Asset, CVaR) %>%
  unique()

CVaR_y

## CVaR Årligt ---------

CVaR = Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  mutate(Return_year = prod(Return + 1) - 1) %>%
  ungroup() %>%
  select(Asset, Year, Return_year) %>%
  unique() %>%
  group_by(Asset) %>%
  mutate(CVaR = CVaR(Return_year, p = 0.95, method = "historical"))

CVaR


##  Maximum drawdown ------

mdd_from_returns <- Return %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Asset) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    wealth = cumprod(1 + Return),         # wealth index from returns
    peak   = cummax(wealth),              # running peak of wealth
    dd     = wealth/peak - 1               # drawdown (≤ 0)
  ) %>%
  summarise(
    mdd       = min(dd, na.rm = TRUE),     # max drawdown (negative)
    #mdd_pct   = 100 * mdd,                 # in percent
    #trough_dt = Date[which.min(dd)],       # trough date
    #peak_dt   = Date[which.max(wealth[1:which.min(dd)])],  # peak date before trough
    .groups   = "drop"
  )

mdd_from_returns


## Time under water ------

tuw_from_returns <- Return %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Asset) %>%
  arrange(Date, .by_group = TRUE) %>%
  mutate(
    wealth = cumprod(1 + Return),
    peak = cummax(wealth),
    dd = wealth / peak - 1,
    # Flag where new peak occurs (wealth == peak and wealth > lag(peak))
    new_peak = (wealth == peak) & (wealth > lag(peak, default = 0))
  ) %>%
  # Identify drawdown periods by assigning drawdown IDs
  mutate(
    drawdown_id = cumsum(new_peak)  # increments at every new peak
  ) %>%
  group_by(Asset, drawdown_id) %>%
  summarise(
    start_date = first(Date),       # peak date starting the drawdown
    end_date = last(Date),          # recovery date (back at peak)
    start_wealth = first(peak),
    end_wealth = last(peak),
    min_wealth = min(wealth),
    .groups = "drop"
  ) %>%
  # Filter only actual drawdowns (where min wealth < start wealth)
  filter(min_wealth < start_wealth) %>%
  # Calculate Time Under Water as difference in days
  mutate(
    tuw_days = as.numeric(end_date - start_date)
  ) %>%
  group_by(Asset) %>%
  summarise(
    mean_tuw_days = mean(tuw_days, na.rm = TRUE),
    #median_tuw_days = median(tuw_days, na.rm = TRUE),
    #max_tuw_days = max(tuw_days, na.rm = TRUE),
    #drawdown_periods = n(),
    .groups = "drop"
  )

tuw_from_returns

# SAML ALT

left_join(left_join(CVaR_y, mdd_from_returns, by = "Asset"), tuw_from_returns, by = "Asset")

# Join the data frames
final_df <- CVaR_y %>%
  left_join(mdd_from_returns, by = "Asset") %>%
  left_join(tuw_from_returns, by = "Asset")

# Export to Excel
write_xlsx(final_df, path = "Summary statistics Opgave 3.xlsx")





# COVARIANCE ------

individual_yearly_avg_ret <- Return %>%
  mutate(Year = year(Date)) %>%
  group_by(Asset, Year) %>%
  summarise(Return = prod(1 + Return) - 1, .groups = "drop") 

cov_matrix <- individual_yearly_avg_ret %>%
  pivot_wider(names_from = Asset, values_from = Return) %>%
  select(-Year) %>%
  { setNames(., gsub(" ", "_", names(.))) } %>%
  as.data.frame() %>%
  cov(use = "pairwise.complete.obs")

rownames(cov_matrix) <- colnames(cov_matrix)

cov_df <- as.data.frame(cov_matrix)
cov_df <- cbind(Asset = rownames(cov_df), cov_df)

write_xlsx(cov_df, path = "COV_Opgave_3.xlsx")


individual_yearly_avg_ret %>%
  group_by(Asset) %>%
  mutate(m = mean(Return)) %>%
  select(Asset, m) %>%
  unique()
