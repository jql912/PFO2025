Return <- read_excel("~/Library/CloudStorage/OneDrive-KøbenhavnsUniversitet/Uni/6. År/PFO2025/Opgave 3/Return.xlsx",col_types = c("date", "text", "numeric", "numeric"))

Assets <- read_excel("~/Library/CloudStorage/OneDrive-KøbenhavnsUniversitet/Uni/6. År/PFO2025/Opgave 3/Porteføljer.xlsx",col_types = c("text","text"))

Return = right_join(Return, Assets, by = c("Asset" = "Name")) %>% select(c(Date, Asset, Return)) %>% filter(year(Date) >= 2013 & year(Date) <= 2019)

write.csv(Return, "Return.csv", row.names = FALSE)