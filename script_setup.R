# Process QOG data for Ch. 4

library(tidyverse)
library(haven)
library(writexl)

qog_raw <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv")

qog <- qog_raw |> 
  filter(ht_region %in% c(2, 10) | 
           cname %in% c("Canada", "Mexico", "United States of America (the)")) |>
  filter(between(year, 1990, 2020)) |> 
  mutate(region = countrycode::countrycode(ccodealp, "iso3c", "region23")) |> 
  select(cname, ccodealp, year, region, wdi_pop, vdem_polyarchy, vdem_corr)

write_rds(qog, "data/sample_qog_bas_ts_jan23.rds")
write_csv(qog, "data/sample_qog_bas_ts_jan23.csv")
write_dta(qog, "data/sample_qog_bas_ts_jan23.dta")
write_sav(qog, "data/sample_qog_bas_ts_jan23.sav")
write_xlsx(qog, "data/sample_qog_bas_ts_jan23.xlsx")
