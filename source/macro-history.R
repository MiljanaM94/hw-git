library(tidyverse)
library(readxl)

cpi <- read_excel("data/SBRS07.xls", sheet = 1, range = "C66:C182", col_names = FALSE) %>%
  setNames("actual_cpi")
gdp <- read_excel("data/SBRS01.xls", sheet = 1, range = "D45:D83", col_names = FALSE) %>% 
  setNames("actual_gdp")
cpi <- cpi %>% 
  filter(row_number() %% 3 == 0) %>% 
  mutate(date = seq(as.Date("2010-06-01"), as.Date("2019-12-01"), by = "quarter")) %>% 
  select(date, actual_cpi)
gdp <- gdp %>% 
  mutate(date = seq(as.Date("2010-06-01"), as.Date("2019-12-01"), by = "quarter")) %>% 
  select(date, actual_gdp)

write_rds(cpi, "results/cpi.RDS")
write_rds(gdp, "results/gdp.RDS")
