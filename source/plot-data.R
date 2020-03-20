library(tidyverse)

forecast <- read_rds("results/forecast.RDS")
cpi <- read_rds("results/cpi.RDS")
gdp <- read_rds("results/gdp.RDS")

forecast %>%
  filter(indicator == "cpi") %>%
  mutate(
    baseline_cpi = base,
    report = paste0("report_", year, "_", quarter)
  ) %>% 
  select(date, report, baseline_cpi) %>% 
  group_by(report) %>% 
  inner_join(cpi) %>% 
  pivot_longer(baseline_cpi:actual_cpi) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~report, scales = "free")

forecast %>%
  filter(indicator == "gdp") %>%
  mutate(
    baseline_gdp = base,
    report = paste0("report_", year, "_", quarter)
  ) %>% 
  select(date, report, baseline_gdp) %>% 
  group_by(report) %>% 
  inner_join(gdp) %>% 
  pivot_longer(baseline_gdp:actual_gdp) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~report, scales = "free")
