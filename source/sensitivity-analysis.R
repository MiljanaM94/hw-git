library(tidyverse)
library(scales)

forecast <- read_rds("results/forecast.RDS")
cpi <- read_rds("results/cpi.RDS")
gdp <- read_rds("results/gdp.RDS")
res <- list()

cpi <- forecast %>%
  filter(indicator == "cpi") %>%
  select(date, year, quarter, base) %>%
  arrange(year, quarter, date) %>%
  group_by(year, quarter) %>%
  mutate(t = 1:8) %>%
  ungroup() %>%
  inner_join(cpi) %>%
  transmute(
    year, quarter, t, date, predicted = base / 100, actual = actual_cpi / 100
  )
gdp <- forecast %>%
  filter(indicator == "gdp") %>%
  select(date, year, quarter, base) %>%
  arrange(year, quarter, date) %>%
  group_by(year, quarter) %>%
  mutate(t = 1:8) %>%
  ungroup() %>%
  inner_join(gdp) %>%
  transmute(
    year, quarter, t, date, predicted = base / 100, actual = actual_gdp / 100
  )
df <- cpi %>% 
  rename("cpi_actual" = actual, "cpi_predicted" = predicted) %>% 
  inner_join(
    gdp %>% 
      rename("gdp_actual" = actual, "gdp_predicted" = predicted) 
  )

cpi_impact <- function(model, intercept, gdp, cpi) {
  result <- df %>% 
    mutate(
      dummy = case_when(
        model == "cc" ~ 0,
        model == "cl" ~ 0,
        model == "sbb" ~ if_else(date <= as.Date("2013-06-01"), 0.85, 0),
        model == "mra" ~ if_else(date <= as.Date("2015-06-01"), 1, 0)
      ),
      z_actual = intercept + gdp * gdp_actual + cpi * cpi_actual + dummy,
      z_predicted = intercept + gdp * gdp_actual + cpi * cpi_predicted + dummy,
      PD_actual_cpi = 1/(1 + exp(-z_actual)),
      PD_forecast_cpi = 1/(1 + exp(-z_predicted))
    ) %>%
    pivot_longer(PD_actual_cpi:PD_forecast_cpi) %>% 
    ggplot(aes(t, value, color = name)) +
    geom_line() +
    facet_grid(year ~ quarter) +
    scale_y_continuous(
      breaks = c(0.01, 0.02, 0.03, 0.04), 
      labels = percent_format(accuracy = 0.1)
    )
  result
}
gdp_impact <- function(model, intercept, gdp, cpi) {
  result <- df %>% 
    mutate(
      dummy = case_when(
        model == "cc" ~ 1,
        model == "cl" ~ 1,
        model == "sbb" ~ if_else(date <= as.Date("2013-06-01"), 0.85, 0),
        model == "mra" ~ if_else(date <= as.Date("2015-06-01"), 1, 0)
      ),
      z_actual = intercept + gdp * gdp_actual + cpi * cpi_actual + dummy,
      z_predicted = intercept + gdp * gdp_predicted + cpi * cpi_actual + dummy,
      PD_actual_cpi = 1/(1 + exp(-z_actual)),
      PD_forecast_cpi = 1/(1 + exp(-z_predicted))
    ) %>%
    pivot_longer(PD_actual_cpi:PD_forecast_cpi) %>% 
    ggplot(aes(t, value, color = name)) +
    geom_line() +
    facet_grid(year ~ quarter) +
    scale_y_continuous(
      breaks = c(0.01, 0.02, 0.03, 0.04), 
      labels = percent_format(accuracy = 0.1)
    )
  result
}

# Credit Cards
res$cc_cpi <- cpi_impact("cc", -4.89, -5.99, 6.46)
res$cc_gdp <- gdp_impact("cc", -4.89, -5.99, 6.46)

# Consumer
res$cl_cpi <- cpi_impact("cl", -4.74, -4.6, 5.65)
res$cl_gdp <- gdp_impact("cl", -4.74, -4.6, 5.65)

# SBB
res$sbb_cpi <- cpi_impact("sbb", -4.17, -7.8, 1.2)
res$sbb_gdp <- gdp_impact("sbb", -4.17, -7.8, 1.2)

# MRA
res$mra_cpi <- cpi_impact("mra", -5.4, -7.4, 7)
res$mra_gdp <- gdp_impact("mra", -5.4, -7.4, 7)

write_rds(res, "results/sensitivity.RDS")
