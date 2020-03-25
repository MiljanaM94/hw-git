library(tidyverse)

forecast <- read_rds("results/forecast.RDS")
cpi <- read_rds("results/cpi.RDS")
gdp <- read_rds("results/gdp.RDS")

result <- list()

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

# INFLATION

result$cpi_actual <- cpi %>%
  distinct(date, actual) %>%
  ggplot(aes(date, actual)) +
  geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%Y")
result$cpi_prediction <- cpi %>%
  pivot_longer(predicted:actual) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  facet_grid(year ~ quarter)
result$cpi_metrics <- cpi %>%
  group_by(t) %>%
  summarize(
    mean_error = mean(actual - predicted),
    median_error = median(actual - predicted),
    root_mean_square_error = sqrt(mean((actual - predicted) ^ 2)),
    # mean_absolute_error = mean(abs(actual - predicted))
  ) %>%
  pivot_longer(-t) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  geom_point()

# delimo u tri grupe
cpi_group <- cpi %>%
  mutate(
    group = case_when(
      year < 2012 | (year == 2012 & quarter != 4) ~ "I",
      year >= 2012 & year <= 2015 ~ "II",
      TRUE ~ "III"
    )
  )

cpi_group %>% 
  filter(group == "I") %>% 
  pivot_longer(predicted:actual) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  facet_grid(year ~ quarter)
cpi_group %>% 
  filter(group == "II") %>% 
  pivot_longer(predicted:actual) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  facet_grid(year ~ quarter)
cpi_group %>% 
  filter(group == "III") %>% 
  pivot_longer(predicted:actual) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  facet_grid(year ~ quarter)

result$cpi_actualvspredicted <- cpi_group %>%
  ggplot(aes(actual, predicted)) +
  geom_point() +
  facet_grid(group ~ t) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  coord_fixed()
result$cpi_density <- cpi_group %>%
  mutate(diff = actual - predicted) %>%
  ggplot(aes(diff)) +
  geom_density() +
  facet_grid(group ~ t) +
  geom_vline(xintercept = 0, color = "blue")
# Prva projekcija
cpi_group %>% 
  mutate(dif = actual - predicted) %>% 
  arrange(desc(dif)) %>% 
  filter(t == 1) %>% 
  slice(1, nrow(.))

# GDP 

result$gdp_actual <- gdp %>%
  distinct(date, actual) %>%
  ggplot(aes(date, actual)) +
  geom_line() +
  scale_x_date(date_breaks = "year", date_labels = "%Y")
result$gdp_prediction <- gdp %>%
  pivot_longer(predicted:actual) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  facet_grid(year ~ quarter)
result$gdp_metrics <- gdp %>%
  group_by(t) %>%
  summarize(
    mean_error = mean(actual - predicted),
    median_error = median(actual - predicted),
    root_mean_square_error = sqrt(mean((actual - predicted) ^ 2)),
    # mean_absolute_error = mean(abs(actual - predicted))
  ) %>%
  pivot_longer(-t) %>%
  ggplot(aes(t, value, color = name)) +
  geom_line() +
  geom_point()
result$gdp_actualvspredicted <- gdp %>%
  ggplot(aes(actual, predicted)) +
  geom_point() +
  facet_wrap(~ t, nrow = 2) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  coord_fixed(xlim = c(-0.03, 0.03, 0.03))
result$gdp_density <- gdp %>%
  mutate(diff = round(actual - predicted, 2)) %>%
  ggplot(aes(diff)) +
  geom_density() +
  facet_wrap(~ t, nrow = 2) +
  geom_vline(xintercept = 0, color = "blue")
# Prva projekcija
gdp %>% 
  mutate(dif = actual - predicted) %>% 
  arrange(dif) %>% 
  filter(t == 1) %>% 
  slice(1, nrow(.))

write_rds(result, "results/plots.RDS")
