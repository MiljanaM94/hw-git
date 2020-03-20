
library(tidyverse)
library(readxl)
source("source/write-functions.R")

read_report <- function(indicator, year, quarter, sheet, range) {
  result <- read_excel(
    path = paste0("data/", report_filename(year, quarter)),
    sheet = sheet,
    range = range,
    col_names = FALSE,
    col_types = "numeric"
  )
  if (length(result) == 20) {
    result[2] <- NULL
  }
  names(result) <- c(
    "base",
    paste0("l_", seq(90, 10, -10)),
    paste0("h_", seq(10, 90, 10))
  )
  result$indicator <- indicator
  result$year <- year
  result$quarter <- quarter
  result$date <- seq.Date(
    as.Date(paste0(year, "-", quarter * 3, "-01")),
    by = "quarter",
    length.out = 8
  )
  result
}

params <- read_csv("data/parameters.csv", col_types = "ciicc")
params %>% filter(!complete.cases(.))
params <- params %>% filter(complete.cases(.))

df <- vector("list", length = nrow(params))
for (i in seq_len(nrow(params))) {
  df[[i]] <- read_report(
    params$indicator[i],
    params$year[i],
    params$quarter[i],
    params$sheet[i],
    params$range[i]
  )
}
df <- bind_rows(df) %>% 
  select(date, everything())

write_rds(df, "results/forecast.RDS")
