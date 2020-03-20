library(tidyverse)
library(readxl)
source("source/write-functions.R")

read_report <- function(indicator, year, quarter) {
  params <- read_csv("data/parameters.csv", col_names = TRUE) 
  params <- params %>% 
    filter(
      params$indicator == indicator &
        params$year == year &
        params$quarter == quarter
    )
  result <- read_excel(
    path = paste0("data/", report_filename(year, quarter)),
    sheet = params$sheet,
    range = params$range,
    col_names = FALSE
  )
  result
}
