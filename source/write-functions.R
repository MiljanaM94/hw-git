library(tidyverse)

report_filename <- function(year, quarter) {
  quarter <- case_when(
    quarter == 1 ~ "02",
    quarter == 2 ~ "05",
    quarter == 3 ~ "08",
    quarter == 4 ~ "11"
  )
  extension <- if_else(year <= 2018, ".xls", ".xlsx")
  result <- if_else(
    year == 2009,
    "2009_11_4_projekcija_inflacije.xls",
    paste0(year, "_", quarter, "_5_projekcija_inflacije", extension)
  )
  result
}

report_url <- function(year, quarter) {
  result <- paste0(
    "https://www.nbs.rs/static/nbs_site/gen/latinica/90/ioi/ioi_", year, "/",
    report_filename(year, quarter)
  )
  result
}

download_report <- function(year, quarter) {
  result <- download.file(
    report_url(year, quarter),
    destfile = paste0("data/", year, "_", quarter, ".xls"),
    mode = "wb"
  )
}
