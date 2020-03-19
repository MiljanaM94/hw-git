library(tidyverse)
library(utils)

download_report <- function(year, quarter) {
  result <- download.file(
    report_url(year, quarter),
    destfile = paste0("data/", year, "_", quarter, ".xls"),
    mode = "wb"
  )
}
