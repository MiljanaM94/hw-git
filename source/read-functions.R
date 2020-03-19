
read_cpi <- function(month, year, sheet, range) { 
  month <- stringr::str_pad(month, 2, pad = "0")
  if (!(month %in% c("02", "05", "08", "11"))) {
    stop("Incorrect MONTH input. Supported values are 2, 5, 8 and 11")
  }
  sheet <- "Inflacija rezime"
  path <- paste0(
    "https://www.nbs.rs/static/nbs_site/gen/latinica/90/ioi/ioi_",
    year, "/", year, "_", month, "_5_projekcija_inflacije.xls"
  )
  file = paste0("data/", year, "_", month, ".xls")
  httr::GET(path, httr::write_disk(file, overwrite = TRUE))
  newnames = c(
    "datum", paste0("low_", seq(90, 10, -10)), paste0("high_", seq(10, 90, 10))
  )
  cpi <- readxl::read_excel(file, sheet = sheet, range = range, col_names = TRUE)
  cpi <- dplyr::rename_at(cpi, dplyr::vars(dplyr::contains("...")), ~newnames)
  cpi <- dplyr::mutate(
    cpi, datum = seq(
      as.Date(paste0(year, "-", month, "-01")) - lubridate::period("2y 2m"),
      by = "quarter", length.out = 17
    )
  )
}

read_gdp <- function(month, year, sheet, range) { 
  month <- stringr::str_pad(month, 2, pad = "0")
  if (!(month %in% c("02", "05", "08", "11"))) {
    stop("Incorrect MONTH input. Supported values are 2, 5, 8 and 11")
  }
  sheet <- "BDP rezime"
  path <- paste0(
    "https://www.nbs.rs/static/nbs_site/gen/latinica/90/ioi/ioi_",
    year, "/", year, "_", month, "_5_projekcija_inflacije.xls"
  )
  file = paste0("data/", year, "_", month, ".xls")
  httr::GET(path, httr::write_disk(file, overwrite = TRUE))
  newnames = c(
    "datum", paste0("low_", seq(90, 10, -10)), paste0("high_", seq(10, 90, 10))
  )
  cpi <- readxl::read_excel(file, sheet = sheet, range = range, col_names = TRUE)
  cpi <- dplyr::rename_at(cpi, dplyr::vars(dplyr::contains("...")), ~newnames)
  cpi <- dplyr::mutate(
    cpi, datum = seq(
      as.Date(paste0(year, "-", month, "-01")) - lubridate::period("2y 2m"),
      by = "quarter", length.out = 17
    )
  )
}

df <- read_cpi(8, 2018, , "F9:Z26")
df1 <- read_gdp(8, 2018, , "E9:Y26")
