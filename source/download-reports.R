library(tidyverse)
source("source/write-functions.R")

year = 2009:2020
quarter = 1:4

df <- crossing(year, quarter) %>% 
  filter(
    (year != 2009 | quarter != 1:3) &
      (year!= 2020 | quarter != 2:4)
  )

for(i in seq_len(nrow(df))) {
  download_report(df$year[i], df$quarter[i])
}
