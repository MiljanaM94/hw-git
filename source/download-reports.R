library(tidyverse)

year = c(2009:2020)
quarter = c(1:4)

df <- crossing(year, quarter) %>% 
  filter(
    (year != 2009 | quarter != c(1,2,3)) &
      (year!= 2020 | quarter != c(2,3,4))
  )
for(i in seq_len(nrow(df))) {
  download_report(df$year[i], df$quarter[i])
}
