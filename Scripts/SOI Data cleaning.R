## SOI test
library(tidyverse)
library(lubridate)

soi_dat <- read_csv("SOI Data.csv") %>% mutate(Date = ym(YearMonth),
                                               Year = year(Date),
                                               Month = month(Date))
