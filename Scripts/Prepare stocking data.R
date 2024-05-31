# Prepare stocking data

library(tidyverse)
library(readxl)

mydata <- list()

mydata[[1]] <- read_excel("Barwon, Darling, Gwydir, Lachlan, Maqu, Murray, Murrum, Namoi MC & GP stockings 1990 - PRES.xlsx", sheet = 1)
head(mydata[[1]])

for(i in 1:12){
  mydata[[i]] <- read_excel("Barwon, Darling, Gwydir, Lachlan, Maqu, Murray, Murrum, Namoi MC & GP stockings 1990 - PRES.xlsx", sheet = i)
  mydata[[i]] <- mydata[[i]] %>% mutate(across(everything(), as.character)) %>%
    rename("Species" = 1,
           "Size" = 2,
           "Site"= 3,
           "Site_Description" = 4,
           "Day" = 5,
           "Month" = 6,
           "Year" = 7,
           "Number" = 8)
  if(ncol(mydata[[i]]) >8){
  mydata[[i]] <- mydata[[i]] %>% rename("Latitude" = 9,
                                        "Longitude" = 10)}
  
}

full_data <- bind_rows(mydata) %>% select(1:10) %>%
  mutate(across(c("Day", "Month", "Year", "Number"), as.numeric))

write_csv(full_data, "Stocking data combined.csv")

stock_dat <- read_csv("Stocking data combined.csv")

key_rivers <- c("Darling", "Barwon", "Warrego") %>% paste(collapse = "|")

stock_dat <- stock_dat %>% 
  mutate(River = str_extract_all(Site, key_rivers, simplify = T),
         River = case_when(River == "Darling"~ "Darling River",
                           River == "Barwon" ~ "Barwon River",
                           T ~ "Other"),
         Date = lubridate::dmy(paste(Day, Month, Year)))

write_csv(stock_dat, "Stocking data combined.csv")
