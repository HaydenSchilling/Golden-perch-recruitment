# Climate index calculations

library(tidyverse)
library(lubridate)

soi_dat <- read_csv("Golden-perch-recruitment/Data/SOI Data.csv") %>% mutate(Date = ym(YearMonth),
                                               Year = year(Date),
                                               Month = month(Date)) %>% select(-Date, -YearMonth)

ssdat <- soi_dat %>% filter(Month <6) %>% filter(Month <12) %>%
  group_by(Year) %>% summarise(Mean_SOI = mean(SOI, na.rm=T))


PDO_dat <- read_csv("Golden-perch-recruitment/Data/PDO data.csv") %>% pivot_longer(2:13, names_to = "Month", values_to = "PDO") %>%
  mutate(Month = match(Month,month.abb))

pdodat_summary <- PDO_dat %>% filter(Month <6) %>% filter(Month <12) %>%
  group_by(Year) %>% summarise(Mean_PDO = mean(PDO, na.rm=T))


monsoon_dat <- read_csv("Golden-perch-recruitment/Data/Monsoon Index BOM.csv") %>%
  group_by(Year, Month) %>% summarise(Mean_Monsoon_Index = mean(`Index (standardised)`, na.rm=T),
                                      Mean_Monsoon_Index2 = mean(`Index (non-stand mm_d)`, na.rm=T))

IOD_Dat <- read_csv("Golden-perch-recruitment/Data/IOD Data.csv")%>%
  mutate(Date = dmy(Date),
         Year = year(Date),
         Month = month(Date)) %>% select(-Date)
  

sam_dat <- read_csv("Golden-perch-recruitment/Data/SAM data.csv") %>%
  pivot_longer(cols= c(2:13),names_to = "Month", values_to = "SAM") %>%
  mutate(Month = as.numeric(Month))

rain_dat <- read_csv("Golden-perch-recruitment/Data/Monthly Rain northern MDB.csv") %>% select(-year)


combined_dat <- PDO_dat %>% left_join(IOD_Dat) %>% left_join(monsoon_dat) %>%
  left_join(soi_dat) %>% left_join(sam_dat) %>% left_join(rain_dat)

psych::cor.plot(combined_dat)

cor.test(monsoon_dat$Mean_Monsoon_Index, monsoon_dat$Mean_Monsoon_Index2)
# combocor.plot()# combo_dat <- ssdat %>% left_join(pdodat_summary) %>% left_join(monsoon_dat) %>% filter(Year >= 1981) %>%
#   mutate(SOI_scaled = scale(Mean_SOI)[,1],
#          PDO_scaled = scale(Mean_PDO)[,1],
#          Monsoon_scaled = scale(Mean_Monsoon_Index)[,1]) %>% as.data.frame()
# 
# write_csv(combo_dat, "Climate_Indicies scaled.csv") 
# 
# combo_dat <- combo_dat %>% select(1,5,6,7) %>%
#   pivot_longer(2:4, names_to = "Index", values_to = "value")

ggplot(combo_dat, aes(x=Year, y=value, col=Index)) + geom_line()

# for plotting
rain_dat <- read_csv("Golden-perch-recruitment/Data/Monthly Rain northern MDB.csv") %>% select(-year)
soi_dat <- read_csv("Golden-perch-recruitment/Data/SOI Data.csv") %>% mutate(Date = ym(YearMonth),
                                               Year = year(Date),
                                               Month = month(Date)) %>% select(-Date, -YearMonth)
PDO_dat <- read_csv("Golden-perch-recruitment/Data/PDO data.csv") %>% pivot_longer(2:13, names_to = "Month", values_to = "PDO") %>%
  mutate(Month = match(Month,month.abb))
monsoon_dat <- read_csv("Golden-perch-recruitment/Data/Monsoon Index BOM.csv") %>% group_by(Year, Month) %>% 
  summarise(Mean_Monsoon_Index = mean(`Index (standardised)`, na.rm=T))

IOD_Dat <- read_csv("Golden-perch-recruitment/Data/IOD Data.csv")%>%
  mutate(Date = dmy(Date),
         Year = year(Date),
         Month = month(Date)) %>% select(-Date)


sam_dat <- read_csv("Golden-perch-recruitment/Data/Golden-perch-recruitment/Data/SAM data.csv") %>%
  pivot_longer(cols= c(2:13),names_to = "Month", values_to = "SAM") %>%
  mutate(Month = as.numeric(Month))

plot_dat <- PDO_dat %>% left_join(soi_dat) %>% left_join(rain_dat) %>% left_join(monsoon_dat) %>%
  left_join(IOD_Dat) %>% left_join(sam_dat) %>%
  filter(Year>= 1989) %>% mutate(date = lubridate::ymd(paste0(Year,"-",Month,"-","15"))) %>%
  select(-Year, -Month) %>% pivot_longer(1:6, names_to = "Index", values_to = "value") %>%
  filter(date < as.Date("2023-05-16")) %>%
  mutate(Index = case_when(Index == "Mean_Monsoon_Index" ~ "Mean Monsoonal Index",
                           Index == "Northern_MDB_Rainfall" ~ "Northern MDB Rainfall (mm)",
                           T ~ Index))

ggplot(plot_dat, aes(date, value)) + facet_wrap(~Index, scales = "free_y", ncol=1) + geom_line() +
  theme_bw() + theme(axis.text = element_text(colour="black", size=12),
                     axis.title = element_text(face="bold", size=14),
                     strip.text = element_text(face="bold", size=14)) +
  xlab("Date")+ ylab("Value")
ggsave("Manuscript/supplementary index fig.png", dpi=300, height=25, width=21, units="cm")

