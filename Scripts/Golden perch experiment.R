# Golden perch YOY summary plot

# Get Data

library(tidyverse)
library(lubridate)
library(zoo)

catch <- read_csv("All_catch_11_08_2023_with_fixed_WRPA.csv")

catch <- catch %>% mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct()

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing


# Final data, filter for boat Electro and get EFishing Seconds from silly string
catch <- catch %>% filter(!project_segment %in% bad_list) %>%
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>% 
  filter(Method == "BTE"|Method == "BPE") %>%
  select(-ProjectName,-Abbreviation,-SegmentName,-project_segment) %>% distinct()

waterb2 <- read_csv("../database exploration/Waterbody dictionary.csv") %>% rename(WaterbodyID = WaterBodyID)

catch <- catch %>% left_join(waterb2)

key_rivers <- c("Lachlan River", "Macquarie River", "Gwydir River", "Namoi River", "Darling River",
                "Murrumbidgee River", "Barwon River", "Murray River",
                "Warrego River", "Bogan River", "Boomi River")

catch <- catch %>% filter(WaterbodyName %in% key_rivers)


bio <- read_csv("All bio_11_08_2023.csv")

bio <- bio %>% mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct()

segments <- bio %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing

# Final data, filter for boat Electro and get EFishing Seconds from silly string
bio <- bio %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>% 
  filter(Method == "BTE"|Method == "BPE") %>%
  select(-ProjectName,-Abbreviation,-SegmentName,-project_segment, -HealthDescription, -NumberOfOccurences) %>% distinct()

species <- "Golden perch"

gp_catch <- catch %>% filter(CommonName == species) %>% filter(NumberCaught >0)
gp_bio <- bio %>% filter(CommonName == species) %>% mutate(Date = SampleDate,
                                                           Month = month(Date))

bio_size_check <- gp_bio %>% group_by(OperationID) %>% summarise(bio_total = n())# %>% filter(OperationID == 208737)

sum(gp_catch$NumberCaught)

gp_combo <- gp_catch %>%left_join(bio_size_check) %>% mutate(diff = NumberCaught-bio_total)

# single_catch <- catch %>% filter(SamplingRecordID == 4609) %>% filter(CommonName == "Golden perch")
# single_bio <- bio %>% filter(OperationID == 208737) %>% filter(CommonName == "Golden perch")

### OK appears to be good enough to go on - NO need to fix undersampled bio ones
need_to_expand <- gp_combo %>% filter(diff >0)
extra_dat <- data.frame()

for(i in 1:nrow(need_to_expand)){
  test2 <- gp_bio %>% filter(OperationID == need_to_expand$OperationID[i]) %>% sample_n(size = need_to_expand$diff[i], replace = T)
  extra_dat <- extra_dat %>% bind_rows(test2)
}

gp_bio_expanded <- gp_bio %>% bind_rows(extra_dat) %>% left_join(waterb2) %>% filter(WaterbodyName %in% key_rivers)



ggplot(gp_bio_expanded, aes(x=Length_mm)) + geom_density() + facet_wrap(~Month)
ggplot(gp_bio_expanded, aes(x=Length_mm)) + geom_histogram() + facet_wrap(~Month)
ggplot(gp_bio_expanded, aes(x=Length_mm)) + geom_histogram() + facet_wrap(~WaterbodyName)


gp_bio_expanded <- gp_bio_expanded %>% mutate(Stage = case_when(Length_mm <= 124 ~ "YOY",
                                                                T ~ "Adult" ),
                                              Year = year(SampleDate),
                                              FinYear = case_when(Month > 6 ~ Year+1,
                                                                  T ~ Year)) 


catch_yoy <- gp_bio_expanded %>% group_by(OperationID) %>% summarise(GP_YOY = sum(Stage == "YOY"),
                                                            GP_Mature = sum(Stage == "Adult"))

yoy <- gp_bio_expanded %>% filter(Stage == "YOY")
hist(yoy$Month)

### Summary plot

summary_dat <- gp_bio_expanded %>% group_by(WaterbodyName, FinYear, Stage) %>%
  summarise(n = n()) %>% pivot_wider(names_from = Stage, values_from = n, values_fill = 0) %>%
  mutate(Total = Adult+YOY,
         Percent_YOY = YOY/Total *100)

summary_dat2 <- summary_dat %>% ungroup() %>% group_by(WaterbodyName) %>%
  summarise(Mean_percent_YOY = mean(Percent_YOY),
            sd_percent_YOY = sd(Percent_YOY),
            n=n(),
            se_percent_YOY = sd_percent_YOY/(sqrt(n)))


library(ggbeeswarm)
ggplot(summary_dat2, aes(WaterbodyName, Mean_percent_YOY)) + geom_point()+
  geom_errorbar(aes(ymin=Mean_percent_YOY-se_percent_YOY, ymax = Mean_percent_YOY+se_percent_YOY))+
  geom_quasirandom(data=summary_dat, aes(y=Percent_YOY, col=FinYear), cex=2, alpha=0.5)+
  theme_classic() + theme(axis.text = element_text(colour="black", size=10),
                          axis.title = element_text(face="bold", size=12))+
  xlab("River") + ylab("Mean Percentage Young of the Year")+
  scale_x_discrete(labels = scales::label_wrap(8))+
  scale_colour_viridis_c(option="turbo")
#ggsave("Golden perch YOY exploration.png", dpi=600, width =21, height=14.8, units="cm")


### Plot for Dave





### Explore Warrego Data
wdat <- gp_bio_expanded %>% filter(WaterbodyName == "Warrego River")
hist(wdat$Month)
hist(wdat$Length_mm)


### old code
mydata <- catch %>% 
  mutate(EffortData = str_replace_all(EffortData, pattern = "\"\"", replacement = "\"")) %>%
  mutate(boat = str_extract(EffortData, pattern="\"ElectrofishingDuration\"......"),
         ESecs = parse_number(boat),
         boat = NULL) %>%
  mutate(Sampling_duration = as.numeric(ESecs),
         ESecs = NULL) %>%
  filter(Sampling_duration>2) %>%
  distinct()# removes a event with sample time of zero secs - there are some negatives???



mydata <- mydata  %>% filter(WaterbodyName == "Warrego River" | 
                               WaterbodyName == "Darling River" | 
                               WaterbodyName == "Barwon River"| 
                               WaterbodyName == "Bogan River"|
                               WaterbodyName == "Boomi River") %>% 
  filter(Sampling_duration >2) %>% distinct(.keep_all = TRUE)
## 


#table(mydata$SWWRPANAME_NEW)
#table(mydata$Method)
#table(mydata$Sampling_duration)

### Go to wide format
mydata_wide <- mydata %>% select(1,4:12,19:23) %>% #drop_na(SWWRPANAME_NEW) %>%
  # this bit needed for some dud database entries.
  group_by(SampleDate, SamplingRecordID, OperationID, SiteName, SampleLatitude, SampleLongitude, WaterbodyName, 
          Method, Sampling_duration, SiteID, CommonName) %>%
  summarise(Caught = sum(NumberCaught)) %>% 
  pivot_wider(names_from = CommonName, values_from = Caught,values_fill = 0) %>%
  left_join(catch_yoy) %>%
  mutate(Date =  as.Date(SampleDate),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         fDate = as.character(Date),
         fYear = as.character(Year),
         fSiteID = as.character(SiteID))

mydata_wide$GP_YOY <- replace_na(mydata_wide$GP_YOY,0)


hist(mydata_wide$GP_YOY)


# prepare data for 3 - 6 month prior
mydata_wide <- mydata_wide %>% mutate(pre3 = Date %m-% months(3),
                       pre6 = Date %m-% months(6),
                       NorthernRain = as.numeric(NA),
                       year_aligned = lubridate::year(Date))

### Now need to match to rain
rain_dat <- read_csv("Monthly Rain northern MDB.csv") %>% select(-year) %>%
  mutate(Date = lubridate::my(paste(Month, Year))+14)

mydata_wide$NorthernRain <- as.numeric(NA)

for(i in 1:nrow(mydata_wide)){
  temp_rain <- rain_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
  mydata_wide$NorthernRain[i] <-  sum(temp_rain$Northern_MDB_Rainfall)
}


ggplot(mydata_wide, aes(NorthernRain, GP_YOY)) + geom_point() + geom_smooth()

# ### insert climate indices
# clim_dat <- read_csv("Climate_Indicies scaled.csv") %>% mutate(year_aligned = Year+1) %>%
#   select(-Year)
# 
# mydata_wide <- mydata_wide %>% left_join(clim_dat)

### SOI
soi_dat <- read_csv("SOI Data.csv") %>% mutate(Date = ym(YearMonth) +14,
                                               Year = year(Date),
                                               Month = month(Date))
mydata_wide$Mean_SOI <- as.numeric(NA)

for(i in 1:nrow(mydata_wide)){
  temp_soi <- soi_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
  mydata_wide$Mean_SOI[i] <-  mean(temp_soi$SOI)
}

### PDO
PDO_dat <- read_csv("PDO data.csv") %>% pivot_longer(2:13, names_to = "Month", values_to = "PDO") %>%
  mutate(Month = match(Month,month.abb),
         Date = my(paste(Month, Year))+14)

mydata_wide$Mean_PDO <- as.numeric(NA)

for(i in 1:nrow(mydata_wide)){
  temp_pdo <- PDO_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
  mydata_wide$Mean_PDO[i] <-  mean(temp_pdo$PDO)
}


### Monsoonal Index
monsoon_dat <- read_csv("Monsoon Index BOM.csv") %>%
  mutate(Date = dmy(paste(Day, Month, Year)))
  
mydata_wide$Mean_MonsoonIndex <- as.numeric(NA)

for(i in 1:nrow(mydata_wide)){
  temp_monsoon <- monsoon_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
  mydata_wide$Mean_MonsoonIndex[i] <-  mean(temp_monsoon$`Index (standardised)`, na.rm=T)
}


### Split up Darling fish
mydata_wide <- mydata_wide %>% 
  mutate(WaterbodyName2 = case_when((WaterbodyName == "Darling River" & SampleLatitude > -30.545) ~ "North Darling River",
                                    (WaterbodyName == "Darling River" & SampleLatitude < -32.967) ~ "South Darling River",
                                    T ~ WaterbodyName))


### Flow data for Warrego
flow_dat <- read_csv("Warrego at Barringun 423004.csv") %>% # Fords Bridge 423001 Flow and Temp.csv
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1) %>%
  mutate(LgFresh_Mag = case_when(Flow > 2200 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 5400 ~ 1,
                                  T ~ 0)) %>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 5, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 2, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 5 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 2 ~ 1,
                                 T ~ 0))

# For Barwon
flow_dat2 <- read_csv("Barwon at Dangar Bridge 422001 Flow and Temp.csv") %>%
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(LgFresh_Mag = case_when(Flow > 6500 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 45000 ~ 1,
                                  T ~ 0))%>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 15, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 10, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 15 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 10 ~ 1,
                                       T ~ 0))

# For North Darling
flow_dat3 <- read_csv("Darling at Bourke Town 425003 Flow and Temp.csv") %>%
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(LgFresh_Mag = case_when(Flow > 15000 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 62000 ~ 1,
                                  T ~ 0))%>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 15, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 10, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 15 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 10 ~ 1,
                                       T ~ 0))

# For Darling
flow_dat4 <- read_csv("Darling at Wilcannia Main Channel 425008 Flow Temp.csv") %>%
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(LgFresh_Mag = case_when(Flow > 14000 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 31000 ~ 1,
                                  T ~ 0))%>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 15, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 15, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 15 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 15 ~ 1,
                                       T ~ 0))

# For South Darling
flow_dat5 <- read_csv("Darling at Burtundy 425007.csv") %>% # Pooncarie 425005 Flow and Temp.csv
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(LgFresh_Mag = case_when(Flow > 6000 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 11000 ~ 1,
                                  T ~ 0))%>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 5, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 14, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 5 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 14 ~ 1,
                                       T ~ 0))

# Bogan River
flow_dat6 <- read_csv("Bogan at Gongolgon 421023 flow and temp.csv") %>% # Pooncarie 425005 Flow and Temp.csv
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(LgFresh_Mag = case_when(Flow > 1500 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 4500 ~ 1,
                                  T ~ 0))%>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 5, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 5, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 5 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 5 ~ 1,
                                       T ~ 0))

# Boomi River
flow_dat7 <- read_csv("Boomi 416037 flow and temp.csv") %>% # Pooncarie 425005 Flow and Temp.csv
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(LgFresh_Mag = case_when(Flow > 365 ~ 1,
                                 T ~ 0),
         OverBank_Mag = case_when(Flow > 2000 ~ 1,
                                  T ~ 0))%>%
  mutate(Lg_Duration = rollsumr(LgFresh_Mag, k = 5, fill = NA),
         OverBank_Duration = rollsumr(OverBank_Mag, k = 1, fill = NA)) %>%
  mutate(Lg_Achieved = case_when(Lg_Duration == 5 ~ 1,
                                 T ~ 0),
         Overbank_Achieved = case_when(OverBank_Duration == 1 ~ 1,
                                       T ~ 0))


#### Max flow variability
# Function from Tonkins code - used in later loop
get_range2 <- function(x) {
  
  out <- NA
  
  if (any(!is.na(x))) {
    out <- 0
    if (min(x, na.rm = TRUE) > 0)
      out <- max(x, na.rm = TRUE) / min(x, na.rm = TRUE)
  }
  out
}
rolling_range <- function(data, lag, variable = NULL){ 
  
  if (length(dim(data)) < 2) {
    data <- as.matrix(data, ncol = 1)
    variable <- 1
  }
  
  if (ncol(data) == 1)
    variable <- 1
  
  if (is.null(variable))
    stop("`variable` must be provided if `data` has more than one column", call. = FALSE)
  
  nrows <- nrow(data)
  
  idx <- sapply(rev(seq_len(lag)) - 1, function(x) rep(x, nrows))
  idx <- sweep(idx, 1, seq_len(nrows), "+")
  idx <- ifelse(idx > nrows, NA, idx)
  
  df <- matrix(data[c(idx), variable], nrow = nrows)
  
  diff <- apply(df, 1, get_range2)
  
  max(diff, na.rm = TRUE)
  
}   




### How to calculate term for time since last large flow event
ffdat <- flow_dat %>% filter(Date >= as.Date("1990-01-01"))

ggplot(ffdat, aes(Date, Flow)) + geom_line()+
  scale_y_sqrt()


quantile(ffdat$Flow, c(.1,.95), na.rm=T)

flow_dat <- flow_dat %>% mutate(above_5_perc = case_when(Flow>1111 ~ "Yes",
                                                         T ~ "No"),
                                days_since_5_perc = as.numeric(0),
                                most_recent_5_perc = Date) 

# now Barwon
ffdat2 <- flow_dat2 %>% filter(Date >= as.Date("1990-01-01"))

ggplot(ffdat2, aes(Date, Flow)) + geom_line()+
  scale_y_sqrt()


quantile(ffdat2$Flow, c(.1,.95), na.rm=T)

flow_dat2 <- flow_dat2 %>% mutate(above_5_perc = case_when(Flow>20274 ~ "Yes",
                                                         T ~ "No"),
                                days_since_5_perc = as.numeric(0),
                                most_recent_5_perc = Date) 

# now North Darling
ffdat3 <- flow_dat3 %>% filter(Date >= as.Date("1990-01-01"))

ggplot(ffdat3, aes(Date, Flow)) + geom_line()+
  scale_y_sqrt()


quantile(ffdat3$Flow, c(.1,.95), na.rm=T)

flow_dat3 <- flow_dat3 %>% mutate(above_5_perc = case_when(Flow>36000 ~ "Yes",
                                                           T ~ "No"),
                                  days_since_5_perc = as.numeric(0),
                                  most_recent_5_perc = Date) 

# now Darling
ffdat4 <- flow_dat4 %>% filter(Date >= as.Date("1990-01-01"))

ggplot(ffdat4, aes(Date, Flow)) + geom_line()+
  scale_y_sqrt()


quantile(ffdat4$Flow, c(.1,.95), na.rm=T)

flow_dat4 <- flow_dat4 %>% mutate(above_5_perc = case_when(Flow>28156 ~ "Yes",
                                                           T ~ "No"),
                                  days_since_5_perc = as.numeric(0),
                                  most_recent_5_perc = Date) 

# now South Darling
ffdat5 <- flow_dat5 %>% filter(Date >= as.Date("1990-01-01"))

ggplot(ffdat5, aes(Date, Flow)) + geom_line()+
  scale_y_sqrt()


quantile(ffdat5$Flow, c(.1,.95), na.rm=T)

flow_dat5 <- flow_dat5 %>% mutate(above_5_perc = case_when(Flow>16174 ~ "Yes",
                                                           T ~ "No"),
                                  days_since_5_perc = as.numeric(0),
                                  most_recent_5_perc = Date) 

# now Bogan
ffdat6 <- flow_dat6 %>% filter(Date >= as.Date("1990-01-01"))

quantile(ffdat6$Flow, c(.1,.95), na.rm=T)

flow_dat6 <- flow_dat6 %>% mutate(above_5_perc = case_when(Flow>2965.5 ~ "Yes",
                                                           T ~ "No"),
                                  days_since_5_perc = as.numeric(0),
                                  most_recent_5_perc = Date) 

# now Boomi
ffdat7 <- flow_dat7 %>% filter(Date >= as.Date("1990-01-01"))

quantile(ffdat7$Flow, c(.1,.95), na.rm=T)

flow_dat7 <- flow_dat7 %>% mutate(above_5_perc = case_when(Flow>897.8 ~ "Yes",
                                                           T ~ "No"),
                                  days_since_5_perc = as.numeric(0),
                                  most_recent_5_perc = Date) 

# Warrego
for(i in 2:nrow(flow_dat)){
  if (flow_dat$above_5_perc[i] == "No"){
    flow_dat$most_recent_5_perc[i] <- flow_dat$most_recent_5_perc[i-1]
  }
}

flow_dat <- flow_dat %>% mutate(days_since_5_perc = Date - most_recent_5_perc)

# now Barwon
for(i in 2:nrow(flow_dat2)){
  if (flow_dat2$above_5_perc[i] == "No"){
    flow_dat2$most_recent_5_perc[i] <- flow_dat2$most_recent_5_perc[i-1]
  }
}

flow_dat2 <- flow_dat2 %>% mutate(days_since_5_perc = Date - most_recent_5_perc)

# North Darling
for(i in 2:nrow(flow_dat3)){
  if (flow_dat3$above_5_perc[i] == "No"){
    flow_dat3$most_recent_5_perc[i] <- flow_dat3$most_recent_5_perc[i-1]
  }
}

flow_dat3 <- flow_dat3 %>% mutate(days_since_5_perc = Date - most_recent_5_perc)

# Darling
for(i in 2:nrow(flow_dat4)){
  if (flow_dat4$above_5_perc[i] == "No"){
    flow_dat4$most_recent_5_perc[i] <- flow_dat4$most_recent_5_perc[i-1]
  }
}

flow_dat4 <- flow_dat4 %>% mutate(days_since_5_perc = Date - most_recent_5_perc)

# South Darling
for(i in 2:nrow(flow_dat5)){
  if (flow_dat5$above_5_perc[i] == "No"){
    flow_dat5$most_recent_5_perc[i] <- flow_dat5$most_recent_5_perc[i-1]
  }
}

flow_dat5 <- flow_dat5 %>% mutate(days_since_5_perc = Date - most_recent_5_perc)

# Bogan
for(i in 2:nrow(flow_dat6)){
  if (flow_dat6$above_5_perc[i] == "No"){
    flow_dat6$most_recent_5_perc[i] <- flow_dat6$most_recent_5_perc[i-1]
  }
}

flow_dat6 <- flow_dat6 %>% mutate(days_since_5_perc = Date - most_recent_5_perc)

# Boomi
for(i in 2:nrow(flow_dat7)){
  if (flow_dat7$above_5_perc[i] == "No"){
    flow_dat7$most_recent_5_perc[i] <- flow_dat7$most_recent_5_perc[i-1]
  }
}

flow_dat7 <- flow_dat7 %>% mutate(days_since_5_perc = Date - most_recent_5_perc)



for(i in 1:nrow(mydata_wide)){
  if(mydata_wide$WaterbodyName2[i] == "Warrego River"){
  temp_flow <- flow_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
  mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
  mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
  mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
 # temp_flow <- flow_dat %>% filter(Date == mydata_wide$pre6[i])
  mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
  mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
  mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  
  }
  else if(mydata_wide$WaterbodyName2[i] == "Barwon River"){
    temp_flow <- flow_dat2 %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
    mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
    #temp_flow <- flow_dat2 %>% filter(Date == mydata_wide$pre6[i])
    mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
    mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
    mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "North Darling River"){
    temp_flow <- flow_dat3 %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
    mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
    #temp_flow <- flow_dat3 %>% filter(Date == mydata_wide$pre6[i])
    mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
    mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
    mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "Darling River"){
    temp_flow <- flow_dat4 %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
    mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
    #temp_flow <- flow_dat4 %>% filter(Date == mydata_wide$pre6[i])
    mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
    mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
    mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "South Darling River"){
    temp_flow <- flow_dat5 %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
    mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
   # temp_flow <- flow_dat5 %>% filter(Date == mydata_wide$pre6[i])
    mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
    mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
    mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "Bogan River"){
    temp_flow <- flow_dat6 %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
    mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
    #temp_flow <- flow_dat6 %>% filter(Date == mydata_wide$pre6[i])
    mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
    mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
    mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "Boomi River"){
    temp_flow <- flow_dat7 %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i])# %>%
    mydata_wide$Mean_Flow[i] <-  mean(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_Flow[i] <-  max(temp_flow$Flow, na.rm=T)
    mydata_wide$Max_variability[i] <- rolling_range(temp_flow$Flow, lag=3)
    #temp_flow <- flow_dat7 %>% filter(Date == mydata_wide$pre6[i])
    mydata_wide$Days_since_large_flow[i] <- mydata_wide$Date[i] - temp_flow$most_recent_5_perc[1]
    mydata_wide$Lg_Fresh[i] <- max(temp_flow$Lg_Achieved, na.rm=T)
    mydata_wide$Over_bank[i] <- max(temp_flow$Overbank_Achieved, na.rm=T)
  }
}






### Add stocking data
stock_dat <- read_csv("Stocking data combined.csv") %>% mutate(Date = dmy(Date))

mydata_wide <- mydata_wide %>% mutate(SampleDate = as.Date(SampleDate))

#barwon <- mydata_wide %>% filter(WaterbodyName == "Barwon River") %>% filter(Date > as.Date("2015-01-01"))

for(i in 1:nrow(mydata_wide)){
  if(mydata_wide$WaterbodyName2[i] == "Warrego River"){
  mydata_wide$stocking[i] <- 0
  }
  else if(mydata_wide$WaterbodyName[i] == "Darling River"){  
    temp_stock <- stock_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i]) %>%
      filter(River == "Darling River")
  mydata_wide$stocking[i] <-  sum(temp_stock$Number, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "Barwon River"){  
    temp_stock <- stock_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i]) %>%
      filter(River == "Barwon River")
    mydata_wide$stocking[i] <-  sum(temp_stock$Number, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "Bogan River"){  
    temp_stock <- stock_dat %>% filter(Date >= mydata_wide$pre6[i], Date <= mydata_wide$pre3[i]) %>%
      filter(River == "Bogan River")
    mydata_wide$stocking[i] <-  sum(temp_stock$Number, na.rm=T)
  }
  else if(mydata_wide$WaterbodyName2[i] == "Boomi River"){  
    mydata_wide$stocking[i] <- 0
  }
}
 
summary(mydata_wide$stocking)



 
write_csv(mydata_wide, "GP Modelling Data prepared_full.csv")

cor_plot_data <- mydata_wide %>%  ungroup() %>%
  select(Mean_SOI, Mean_PDO, Mean_MonsoonIndex, Mean_Flow, Days_since_large_flow, NorthernRain, Max_variability)
  
library(psych)
psych::cor.plot(cor_plot_data)

ggplot(mydata_wide, aes(Mean_Flow, Days_since_large_flow)) + geom_point() + geom_smooth() 


# cor.test(mydata_wide$Mean_PDO, mydata_wide$Mean_SOI)
# cor.test(mydata_wide$Mean_PDO, mydata_wide$Mean_Monsoon_Index)
# cor.test(mydata_wide$Mean_SOI, mydata_wide$Mean_Monsoon_Index)
# cor.test(mydata_wide$Mean_SOI, mydata_wide$NorthernRain)
# cor.test(mydata_wide$NorthernRain, mydata_wide$Mean_Monsoon_Index)
# cor.test(mydata_wide$Mean_Flow, mydata_wide$Max_Flow) # highly correlated, don't need both
# cor.test(mydata_wide$Mean_Flow, mydata_wide$NorthernRain) # borderline high correlation
# cor.test(mydata_wide$Mean_Flow, mydata_wide$Mean_SOI) # borderline high correlation (only warrego)
# cor.test(mydata_wide$Mean_Flow, mydata_wide$Mean_Monsoon_Index) 
# cor.test(mydata_wide$Mean_Flow, mydata_wide$Days_since_large_flow) 
# cor.test(mydata_wide2$Mean_SOI, mydata_wide2$stocking) 


warrego_only <- mydata_wide %>% filter(WaterbodyName == "Warrego River")


library(glmmTMB)
mydata_wide2 <- mydata_wide %>% group_by(WaterbodyName2) %>% 
  mutate(Mean_Flow_scaled = scale(Mean_Flow)) %>% ungroup() %>%
  filter(Month <= 13) %>% mutate(Mean_Flow_scaled = as.vector(as.numeric(Mean_Flow_scaled)))

cor_plot_data <- mydata_wide2 %>%  ungroup() %>%
  select(Mean_SOI, Mean_PDO, Mean_MonsoonIndex, Mean_Flow_scaled, NorthernRain, stocking) #Days_since_large_flow

psych::cor.plot(cor_plot_data)

#### test unscaling
scale_sum <- mydata_wide2 %>% group_by(WaterbodyName2) %>%
  summarise(mean_flow_calibrate= mean(Mean_Flow, na.rm=T),
            sd_flow_calibrate = sd(Mean_Flow, na.rm=T),
            min_flow = min(Mean_Flow, na.rm=T),
            max_flow = max(Mean_Flow, na.rm=T))

write_csv(scale_sum, "flow scaling factors.csv")


write_csv(mydata_wide2, "GP YOY modelling data.csv")
mydata_wide2 <- read_csv("GP YOY modelling data.csv")

# mydata_wide2 <- mydata_wide2 %>% select(stocking, GP_YOY,Mean_Flow_scaled,
#                                         Days_since_large_flow,WaterbodyName,WaterbodyName2, NorthernRain,
#                                         SampleDate, SiteID, Sampling_duration, Mean_SOI, Mean_PDO,
#                                         Mean_MonsoonIndex, Lg_Fresh, Over_bank) %>% # , Max_variability
#   drop_na()


ggplot(mydata_wide2, aes(Days_since_large_flow, y=Mean_Flow_scaled)) + geom_point() +
  facet_wrap(~WaterbodyName2)
ggsave("Time and Flow poor data.png", dpi=600)

plot(mydata_wide2$Mean_SOI, mydata_wide2$Mean_MonsoonIndex)


### Climate models separate
library(tidyverse)
mydata_wide2 <- read_csv("GP YOY modelling data.csv")

mydata_wide3 <- mydata_wide2 %>% select(stocking, GP_YOY, #Mean_Flow_scaled,
                                        WaterbodyName,WaterbodyName2, NorthernRain, #
                                        SampleDate, SiteID, Sampling_duration, Mean_SOI, Mean_PDO,
                                        Mean_MonsoonIndex, Lg_Fresh, Over_bank) %>% # , Max_variability
  drop_na()

mydata_wide3$fSiteID <- as.factor(as.character(mydata_wide3$SiteID))

library(glmmTMB)

c1 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_SOI) + scale(Mean_MonsoonIndex) + 
                         scale(NorthernRain)+ scale(Mean_PDO) + # Full model
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate) + (1|WaterbodyName2), 
                       offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

summary(mydata_wide3)

 summary(c1)
 car::Anova(c1)
 performance::r2(c1, tolerance = 1e-08)
 AIC(c1)

c2 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # drop SOI
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate)+ (1|WaterbodyName2), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())
 summary(c2)
 car::Anova(c2)
# performance::r2(c2)
 #MuMIn::AICc(c2,c3)
 AIC(c2)
# plot(effects::allEffects(c2))
#plot(ggeffects::ggpredict(c2))

c3 <- glmmTMB::glmmTMB(GP_YOY ~  scale(Mean_SOI) +  scale(NorthernRain)+ scale(Mean_PDO) + # drop Monsoon
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         #Mean_Flow_scaled:WaterbodyName+
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate)+ (1|WaterbodyName2), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())
# summary(c3)
# car::Anova(c3)
# performance::r2(c3)
 AIC(c3) # polynomial for flow is a worse fit by 3 AIC :: poly(Mean_Flow_scaled, degree=2)
# 

c4 <- glmmTMB::glmmTMB(GP_YOY ~  scale(Mean_SOI) + scale(Mean_MonsoonIndex) +  scale(Mean_PDO) + # drop rainfall
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate)+ (1|WaterbodyName2), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())
# summary(c4)
# car::Anova(c4)
# performance::r2(c4)
# AIC(c4)


c5 <- glmmTMB::glmmTMB(GP_YOY ~  scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ # drop PDO
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate)+ (1|WaterbodyName2), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())
# summary(c5)
# car::Anova(c5)
# performance::r2(c5)
# AIC(c5)

c6 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # drop stocking
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         #scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate)+ (1|WaterbodyName2), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


MuMIn::AICc(c1,c2,c3,c4,c5,c6) # c2 is the best



c7 <- glmmTMB::glmmTMB(GP_YOY ~ scale(NorthernRain)+ scale(Mean_PDO) + # c2 + drop monsoon
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


c8 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_MonsoonIndex) +  scale(Mean_PDO) + # c2 + drop rain
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


c9 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_MonsoonIndex) + scale(NorthernRain)+   # c2 + drop pdo
                        # poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


c10 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # c2 + drop stocking
                         #poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         #scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

MuMIn::AICc(c2, c7, c8,c9,c10) # c9 is the best

c11 <- glmmTMB::glmmTMB(GP_YOY ~ scale(NorthernRain)+  + # c9 + drop Monsoon
                         # poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

c12 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_MonsoonIndex) +# scale(NorthernRain)+  + # c9 + drop rain
                         # poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

c13 <- glmmTMB::glmmTMB(GP_YOY ~ scale(Mean_MonsoonIndex) + scale(NorthernRain)+  + # c9 + drop stocking
                         # poly(Mean_Flow_scaled,2)*WaterbodyName2+
                        # scale(stocking)+#Max_variability+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


MuMIn::AICc(c9, c11,c12, c13) # c9 is the best



library(DHARMa)
resids <- simulateResiduals(c9) # residuals look OK
plot(resids)
testDispersion(c9)
testOutliers(c9, type="bootstrap")

summary(c9)
car::Anova(c9)
performance::r2(c9, tolerance = 1e-08)

tid_sum <- broom.mixed::tidy(c9)
tid_sum
write_csv(tid_sum, "GP Climate model output.csv")

plot(effects::allEffects(c9))
#str(c1)

#### Prediction plots


full_pred <- list()
# Monsoon index

new_data <- expand.grid(Mean_MonsoonIndex = seq(min(mydata_wide3$Mean_MonsoonIndex), max(mydata_wide3$Mean_MonsoonIndex), by=0.01),
                        NorthernRain = median(mydata_wide3$NorthernRain),
                        Mean_PDO = median(mydata_wide3$Mean_PDO),
                        stocking = 0,
                        SiteID = NA,
                        WaterbodyName2 = NA,
                        SampleDate = NA,
                        Sampling_duration = 90)


pred_dat <- predict(c9, newdata = new_data, se=T, type="response")


new_data$PredictedFit <- pred_dat$fit
new_data$PredictedSE <- pred_dat$se.fit


ggplot(new_data, aes(Mean_MonsoonIndex, PredictedFit)) + geom_line()+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5)

full_pred[[1]] <- new_data %>% select(Mean_MonsoonIndex, PredictedFit, PredictedSE) %>% mutate(Variable = "Mean Monsoon Index") %>%
  rename(xAxis = Mean_MonsoonIndex)

head(full_pred[[1]])

# Rain

new_data <- expand.grid(Mean_MonsoonIndex = median(mydata_wide3$Mean_MonsoonIndex),
                        NorthernRain = seq(min(mydata_wide3$NorthernRain), max(mydata_wide3$NorthernRain), by=1),
                        Mean_PDO = median(mydata_wide3$Mean_PDO),
                        stocking = 0,
                        SiteID = NA,
                        WaterbodyName2 = NA,
                        SampleDate = NA,
                        Sampling_duration = 90)


pred_dat <- predict(c2, newdata = new_data, se=T, type="response")


new_data$PredictedFit <- pred_dat$fit
new_data$PredictedSE <- pred_dat$se.fit


ggplot(new_data, aes(NorthernRain, PredictedFit)) + geom_line()+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5)


full_pred[[2]] <- new_data %>% select(NorthernRain, PredictedFit, PredictedSE) %>% mutate(Variable = "Northern Rain (mm)") %>%
  rename(xAxis = NorthernRain)


# # PDO
# 
# new_data <- expand.grid(Mean_MonsoonIndex = median(mydata_wide3$Mean_MonsoonIndex),
#                         NorthernRain = median(mydata_wide3$NorthernRain),
#                         Mean_PDO =  seq(min(mydata_wide3$Mean_PDO), max(mydata_wide3$Mean_PDO), by=0.01),
#                         stocking = 0,
#                         SiteID = NA,
#                         WaterbodyName2 = NA,
#                         SampleDate = NA,
#                         Sampling_duration = 90)
# 
# 
# pred_dat <- predict(c2, newdata = new_data, se=T, type="response")
# 
# 
# new_data$PredictedFit <- pred_dat$fit
# new_data$PredictedSE <- pred_dat$se.fit
# 
# 
# ggplot(new_data, aes(Mean_PDO, PredictedFit)) + geom_line()+
#   geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5)
# 
# 
# full_pred[[3]] <- new_data %>% select(Mean_PDO, PredictedFit, PredictedSE) %>% mutate(Variable = "Mean PDO") %>%
#   rename(xAxis = Mean_PDO)


# stocking

new_data <- expand.grid(Mean_MonsoonIndex = median(mydata_wide3$Mean_MonsoonIndex),
                        NorthernRain = median(mydata_wide3$NorthernRain),
                        Mean_PDO =  median(mydata_wide3$Mean_PDO),
                        stocking = seq(min(mydata_wide3$stocking), max(mydata_wide3$stocking),by=100),
                        SiteID = NA,
                        WaterbodyName2 = NA,
                        SampleDate = NA,
                        Sampling_duration = 90)


pred_dat <- predict(c9, newdata = new_data, se=T, type="response")


new_data$PredictedFit <- pred_dat$fit
new_data$PredictedSE <- pred_dat$se.fit


ggplot(new_data, aes(stocking, PredictedFit)) + geom_line()+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5)# +
  #scale_y_log10()


# full_pred[[4]] <- new_data %>% select(stocking, PredictedFit, PredictedSE) %>% mutate(Variable = "Total Stocking") %>%
#   rename(xAxis = stocking)

xxdat <- new_data %>% select(stocking, PredictedFit, PredictedSE) %>% mutate(Model = "Climate Model")

stocking_plot_dat <- list()
stocking_plot_dat[[1]] <- xxdat

### combine for plotting

full_pred <- bind_rows(full_pred)

head(full_pred)


ggplot(full_pred, aes(xAxis, y = PredictedFit)) + facet_wrap(~Variable, scales="free_x", strip.position = "bottom")+
  geom_line()+ xlab(NULL)+ ylab("Predicted GP YOY ± SE\n(90s e-Fishing)")+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5) +
  theme_classic()+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size=14),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))# +
  #scale_y_log10()
ggsave("GP Climate model output.png", dpi =600, width = 21, height=8, units="cm")
ggsave("GP Climate model output.pdf", dpi =600, width = 21, height=8, units="cm")




#### Now Do flow modelling
library(tidyverse)
mydata_wide2 <- read_csv("GP YOY modelling data.csv")

# mydata_wide2 <- mydata_wide2 %>% select(stocking, GP_YOY,Mean_Flow_scaled,
#                                         Days_since_large_flow,WaterbodyName,WaterbodyName2, NorthernRain,
#                                         SampleDate, SiteID, Sampling_duration, Mean_SOI, Mean_PDO,
#                                         Mean_MonsoonIndex, Lg_Fresh, Over_bank) %>% # , Max_variability
#   drop_na()

mydata_wide3 <- mydata_wide2 %>% select(stocking, GP_YOY, Mean_Flow_scaled,
                                        WaterbodyName,WaterbodyName2, #NorthernRain, #
                                        SampleDate, SiteID, Sampling_duration, #Mean_SOI, Mean_PDO,
                                        Lg_Fresh, Over_bank) %>% # , Max_variability, Mean_MonsoonIndex
  drop_na()

library(glmmTMB)




f1 <- glmmTMB::glmmTMB(GP_YOY ~ poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         #scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # Full model
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


f2 <- glmmTMB::glmmTMB(GP_YOY ~ Mean_Flow_scaled*WaterbodyName2+
                         #scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # linear flow
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


f3 <- glmmTMB::glmmTMB(GP_YOY ~ poly(Mean_Flow_scaled,2)+WaterbodyName2+
                         #scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # no interaction
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

f4 <- glmmTMB::glmmTMB(GP_YOY ~ Mean_Flow_scaled+WaterbodyName2+
                         #scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # linear no interaction
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

f5 <- glmmTMB::glmmTMB(GP_YOY ~ poly(Mean_Flow_scaled,2)*WaterbodyName2+
                         #scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # remove stocking
                         #scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())

f6 <- glmmTMB::glmmTMB(GP_YOY ~ poly(Mean_Flow_scaled,2)+ #*WaterbodyName2+
                         #scale(Mean_SOI) + scale(Mean_MonsoonIndex) + scale(NorthernRain)+ scale(Mean_PDO) + # remove waterbody
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom2())


AIC(f1,f2,f3,f4,f5,f6) # full model clearly the best



library(DHARMa)
resids <- simulateResiduals(f1)
plot(resids)

summary(f1)

x1 <- as.data.frame(car::Anova(f1))
x1$Parameter <- row.names(x1)
write_csv(x1, "Flow model results.csv")

x1

f1_sum <- broom.mixed::tidy(car::Anova(f1))
f1_sum

performance::r2(f1)



### Can i make nicer prediction plots
Darling <- mydata_wide2 %>% filter(WaterbodyName2 == "Darling River")
Darling2 <- mydata_wide2 %>% filter(WaterbodyName2 == "South Darling River")
Darling3 <- mydata_wide2 %>% filter(WaterbodyName2 == "North Darling River")
Barwon <- mydata_wide2 %>% filter(WaterbodyName2 == "Barwon River")
Warrego <- mydata_wide2 %>% filter(WaterbodyName2 == "Warrego River")
Bogan <- mydata_wide2 %>% filter(WaterbodyName2 == "Bogan River")
Boomi <- mydata_wide2 %>% filter(WaterbodyName2 == "Boomi River")

predict_dat <- list(Darling,Darling2,Darling3, Barwon, Warrego, Bogan, Boomi)
predict_dat_final <- list()

for(j in 1:length(predict_dat)){
# range(predict_dat[[j]]$Days_since_large_flow)
# day_seq <- c(180, 365, 730, 1095, 1460, 1800)
#   #seq(min(predict_dat[[1]]$Days_since_large_flow, na.rm=T), max(predict_dat[[j]]$Days_since_large_flow, na.rm=T), length.out=5)
plot_dat <- list()

for(i in 1:1){
tmp <- data.frame(Mean_Flow_scaled =seq(min(predict_dat[[j]]$Mean_Flow_scaled, na.rm=T),max(predict_dat[[j]]$Mean_Flow_scaled, na.rm=T),by=0.1),
                  
                       #Days_since_large_flow =day_seq[i],
                       #Mean_PDO = mean(mydata_wide2$Mean_PDO, na.rm=T),
                      #Mean_MonsoonIndex = mean(mydata_wide2$NorthernRain, na.rm=T),
                  #NorthernRain = mean(mydata_wide2$NorthernRain, na.rm=T),
                        stocking = 0,
                       #Mean_Monsoon_Index= median(predict_dat[[j]]$Mean_Monsoon_Index),
                       SamplingRecordID = NA,
                       Sampling_duration = 90,
                        SiteID = NA,
                        SampleDate=NA,
                  WaterbodyName2 = predict_dat[[j]]$WaterbodyName2[1])
plot_dat[[i]] <- tmp
}
plot_dat <- bind_rows(plot_dat)
predict_dat_final[[j]] <- plot_dat
}

# 
# day_bounds <- mydata_wide2 %>% group_by(WaterbodyName2) %>%
#   summarise(min_days = min(Days_since_large_flow),
#             max_days = max(Days_since_large_flow))
# day_bounds

predict_dat_final <- bind_rows(predict_dat_final) 

Predicted_GP <- predict(f1, newdata = predict_dat_final, re.form = NULL, se.fit = T, type="link")

predict_dat_final$Predicted_GP <- Predicted_GP$fit
predict_dat_final$SE_GP <- Predicted_GP$se.fit
# predict_dat_final <- predict_dat_final %>% left_join(day_bounds) %>%
#   mutate(Predicted_GP = case_when((max_days < Days_since_large_flow) ~ NA_real_,
#          T ~ Predicted_GP)) %>%
#   mutate(SE_GP = case_when((max_days < Days_since_large_flow) ~ NA_real_,
#          T ~ SE_GP))


scalings <- read_csv("flow scaling factors.csv")
predict_dat_final <- predict_dat_final %>% left_join(scalings)
predict_dat_final$Mean_Flow_unscaled <- predict_dat_final$sd_flow_calibrate*predict_dat_final$Mean_Flow_scaled+predict_dat_final$mean_flow_calibrate

predict_dat_final <- predict_dat_final %>% mutate(River_labels = case_when(WaterbodyName2 == "Boomi River" ~ "a) Boomi River",
                                                                           WaterbodyName2 == "Bogan River" ~ "c) Bogan River",
                                                                           WaterbodyName2 == "Warrego River" ~ "b) Warrego River",
                                                                           WaterbodyName2 == "Darling River" ~ "f) Central Darling-Baaka",
                                                                           WaterbodyName2 == "North Darling River" ~ "e) Northern Darling-Baaka",
                                                                           WaterbodyName2 == "South Darling River" ~ "g) Southern Darling-Baaka",
                                                                           WaterbodyName2 == "Barwon River" ~ "d) Barwon River",))

mydata_wide2 <- mydata_wide2 %>% mutate(River_labels = case_when(WaterbodyName2 == "Boomi River" ~ "a) Boomi River",
                                                                           WaterbodyName2 == "Bogan River" ~ "c) Bogan River",
                                                                           WaterbodyName2 == "Warrego River" ~ "b) Warrego River",
                                                                           WaterbodyName2 == "Darling River" ~ "f) Central Darling-Baaka",
                                                                           WaterbodyName2 == "North Darling River" ~ "e) Northern Darling-Baaka",
                                                                           WaterbodyName2 == "South Darling River" ~ "g) Southern Darling-Baaka",
                                                                           WaterbodyName2 == "Barwon River" ~ "d) Barwon River",))



ggplot(predict_dat_final, aes(Mean_Flow_unscaled, exp(Predicted_GP))) + geom_line() +
  geom_ribbon(aes(ymin=exp(Predicted_GP-SE_GP), ymax = exp(Predicted_GP+SE_GP)), alpha=0.3) +
  geom_rug(data=mydata_wide2, aes(x=Mean_Flow), inherit.aes = F)+
  facet_wrap(~River_labels, scales = "free") +
  #scale_y_log10()+
  theme_classic()+
  xlab(bquote(bold("Mean daily river discharge 3-6 month prior to sampling (ML" ~Day^'-1' *")")))+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size=12),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))+
  ylab("Predicted GP YOY ± SE (90s e-Fishing)") 
  #ggtitle("Days since last large flow (top 5%)")

ggsave("GP YOY River Flow plots.png", dpi =600, width = 21, height=14.8, units="cm")
ggsave("GP YOY River Flow plots.pdf", dpi =600, width = 21, height=14.8, units="cm")



# Stocking plot
table(mydata_wide3$WaterbodyName2)
new_data <- expand.grid(Mean_Flow_scaled = median(mydata_wide3$Mean_Flow_scaled),
                        stocking = seq(min(mydata_wide3$stocking), max(mydata_wide3$stocking),by=100),
                        WaterbodyName2 = "Darling River", # most common
                        SiteID = NA,
                        SampleDate = NA,
                        Sampling_duration = 90)


pred_dat <- predict(f1, newdata = new_data, se=T, type="response")


new_data$PredictedFit <- pred_dat$fit
new_data$PredictedSE <- pred_dat$se.fit

xxdat <- new_data %>% select(stocking, PredictedFit, PredictedSE) %>% mutate(Model = "Flow Model")

stocking_plot_dat[[2]] <- xxdat


ggplot(new_data, aes(stocking, PredictedFit)) + geom_line()+# facet_wrap(~WaterbodyName2)+
  #scale_y_log10()+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5) +
  ylab("Predicted GP YOY ± SE\n(90s e-Fishing)")+ theme_classic()+
  xlab("Total Stocking")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size=14),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))
#scale_y_log10()
ggsave("GP YOY River Flow stocking plot.png", dpi =600, width = 10, height=8, units="cm")
ggsave("GP YOY River Flow stocking plot.pdf", dpi =600, width = 10, height=8, units="cm")






#### Try to test EWR relationships

library(tidyverse)
library(glmmTMB)
mydata_wide2 <- read_csv("GP YOY modelling data.csv")

mydata_wide3 <- mydata_wide2 %>% dplyr::select(stocking, GP_YOY,Mean_Flow_scaled,WaterbodyName,
                                        Days_since_large_flow,WaterbodyName2, NorthernRain,
                                        SampleDate, SiteID, Sampling_duration, Mean_SOI, Mean_PDO, # , Max_variability
                                        Mean_MonsoonIndex, Lg_Fresh, Over_bank) %>%
  mutate(Lg_Fresh = as.factor(as.character(Lg_Fresh)),
         Over_bank = as.factor(as.character(Over_bank)),
         EWR = as.factor(case_when((Lg_Fresh == "1" & Over_bank == "0")~ "Lg_Fresh",
                         (Over_bank == "1")~ "Over_bank",
                         T ~ "None"))) %>%
    
  drop_na()

table(mydata_wide3$EWR, mydata_wide3$WaterbodyName2)

# mydata_wide3 <- mydata_wide3 %>% filter(WaterbodyName2 != "Warrego River") %>%
#   filter(WaterbodyName2 != "South Darling River") %>%
#   filter(EWR != "Over_bank")


e1 <- glmmTMB::glmmTMB(GP_YOY ~ EWR*WaterbodyName2 + # full model
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom1())


e2 <- glmmTMB::glmmTMB(GP_YOY ~ EWR+WaterbodyName2 + # drop interaction
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom1())


e3 <- glmmTMB::glmmTMB(GP_YOY ~ EWR + # drop waterbody
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom1())


e4 <- glmmTMB::glmmTMB(GP_YOY ~ WaterbodyName2 + # drop EWR
                         scale(stocking)+
                         #(1|SamplingRecordID) , offset = log(Sampling_duration),
                         (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                       data = as.data.frame(mydata_wide3), family = nbinom1())

e5 <- glmmTMB::glmmTMB(GP_YOY ~ EWR*WaterbodyName2 + # drop stocking
                            #scale(stocking)+
                            #(1|SamplingRecordID) , offset = log(Sampling_duration),
                            (1|SiteID) + (1|SampleDate), offset = log(Sampling_duration),
                          data = as.data.frame(mydata_wide3), family = nbinom1())


AIC(e1,e2,e3,e4,e5) # e1 is clear winner

summary(e1)
car::Anova(e1)
eee <- as.data.frame(car::Anova(e1))
eee$Parameter <- row.names(eee)
write_csv(eee, "EWR model results.csv")

performance::r2_nakagawa(e1, tolerance = 1e-10)
performance::r2(e1)

eee


library(performance)

library(DHARMa)
resids <- simulateResiduals(e1)
plot(resids)
testOutliers(e1, type="bootstrap")
testDispersion(e1)

plot(effects::allEffects(e1))

plot(ggeffects::ggpredict(e1, terms=c("EWR", "WaterbodyName2")))


### EWR Plot only
new_data <- expand.grid(EWR =unique(mydata_wide3$EWR),
                        stocking = 0,#seq(min(mydata_wide3$stocking), max(mydata_wide3$stocking),by=100),
                        WaterbodyName2 = unique(mydata_wide3$WaterbodyName2), #"Darling River", # most common
                        SiteID = NA,
                        SampleDate = NA,
                        Sampling_duration = 90)


pred_dat <- predict(e1, newdata = new_data, se=T, type="response")


new_data$PredictedFit <- pred_dat$fit
new_data$PredictedSE <- pred_dat$se.fit
new_data$lower_CI <- pred_dat$fit-pred_dat$se.fit
new_data$upper_CI <- pred_dat$fit+pred_dat$se.fit
new_data <- new_data %>% mutate(lower_CI = case_when(lower_CI < 0 ~ 0,
                                                     T ~ lower_CI),
                                EWR2 = case_when(EWR == "None" ~ "None",
                                                 EWR == "Lg_Fresh" ~ "Large\nFresh",
                                                 EWR == "Over_bank" ~ "Overbank"))


xxdat <- new_data %>% select(EWR, PredictedFit, PredictedSE) %>% mutate(Model = "EWR Model")

stocking_plot_dat[[3]] <- xxdat

dud1 <- new_data %>% filter(WaterbodyName2 == "Boomi River" & EWR == "Lg_Fresh")
dud2 <- new_data %>% filter(WaterbodyName2 == "South Darling River" & EWR == "Lg_Fresh")
dud3 <- new_data %>% filter(WaterbodyName2 == "Warrego River" & EWR == "Lg_Fresh")

new_data2 <- new_data %>% anti_join(dud1) %>% anti_join(dud2) %>% anti_join(dud3)

new_data2 <- new_data2 %>% mutate(River_labels = case_when(WaterbodyName2 == "Boomi River" ~ "a) Boomi River",
                                                                           WaterbodyName2 == "Bogan River" ~ "c) Bogan River",
                                                                           WaterbodyName2 == "Warrego River" ~ "b) Warrego River",
                                                                           WaterbodyName2 == "Darling River" ~ "f) Central Darling-Baaka",
                                                                           WaterbodyName2 == "North Darling River" ~ "e) Northern Darling-Baaka",
                                                                           WaterbodyName2 == "South Darling River" ~ "g) Southern Darling-Baaka",
                                                                           WaterbodyName2 == "Barwon River" ~ "d) Barwon River",))

ggplot(new_data2, aes(EWR2, PredictedFit)) + geom_point()+ facet_wrap(~River_labels, scales = "free_y")+
  #scale_y_log10()+
  theme_classic()+
  geom_errorbar(aes(ymin=lower_CI, ymax=upper_CI), alpha=0.5) +
  scale_x_discrete(breaks=c("None", "Large\nFresh", "Overbank"), limits = c("None", "Large\nFresh", "Overbank"))+
  ylab("Predicted GP YOY ± SE\n(90s e-Fishing)")+
  xlab("EWR")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size=12),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

ggsave("EWR Achievement with duration.png", width = 21, height=14.8, units="cm", dpi=300)
ggsave("EWR Achievement with duration.pdf", width = 21, height=14.8, units="cm", dpi=300)


table(mydata_wide3$EWR, mydata_wide3$WaterbodyName2)

### Stocking plot
#table(mydata_wide3$WaterbodyName2)
new_data <- expand.grid(EWR = "None",
                        stocking = seq(min(mydata_wide3$stocking), max(mydata_wide3$stocking),by=100),
                        WaterbodyName2 = "Darling River", # most common
                        SiteID = NA,
                        SampleDate = NA,
                        Sampling_duration = 90)


pred_dat <- predict(e1, newdata = new_data, se=T, type="response")


new_data$PredictedFit <- pred_dat$fit
new_data$PredictedSE <- pred_dat$se.fit

xxdat <- new_data %>% select(stocking, PredictedFit, PredictedSE) %>% mutate(Model = "EWR Model")

stocking_plot_dat[[3]] <- xxdat



ggplot(new_data, aes(stocking, PredictedFit)) + geom_line()+# facet_wrap(~WaterbodyName2)+
  #scale_y_log10()+
  theme_classic()+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5) +
  ylab("Predicted GP YOY ± SE\n(90s e-Fishing)")+
  xlab("Total Stocking")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size=14),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))
#scale_y_log10()
ggsave("GP YOY EWR stocking plot.png", dpi =600, width = 10, height=8, units="cm")
ggsave("GP YOY EWR stocking plot.pdf", dpi =600, width = 10, height=8, units="cm")

#### Stocking plot combined
stocking_plot_dat_full <- bind_rows(stocking_plot_dat) %>%
  mutate(Model = case_when(Model == "Climate Model" ~ "a) Climate Model",
                           Model == "Flow Model" ~ "b) Flow Model",
                           Model == "EWR Model" ~ "c) EWR Model"))



ggplot(stocking_plot_dat_full, aes(stocking/1000, PredictedFit)) + geom_line()+# facet_wrap(~WaterbodyName2)+
  #scale_y_log10()+
  theme_classic()+ facet_wrap(~Model, scales = "free_y")+ 
  coord_cartesian(ylim = c(0, NA))+ scale_y_continuous(expand = c(0,0))+
  geom_ribbon(aes(ymin=PredictedFit-PredictedSE, ymax=PredictedFit+PredictedSE), alpha=0.5) +
  ylab("Predicted GP YOY ± SE\n(90s e-Fishing)")+
  xlab("Total Stocking ('000)")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        strip.placement = "outside",
        strip.text = element_text(face="bold", size=14),
        strip.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA))

ggsave("GP YOY stocking plot combined.png", dpi =600, width = 21, height=8, units="cm")
ggsave("GP YOY stocking plot combined.pdf", dpi =600, width = 21, height= 8, units="cm")



