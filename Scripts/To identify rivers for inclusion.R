# Script to identify rivers for inclusion
# In addition to density plots

# Golden perch YOY summary plot

# Get Data

library(tidyverse)
library(lubridate)
library(zoo)

catch <- read_csv("Golden-perch-recruitment/Data/All_catch_11_08_2023_with_fixed_WRPA.csv")

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
                "Murrumbidgee River", "Barwon River", "Murray River", "Warrego River", "Boomi River",
                "Severn River", "Dumaresq River", "Macintyre River", "Bogan River", "Castlereagh River", "Mole River")


catch <- catch %>% filter(WaterbodyName %in% key_rivers)


bio <- read_csv("Golden-perch-recruitment/Data/All bio_11_08_2023.csv")

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



mydata <- mydata  %>% 
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

###
total_effort <- mydata_wide %>% group_by(WaterbodyName) %>%
  summarise(Total_sampling = sum(Sampling_duration, na.rm=T))
total_YOY <- mydata_wide %>% group_by(WaterbodyName) %>%
  summarise(YOY = sum(GP_YOY, na.rm=T))
ttt <- total_effort %>% left_join(total_YOY) %>%
  mutate(YOY_90sec = YOY/Total_sampling*90)