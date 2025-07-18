# Figure 1 density plot

# Golden perch YOY summary plot

library(tidyverse)
library(lubridate)

catch <- read_csv("Golden-perch-recruitment/Data/All_catch_11_08_2023_with_fixed_WRPA.csv")

catch <- catch %>% mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct()

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

#test <- catch %>% filter(SamplingRecordID == 4076)

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing


# Final data, filter for boat Electro and get EFishing Seconds from silly string
catch <- catch %>% filter(!project_segment %in% bad_list) %>% filter(!project_segment %in% bad2$project_segment) %>%
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
bad3 <- segments %>% filter(grepl("*Selective*",project_segment))

# Final data, filter for boat Electro and get EFishing Seconds from silly string
bio <- bio %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) %>% 
  filter(Method == "BTE" | Method == "BPE") %>%
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


gp_bio_expanded <- gp_bio_expanded %>% mutate(Stage = case_when(Length_mm <= 150 ~ "YOY",
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


### Explore Warrego Data
wdat <- gp_bio_expanded %>% filter(WaterbodyName == "Boomi River")
hist(wdat$Month)
hist(wdat$Length_mm)

river_sum1 <- gp_bio_expanded %>% filter(Length_mm <= 124) %>% group_by(WaterbodyName) %>% summarise(YOY = n())
river_sum1

river_sum2 <- gp_bio_expanded %>%  group_by(WaterbodyName) %>% summarise(total = n())
river_sum2

river_sum <- river_sum1 %>% left_join(river_sum2) %>% mutate(Perc_YOY = YOY/total*100)
river_sum 

# 
# river_sum1 <- gp_bio_expanded %>% filter(Length_mm <= 124) %>% 
#   group_by(WaterbodyName, FinYear) %>% summarise(YOY = n()) 
# river_sum1
# 
# river_sum2 <- gp_bio_expanded %>%  group_by(WaterbodyName, FinYear) %>% summarise(total = n(), mean_size = mean(Length_mm,na.rm=T))
# river_sum2
# 
# river_sum <- river_sum2 %>% left_join(river_sum1) %>% mutate(Perc_YOY = YOY/total*100) %>%
#   replace_na(list(n =0, Perc_YOY = 0))
# river_sum 
# 
# river_sum_sum <- river_sum %>% ungroup() %>% mutate(Over_10 = case_when(Perc_YOY>5 ~ 1,
#                                                                         T ~0),
#                                                     size_124 = case_when(mean_size<=200 ~ 1,
#                                                                          T ~0)) %>%
#   group_by(WaterbodyName) %>% summarise(Year_over = sum(Over_10),
#                                         years_small_size = sum(size_124))
# river_sum_sum
# 
# yoy_tots <- river_sum %>% ungroup() %>% group_by(WaterbodyName) %>% summarise(YOY_tot = sum(YOY, na.rm=T))
# yoy_tots
# 
# ### old code
# mydata <- catch %>% 
#   mutate(EffortData = str_replace_all(EffortData, pattern = "\"\"", replacement = "\"")) %>%
#   mutate(boat = str_extract(EffortData, pattern="\"ElectrofishingDuration\"......"),
#          ESecs = parse_number(boat),
#          boat = NULL) %>%
#   mutate(Sampling_duration = as.numeric(ESecs),
#          ESecs = NULL) %>%
#   filter(Sampling_duration>0) %>%
#   distinct()# removes a event with sample time of zero secs - there are some negatives???
# 
# 
# 
# mydata <- mydata  %>% filter(WaterbodyName == "Warrego River" | 
#                                WaterbodyName == "Darling River" | 
#                                WaterbodyName == "Barwon River") %>% 
#   filter(Sampling_duration >0) %>% distinct(.keep_all = TRUE)
# ## 
# 
# 
# #table(mydata$SWWRPANAME_NEW)
# #table(mydata$Method)
# #table(mydata$Sampling_duration)
# 
# ### Go to wide format
# mydata_wide <- mydata %>% select(1,4:11,13,14,21,22) %>% #drop_na(SWWRPANAME_NEW) %>%
#   # this bit needed for some dud database entries.
#   group_by(SampleDate, SamplingRecordID, OperationID, SiteName, SampleLatitude, SampleLongitude, WaterbodyName, 
#            Method, Sampling_duration, SiteID, CommonName) %>%
#   summarise(Caught = sum(NumberCaught)) %>% 
#   pivot_wider(names_from = CommonName, values_from = Caught,values_fill = 0) %>%
#   left_join(catch_yoy) %>%
#   mutate(Date =  as.Date(SampleDate),
#          Year = lubridate::year(Date),
#          Month = lubridate::month(Date),
#          fDate = as.character(Date),
#          fYear = as.character(Year),
#          fSiteID = as.character(SiteID))
# 
# mydata_wide$GP_YOY <- replace_na(mydata_wide$GP_YOY,0)
# 
# 
hist(gp_bio_expanded2$Length_mm)

ggplot(gp_bio_expanded2, aes(x=Length_mm)) + geom_histogram(binwidth=10)+
  scale_x_continuous(breaks=seq(0,600,30)) + theme_bw() +
  geom_vline(aes(xintercept = 124), col= "red", lty="dashed")+
  geom_vline(aes(xintercept = 62), col= "blue", lty="dashed") +
  xlab("Total Length (mm)") +
  theme(axis.text = element_text(size=11, colour="black"),
        axis.title = element_text(size=12, face="bold"))
ggsave("Golden-perch-recruitment/Figures/All samples histogram.png", dpi=300, width=21, height=14.8, units = "cm")

mean_lengths <- gp_bio_expanded %>% group_by(FinYear, WaterbodyName) %>%
  summarise(Mean_Length = mean(Length_mm)) %>% filter(FinYear != 2024)

gp_bio_expanded2 <- gp_bio_expanded %>% left_join(mean_lengths)
wdat2 <- gp_bio_expanded2 %>% filter(WaterbodyName == "Boomi River")

library(ggridges)
ggplot(gp_bio_expanded2, aes(x=Length_mm,y=FinYear, group=FinYear, fill = Mean_Length)) + 
  geom_density_ridges(rel_min_height=0.05) + facet_wrap(~WaterbodyName, ncol=4) + xlim(c(0,650))+
  scale_fill_viridis_c(option = "turbo", name="Mean\nLength (mm)") + theme_bw() +
  ylab("Sampling Year Ending June 30th") + xlab("Length (mm)") +
  scale_y_reverse()+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        legend.title = element_text(face="bold", size=12),
        legend.position = "bottom",
        legend.key.width = unit(2, 'cm'),
        strip.background = element_rect(fill=NA),
        strip.text = element_text(face="bold", size=10))

#ggsave("GP Juvenile Densities.png", dpi=600, width = 21, height=27, units="cm")
#ggsave("GP Juvenile Densities.pdf", dpi=600, width = 21, height=27, units="cm")

#### Summarise sampling efforts

sampling <- gp_bio_expanded %>% ungroup () %>% group_by(WaterbodyName, FinYear) %>%
  summarise(`Sampling Events` = n_distinct(SamplingRecordID)) %>% filter(FinYear != 2024) %>% 
  pivot_wider(names_from = FinYear, values_from = `Sampling Events`, values_fill = 0) %>%
  ungroup() %>% rowwise(WaterbodyName) %>%
  mutate(Total = sum(c_across(`1998`:`2023`)))
sampling <- sampling %>% left_join(river_sum1)


write_csv(sampling, "Electrofishing Events by River.csv")



p1 <- ggplot(sampling, aes(x=WaterbodyName, y = Total)) + geom_col() + 
  theme_bw() + ylab("Total Number of \nElectrofishing Events")+ xlab(NULL)+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        axis.text.x = element_text(colour="black", size=12, angle=45, hjust=1))

p2 <- ggplot(sampling, aes(x=WaterbodyName, y = YOY)) + geom_col() + 
  theme_bw() + ylab("Total Number of \nObserved YOY")+ xlab(NULL)+
  scale_x_discrete() +
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        axis.text.x = element_text(colour="black", size=12, angle=45, hjust=1))

p3 <- ggplot(sampling, aes(x=WaterbodyName, y = YOY/Total)) + geom_col() + 
  theme_bw() + ylab("YOY per\nElectrofishing Event")+ xlab("River")+
  scale_x_discrete() +
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        axis.text.x = element_text(colour="black", size=12, angle=45, hjust=1))

library(patchwork)
p1+p2+p3 + plot_layout(ncol=1)
#ggsave("Juvenile exploration.png", dpi = 300, width=21, height=28, units="cm")

mean_lengths2 <- gp_bio_expanded %>% group_by(FinYear) %>%
  summarise(Mean_Length = mean(Length_mm)) %>% filter(FinYear != 2024)
gp_bio_expanded3 <- gp_bio_expanded %>% left_join(mean_lengths2) 

ggplot(gp_bio_expanded3, aes(x=Length_mm,y=FinYear, group=FinYear)) + #, fill = Mean_Length
  geom_density_ridges(rel_min_height=0.05,
                      quantile_lines=TRUE,
                      quantile_fun=function(x,...)median(x)) +# facet_wrap(~WaterbodyName) + xlim(c(0,650))+
  #scale_fill_viridis_c(option = "magma", name="Mean\nLength (mm)") + 
  theme_bw() + ylab("Sampling Year Ending June 30th") + xlab("Length (mm)") +
  scale_y_reverse(breaks = c(1995,2000,2005,2010,2015,2020))+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=12),
        legend.position = "bottom",
        strip.background = element_rect(fill=NA),
        strip.text = element_text(face="bold", size=10))

#ggsave("Length density plot grey.png", dpi=600, units="cm",
#       width=21, height=21)
