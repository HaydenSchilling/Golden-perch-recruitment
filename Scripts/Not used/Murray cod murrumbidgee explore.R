# Murrumbidgee play around
library(tidyverse)
library(lubridate)

catch <- read_csv("Murrumbidgee Boat Catch Data.csv")
bio <- read_csv("Murrumbidgee Boat Bio Data.csv")

species <- "Golden perch"

mc_catch <- catch %>% filter(CommonName == species) %>% filter(Caught >0)
mc_bio <- bio %>% filter(CommonName == species) %>% mutate(Date = dmy(Date),
                                                                Month = month(Date))

bio_size_check <- mc_bio %>% group_by(OperationID) %>% summarise(bio_total = n())

sum(mc_catch$Caught)

mc_combo <- mc_catch %>%left_join(bio_size_check) %>% mutate(diff = Caught-bio_total)

other <- bio_size_check %>%anti_join(mc_combo)

plot(mc_combo$Caught, mc_combo$bio_total)

ggplot(mc_bio, aes(x=Length_mm)) + geom_density() + facet_wrap(~Month)
ggplot(mc_bio, aes(x=Length_mm)) + geom_histogram() + facet_wrap(~Month)

if(species == "Murray cod"){
mc_bio <- mc_bio %>% mutate(Stage = case_when(Length_mm <= 200 ~ "YOY",
                                              Length_mm > 500 ~ "Mature",
                            T ~ "Adult" ))
}

if(species == "Golden perch"){ ### This needs checking
  mc_bio <- mc_bio %>% mutate(Stage = case_when(Length_mm <= 250 ~ "YOY",
                                                Length_mm > 360 ~ "Mature",
                                                T ~ "Adult" ))
}

table(mc_bio$Stage)
catch_yoy <- mc_bio %>% group_by(OperationID) %>% summarise(MC_YOY = sum(Stage == "YOY"),
                                                            MC_Mature = sum(Stage == "Mature"))


### old code
mydata <- catch %>% filter(ElectrofishingDuration >0) %>% distinct(.keep_all = TRUE)

#table(mydata$SWWRPANAME_NEW)
#table(mydata$Method)
#table(mydata$Sampling_duration)

### Go to wide format
mydata_wide <- mydata %>% select(1:12,19,20,25,27,30,35) %>% #drop_na(SWWRPANAME_NEW) %>%
  # this bit needed for some dud database entries.
  group_by(Date, SamplingRecordID, OperationID, SiteName, SampleLatitude, SampleLongitude, WaterbodyName, 
           BasinName, CatchmentName, SubCatchmentName, Altitude, Method, ElectrofishingDuration, SiteID, CommonName) %>%
  summarise(Caught = sum(Caught)) %>% 
  pivot_wider(names_from = CommonName, values_from = Caught,values_fill = 0) %>%
  left_join(catch_yoy) %>%
  mutate(Date =  lubridate::dmy(Date),
         Year = lubridate::year(Date),
         Month = lubridate::month(Date),
         fDate = as.character(Date),
         fYear = as.character(Year),
         fSiteID = as.character(SiteID))

#ggplot(mydata, aes(Date, Caught)) + geom_point()+
#  geom_smooth()
mydata_wide$MC_YOY <- replace_na(mydata_wide$MC_YOY,0)
#  geom_smooth()
mydata_wide$MC_Mature <- replace_na(mydata_wide$MC_Mature,0)

adult_abund <- mydata_wide %>% group_by(Year) %>% summarise(adult_CPUE = sum(MC_Mature)/sum(ElectrofishingDuration)*90) %>%
  mutate(Year = Year-1)

table(mydata_wide$MC_YOY)
table(mydata_wide$fYear)
table(mydata_wide$fSiteID)

dat1 <- mydata_wide %>% filter(Month <6) %>% left_join(adult_abund) %>% filter(SampleLongitude <=148.58) %>%
  filter(SiteName != "Above Burrinjuck Dam") %>% rename(aligned_Year = Year)

water_variables <- read_csv("Water variables for modelling combined Bidgee.csv") %>% select(-Year, -FinYear)
str(water_variables)
psych::cor.plot(water_variables)

final_water <- water_variables %>%
  select(Balranald_mean_temp, Balranald_delta_spring_winter_flow, Balranald_proportional_change, 
         aligned_Year, Balranald_GreatRateFallSpring,
         Wagga_mean_temp, Wagga_delta_spring_winter_flow, Wagga_proportional_change, 
         aligned_Year, Wagga_GreatRateFallSpring)
psych::cor.plot(final_water)

dat1 <- dat1 %>% left_join(final_water)

plot(dat1$aligned_Year, dat1$adult_CPUE)

library(glmmTMB)
m1 <- glmmTMB(MC_YOY ~  Balranald_mean_temp + Balranald_delta_spring_winter_flow+
                Balranald_proportional_change + Balranald_GreatRateFallSpring+
                (1|fDate) + (1|fSiteID) + adult_CPUE,
              offset = log(ElectrofishingDuration),
              data = dat1, family = "nbinom2")

library(DHARMa)
resids <- simulateResiduals(m1)
plot(resids)
testZeroInflation(m1)

performance::r2(m1)

car::Anova(m1)

plot(effects::allEffects(m1))
plot(ggeffects::ggeffect(m1))

# now wagga
m2 <- glmmTMB(MC_YOY ~  Wagga_mean_temp + Wagga_delta_spring_winter_flow+
                Wagga_proportional_change + Wagga_GreatRateFallSpring+
                (1|fDate) + (1|fSiteID) + adult_CPUE,
              offset = log(ElectrofishingDuration),
              data = dat1, family = "nbinom2")

library(DHARMa)
resids <- simulateResiduals(m2)
plot(resids)
testZeroInflation(m2)

performance::r2(m2)

car::Anova(m2)

plot(effects::allEffects(m2))
plot(ggeffects::ggeffect(m2))

hist(dat1$MC_YOY)


#install.packages("mixedup")
library(mixedup)

t1 <- extract_random_effects(m1, re = "fYear") %>% mutate("Year" = as.numeric(as.character(group)))
ggplot(t1, aes(Year, value)) + geom_line() + geom_point() +
  geom_ribbon(aes(ymin=lower_2.5, ymax=upper_97.5), alpha=0.2)


#### play with water data
flow_dat <- read_csv("Flow at Wagga Wagga 410001.csv", skip=9) %>% rename(Flow_cm_s = Value)
water_leve_dat <- read_csv("Water level at Wagga 410001.csv", skip=9) %>% rename(Water_level_m = Value)
wdat <- flow_dat %>% left_join(water_leve_dat) %>% mutate(Year = year(`#Timestamp`),
                                                          Month = month(`#Timestamp`))

plot(wdat$Flow_cm_s, wdat$Water_level_m)

wdat_sum <- wdat %>%filter(Month >7) %>% group_by(Year) %>% summarise(mean_flow = mean(Flow_cm_s, na.rm=T),
                                                                  mean_level = mean(Water_level_m, na.rm=T),
                                                                  Year = Year+1)



full_dat <- dat1 %>% left_join(wdat_sum)

library(glmmTMB)
m1 <- glmmTMB(MC_YOY ~  (1|fYear) + (1|fDate) + (1|fSiteID) + adult_CPUE + mean_level,
              offset = log(ElectrofishingDuration),
              data = full_dat, family = "nbinom1")

library(DHARMa)
resids <- simulateResiduals(m1)
plot(resids)
testZeroInflation(m1)

plot(effects::allEffects(m1))




#install.packages("lfstat")
library(lfstat)

wdat2 <- as.xts(wdat)
x <- baseflow(wdat$Flow_cm_s)
hist(x)
plot(x)
