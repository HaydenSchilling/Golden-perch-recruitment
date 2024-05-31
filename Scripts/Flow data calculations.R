# Murrumbidgee water data play 

library(tidyverse)
library(lubridate)


wdat <- read_csv("Warrego at Fords Bridge 423001 Flow and Temp.csv") %>%
#wdat <- read_csv("DPE Murrumbidgee Balranald flow temp.csv") %>%
#wdat <- read_csv("DPE Murrumbidgee Wagga flow  temp - 410001.csv") %>%
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1) %>%
  mutate(Month = month(Date),
         Year = year(Date),
         FinYear = case_when(Month>5 ~ Year+1,
                             T ~ Year)) %>%
  rename(Mean_Temp = Temp,
         Mean_Flow = Flow)

plot_dat <- wdat %>% filter(Year >= 1990)


## Warrego2

wdat2 <- read_csv("Warrego at Barringun 423004.csv") %>%
  #wdat <- read_csv("DPE Murrumbidgee Balranald flow temp.csv") %>%
  #wdat <- read_csv("DPE Murrumbidgee Wagga flow  temp - 410001.csv") %>%
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1) %>%
  mutate(Month = month(Date),
         Year = year(Date),
         FinYear = case_when(Month>5 ~ Year+1,
                             T ~ Year)) %>%
  rename(Mean_TempB = Temp,
         Mean_FlowB = Flow)

plot_dat2 <- wdat2 %>% filter(Year >= 1990)

cdat <- plot_dat %>% left_join(plot_dat2)

ggplot(cdat, aes(Date, Mean_Flow), col="red") + geom_line()+
  geom_line(aes(y=Mean_FlowB), col="green")



# fill missing values (only temp for Wagga, and temp and flow for Balranald)
library(zoo)

plot_dat$Temp_filled <- plot_dat$Mean_Temp
plot_dat$Temp_filled <- na.approx(plot_dat$Temp_filled, na.rm=F)

ggplot(plot_dat, aes(Date, Temp_filled)) + geom_line()+
  geom_line(aes(y=Mean_Temp), col = "red")+
  facet_wrap(~Year, scales="free_x")

summary(plot_dat)

plot_dat$Flow_filled <- plot_dat$Mean_Flow
plot_dat$Flow_filled <- na.approx(plot_dat$Flow_filled, na.rm=F)

ggplot(plot_dat, aes(Date, Flow_filled)) + geom_line()+
  geom_line(aes(y=Mean_Flow), col = "red")+
  facet_wrap(~Year, scales="free_x") +
  scale_y_log10()


# Temp at time of spawning (Oct - Jan)

spawning_months <- c(10,11,12,1)

spawn_temp <- plot_dat %>% filter(Month %in% spawning_months) %>%
  filter(Year >= 1990) %>%
  group_by(FinYear) %>%
  summarise(mean_temp = mean(Temp_filled))


# Change in flow from winter to spring

winter_months <- c(6,7,8)
spring_months <- c(9,10,11)


winter_flow <- plot_dat %>% filter(Month %in% winter_months) %>%
  filter(Year >= 1990) %>%
  group_by(Year) %>%
  summarise(mean_winter_flow = mean(Flow_filled))

spring_flow <- plot_dat %>% filter(Month %in% spring_months) %>%
  filter(Year >= 1990) %>%
  group_by(Year) %>%
  summarise(mean_spring_flow = mean(Flow_filled))

winter_spring_flow <- winter_flow %>% left_join(spring_flow) %>%
  mutate(delta_spring_winter_flow = mean_spring_flow - mean_winter_flow)

# Mean flow Jan - September
annual_flow <- plot_dat %>% filter(Month <10) %>%
  group_by(Year) %>% summarise(Jan_Sept_mean_flow = mean(Flow_filled))


#### Short term variability in flow (Oct - Dec)
variable_months <- c(10,11,12)

# Function from Tonkins code
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

max_variability <- plot_dat %>% filter(Month %in% variable_months) %>%
  group_by(Year) %>% summarise(proportional_change = rolling_range(Flow_filled, lag = 3))


### min winter flow

min_winter_flow <- plot_dat %>% filter(Month %in% winter_months) %>%
  filter(Year >= 1990) %>%
  group_by(Year) %>%
  summarise(min_winter_flow = min(Flow_filled))



# Get the DPE water variables

# Number of total days 1,400 - 5,000 ML/day and 5,000-16,000
num_days <- plot_dat %>% filter(Month %in% spawning_months) %>%
  mutate(day_1400_5000 = case_when((Flow_filled >= 1400 & Flow_filled < 5000) ~ 1,
                                   T ~ 0),
         day_5000_16000 = case_when((Flow_filled >= 5000 & Flow_filled < 16000) ~ 1,
                                    T ~ 0)) %>%
  group_by(FinYear) %>% 
  summarise(days_1400_5000 = sum(day_1400_5000),
            days_5000_16000 = sum(day_5000_16000))

#### What was 5th percentile flow and 90th during spawning period
percentile_flow <- plot_dat %>% filter(Month %in% spawning_months) %>%
  group_by(FinYear) %>% summarise(percentile_5 = quantile(Flow_filled, 0.05),
                                  percentile_90 = quantile(Flow_filled, 0.9))




### How to join meaningfully
# gather FinYear variables
fin_year_variables <- spawn_temp %>% left_join(num_days) %>%
  left_join(percentile_flow) %>% mutate(aligned_Year = FinYear)

# gather year variables
year_variables <- winter_spring_flow %>% left_join(max_variability) %>%
  left_join(min_winter_flow) %>% left_join(annual_flow) %>%
  mutate(aligned_Year = Year+1)

water_variables <- fin_year_variables %>% left_join(year_variables)

write_csv(water_variables, "Murrumbidgee water variables balranald.csv")



### Remove NA from Balranald

library(tidyverse)
library(lubridate)

wdat <- read_csv("DPE Murrumbidgee Balranald flow temp.csv") %>%
  #wdat <- read_csv("DPE Murrumbidgee Wagga flow  temp - 410001.csv") %>%
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1) %>%
  mutate(Month = month(Date),
         Year = year(Date),
         FinYear = case_when(Month>5 ~ Year+1,
                             T ~ Year))


plot_dat <- wdat %>% filter(Year >= 1990) %>% select(-Mean_Temp, -Month, -Year, -FinYear)

plot_dat$Flow_filled <- plot_dat$Mean_Flow
plot_dat$Flow_filled <- zoo::na.approx(plot_dat$Flow_filled, na.rm=F)


plot_dat <- plot_dat %>% mutate(Mean_Flow = Flow_filled) %>% select(-Flow_filled)

write_csv(plot_dat, "Balranald flow na removed.csv")

#### River rise and fall data
rise_fall <- read_csv("rates of rise Murrumbidgee oct_dec.csv") %>%
  mutate(Year = as.numeric(str_sub(Variable_short, start=-4, end=-1)),
         Variable_name = str_sub(Variable_short, start=1, end=-6),
         aligned_Year = Year+1) %>%
  select(-1, -2)

ggplot(rise_fall, aes(Wagga, Balranald)) + geom_point() + geom_smooth() +
  facet_wrap(~Variable_name)
ggsave("Falling and rising site comparison.png", width=21, height=14.8, units="cm", dpi = 600)
write_csv(rise_fall, "rates of rise clean.csv")


### old stuff

library(tidyverse)
library(lubridate)

flow_dat <- read_csv("Flow at Wagga Wagga 410001.csv", skip=9) %>% rename(Flow_cm_s = Value)
water_leve_dat <- read_csv("Water level at Wagga 410001.csv", skip=9) %>% rename(Water_level_m = Value)
temp_dat <- read_csv("Temp at Wagga Wagga 410001.csv", skip=9) %>% rename(Temperature = Value)
wdat <- flow_dat %>% left_join(water_leve_dat) %>% left_join(temp_dat) %>% mutate(Year = year(`#Timestamp`),
                                                          Month = month(`#Timestamp`)) %>%
  mutate(FinYear = case_when(Month >6 ~ Year +1,
                            T ~ Year))

plot(wdat$Flow_cm_s, wdat$Water_level_m)
plot(wdat$Flow_cm_s, wdat$Temperature)

wdat_sum <- wdat %>%filter(Month >7) %>% group_by(Year) %>% summarise(mean_flow = mean(Flow_cm_s, na.rm=T),
                                                                      mean_level = mean(Water_level_m, na.rm=T),
                                                                      Year = Year+1) %>% distinct()

spring_summer_water_level <- wdat %>% filter(!between(Month,3,8)) %>% group_by(FinYear) %>%
  summarise(mean_level = mean(Water_level_m, na.rm=T))# %>% distinct()

spawning_temp <- wdat %>% filter(Month <9) %>% group_by(FinYear) %>%
  summarise(mean_temp = mean(Temperature, na.rm=T)) %>% drop_na()

plot(spring_summer_water_level$mean_level, type="l")
plot(spawning_temp$mean_temp)

#install.packages("lfstat")
library(lfstat)

wdat2 <- as.xts(wdat)
x <- baseflow(wdat$Flow_cm_s)
hist(x)
plot(x)
