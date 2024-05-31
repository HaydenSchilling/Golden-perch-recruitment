#### Flow plots

library(tidyverse)
library(lubridate)
library(zoo)

### Flow data for Warrego
flow_dat <- read_csv("Warrego at Barringun 423004.csv") %>% # Fords Bridge 423001 Flow and Temp.csv
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1) %>%
  mutate(Gauge = "b) Warrego at Barringun 423004") %>%
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
  mutate(Gauge = "d) Barwon at Dangar Bridge 422001")%>%
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
  mutate(Gauge = "e) Darling at Bourke Town 425003")%>%
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
  mutate(Gauge ="f) Darling at Wilcannia Main Channel 425008") %>%
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
  mutate(Gauge = "g) Darling at Burtundy 425007")%>%
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
flow_dat6 <- read_csv("Bogan at Gongolgon 421023 Flow and Temp.csv") %>% # Pooncarie 425005 Flow and Temp.csv
  mutate(Date1 = as.Date(dmy_hm(Date)),
         Date2 = as.Date(dmy_hms(Date)),
         Date3 = case_when(is.na(Date1)~ Date2,
                           T ~ Date1),
         Date = Date3) %>% select(-Date2, -Date3, -Date1)%>%
  mutate(Gauge = "c) Bogan at Gongolgon 421023") %>%
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
  mutate(Gauge = "a) Boomi at Boomi Weir Offtake 416037") %>%
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



all_flow <- bind_rows(flow_dat, flow_dat2, flow_dat3, flow_dat3, flow_dat4, flow_dat5, flow_dat6, flow_dat7) %>% filter(Date > as.Date("1990-01-01"))


EWR <- read_csv("EWR Rules.csv") %>% pivot_longer(cols = c(2,3), names_to = "EWR", values_to = "Threshold")


fish_dat <- read_csv("GP YOY modelling data.csv") %>% select(SampleDate, WaterbodyName2) %>% distinct() %>%
  mutate(Gauge = case_when(WaterbodyName2 == "Barwon River" ~ "d) Barwon at Dangar Bridge 422001",
                           WaterbodyName2 == "Warrego River" ~ "b) Warrego at Barringun 423004",
                           WaterbodyName2 == "North Darling River" ~ "e) Darling at Bourke Town 425003",
                           WaterbodyName2 == "Darling River" ~ "f) Darling at Wilcannia Main Channel 425008",
                           WaterbodyName2 == "South Darling River" ~ "g) Darling at Burtundy 425007",
                           WaterbodyName2 == "Bogan River" ~ "c) Bogan at Gongolgon 421023",
                           WaterbodyName2 == "Boomi River" ~ "a) Boomi at Boomi Weir Offtake 416037"
                           )) %>%
  mutate(Date=as.Date(SampleDate))


ggplot(all_flow, aes(Date, Flow/1000)) + geom_line() + facet_wrap(~Gauge, ncol=1, scales = "free_y") +
  geom_hline(data=EWR, aes(yintercept=Threshold/1000, col = EWR), linetype="dashed")+
  scale_color_manual(values = c("blue", "red"), name = "Flow Magnitude Thresholds")+ 
  geom_rug(data=fish_dat, aes(x=Date), inherit.aes = F, sides = "t", col="purple", length = unit(0.05, "npc"))+
  #geom_hline(data=EWR, aes(yintercept=`Large Fresh`), inherit.aes = F, col="blue", linetype="dashed") +
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                         axis.title = element_text(face="bold", size=14),
                         strip.text = element_text(size=12, face="bold", hjust=0),
                         legend.title = element_text(face="bold", size=12),
                         legend.text = element_text(size=10, colour="black"),
                         legend.position = "bottom",
                         strip.background = element_blank(),
                         panel.border = element_rect(colour="black", fill=NA),
                         legend.key.width = unit(1.5, units = "cm"))+
  ylab(bquote(bold("River Discharge ('000 ML" ~Day^'-1' *")")))

ggsave("Flow since 1990.png", width = 21, height = 25, units="cm", dpi =600)
ggsave("Flow since 1990.pdf", width = 21, height = 25, units="cm", dpi =600)


LG_dat <- all_flow %>% filter(Lg_Achieved == 1)
OB_dat <- all_flow %>% filter(Overbank_Achieved == 1)

### Test EWR Plot - when did EWRs occur
ggplot(all_flow, aes(Date, Flow/1000)) + geom_line() + facet_wrap(~Gauge, ncol=1, scales = "free_y") +
  geom_hline(data=EWR, aes(yintercept=Threshold/1000, col = EWR), linetype="dashed")+
  
  geom_vline(data=LG_dat, aes(xintercept=Date),col="blue", alpha=0.05)+
  
  scale_color_manual(values = c("blue", "red"), name = "Flow Magnitude Thresholds")+ 
  geom_rug(data=fish_dat, aes(x=Date), inherit.aes = F, sides = "t", col="purple", length = unit(0.05, "npc"))+
  #geom_hline(data=EWR, aes(yintercept=`Large Fresh`), inherit.aes = F, col="blue", linetype="dashed") +
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(face="bold", size=14),
                          strip.text = element_text(size=12, face="bold", hjust=0),
                          legend.title = element_text(face="bold", size=12),
                          legend.text = element_text(size=10, colour="black"),
                          legend.position = "bottom",
                          strip.background = element_blank(),
                          panel.border = element_rect(colour="black", fill=NA),
                          legend.key.width = unit(1.5, units = "cm"))+
  ylab(bquote(bold("River Discharge ('000 ML" ~Day^'-1' *")")))
ggsave("Flow since 1990 LARGE FRESH.png", width = 21, height = 25, units="cm", dpi =600)


### Test EWR Plot
ggplot(all_flow, aes(Date, Flow/1000)) + geom_line() + facet_wrap(~Gauge, ncol=1, scales = "free_y") +
  geom_hline(data=EWR, aes(yintercept=Threshold/1000, col = EWR), linetype="dashed")+
  geom_vline(data=LG_dat, aes(xintercept=Date),col="blue", alpha=0.05)+
  geom_vline(data=OB_dat, aes(xintercept=Date),col="red", alpha=0.05)+
  
  scale_color_manual(values = c("blue", "red"), name = "Flow Magnitude Thresholds")+ 
  geom_rug(data=fish_dat, aes(x=Date), inherit.aes = F, sides = "t", col="purple", length = unit(0.05, "npc"))+
  #geom_hline(data=EWR, aes(yintercept=`Large Fresh`), inherit.aes = F, col="blue", linetype="dashed") +
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(face="bold", size=14),
                          strip.text = element_text(size=12, face="bold", hjust=0),
                          legend.title = element_text(face="bold", size=12),
                          legend.text = element_text(size=10, colour="black"),
                          legend.position = "bottom",
                          strip.background = element_blank(),
                          panel.border = element_rect(colour="black", fill=NA),
                          legend.key.width = unit(1.5, units = "cm"))+
  ylab(bquote(bold("River Discharge ('000 ML" ~Day^'-1' *")")))
ggsave("Flow since 1990 Large Fresh and OVERBANK.png", width = 21, height = 25, units="cm", dpi =600)
