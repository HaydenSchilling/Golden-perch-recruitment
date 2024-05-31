### Stocking for GP Stock Status

library(tidyverse)

stock_dat <- read_csv("Full Stocking Data to Aug2023 MC GP.csv") %>% rename(Site = SITE)

# classifications <- read_csv("../BPEOM round 1/stocking location classification.csv")
# 
# classes <- read_csv("Stocking classification natural impoundment.csv")
# 
# 
# sites <- stock_dat %>% select(Site) %>% distinct() %>% left_join(classifications) %>% drop_na()
# sites2 <- stock_dat %>% select(Site) %>% distinct() %>% anti_join(sites) %>% left_join(classes)
# sites3 <- bind_rows(sites, sites2)
# write_csv(sites3, "Stocking classification natural impoundment_full.csv")

classes <- read_csv("Stocking classification natural impoundment_full.csv")

stock_dat <- stock_dat %>% left_join(classes) %>%
  mutate(Species = stringr::str_to_sentence(SPECIES),
         Sampling_Year= case_when(MONTH>6 ~ YEAR+1,
                                  T ~ YEAR))

sum_dat <- stock_dat %>% group_by(Sampling_Year, Species) %>%
  summarise(Total_Stocked = sum(NUMBER)) %>% drop_na() %>% ungroup() %>%
  complete(Species, Sampling_Year, fill=list(Total_Stocked = 0))

ggplot(sum_dat, aes(x=Sampling_Year, y = Total_Stocked)) + geom_col(position="dodge") +
  facet_wrap(~Species, ncol=1) + theme_classic()+ xlab("Year ending June 30th")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position="bottom",
        legend.text = element_text(size=10, colour = "black"),
        legend.title = element_text(size=12, face="bold", colour="black"),
        strip.text = element_text(size=12, face="bold"))+
  ylab("Total Stocked")

ggsave("Stocking over time.png", dpi=600, width=21, height=21, units="cm")

gp_dat <- sum_dat %>% filter(Species == "Golden perch")
write_csv(gp_dat, "Stocking data sampling year GP.csv")

ggplot(gp_dat, aes(x=Sampling_Year, y = Total_Stocked/1000000)) + geom_col(position="dodge") +
  theme_classic()+ xlab("Year ending June 30th")+
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5))+
  #ylim(c(0,3.5))+
  scale_x_continuous(breaks = seq(1960,2020,5))+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        panel.border = element_rect(fill=NA, colour="black"),
        legend.position="bottom",
        legend.text = element_text(size=10, colour = "black"),
        legend.title = element_text(size=12, face="bold", colour="black"),
        strip.text = element_text(size=12, face="bold"))+
  ylab("Total Stocked (million)")

ggsave("GP Stocking over time.png", dpi=600, width=21, height=14.8, units="cm")
