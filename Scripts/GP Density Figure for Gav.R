# Figure 1 density plot

# Golden perch density plot

#load packages
library(tidyverse)
library(lubridate)
library(ggridges)

# load data
catch <- read_csv("small_catch_example.csv")
# catch <- read_csv("All_catch_11_08_2023_with_fixed_WRPA.csv")
# 
# 
# 
# catch_small1 <- catch %>% filter(SamplingRecordID == 6888)
# catch_small2 <- catch %>% sample_n(1000)
# catch_small <- bind_rows(catch_small1, catch_small2)
# write_csv(catch_small, "small_catch_example.csv")

### This removes a bunch of non-representative sampling
catch <- catch %>% mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct()

segments <- catch %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")


bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment)) # all extra fishing


catch <- catch %>% filter(!project_segment %in% bad_list) %>% filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment)
  
# Final data, filter for boat and backpack Electro and remove any duplicates (linked to multiple projects)
catch <- catch %>% filter(Method == "BTE"|Method == "BPE") %>%
  select(-ProjectName,-Abbreviation,-SegmentName,-project_segment) %>% distinct()

# ### link to waterbodies and filter if needed
# waterb2 <- read_csv("../database exploration/Waterbody dictionary.csv") %>% rename(WaterbodyID = WaterBodyID)
# 
# catch <- catch %>% left_join(waterb2)
# 
# key_rivers <- c("Lachlan River", "Macquarie River", "Gwydir River", "Namoi River", "Darling River",
#                 "Murrumbidgee River", "Barwon River", "Murray River", "Warrego River",
#                 "Severn River", "Dumaresq River", "Macintyre River", "Bogan River", "Castlereagh River", "Mole River")
# 
# catch <- catch %>% filter(WaterbodyName %in% key_rivers)

# load bio data
bio <- read_csv("Bio_small_example.csv")

# Again remove non-representative sampling segments
bio <- bio %>% mutate(project_segment = paste0(ProjectName,":",SegmentName)) %>% distinct()

segments <- bio %>% distinct(project_segment) %>% arrange(project_segment)

bad_list <- c("Edward-Wakool Blackwater restocking:NETTING AND EXTRA E FISHING", "Edward-Wakool Blackwater restocking:NETTING AND EXTRA EFISHING",
              "Edward-Wakool Blackwater restocking:NETTING AND EXTRA ELECTRO", "Koondrook Perricoota Accumulation Sites:2014",
              "Koondrook Perricoota Accumulation Sites:2015", "Koondrook Perricoota Accumulation Sites:2016",
              "Lachlan Carp Demo:GCS - YOY CARP", "Murray Cod Slot Limit Assessment:2019/Extra",
              "Murray Cod Slot Limit Assessment:2020/Extra")

bad2 <- segments %>% filter(grepl("*Extra*",project_segment)) # all extra fishing
bad3 <- segments %>% filter(grepl("*Selective*",project_segment))


bio <- bio %>% filter(!project_segment %in% bad_list) %>% 
  filter(!project_segment %in% bad2$project_segment) %>%
  filter(!project_segment %in% bad3$project_segment) 


# Final data, filter for boat Electro and remove duplicates
bio <- bio %>% filter(Method == "BTE" | Method == "BPE") %>%
  select(-ProjectName,-Abbreviation,-SegmentName,-project_segment, -HealthDescription, -NumberOfOccurences) %>% distinct()

# which species
species <- "Golden perch"

# filter for species and get month/dates
gp_catch <- catch %>% filter(CommonName == species) %>% filter(NumberCaught >0)
gp_bio <- bio %>% filter(CommonName == species) %>% mutate(Date = SampleDate,
                                                           Month = month(Date))

# check if the bio numbers match the catch numbers
bio_size_check <- gp_bio %>% group_by(OperationID) %>% summarise(bio_total = n())# %>% filter(OperationID == 208737)

sum(gp_catch$NumberCaught)

gp_combo <- gp_catch %>%left_join(bio_size_check) %>% mutate(diff = NumberCaught-bio_total)


### if there are more catch then bio then expand the bio numbers to be more representative
### I used random sampling from observed lengths - could be done differently but this retains whole numbers
### major assumption is the random draw of 'unsampled' fish is close to the true sizes
need_to_expand <- gp_combo %>% filter(diff >0)
extra_dat <- data.frame()

if(nrow(need_to_expand > 0)){
for(i in 1:nrow(need_to_expand)){
  test2 <- gp_bio %>% filter(OperationID == need_to_expand$OperationID[i]) %>% sample_n(size = need_to_expand$diff[i], replace = T)
  extra_dat <- extra_dat %>% bind_rows(test2)
}
}
gp_bio_expanded <- gp_bio %>% bind_rows(extra_dat)

# # join to waterbody ID if needed
#gp_bio_expanded <- gp_bio_expanded  %>% left_join(waterb2) %>% filter(WaterbodyName %in% key_rivers)



ggplot(gp_bio_expanded, aes(x=Length_mm)) + geom_density() + facet_wrap(~Month)
ggplot(gp_bio_expanded, aes(x=Length_mm)) + geom_histogram() + facet_wrap(~Month)
# ggplot(gp_bio_expanded, aes(x=Length_mm)) + geom_histogram() + facet_wrap(~WaterbodyName)


# set sampling season/financial year
gp_bio_expanded <- gp_bio_expanded %>% mutate(Year = year(SampleDate),
                                              FinYear = case_when(Month > 6 ~ Year+1,
                                                                  T ~ Year))

# get mean lengths for each year and drop 2024 (very few samples)
mean_lengths2 <- gp_bio_expanded %>% group_by(FinYear) %>%
  summarise(Mean_Length = mean(Length_mm)) %>% filter(FinYear != 2024)
gp_bio_expanded3 <- gp_bio_expanded %>% left_join(mean_lengths2) %>% filter(FinYear != 2024)


# Make plot
ggplot(gp_bio_expanded3, aes(x=Length_mm,y=FinYear, group=FinYear, fill = Mean_Length)) + #, fill = Mean_Length
  geom_density_ridges(rel_min_height=0.05,
                      quantile_lines=TRUE,
                      quantile_fun=function(x,...)median(x)) +# facet_wrap(~WaterbodyName) + xlim(c(0,650))+
  scale_fill_viridis_c(option = "magma", name="Mean\nLength (mm)") + 
  theme_bw() + ylab("Sampling Year Ending June 30th") + xlab("Length (mm)") +
  scale_y_reverse(breaks = c(1995,2000,2005,2010,2015,2020))+
  theme(axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=12),
        legend.position = "bottom",
        strip.background = element_rect(fill=NA),
        strip.text = element_text(face="bold", size=10))

ggsave("Length density plot.png", dpi=600, units="cm",
       width=21, height=21)
