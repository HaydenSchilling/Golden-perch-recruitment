# Merge water variables
library(tidyverse)

rates_dat <- read_csv("rates of rise clean.csv") %>%
  pivot_longer(c(1,2), names_to = "site", values_to = "Value") %>%
  mutate(FinYear = Year+1)
wagga_dat <- read_csv("Murrumbidgee water variables.csv") %>% 
  pivot_longer(c(2:6,9:14), names_to = "Variable_name", values_to = "Value") %>%
  mutate(site = "Wagga")
bal_dat <- read_csv("Murrumbidgee water variables balranald.csv") %>% 
  pivot_longer(c(2:6,9:14), names_to = "Variable_name", values_to = "Value") %>%
  mutate(site = "Balranald")


combined_dat <- wagga_dat %>% bind_rows(bal_dat) %>% bind_rows(rates_dat) %>% pivot_wider(names_from = "site", values_from = Value)

ggplot(combined_dat, aes(Wagga, Balranald)) + geom_point() +
  geom_smooth() + facet_wrap(~Variable_name, scales="free")

ggsave("Water comparison all variable Wagga Balranald.png", dpi=600, width = 14.8*2 , height=21, units="cm")


combined_wide <- combined_dat %>% pivot_wider(names_from = c(Variable_name), values_from = c("Wagga","Balranald"))
write_csv(combined_wide, "Water variables for modelling combined Bidgee.csv")
