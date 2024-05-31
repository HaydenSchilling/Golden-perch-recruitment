# GP Map figure

library(tidyverse)
library(lubridate)

catch <- read_csv("All_catch_11_08_2023_with_fixed_WRPA.csv")

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
severn_bad <- catch %>% filter(WaterbodyName == "Severn River") %>% filter(SampleLatitude > -29)

catch <- catch %>% anti_join(severn_bad)



plot_dat <- catch %>% group_by(WaterbodyName, SampleLatitude, SampleLongitude) %>% 
  summarise(n_Surveys = n_distinct(SamplingRecordID))

### Shapefile data
library(sf)
library(ggspatial)

stream<-st_read("../Jerom/RMaps/mdb river/MDB_Main _Rivers.shp")
MDB<-st_read("../Jerom/RMaps/MDB/mdb_boundary.shp")
gauges <- read_csv("Gauge lat lons.csv")
                #mdb_Stream_layer_GDA1994.shp") 
states<-st_read("../Jerom/RMaps/Australian_states_GDA1994.shp")


### Site Map
p1 <- ggplot(plot_dat, aes(SampleLongitude, SampleLatitude, col = WaterbodyName, size=n_Surveys)) +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="grey70")+
  layer_spatial(data=MDB,colour="grey", lwd=0.8, fill="grey90", alpha=0.6)+
  #layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  layer_spatial(data=stream,colour="grey30")+
  #scale_size_continuous(label="Samples")+
  #layer_spatial(data=rivers,colour="red")+
  #geom_point(data=data_summaryX, aes(coords.x1, coords.x2, size=n), inherit.aes = F, col="black", fill="white", shape=21)+
  geom_point(shape=21, fill="white")+
  geom_point(data=gauges, aes(Longitude, Latitude), inherit.aes = F, shape = 17, colour="blue")+
  # scale_fill_manual(name="Water Resource Planning Area",
  #                   values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
  #                                  "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
  #                                  "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
  # scale_colour_manual(values=c("hotpink"), name=NULL)+
  coord_sf(xlim=c(138, 153), 
           ylim= c(-40,-25))+
  scale_x_continuous(breaks=c(138,142,146,150))+
  theme_bw()+
  ylab("Latitude") + xlab("Longitude")+
  scale_size_continuous(name = "Number\nof surveys")+
  scale_color_discrete(name="River")+
  theme(legend.position = "right",
        axis.text = element_text(colour="black", size=12),
        axis.title = element_text(face="bold", size=14),
        legend.title = element_text(face="bold", size=11),
        legend.text = element_text(colour="black", size=10))
p1


p2 <- ggplot() +
  layer_spatial(data=states,colour="black", lwd=0.8, fill="khaki3")+
  layer_spatial(MDB,col="black", fill = "grey90" ,
                alpha=0.9, size=0.5)+
  geom_rect(col="black", aes(xmin=138,xmax= 153, 
                             ymin= -40, ymax = -24.6),
            fill=NA)+
  #layer_spatial(BDW, aes(col = SWWRPANAME), size=2, fill=NA)+
  #layer_spatial(WRPA, aes(fill=SWWRPANAME))+
  #layer_spatial(data=stream,colour="grey30")+
  #layer_spatial(data=rivers,colour="red")+
  #geom_point(shape=4,size=2)+
  #scale_fill_manual(name="Water Resource Planning Area",
  #                  values = rev(c("grey90", "#4662D7FF", "#36AAF9FF", "#1AE4B6FF",
  #                                 "#72FE5EFF", "#C7EF34FF", "#FABA39FF", "#F66B19FF",
  #                                 "#CB2A04FF", "#7A0403FF")))+ # turbo colour scale
  #scale_colour_manual(values=c("hotpink"), name=NULL)+
  #coord_sf(xlim=c(st_bbox(WRPA_all)[1], st_bbox(WRPA_all)[3]), 
#         ylim= c(st_bbox(WRPA_all)[2],st_bbox(WRPA_all)[4]))+
#scale_x_continuous(breaks=c(138,142,146,150))+
theme_classic()+
  ylab("Latitude") + xlab("Longitude")+
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(colour="black", size=10),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.border = element_rect(fill=NA, colour="black"),
        plot.background = element_blank())

p2

library(patchwork)

#p2 + p1 + plot_layout(guides = 'collect', widths = c(0.5,1))# & theme(legend.position = "bottom")
p1 + inset_element(p2, left = 0.01, bottom = 0.65, right = .35, top = 1.03, align_to = "panel")

#ggsave("Site map.png", dpi = 600, width=21, height=21, units="cm")
#ggsave("Site map.pdf", dpi = 600, width=21, height=21, units="cm") ## number labels added in Illustrator

