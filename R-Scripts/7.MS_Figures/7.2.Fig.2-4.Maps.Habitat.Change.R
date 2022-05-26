###################################################################
####### Fig. 3: maps of habitat loss, gain and no change ##########
#######            SUMMER and WINTER
###################################################################

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(png)


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")







##################################################
# import background map with adequate projection
##################################################
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Russia"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland"
                      | north_map$region=="Iceland",]




########################
# import rasters
########################

# belugas 
#---------
bel126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                      "Hab_Change/HabChange_Mean_3GCMs_Summer_Bel_West_ssp126.rds"))
bel585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Summer_Bel_West_ssp585.rds"))
bel = rbind(bel126, bel585)
bel$season = "Summer"

# bowheads 
#---------
bw126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Summer_Bw_West_ssp126.rds"))
bw585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Summer_Bw_West_ssp585.rds"))
west = rbind(bw126, bw585)

bw126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Summer_Bw_East_ssp126.rds"))
bw585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Summer_Bw_East_ssp585.rds"))
east = rbind(bw126, bw585)
bw_summer = rbind(west, east)
bw_summer$season = "Summer"


# narwhals 
#---------
nar126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Summer_Nar_West_ssp126.rds"))
nar585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Summer_Nar_West_ssp585.rds"))
west = rbind(nar126, nar585)

nar126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Summer_Nar_East_ssp126.rds"))
nar585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Summer_Nar_East_ssp585.rds"))
east = rbind(nar126, nar585)
nar_summer = rbind(west, east)
nar_summer$season = "Summer"











############################################
# plot 3 species in Summer for both ssp
############################################

# belugas
#----------
bel$sp = "Belugas"
a = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-95,-20), ylim=c(60,80)) + # xlim=c(-105,-40)
  geom_polygon(data=bel[bel$hab=="Loss",],aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=bel[bel$hab=="Gain",],aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  geom_polygon(data=bel[bel$hab=="No change",],aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_grid(sp~ssp, switch = 'y') +
  annotate("text", x=-18.5,y=62, label="IS", colour="black",size=2) +
  annotate(geom="text", x=-41, y=72, label="Greenland", angle=45, size=2,colour="black") +
  labs(y="",x="",fill="Habitat",title="SUMMER") + 
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(),
        strip.background = element_rect(fill="steelblue4", size=0.1),
        strip.text = element_text(colour='white',size=12,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, 
                                hjust=0.5, colour="steelblue4"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0,0.3,-1.2,-0.15),"cm")) #top,right,bottom,left

# bowheads
#----------
bw_summer$sp = "Bowheads"
b = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-105,75), ylim=c(60,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_polygon(data=bw_summer[bw_summer$hab=="Loss" & bw_summer$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=bw_summer[bw_summer$hab=="Gain" & bw_summer$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  geom_polygon(data=bw_summer[bw_summer$hab=="No change" & bw_summer$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=bw_summer[bw_summer$hab=="Loss" & bw_summer$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=bw_summer[bw_summer$hab=="No change" & bw_summer$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=bw_summer[bw_summer$hab=="Gain" & bw_summer$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  labs(x="", y="") +
  annotate(geom="text", x=-41, y=72, label="Greenland", angle=45, size=2,colour="black") +
  facet_grid(sp~ssp, switch = 'y') +
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(), 
        strip.background.x = element_blank(),
        strip.background.y = element_rect(fill="steelblue4", size=0.1),
        strip.text.x = element_blank(),
        strip.text.y = element_text(colour='white',size=12,face="bold",
                                    margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, hjust=0),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(-1.2,0.3,-1.3,-0.15),"cm")) #top,right,bottom,left

# narwhals
#----------
nar_summer$sp = "Narwhals"
c = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-95,-20), ylim=c(60,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_polygon(data=nar_summer[nar_summer$hab=="Loss" & nar_summer$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=nar_summer[nar_summer$hab=="Gain" & nar_summer$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  geom_polygon(data=nar_summer[nar_summer$hab=="No change" & nar_summer$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=nar_summer[nar_summer$hab=="Loss" & nar_summer$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=nar_summer[nar_summer$hab=="No change" & nar_summer$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=nar_summer[nar_summer$hab=="Gain" & nar_summer$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  labs(x="", y="") +
  annotate(geom="text", x=-41, y=72, label="Greenland", angle=45, size=2,colour="black") +
  facet_grid(sp~ssp, switch = 'y') +
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(), 
        strip.background.x = element_blank(),
        strip.background.y = element_rect(fill="steelblue4", size=0.1),
        strip.text.x = element_blank(),
        strip.text.y = element_text(colour='white',size=12,face="bold",
                                    margin=margin(0.1,0.1,0.1,0.1,"cm")),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, hjust=0),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(-2.5,0.3,-0.3,-0.22),"cm")) #top,right,bottom,left

ggsave(filename="./PAPER/Science/2.Resubmission_March2022/Figures/Fig.3.Hab_Change_Summer.pdf",
       grid.arrange(a, b, c, ncol=1),
       width=5,height=5.4,units="in",dpi=400,family="Helvetica")


















############################################
############    WINTER       ###############
############################################

# bowheads 
#---------
bw126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Winter_Bw_West_ssp126.rds"))
bw585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Winter_Bw_West_ssp585.rds"))
west = rbind(bw126, bw585)

bw126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Winter_Bw_East_ssp126.rds"))
bw585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                        "Hab_Change/HabChange_Mean_3GCMs_Winter_Bw_East_ssp585.rds"))
east = rbind(bw126, bw585)
bw_winter = rbind(west, east)
bw_winter$season = "Winter"


bw_winter$sp = "Bowheads"
b = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-105,75), ylim=c(60,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_polygon(data=bw_winter[bw_winter$hab=="Loss" & bw_winter$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=bw_winter[bw_winter$hab=="Gain" & bw_winter$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  geom_polygon(data=bw_winter[bw_winter$hab=="No change" & bw_winter$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=bw_winter[bw_winter$hab=="Loss" & bw_winter$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=bw_winter[bw_winter$hab=="No change" & bw_winter$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=bw_winter[bw_winter$hab=="Gain" & bw_winter$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  labs(x="", y="", title="WINTER") +
  annotate(geom="text", x=-41, y=72, label="Greenland", angle=45, size=2,colour="black") +
  facet_grid(sp~ssp, switch = 'y') +
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(),
        strip.background = element_rect(fill="steelblue4", size=0.1),
        strip.text = element_text(colour='white',size=12,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, 
                                hjust=0.5, colour="steelblue4"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0,0.3,-0.7,-0.15),"cm")) #top,right,bottom,left



# narwhals 
#---------
nar126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Winter_Nar_West_ssp126.rds"))
nar585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Winter_Nar_West_ssp585.rds"))
west = rbind(nar126, nar585)

nar126 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Winter_Nar_East_ssp126.rds"))
nar585 <- readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/HabChange_Mean_3GCMs_Winter_Nar_East_ssp585.rds"))
east = rbind(nar126, nar585)
nar_winter = rbind(west, east)
nar_winter$season = "Winter"


nar_winter$sp = "Narwhals"
c = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-100,-20), ylim=c(60,80)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  geom_polygon(data=nar_winter[nar_winter$hab=="Loss" & nar_winter$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=nar_winter[nar_winter$hab=="Gain" & nar_winter$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  geom_polygon(data=nar_winter[nar_winter$hab=="No change" & nar_winter$side=="West",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=nar_winter[nar_winter$hab=="Loss" & nar_winter$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="firebrick3",lwd=0.1) +
  geom_polygon(data=nar_winter[nar_winter$hab=="No change" & nar_winter$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="orange",lwd=0.1) +
  geom_polygon(data=nar_winter[nar_winter$hab=="Gain" & nar_winter$side=="East",],
               aes(x=long, y=lat, group=group),
               fill="lightseagreen",lwd=0.1) +
  labs(x="", y="") +
  annotate(geom="text", x=-41, y=72, label="Greenland", angle=45, size=2,colour="black") +
  facet_grid(sp~ssp, switch = 'y') +
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(), 
        strip.background.x = element_blank(),
        strip.background.y = element_rect(fill="steelblue4", size=0.1),
        strip.text.x = element_blank(),
        strip.text.y = element_text(colour='white',size=12,face="bold",
                                    margin=margin(0.1,0.1,0.1,0.1,"cm")),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, hjust=0),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(-0.5,0.3,-0.3,-0.22),"cm")) #top,right,bottom,left

ggsave(filename="./PAPER/Science/2.Resubmission_March2022/Figures/Fig.4.Hab_Change_Winter.pdf",
       grid.arrange(b, c, ncol=1),
       width=5,height=3.5,units="in",dpi=400,family="Helvetica")
