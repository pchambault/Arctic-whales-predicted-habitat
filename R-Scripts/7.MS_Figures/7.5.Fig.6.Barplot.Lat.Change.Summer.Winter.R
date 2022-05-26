##############################################################
####### Fig. 5: Northward shift in SUMMER and WINTER #########
##############################################################

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")



#############################
# import data
#############################

# summer
#--------
lat <- readRDS("./RDATA/6.Climatic_projections/Area/Proba_0.5/Lat_change_Summer_3sp_6var_3GCMs.rds")
lat$species = recode_factor(lat$species, 
                            Bel = "Beluga", 
                            Bw  = "Bowhead",
                            Nar = "Narwhal")
lat %>%
  group_by(species, side) %>%
  summarise(mean_126 = mean(diff_ssp126, na.rm=T),
            mean_ssp585 = mean(diff_ssp585, na.rm=T),
            sd_126 = sd(diff_ssp126, na.rm=T),
            sd_585 = sd(diff_ssp585, na.rm=T))




# lat change SUMMER
#----------------------
ssp126 <- summarySE(lat, measurevar="diff_ssp126",na.rm=T, 
                    groupvars=c("species","side"))
ssp585 <- summarySE(lat, measurevar="diff_ssp585", na.rm=T, 
                    groupvars=c("species","side"))

names(ssp126) = c("species","side","N","mean","sd","se","ci")
ssp126$ssp = "ssp126"
names(ssp585) = c("species","side","N","mean","sd","se","ci")
ssp585$ssp = "ssp585"
summer = rbind(ssp126, ssp585)  
summer

a = ggplot(summer, aes(x=species, y=mean)) + 
  geom_bar(stat="identity",position=position_dodge(),
           width=0.5, fill="lightseagreen") +
  facet_grid(ssp~side) +
  ylim(-100,740) +
  geom_hline(yintercept=0, lty=2, lwd=0.3) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, lwd=0.3, 
                position=position_dodge(.9)) +
  labs(x="", y="Northward shift (km)",title="a) Summer") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = "none",
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 




#--------
# winter
#--------
lat <- readRDS("./RDATA/6.Climatic_projections/Area/Proba_0.5/Lat_change_Winter_2sp_6var_3GCMs.rds")
lat$species = recode_factor(lat$species, 
                            Bw  = "Bowhead",
                            Nar = "Narwhal")
lat %>%
  group_by(species, side) %>%
  summarise(mean_126 = mean(diff_ssp126, na.rm=T),
            mean_ssp585 = mean(diff_ssp585, na.rm=T),
            sd_126 = sd(diff_ssp126, na.rm=T),
            sd_585 = sd(diff_ssp585, na.rm=T))


# lat change SUMMER
#----------------------
ssp126 <- summarySE(lat, measurevar="diff_ssp126",na.rm=T, 
                    groupvars=c("species","side"))
ssp585 <- summarySE(lat, measurevar="diff_ssp585", na.rm=T, 
                    groupvars=c("species","side"))

names(ssp126) = c("species","side","N","mean","sd","se","ci")
ssp126$ssp = "ssp126"
names(ssp585) = c("species","side","N","mean","sd","se","ci")
ssp585$ssp = "ssp585"
winter = rbind(ssp126, ssp585)  
winter

b = ggplot(winter, aes(x=species, y=mean)) + 
  geom_bar(stat="identity",position=position_dodge(),
           width=0.35, fill="lightseagreen") +
  facet_grid(ssp~side) +
  ylim(-100,740) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, lwd=0.3, 
                position=position_dodge(.9)) +
  geom_hline(yintercept=0, lty=2, lwd=0.3) +
  labs(x="", y="Northward shift (km)",title="b) Winter") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = "none",
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 


library(gridExtra)
ggsave(filename=paste0("./PAPER/Science/2.Resubmission_March2022/",
                       "Figures/Fig.6_Barplot_LatChange_Summer_Winter_SD.pdf"),
       width=3.8,height=5.5,
       units="in",dpi=400,family="Helvetica",
       grid.arrange(a,b,ncol=1))







####################################
# some statistics ...
####################################
summer$season = "summer"
winter$season = "winter"
tot = rbind(summer, winter)
tot = as_tibble(tot)
summary(summer$mean[!(summer$species=="nar" & summer$side=="East")]) # -25%
mean(tot$mean)  # -189 km
sd(tot$mean)    # 132
range(tot$mean) # 37 to 475 km

tot
tot[which.min(tot$mean),]  # nar west winter
tot[which.max(tot$mean),]  # bw west summer
tot$season = as.factor(tot$season)
tot %>%
  group_by(season) %>%
  summarize(mean = mean(mean, na.rm=T),
            sd = mean(sd, na.rm=T))
mean(tot$mean[tot$season=="summer"])  # 243
sd(tot$mean[tot$season=="summer"])    # 142
mean(tot$mean[tot$season=="winter"])  # 121 km
sd(tot$mean[tot$season=="winter"])    # 84
kruskal.test(mean~season, tot)        # p=0.06
  
