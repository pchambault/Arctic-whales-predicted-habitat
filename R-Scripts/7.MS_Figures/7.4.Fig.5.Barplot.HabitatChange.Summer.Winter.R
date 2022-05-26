##############################################################
####### Fig. 5: Habitat loss in SUMMER and WINTER   ##########
##############################################################

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")






#############################
# import data
#############################

#------------
# summer
#------------
hab <- readRDS("./RDATA/6.Climatic_projections/Area/Proba_0.5/Area_change_Summer_3sp_6var_3GCMs.rds")
hab$species = recode_factor(hab$species, 
                            Bel = "Beluga", 
                            Bw = "Bowhead",
                            Nar = "Narwhal")
library(FSA)
hab %>%
  group_by(species, side) %>%
  summarise(mean_126 = mean(prop_ssp126),
            mean_ssp585 = mean(prop_ssp585),
            sd_126 = sd(prop_ssp126),
            sd_585 = sd(prop_ssp585))



# habitat change SUMMER
#----------------------
library(Rmisc)
ssp126 <- summarySE(hab, measurevar="prop_ssp126", 
                    groupvars=c("species","side"))
ssp585 <- summarySE(hab, measurevar="prop_ssp585", 
                    groupvars=c("species","side"))

names(ssp126) = c("species","side","N","mean","sd","se","ci")
ssp126$ssp = "ssp126"
names(ssp585) = c("species","side","N","mean","sd","se","ci")
ssp585$ssp = "ssp585"
summer = rbind(ssp126, ssp585)  
summer = summer %>%
  mutate(type = ifelse(mean < 0, "loss", "gain"))

a = ggplot(summer, aes(x=species, y=mean, fill=type)) + 
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  facet_grid(ssp~side) +
  geom_hline(yintercept=0, lty=2, lwd=0.3) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, lwd=0.3, 
                position=position_dodge(.9)) +
  labs(x="", y="Habitat change (%)",title="a) Summer") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.16,0.88),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.05,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=4),
        legend.box.spacing = unit(0.01,'cm'),
        legend.key = element_rect(colour = "transparent", 
                                  fill = "transparent"),
        legend.spacing.x = unit(0.02, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(t=-0.0, unit='cm'),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 


mean(summer$mean[!(summer$species=="nar" & summer$side=="East")]) # -25%
sd(summer$mean[!(summer$species=="nar" & summer$side=="East")])   # 66%
unique(summer$species[!(summer$species=="nar" & summer$side=="East")])
unique(summer$side[!(summer$species=="nar" & summer$side=="East")])




#------------
# WINTER
#------------
hab <- readRDS("./RDATA/6.Climatic_projections/Area/Proba_0.5/Area_change_Winter_2sp_6var_3GCMs.rds")
hab$species = recode_factor(hab$species, 
                            Bel = "Beluga", 
                            Bw = "Bowhead",
                            Nar = "Narwhal")
hab %>%
  group_by(species, side) %>%
  summarise(mean_126 = mean(prop_ssp126),
            mean_ssp585 = mean(prop_ssp585),
            sd_126 = sd(prop_ssp126),
            sd_585 = sd(prop_ssp585))

# habitat change Winter
#----------------------
ssp126 <- summarySE(hab, measurevar="prop_ssp126", 
                    groupvars=c("species","side"))
ssp585 <- summarySE(hab, measurevar="prop_ssp585", 
                    groupvars=c("species","side"))

names(ssp126) = c("species","side","N","mean","sd","se","ci")
ssp126$ssp = "ssp126"
names(ssp585) = c("species","side","N","mean","sd","se","ci")
ssp585$ssp = "ssp585"
winter = rbind(ssp126, ssp585)  
winter = winter %>%
  mutate(type = ifelse(mean < 0, "loss", "gain"))


b = ggplot(winter, aes(x=species, y=mean, fill=type)) + 
  geom_bar(stat="identity",position=position_dodge(), width=0.4) +
  facet_grid(ssp~side) +
  geom_hline(yintercept=0, lty=2, lwd=0.3) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1, lwd=0.3, 
                position=position_dodge(.9)) +
  labs(x="", y="Habitat change (%)",title="b) Winter") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=8,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = "none",#c(0.16,0.9),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.05,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=4),
        legend.box.spacing = unit(0.05,'cm'),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.spacing.x = unit(0.03, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(t=-0.0, unit='cm'),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        axis.text  = element_text(size=6, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 

library(gridExtra)
ggsave(filename=paste0("./PAPER/Science/2.Resubmission_March2022/",
                       "Figures/Fig.5_Barplot_HabChange_Summer_Winter_SD.pdf"),
       width=3.8,height=5.5,
       units="in",dpi=400,family="Helvetica",
       grid.arrange(a,b,ncol=1))








#################################
# some statistics ...
#################################
summer$season="summer"
winter$season="winter"
tot = rbind(summer, winter)
tot = as_tibble(tot)
tot
summary(summer$mean[!(summer$species=="nar" & summer$side=="East")]) # -25%
mean(tot$mean[tot$season=="summer"])  # -25%
sd(tot$mean[tot$season=="summer"])    # 66
range(tot$mean[tot$season=="summer"]) # -88 to +105%

mean(tot$mean[tot$season=="winter"])  # 2.6%
sd(tot$mean[tot$season=="winter"])    # 47
range(tot$mean[tot$season=="winter"]) # -63 to +50%

