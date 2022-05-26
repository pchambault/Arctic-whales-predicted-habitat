########################################################
##############    SI Fig 2: perf metrics  ##############
########################################################


library(ggplot2)
library(gridExtra)
library(tidyquant)
library(tidyverse)




#########################################
######          SUMMER        ###########
#########################################

#--------
# AWI
#--------
# bel nnet, bwW: BRT, bwE: nnet, narW: BRT, narE: BRT
awi <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Summer_7algo_6var_3sp_AWI-CM-1-1-MR_.rds")
awi = awi %>%
  filter(model == "nnet" | model == "blackboost") %>%
  filter(!(model == "blackboost" & species == "Bel")) %>%
  filter(!(model == "nnet" & species == "Bw" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Bw" & side == "East")) %>%
  filter(!(model == "nnet" & species == "Nar" & side == "West")) %>%
  filter(!(model == "nnet" & species == "Nar" & side == "East")) %>%
  mutate("season" = "Summer", "gcm" = "AWI-CM-1-1-MR")

#---------
# CNRM
#---------
# bel nnet, bwW: BRT, bwE: BRT, narW: BRT, narE: nnet
cnrm <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Summer_7algo_6var_3sp_CNRM-CM6-1-HR_.rds")
cnrm = cnrm %>%
  filter(model == "nnet" | model == "blackboost") %>%
  filter(!(model == "blackboost" & species == "Bel")) %>%
  filter(!(model == "nnet" & species == "Bw" & side == "West")) %>%
  filter(!(model == "nnet" & species == "Bw" & side == "East")) %>%
  filter(!(model == "nnet" & species == "Nar" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Nar" & side == "East")) %>%
  mutate("season" = "Summer", "gcm" = "CNRM-CM6-1-HR")


#---------
# HAD
#---------
# bel: brt, bwW: brt, bwE: glm, narW: brt, narE: brt
had <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Summer_7algo_6var_3sp_HadGEM3-GC31-MM_.rds")
had = had %>%
  filter(model == "nnet" | model == "blackboost"  | model == "glmboost") %>%
  filter(!(model == "nnet" & model == "glmboost" & species == "Bel")) %>%
  filter(!(model == "nnet" & model == "glmboost" & species == "Bw" & side == "West")) %>%
  filter(!(model == "nnet" & model == "blackboost" & species == "Bw" & side == "East")) %>%
  filter(!(model == "nnet" & model == "glmboost" & species == "Nar" & side == "West")) %>%
  filter(!(model == "nnet" & model == "glmboost" & species == "Nar" & side == "East")) %>%
  mutate("season" = "Summer", "gcm" = "HadGEM3-GC31-MM")

summer = rbind(awi, cnrm, had)
summary(summer$accur)
summer %>%
  group_by(species, side) %>%
  summarise(mean = mean(accur),
            sd = sd(accur),
            min = min(accur),
            max = max(accur))









#########################################
######          Winter        ###########
#########################################

#--------
# AWI
#--------
# bwW: nnet, bwE: nnet, narW: BRT, narE: nnet
awi <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_AWI-CM-1-1-MR_.rds")
awi = awi %>%
  filter(model == "nnet" | model == "blackboost") %>%
  filter(!(model == "blackboost" & species == "Bw" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Bw" & side == "East")) %>%
  filter(!(model == "nnet" & species == "Nar" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Nar" & side == "East")) %>%
  mutate("season" = "Winter", "gcm" = "AWI-CM-1-1-MR")

#---------
# CNRM
#---------
# bwW: BRT, bwE: nnet, narW: brt, narE: nnet
cnrm <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_CNRM-CM6-1-HR_.rds")
cnrm = cnrm %>%
  filter(model == "nnet" | model == "blackboost") %>%
  filter(!(model == "nnet" & species == "Bw" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Bw" & side == "East")) %>%
  filter(!(model == "nnet" & species == "Nar" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Nar" & side == "East")) %>%
  mutate("season" = "Winter", "gcm" = "CNRM-CM6-1-HR")


#---------
# HAD
#---------
# bwW: nnet, bwE: nnet, narW: brt, narE: nnet
had <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_HadGEM3-GC31-MM_.rds")
had = had %>%
  filter(model == "nnet" | model == "blackboost") %>%
  filter(!(model == "blackboost" & species == "Bw" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Bw" & side == "East")) %>%
  filter(!(model == "nnet" & species == "Nar" & side == "West")) %>%
  filter(!(model == "blackboost" & species == "Nar" & side == "East")) %>%
  mutate("season" = "Winter", "gcm" = "HadGEM3-GC31-MM")

winter = rbind(awi, cnrm, had)
perf = rbind(summer, winter)

perf %>%
  group_by(species, side, season) %>%
  summarise(mean = mean(accur),
            sd = sd(sensi),
            min = min(sensi),
            max = max(sensi))

perf %>%
  group_by(species, side, season) %>%
  summarise(mean = mean(sensi),
            sd = sd(sensi),
            min = min(sensi),
            max = max(sensi))

perf %>%
  group_by(species, side, season) %>%
  summarise(mean = mean(speci),
            sd = sd(sensi),
            min = min(sensi),
            max = max(sensi))

sub = winter %>%
  filter(species == "Bw" & side == "West" & gcm ==  "AWI-CM-1-1-MR")
mean(sub$sensi)  # 0.56
sd(sub$sensi)    # 0.11

winter %>%
  group_by(species, side) %>%
  summarise(mean = mean(sensi),
            sd = sd(sensi),
            min = min(sensi),
            max = max(sensi))




#-----------------------------
# plot accuracy
#-----------------------------
library(Rmisc)
metric <- summarySE(perf, measurevar="accur", 
                  groupvars=c("species","side","gcm","season"))
metric$side = factor(metric$side, levels = c("West", "East"))
metric$species = recode_factor(metric$species, 
                        Bel = "Beluga", Bw = "Bowhead", Nar = "Narwhal")

a = ggplot(metric, aes(x=species, y=accur, colour=side)) + 
  geom_errorbar(aes(ymin=accur-sd, ymax=accur+sd), width=0, 
                position=position_dodge(0.5), lwd=0.3) +
  geom_line(position=position_dodge(0.5), lwd=0.3) +
  geom_point(position=position_dodge(0.5), size=0.3) +
  facet_grid(season~gcm) +
  ylim(0,1) +
  scale_fill_manual(values=c("darkorchid","coral2")) + 
  scale_colour_manual(values=c("darkorchid","coral2")) + 
  labs(x="", y="Accuracy",title="a) Accuracy") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=7,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.04,0.36),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.1,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.05,'cm'),
        legend.key = element_rect(fill = NA),
        legend.spacing.x = unit(0.03, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(t=-0.0, unit='cm'),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        axis.text  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 





#-----------------------------
# plot sensi
#-----------------------------
metric <- summarySE(perf, measurevar="sensi", 
                    groupvars=c("species","side","gcm","season"))
metric$side = factor(metric$side, levels = c("West", "East"))
metric$species = recode_factor(metric$species, 
                               Bel = "Beluga", Bw = "Bowhead", Nar = "Narwhal")

b = ggplot(metric, aes(x=species, y=sensi, colour=side)) + 
  # geom_boxplot(outlier.shape=NA, fatten=0.8, lwd=0.2, width=0.5) +
  geom_errorbar(aes(ymin=sensi-sd, ymax=sensi+sd), width=0, 
                position=position_dodge(0.5), lwd=0.3) +
  geom_line(position=position_dodge(0.5), lwd=0.3) +
  geom_point(position=position_dodge(0.5), size=0.3) +
  facet_grid(season~gcm) +
  ylim(0,1) +
  scale_fill_manual(values=c("darkorchid","coral2")) + # "cadetblue4",
  scale_colour_manual(values=c("darkorchid","coral2")) + # "cadetblue4",
  labs(x="", y="Sensitivity",title="b) Sensitivity") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=7,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.04,0.36),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.1,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.05,'cm'),
        legend.key = element_rect(fill = NA),
        legend.spacing.x = unit(0.03, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(t=-0.0, unit='cm'),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        axis.text  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 




#-----------------------------
# plot speci
#-----------------------------
metric <- summarySE(perf, measurevar="speci", 
                    groupvars=c("species","side","gcm","season"))
metric$side = factor(metric$side, levels = c("West", "East"))
metric$species = recode_factor(metric$species, 
                               Bel = "Beluga", Bw = "Bowhead", Nar = "Narwhal")

c = ggplot(metric, aes(x=species, y=speci, colour=side)) + 
  # geom_boxplot(outlier.shape=NA, fatten=0.8, lwd=0.2, width=0.5) +
  geom_errorbar(aes(ymin=speci-sd, ymax=speci+sd), width=0, 
                position=position_dodge(0.5), lwd=0.3) +
  geom_line(position=position_dodge(0.5), lwd=0.3) +
  geom_point(position=position_dodge(0.5), size=0.3) +
  facet_grid(season~gcm) +
  ylim(0,1) +
  scale_fill_manual(values=c("darkorchid","coral2")) + 
  scale_colour_manual(values=c("darkorchid","coral2")) + 
  labs(x="", y="Specificity",title="c) Specificity") +
  theme(panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=7,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=8, hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = c(0.04,0.36),
        legend.key.size = unit(0.1,"line"),
        legend.key.width = unit(0.1,"cm"),
        legend.key.height = unit(0.1,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=5),
        legend.box.spacing = unit(0.05,'cm'),
        legend.key = element_rect(fill = NA),
        legend.spacing.x = unit(0.03, 'cm'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.margin = margin(t=-0.0, unit='cm'),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        axis.text  = element_text(size=5, hjust=0.5),
        title = element_text(colour="black",size=10,face="bold"),
        plot.title=element_text(size=10, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(0.2,0.3,0.4,0.2),"cm"))  #top,right,bottom,left 


ggsave(filename=paste0("./PAPER/Science/2.Resubmission_March2022/",
                       "Figures/SI.Fig.2_Perf.Metrics.pdf"),
       width=4.8,height=6,
       units="in",dpi=400,family="Helvetica",
       grid.arrange(a,b,c,ncol=1))




