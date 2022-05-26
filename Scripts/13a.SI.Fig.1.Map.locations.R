##############################################
####  FIG.SI 1. Map of whales locations ######
##############################################


library(rgdal)
library(raster)
library(maps)
library(ggplot2)
library(ggmap)
library(dplyr)
library(gridExtra)
library(png)
library(cowplot)

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")



##############################
# load dataset
##############################

# Winter #
#-------------
bwW  = readRDS("./RDATA/4.Pseudo-abs/Winter/AWI-CM-1-1-MR/abs_Winter_Indiv_6var_AWI-CM-1-1-MR_Bw_West.rds")
bwE  = readRDS("./RDATA/4.Pseudo-abs/Winter/AWI-CM-1-1-MR/abs_Winter_Indiv_6var_AWI-CM-1-1-MR_Bw_East.rds")
narW = readRDS("./RDATA/4.Pseudo-abs/Winter/AWI-CM-1-1-MR/abs_Winter_Indiv_6var_AWI-CM-1-1-MR_Nar_West.rds")
narE = readRDS("./RDATA/4.Pseudo-abs/Winter/AWI-CM-1-1-MR/abs_Winter_Indiv_6var_AWI-CM-1-1-MR_Nar_East.rds")

winter = rbind(bwW, bwE, narW, narE)
table(winter$month)
str(winter)
winter$species = as.factor(winter$species)
winter$side    = as.factor(winter$side)
winter = winter %>%
  filter(run == 1 & pres == 1)
dim(winter)    # 45 072
table(winter$species, winter$side)
length(unique(winter$id[winter$side=="West" & winter$species=="Bw"]))    # 9 whales
length(unique(winter$id[winter$side=="East" & winter$species=="Bw"]))    # 10 whales
length(unique(winter$id[winter$side=="West" & winter$species=="Nar"]))   # 28 whales
length(unique(winter$id[winter$side=="East" & winter$species=="Nar"]))   # 36 whales
winter$side <- factor(winter$side, levels=c("West","East"))
length(unique(winter$id))   # 83 whales in total in winter
rm(bwW, bwE, narW, narE)


# Summer #
#-------------
bwW = readRDS("./RDATA/4.Pseudo-abs/Summer/AWI-CM-1-1-MR/abs_Summer_Indiv_6var_AWI-CM-1-1-MR_Bw_West.rds")
bwE = readRDS("./RDATA/4.Pseudo-abs/Summer/AWI-CM-1-1-MR/abs_Summer_Indiv_6var_AWI-CM-1-1-MR_Bw_East.rds")
narW = readRDS("./RDATA/4.Pseudo-abs/Summer/AWI-CM-1-1-MR/abs_Summer_Indiv_6var_AWI-CM-1-1-MR_Nar_West.rds")
narE = readRDS("./RDATA/4.Pseudo-abs/Summer/AWI-CM-1-1-MR/abs_Summer_Indiv_6var_AWI-CM-1-1-MR_Nar_East.rds")
bel = readRDS("./RDATA/4.Pseudo-abs/Summer/AWI-CM-1-1-MR/abs_Summer_Indiv_6var_AWI-CM-1-1-MR_Bel_West.rds")
narW$species = recode_factor(narW$species, "nar"  = "Nar")
unique(narW$species)

summer = rbind(bwW, bwE, narW, narE, bel)
summer = summer %>%
  filter(run == 1 & pres == 1) 
dim(summer)   # 87 837 rows
str(summer)
summer$species = as.factor(summer$species)
summer$side    = as.factor(summer$side)
table(summer$species, summer$side)

length(unique(summer$id[summer$side=="West" & summer$species=="Bw"]))    # 56 whales
length(unique(summer$id[summer$side=="East" & summer$species=="Bw"]))    # 12 whales
length(unique(summer$id[summer$side=="West" & summer$species=="Nar"]))   # 69 whales
length(unique(summer$id[summer$side=="East" & summer$species=="Nar"]))   # 54 whales
length(unique(summer$id[summer$species=="Bel"]))                         # 29 whales

summer$side <- factor(summer$side, levels=c("West","East"))
length(unique(summer$id))   # 218 individuals in total in summer

loc = rbind(summer, winter)
length(unique(loc$id))      # 227 individuals in total
length(unique(loc$year))    # 28 years
loc %>%
  group_by(species) %>%
  summarise(n=n_distinct(id))

table(summer$species, summer$side)

summer %>%
  group_by(species, side) %>%
  summarise(n=n_distinct(id))



#-------------------
table(winter$species, winter$side)

winter %>%
  group_by(species, side) %>%
  summarise(n=n_distinct(id))



# summer + winter
#------------------
summer$season = "Summer"
winter$season = "Winter"
tot = rbind(summer, winter)
length(unique(tot$id))   
tot %>%
  group_by(species) %>%
  summarise(n=n_distinct(id))


#-------------------------
# timeframe in summer
#-------------------------
# Bw West : 2002-2019
# Bw East : 2017-2019
# Nar West : 2010-2019
# Nar East : 1993-2012
# Bel West : 1995-2001
i = "Bel"
j = "West"
dat = summer[summer$species==i & summer$side==j,]
dat$year = as.numeric(as.character(dat$year))
head(dat$year[order(dat$year)], 1)
tail(dat$year[order(dat$year)], 1)


#-------------------------
# timeframe in winter
#-------------------------
# Bw West : 2005-2011
# Bw East : 2017-2020
# Nar West : 1994-2009
# Nar East : 2010-2020
i = "Nar"
j = "West"
dat = winter[winter$species==i & winter$side==j,]
dat$year = as.numeric(as.character(dat$year))
head(dat$year[order(dat$year)], 1)
tail(dat$year[order(dat$year)], 1)










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







#####################
# import png images
#####################
png_bel <- readPNG('./FIGURES/Illustrations/Beluga_background.png')
png_bw  <- readPNG('./FIGURES/Illustrations/bowhead_transaprent.png')
png_nar <- readPNG('./FIGURES/Illustrations/narwhal2-removebg-preview.png')









##################################################
# plot
##################################################

#-----------------------
## summer ##
#-----------------------
length(unique(summer$id[summer$species=="Bw"]))    # 68 whales
length(unique(summer$id[summer$species=="Nar"]))   # 123 whales
length(unique(summer$id[summer$species=="Bel"]))   # 29 whales
length(unique(winter$id[winter$species=="Bw"]))    # 19 whales
length(unique(winter$id[winter$species=="Nar"]))   # 64 whales
text <- data.frame(long    = c(50,50,50),
                   lat     = c(61,61,61),
                   lab     = c("n=29","n=68","n=123"),
                   species = c("Bel","Bw","Nar"))


sp.labs <- c("Beluga","Bowhead","Narwhal")
names(sp.labs) <- c("Bel","Bw","Nar")
side.labs <- c("West", "East")
names(side.labs) <- c("West","East")

a = ggplot(shore, aes(long, lat)) +
  coord_map("azequidistant", xlim=c(-105,75), ylim=c(55,80)) +
  geom_point(data=tot,aes(x,y,colour=species), stroke=0, shape=16, size=0.35) + 
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  scale_y_continuous("Latitude (°N)", breaks=c(60,70,80), labels=c(60,70,80)) +
  scale_colour_manual(values=c("darkorchid","cadetblue4","coral2"),name="Species",
                      labels=c("Beluga","Bowhead","Narwhal")) +
  facet_grid(season~species, 
             labeller=labeller(species=sp.labs, side=side.labs)) +
  geom_text(data=text, aes(label=lab), size=1.8) + 
  # colour=c("darkorchid","cadetblue4","coral2")) +
  labs(y="Latitude (°N)",x="",title="", colour="Species") +
  annotate("text", x=-18.5,y=62, label="IS", colour="black",size=2) +
  annotate(geom="text", x=-42,  y=72, label="Greenland", angle=45, size=2,colour="black") +
  annotate(geom="text", x=-106, y=61, label="Canada", angle=0, size=2,colour="black") +
  annotate(geom="text", x=69,   y=61, label="Russia", angle=0, size=2,colour="black") +
  annotate("text", x=33, y=75, label="SVB", colour="black",size=2) +
  annotate("text", x=-6, y=61, label="60°N", colour="gray23",size=1.2, angle=5) +
  annotate("text", x=-6, y=71, label="70°N", colour="gray23",size=1.2, angle=5) +
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(),
        strip.background = element_rect(fill="steelblue4", size=0.1),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.position = "none",  
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, hjust=0),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(-0.6,0.1,-0.8,-0.4),"cm")) #top,right,bottom,left



a2 = ggdraw() +
  draw_plot(a) +
  draw_image(png_bel, x=-0.24,y=-0.30, scale=.37) +
  draw_image(png_bw,  x=0.08, y=-0.30, scale=.17) +
  draw_image(png_nar, x=0.41, y=-0.27, scale=.22)


ggsave(filename="./PAPER/Science/2.Resubmission_March2022/Figures/SI.Fig.1.Map.Locs.3species.pdf",
       width=6,height=3.8,units="in",dpi=400,family="Helvetica",a)
