##########################################################
#### FIG.3: maps of predictions in Winter          #######
##########################################################


library(dplyr)
library(here)
library(raster)
library(viridis)
library(maps)
library(fields)
library(ggplot2)
library(gridExtra)
library(png)
library(cowplot)



########################
# import png images
########################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
png_bel <- readPNG('./FIGURES/Illustrations/Beluga_background.png')
png_bw  <- readPNG('./FIGURES/Illustrations/bowhead_transaprent.png')
png_nar <- readPNG('./FIGURES/Illustrations/narwhal2-removebg-preview.png')





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







########################################
# load rasters in Winter from best algo
########################################

#-----------------------------------
# current predictions from MEAN
#-----------------------------------
bwW  <- readRDS("./RDATA/5.CARET/Mean_pred_3GCM/Winter/PredMean_3GCMs_ffs_6var_Winter_Bw_West.rds")
bwE  <- readRDS("./RDATA/5.CARET/Mean_pred_3GCM/Winter/PredMean_3GCMs_ffs_6var_Winter_Bw_East.rds")
narW <- readRDS("./RDATA/5.CARET/Mean_pred_3GCM/Winter/PredMean_3GCMs_ffs_6var_Winter_Nar_West.rds")
narE <- readRDS("./RDATA/5.CARET/Mean_pred_3GCM/Winter/PredMean_3GCMs_ffs_6var_Winter_Nar_east.rds")


#--------------------------------------
# predictions in Winter 2100 ssp126
#--------------------------------------
bwW_2100_126  <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Bw_West_ssp126.rds")
bwE_2100_126  <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Bw_East_ssp126.rds")
narW_2100_126 <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Nar_West_ssp126.rds")
narE_2100_126 <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Nar_East_ssp126.rds")


#--------------------------------------
# predictions in Winter 2100 ssp585
#--------------------------------------
bwW_2100_585  <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Bw_West_ssp585.rds")
bwE_2100_585  <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Bw_East_ssp585.rds")
narW_2100_585 <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Nar_West_ssp585.rds")
narE_2100_585 <- readRDS("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/ProjMean_3GCMs_ffs_6var_Winter2100_Nar_East_ssp585.rds")







###############################################
# conver rasters to tibble
###############################################

#--------------------------------
# bowheads in Winter
#--------------------------------
bwW     <-  as_tibble(rasterToPoints(bwW))
bwW$ssp = "Present"
bwW2100_126 <-  as_tibble(rasterToPoints(bwW_2100_126))
bwW2100_126$ssp = "2100 (ssp126)"
bwW2100_585 <-  as_tibble(rasterToPoints(bwW_2100_585))
bwW2100_585$ssp = "2100 (ssp585)"
west_bw      = rbind(bwW, bwW2100_126, bwW2100_585)
west_bw
west_bw$ssp = factor(west_bw$ssp, 
                     levels=c("Present","2100 (ssp126)",
                              "2100 (ssp585)"))
west_bw$side = "West"
west_bw$sp   = "Bowheads"
west_bw$sp   = factor(west_bw$sp)
west_bw$side = factor(west_bw$side)

bwE     <-  as_tibble(rasterToPoints(bwE))
bwE$ssp = "Present"
bwE2100_126 <-  as_tibble(rasterToPoints(bwE_2100_126))
bwE2100_126$ssp = "2100 (ssp126)"
bwE2100_585 <-  as_tibble(rasterToPoints(bwE_2100_585))
bwE2100_585$ssp = "2100 (ssp585)"
east_bw      = rbind(bwE, bwE2100_126, bwE2100_585)
east_bw
east_bw$ssp = factor(east_bw$ssp, 
                     levels=c("Present","2100 (ssp126)",
                              "2100 (ssp585)"))
east_bw$side = "East"
east_bw$sp   = "Bowheads"
east_bw$sp   = factor(east_bw$sp)
east_bw$side = factor(east_bw$side)



#--------------------------------
# narwhals in Winter
#--------------------------------
narW     <-  as_tibble(rasterToPoints(narW))
narW$ssp = "Present"
narW2100_126 <-  as_tibble(rasterToPoints(narW_2100_126))
narW2100_126$ssp = "2100 (ssp126)"
narW2100_585 <-  as_tibble(rasterToPoints(narW_2100_585))
narW2100_585$ssp = "2100 (ssp585)"
west_nar      = rbind(narW, narW2100_126, narW2100_585)
west_nar
west_nar$ssp = factor(west_nar$ssp,
                      levels=c("Present","2100 (ssp126)",
                               "2100 (ssp585)"))
west_nar$side = "West"
west_nar$sp   = "Narwhals"
west_nar$sp   = factor(west_nar$sp)
west_nar$side = factor(west_nar$side)

narE     <-  as_tibble(rasterToPoints(narE))
narE$ssp = "Present"
narE2100_126 <-  as_tibble(rasterToPoints(narE_2100_126))
narE2100_126$ssp = "2100 (ssp126)"
narE2100_585 <-  as_tibble(rasterToPoints(narE_2100_585))
narE2100_585$ssp = "2100 (ssp585)"
east_nar      = rbind(narE, narE2100_126, narE2100_585)
east_nar
east_nar$ssp = factor(east_nar$ssp,
                      levels=c("Present","2100 (ssp126)",
                               "2100 (ssp585)"))
east_nar$side = "East"
east_nar$sp   = "Narwhals"
east_nar$sp   = factor(east_nar$sp)
east_nar$side = factor(east_nar$side)



# combine 2 species per side
#----------------------------
west = rbind(west_bw, west_nar)
east = rbind(east_bw, east_nar)


bw = ggplot(shore, aes(long, lat)) +
  geom_contour_filled(data=west[west$sp=="Bowheads",], 
                      aes(x,y,z=layer), breaks=seq(0,1,by=0.2)) +
  geom_contour_filled(data=east[east$sp=="Bowheads",], 
                      aes(x,y,z=layer), breaks=seq(0,1,by=0.2)) +
  scale_fill_brewer(palette="BuPu") + 
  coord_map("azequidistant", xlim=c(-105,75), ylim=c(60,80)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_grid(sp~ssp, switch='y') +
  annotate("text", x=-18.5,y=62, label="IS", colour="black",size=2) +
  annotate(geom="text", x=-42,  y=72, label="Greenland", angle=45, size=2,colour="black") +
  annotate(geom="text", x=65, y=63, label="Russia", angle=-50, size=2,colour="black") +
  annotate("text", x=-70,y=67, label="BA", colour="black",size=2, angle=45) +
  annotate("text", x=33, y=74, label="SVB", colour="black",size=2) +
  labs(y="",x="",fill="Habitat \nsuitability",title="WINTER") + 
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(),
        strip.background = element_rect(fill="steelblue4", size=0.1),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, 
                                hjust=0.5, colour="steelblue4"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(-1.2,0.1,-1.9,-0.4),"cm")) #top,right,bottom,left



nar = ggplot(shore, aes(long, lat)) +
  geom_contour_filled(data=west[west$sp=="Narwhals",],
                      aes(x,y,z=layer), breaks=seq(0,1,by=0.2)) +
  geom_contour_filled(data=east[east$sp=="Narwhals",],
                      aes(x,y,z=layer), breaks=seq(0,1,by=0.2)) +
  scale_fill_brewer(palette="BuPu") +
  coord_map("azequidistant", xlim=c(-103,-20), ylim=c(60,80)) +
  scale_y_continuous(breaks=c(60,70), labels=c(60,70)) +
  geom_polygon(aes(group=group), fill="lightgrey",lwd=1) +
  facet_grid(sp~ssp, switch='y') +
  annotate("text", x=-18.5,y=62, label="IS", colour="black",size=2) +
  annotate(geom="text", x=-42,  y=72, label="Greenland", angle=65, size=2,colour="black") +
  annotate(geom="text", x=-110, y=65, label="Canada", angle=-50, size=2,colour="black") +
  annotate("text", x=-70,y=67, label="BA", colour="black",size=2, angle=45) +
  labs(y="",x="",fill="Habitat \nsuitability",title="") + 
  theme(axis.text.y  = element_blank(),
        axis.text.x  = element_blank(),
        axis.ticks   = element_blank(),
        strip.background.x = element_blank(),
        strip.background.y = element_rect(fill="steelblue4", size=0.1),
        strip.text.x = element_blank(),
        strip.text.y = element_text(colour='white',size=10,face="bold",
                                    margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_text(size=7),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=14,face="bold"),
        plot.title=element_text(size=14, vjust=-0.5, hjust=0),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1),
        plot.margin = unit(c(-2.1,0.1,-1.5,-0.4),"cm")) #top,right,bottom,left

b1 = grid.arrange(bw, nar, ncol=1)

b2 = ggdraw() +
  draw_plot(b1) +
  
  draw_image(png_bw, x=-0.24, y=0.05, scale=.14) +
  draw_image(png_bw, x=0.04,  y=0.05, scale=.14) +
  draw_image(png_bw, x=0.33,  y=0.05, scale=.14) +
  
  draw_image(png_nar, x=-0.25, y=-0.34, scale=.18) +
  draw_image(png_nar, x=0.04,  y=-0.34, scale=.18) +
  draw_image(png_nar, x=0.32,  y=-0.34, scale=.18)

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
ggsave(filename="./PAPER/Science/2.Resubmission_March2022/Figures/Fig.2.Winter.pdf",
       width=6,height=2.7,units="in",dpi=400,family="Helvetica",b2)



