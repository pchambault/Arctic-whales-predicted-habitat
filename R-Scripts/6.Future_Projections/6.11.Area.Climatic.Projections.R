################################################################################
### calculate area of suitable habitat (proba>0.5) for each year of prediction #
###                SUMMER (Aug+Sep) and WINTER (Dec-Mar) - 6 variables, CMIP6
###                       from best algorithm
################################################################################

library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(maps)
library(stringr)
library(dplyr)   
library(trip)
library(tidyquant)
library(dplyr)
library(tidyverse)

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")



##########################################
#####           SUMMER            ########
##########################################
species  = c("Bel_West", "Bw_West","Nar_West", "Bw_East","Nar_East")
scenario = c("ssp126", "ssp585")
utm      = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
models   = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
runs     = seq(1,10,by=1)
runs     = as.character(runs)



#-----------------------------------
areaF2 = NULL
system.time({  # 54 sec
  for (i in species) { 
    for (model in models) {
      area1  = data.frame("species"=0,"side"=0,"area"=0,"lat"=0,"lon"=0,
                          "ssp"=0,"model"=0,"run"=0)
      
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      file = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                               "/RDATA/5.CARET/",i,"/Predictions/Summer/",
                               model), pattern=paste0("bestAlgo_",model))
      setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                   "/RDATA/5.CARET/",i,"/Predictions/Summer/",model))
      r = readRDS(file)
      
      
      ################################################
      # 1) calculate areas for contemporary period
      # for each run 
      ################################################
      for (n in runs) { 
        # retrieve name of layer to extract
          layer_to_extract = sapply(names(r), function(x) {
            ifelse(
              # split the string using "_run"
              strsplit(x, "_run") %>%
                # unlist the result
                unlist(.) %>%
                # keep the last item
                last(.) %>%
                # check if equal to n
                identical(., n),
              # if TRUE, return name
              x,
              # if not, return NA
              NA
            )
          })
          # remove NA
          layer_to_extract = layer_to_extract[!is.na(layer_to_extract)]
          # extraction layers of run n
          pred <- raster::subset(r, layer_to_extract)
          pred = calc(pred, fun=mean)
          pred[pred < 0.5] = NA
        
        # convert raster to dataframe for current data
        df = as_tibble(rasterToPoints(pred)) %>%
          dplyr::rename("pred" = "layer") %>%
          mutate("period" = "current") %>%
          filter(pred>0.5)
        
        # extract current area
        crs(pred) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        r_utm <- pred
        r_utm <- projectRaster(r_utm, crs=utm)
        poly = rasterToPolygons(r_utm)
        area1$area    = gArea(poly)  * 1e-06   # unit: km²
        area1$species = sub("_.*", "", i)
        area1$side    = sub(".*_", "", i) 
        area1$lat     = mean(df$y[df$pred>0.5])
        area1$lon     = mean(df$x[df$pred>0.5])
        area1$ssp     = "Present"
        area1$model   = model
        area1$run     = n
        
        
        
        
        
        ############################################################
        # 2) calculate areas for 2100, each run, ssp and model
        ############################################################
        areaF  = NULL
        area   = NULL
        area2  = data.frame("species"=0,"side"=0,"area"=0,"lat"=0,"lon"=0,
                            "ssp"=0,"run"=0)
        
        for (ssp in scenario) { 
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          file = list.files(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/",
                                   "Summer/",model,"_",ssp,"/"), 
                            pattern=paste0("_run",n,".rds"))
          setwd(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/",
                       "Summer/",model,"_",ssp,"/"))
          pred2100 = readRDS(file)
          pred2100 = calc(pred2100, fun=mean)
          pred2100[pred2100 < 0.5] = NA
          
          # convert raster to dataframe for future data
          df2100 = as_tibble(rasterToPoints(pred2100)) %>%
            dplyr::rename("pred" = "layer") %>%
            mutate("period" = "future") %>%
            filter(pred>0.5)
          
          # extract future area
          crs(pred2100) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          r_utm2 <- pred2100
          r_utm2 <- projectRaster(r_utm2, crs=utm)
          poly2 = rasterToPolygons(r_utm2)
          
          if(is.null(poly2)) {
            area2$area = 0
            } else{
            area2$area = gArea(poly2) * 1e-06
            }
          
          area2$species = sub("_.*", "", i)
          area2$side    = sub(".*_", "", i) 
          area2$lat     = mean(df2100$y)
          area2$lon     = mean(df2100$x)
          area2$ssp     = ssp
          area2$model   = model
          area2$run     = n
          
          area  = rbind(area, area2)  # rbind the 2 scenarios
          
        }   # loop over scenario
        areaF  = rbind(area1, area)
        areaF2 = rbind(areaF2, areaF)
        areaF2 = as_tibble(areaF2)
      }   # loop over each run
    }     # loop over models
  }       # loop over species
  
  setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
  saveRDS(areaF2, file=paste0("./RDATA/6.Climatic_projections/Area/Proba_0.5/",
                              "Area_probaSup0.5_Summer_0.25deg_3GCMs_3sp_perRun.rds"))
})
unique(areaF2$model)
table(areaF2$species)
table(areaF2$run)
table(areaF2$species, areaF2$side)
areaF2
areaF2 -> area
area$species = as.factor(area$species)
area$side    = as.factor(area$side)
area$side <- factor(area$side, levels=c("West","East"))









#################################
# plot area changes over time
#################################
ggplot(area, aes(y=area/1000, x=ssp)) +
  geom_boxplot(aes(colour=species)) +
  geom_point(aes(colour=species)) +
  labs(x="", y="Predicted habitat (*1000 km²)",title="Summer") +
  facet_grid(species~side, scales="free") +
  theme_tq() +
  scale_colour_manual(values=c("darkorchid","cadetblue4","coral2"), 
                      labels=c("Belugas","Bowheads","Narwhals")) +
  theme(axis.title.y = element_text(size=10,color="black"),
        axis.title.x = element_text(size=10,color="black"),
        axis.text.x  = element_text(size=7,angle=90,vjust=0.5,hjust=0.5,color="black"),
        axis.text.y  = element_text(size=8,color="black"),
        title = element_text(colour="black",size=7,face="bold"),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.background = element_blank(),
        legend.position="none",#c(0.1,0.1),
        panel.grid.major = element_line(colour="lightgrey", size=0.1),
        panel.grid.minor = element_line(colour="lightgrey", size=0.1),
        axis.line = element_line(colour="black", size=0.2),
        plot.margin = unit(c(0.1,0.1,0.05,0.1), "cm")) #(top,right,bottom,left)
ggsave(paste0("./FIGURES/CLIMATO/Area/",
              "PredHabitat_1993-2100_3sp_6var_summer_bestAlgo.png"),
       width=5.5,height=4,units="in",dpi=400)

ggplot(area, aes(y=lat, x=ssp)) + 
  geom_boxplot(aes(colour=species)) +
  geom_point(aes(colour=species)) +
  labs(x="", y="Latitude",title="Summer") +
  facet_grid(species~side, scales="free") +
  theme_bw() +
  scale_colour_manual(values=c("darkorchid","cadetblue4","coral2"), 
                      labels=c("Belugas","Bowheads","Narwhals")) +
  theme(axis.title.y = element_text(size=10,color="black"),
        axis.title.x = element_text(size=10,color="black"),
        axis.text.x  = element_text(size=7,angle=90,vjust=0.5,hjust=0.5,color="black"),
        axis.text.y  = element_text(size=8,color="black"),
        strip.background = element_rect(fill="gray23", size=0.1),
        strip.text = element_text(colour='white',size=10,margin=margin(0.1,0.1,0.1,0.1,"cm")),
        title = element_text(colour="black",size=7,face="bold"),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.background = element_blank(),
        legend.position="none",#c(0.1,0.1),
        panel.grid.major = element_line(colour="lightgrey", size=0.1),
        panel.grid.minor = element_line(colour="lightgrey", size=0.1),
        axis.line = element_line(colour="black", size=0.2),
        plot.margin = unit(c(0.1,0.1,0.05,0.1), "cm")) #(top,right,bottom,left)

ggsave(paste0("./FIGURES/CLIMATO/Area/PredLatitude_1993-2100_3sp_6var_",
              "Summer_bestAlgo.png"),
       width=5.5,height=4,units="in",dpi=400)











#########################################
# calculate % of change in area 
# between contemporary and future period
#########################################
rm(area1, area2, areaF, areaF2, df, df2100, pred, poly, poly2, pred2100)

# habitat change
#-----------------
hab = area %>%
  select(-c(lat,lon)) %>%
  group_by(species, side, model, run) %>%
  spread("ssp","area")
hab
area %>% filter(species == "Bel")

hab = hab %>%
  mutate(diff_ssp126 = ssp126 - Present,
         diff_ssp585 = ssp585 - Present)
hab = hab %>%
  mutate(prop_ssp126 = (diff_ssp126 / Present) * 100,
         prop_ssp585 = (diff_ssp585 / Present) * 100)
hab #  % increase = (difference ÷ original Number) × 100
summary(hab$prop_ssp126)
summary(hab$prop_ssp585)
hab

hab$species = recode_factor(hab$species, 
                            Bel = "Beluga", 
                            Bw  = "Bowhead",
                            Nar = "Narwhal")
summary(hab)
hab = hab[hab$prop_ssp585<500,]  # remove 2 outliers from ssp585
hab -> area_change

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
saveRDS(area_change, file=paste0("./RDATA/6.Climatic_projections/Area/",
                                 "Area_change_Summer_3sp_6var_3GCMs.rds"))




#----------------
# boxplot
#----------------
pivot = hab[,c("model","species","side","prop_ssp126","prop_ssp585","run")] %>%
  pivot_longer(!c(model,species,side,run), 
               names_to = "ssp", values_to = "prop") 
pivot$ssp = recode_factor(pivot$ssp, 
                          prop_ssp126 = "ssp126", 
                          prop_ssp585 = "ssp585")

ggplot(pivot, aes(y=prop, x=species, fill=ssp)) +
  geom_boxplot(outlier.shape=NA, fatten=0.8, lwd=0.3, width=0.5) +
  geom_jitter(shape=16, position=position_jitterdodge(dodge.width=0.9),
              size=0.3,aes(fill=ssp)) +
  facet_wrap(.~side) +
  # coord_flip() +
  geom_hline(yintercept=0, lty=2, lwd=0.2) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Habitat change (%)",title="a) Summer") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))
        # plot.margin = unit(c(0,0.1,0,0),"cm"))  #top,right,bottom,left



# barplot+SE
#--------------
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
summer$side = factor(summer$side, levels=c("West","East"))

ggplot(summer, aes(x=species, y=mean, fill=type)) + 
  geom_bar(stat="identity",position=position_dodge(),width=0.5) +
  facet_grid(ssp~side) +
  geom_hline(yintercept=0, lty=2, lwd=0.2) +
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
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(0.05,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=8),
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

# check inter-GCM variability for nar West
#-------------------------------------------
dat = hab %>%
  filter(species =="Narwhal" & side == "West") %>%
  select(c(species,side,model,run,prop_ssp126))
detach("package:plyr", unload = TRUE)
dat %>%
  group_by(model) %>%
  summarize(mean = mean(prop_ssp126),
            sd = sd(prop_ssp126),
            min = min(prop_ssp126),
            max = max(prop_ssp126))

# check inter-GCM variability for Bw East
#-------------------------------------------
dat = hab %>%
  filter(species =="Bowhead" & side == "East") %>%
  select(c(species,side,model,run,prop_ssp126,prop_ssp585))
dat %>%
  group_by(model) %>%
  summarize(mean_126 = mean(prop_ssp126),
            sd_126 = sd(prop_ssp126),
            mean_585 = mean(prop_ssp585),
            sd_585 = sd(prop_ssp585),)


# plot mean+errorbars
#------------------------
ggplot(data = pivot) + 
  stat_summary(
    mapping = aes(x = species, y = prop, colour=ssp),
    position=position_jitterdodge(dodge.width=0.6),
    fun.min = min,
    fun.max = max,
    fun = "mean") +
  facet_wrap(.~side) +
  scale_colour_manual(values=c("firebrick3","dodgerblue4")) +
  labs(x="", y="Habitat change (%)",title="a) Summer") +
  geom_hline(yintercept=0, lty=2, lwd=0.2) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Habitat change (%)",title="a) Summer") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))









#########################################
# latitudinal change
# between contemporary and future period
#########################################
lat = area %>%
  select(-c(area,lon)) %>%
  group_by(species, side, model,run) %>%
  spread("ssp","lat") 
lat

# convert into kilometers
lat$diff_ssp585 = NA
lat$diff_ssp126 = NA
for (i in 1: nrow(lat)) { 
  lat$diff_ssp126[i] = trackDistance(x1=-60, 
                                     x2=-60,
                                     y1=lat$Present[i],
                                     y2=lat$ssp126[i],
                                     longlat=TRUE) 
  lat$diff_ssp585[i] = trackDistance(x1=-60, 
                                     x2=-60,
                                     y1=lat$Present[i],
                                     y2=lat$ssp585[i],
                                     longlat=TRUE) 
}
lat

lat$species = recode_factor(lat$species, 
                            Bel = "Beluga", 
                            Bw  = "Bowhead",
                            Nar = "Narwhal")
summary(lat)
lat -> lat_change
saveRDS(lat_change, file=paste0("./RDATA/6.Climatic_projections/Area/",
                                "Lat_change_Summer_3sp_6var_3GCMs.rds"))

#----------------
# plot
#----------------
pivot = lat[,c("model","species","side","diff_ssp126","diff_ssp585","run")] %>%
  pivot_longer(!c(model,species,side,run), 
               names_to = "ssp", values_to = "km") 
pivot
pivot$ssp = recode_factor(pivot$ssp, 
                          diff_ssp126 = "ssp126", 
                          diff_ssp585 = "ssp585")

ggplot(pivot, aes(y=km, x=species, fill=ssp)) +
  geom_boxplot(outlier.shape=NA, fatten=0.8, lwd=0.3, width=0.5) +
  facet_wrap(.~side) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Latitudonal change (km)",title="Summer") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))


# plot mean+errorbars
#------------------------
ggplot(data = pivot) +
  stat_summary(
    mapping = aes(x = species, y = km, colour=ssp),
    position=position_jitterdodge(dodge.width=0.6),
    fun.min = min,
    fun.max = max,
    fun = "mean") +
  facet_wrap(.~side) +
  scale_colour_manual(values=c("dodgerblue4","firebrick3")) +
  labs(x="", y="Latitudinal change (km)",title="Summer") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))













##########################################
#####           WINTER            ########
##########################################
species  = c("Bw_West", "Nar_West", "Bw_East","Nar_East")
scenario = c("ssp126", "ssp585")
utm      = "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"
models   = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
runs = seq(1,10,by=1)
runs = as.character(runs)


#-----------------------------------
areaF2 = NULL
system.time({  # 58 sec
  for (i in species) { 
    for (model in models) {
      area1  = data.frame("species"=0,"side"=0,"area"=0,"lat"=0,"lon"=0,
                          "ssp"=0,"model"=0,"run"=0)
      
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      file = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                               "/RDATA/5.CARET/",i,"/Predictions/Winter/",
                               model), pattern=paste0("bestAlgo_",model))
      setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                   "/RDATA/5.CARET/",i,"/Predictions/Winter/",model))
      r = readRDS(file)
      
      
      ################################################
      # 1) calculate areas for contemporary period
      # for each run 
      ################################################
      for (n in runs) { 
        # retrieve name of layer to extract
        layer_to_extract = sapply(names(r), function(x) {
          ifelse(
            # split the string using "_run"
            strsplit(x, "_run") %>%
              # unlist the result
              unlist(.) %>%
              # keep the last item
              last(.) %>%
              # check if equal to n
              identical(., n),
            # if TRUE, return name
            x,
            # if not, return NA
            NA
          )
        })
        # remove NA
        layer_to_extract = layer_to_extract[!is.na(layer_to_extract)]
        # extraction layers of run n
        pred <- raster::subset(r, layer_to_extract)
        pred = calc(pred, fun=mean)
        pred[pred < 0.5] = NA
        
        # convert raster to dataframe for current data
        df = as_tibble(rasterToPoints(pred)) %>%
          dplyr::rename("pred" = "layer") %>%
          mutate("period" = "current") %>%
          filter(pred>0.5)
        
        # extract current area
        crs(pred) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        r_utm <- pred
        r_utm <- projectRaster(r_utm, crs=utm)
        poly = rasterToPolygons(r_utm)
        area1$area    = gArea(poly)  * 1e-06   # unit: km²
        area1$species = sub("_.*", "", i)
        area1$side    = sub(".*_", "", i) 
        area1$lat     = mean(df$y[df$pred>0.5])
        area1$lon     = mean(df$x[df$pred>0.5])
        area1$ssp     = "Present"
        area1$model   = model
        area1$run     = n
        
        
        
        
        
        ###########################################
        # 2) calculate areas for 2100 and each run
        ###########################################
        areaF  = NULL
        area   = NULL
        area2  = data.frame("species"=0,"side"=0,"area"=0,"lat"=0,"lon"=0,
                            "ssp"=0,"run"=0)
        
        for (ssp in scenario) { 
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          file = list.files(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/",
                                   "Winter/",model,"_",ssp,"/"), 
                            pattern=paste0("_run",n,".rds"))
          setwd(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/",
                       "Winter/",model,"_",ssp,"/"))
          pred2100 = readRDS(file)
          pred2100 = calc(pred2100, fun=mean)
          pred2100[pred2100 < 0.5] = NA
          
          # convert raster to dataframe for future data
          df2100 = as_tibble(rasterToPoints(pred2100)) %>%
            dplyr::rename("pred" = "layer") %>%
            mutate("period" = "future") %>%
            filter(pred>0.5)
          
          # extract future area
          crs(pred2100) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
          r_utm2 <- pred2100
          r_utm2 <- projectRaster(r_utm2, crs=utm)
          poly2 = rasterToPolygons(r_utm2)
          
          if(is.null(poly2)) {
            area2$area = 0
          } else{
            area2$area = gArea(poly2) * 1e-06
          }
          
          area2$species = sub("_.*", "", i)
          area2$side    = sub(".*_", "", i) 
          area2$lat     = mean(df2100$y)
          area2$lon     = mean(df2100$x)
          area2$ssp     = ssp
          area2$model   = model
          area2$run     = n
          
          area  = rbind(area, area2)  # rbind the 2 scenarios
          
        }   # loop over scenario
        areaF  = rbind(area1, area)
        areaF2 = rbind(areaF2, areaF)
        areaF2 = as_tibble(areaF2)
      }   # loop over each run
    }     # loop over models
  }       # loop over species
  
  setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
  saveRDS(areaF2, file=paste0("./RDATA/6.Climatic_projections/Area/",
                              "Area_probaSup0.5_Winter_0.25deg_3GCMs_2sp_perRun.rds"))
})
unique(areaF2$model)
table(areaF2$species)
table(areaF2$run)
table(areaF2$species, areaF2$side)
areaF2
areaF2 -> area
area$species = as.factor(area$species)
area$side    = as.factor(area$side)
area$side <- factor(area$side, levels=c("West","East"))









#################################
# plot area changes over time
#################################
area$species = as.factor(area$species)
area$side    = as.factor(area$side)
area$side <- factor(area$side, levels=c("West","East"))

ggplot(area, aes(y=area/1000, x=ssp)) +
  geom_boxplot(aes(colour=species)) +
  geom_point(aes(colour=species)) +
  labs(x="", y="Predicted habitat (*1000 km²)",
       title="Winter") +
  facet_grid(species~side, scales="free") +
  theme_tq() +
  scale_colour_manual(values=c("darkorchid","cadetblue4","coral2"), 
                      labels=c("Belugas","Bowheads","Narwhals")) +
  theme(axis.title.y = element_text(size=10,color="black"),
        axis.title.x = element_text(size=10,color="black"),
        axis.text.x  = element_text(size=7,angle=0,vjust=0.5,hjust=0.5,color="black"),
        axis.text.y  = element_text(size=8,color="black"),
        title = element_text(colour="black",size=7,face="bold"),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.background = element_blank(),
        legend.position="none",#c(0.1,0.1),
        panel.grid.major = element_line(colour="lightgrey", size=0.1),
        panel.grid.minor = element_line(colour="lightgrey", size=0.1),
        axis.line = element_line(colour="black", size=0.2),
        plot.margin = unit(c(0.1,0.1,0.05,0.1), "cm")) #(top,right,bottom,left)
ggsave(paste0("./FIGURES/CLIMATO/Area/",
              "PredHabitat_1993-2100_2sp_6var_Winter_bestAlgo.png"),
       width=5.5,height=4,units="in",dpi=400)

ggplot(area, aes(y=lat, x=ssp)) + 
  geom_boxplot(aes(colour=species)) +
  geom_point(aes(colour=species)) +
  labs(x="", y="Latitude",title="Winter") +
  facet_grid(species~side, scales="free") +
  theme_bw() +
  scale_colour_manual(values=c("darkorchid","cadetblue4","coral2"), 
                      labels=c("Belugas","Bowheads","Narwhals")) +
  theme(axis.title.y = element_text(size=10,color="black"),
        axis.title.x = element_text(size=10,color="black"),
        axis.text.x  = element_text(size=7,angle=90,vjust=0.5,hjust=0.5,color="black"),
        axis.text.y  = element_text(size=8,color="black"),
        strip.background = element_rect(fill="gray23", size=0.1),
        strip.text = element_text(colour='white',size=10,margin=margin(0.1,0.1,0.1,0.1,"cm")),
        title = element_text(colour="black",size=7,face="bold"),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        panel.background = element_blank(),
        legend.position="none",#c(0.1,0.1),
        panel.grid.major = element_line(colour="lightgrey", size=0.1),
        panel.grid.minor = element_line(colour="lightgrey", size=0.1),
        axis.line = element_line(colour="black", size=0.2),
        plot.margin = unit(c(0.1,0.1,0.05,0.1), "cm")) #(top,right,bottom,left)

ggsave(paste0("./FIGURES/CLIMATO/Area/PredLatitude_1993-2100_2sp_6var_",
              "Winter_bestAlgo.png"),
       width=5.5,height=4,units="in",dpi=400)











#########################################
# calculate % of change in area 
# between contemporary and future period
#########################################
rm(area1, area2, areaF, areaF2, df, df2100, pred, poly, poly2, pred2100)

# habitat change
#-----------------
hab = area %>%
  select(-c(lat,lon)) %>%
  group_by(species, side, model,run) %>%
  spread("ssp","area") 
hab
area %>% filter(species == "Nar")

hab = hab %>%
  mutate(diff_ssp126 = ssp126 - Present,
         diff_ssp585 = ssp585 - Present)
hab = hab %>%
  mutate(prop_ssp126 = (diff_ssp126 / Present) * 100,
         prop_ssp585 = (diff_ssp585 / Present) * 100)
hab #  % increase = (difference ÷ original Number) × 100

hab$species = recode_factor(hab$species, 
                            Bel = "Beluga", 
                            Bw  = "Bowhead",
                            Nar = "Narwhal")

summary(hab)
hab -> area_change
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
saveRDS(area_change, file=paste0("./RDATA/6.Climatic_projections/Area/",
                                 "Area_change_Winter_2sp_6var_3GCMs.rds"))




#----------------
# plot
#----------------
pivot = hab[,c("model","species","side","prop_ssp126","prop_ssp585","run")] %>%
  pivot_longer(!c(model,species,side,run), 
               names_to = "ssp", values_to = "prop") 
pivot$ssp = recode_factor(pivot$ssp, 
                          prop_ssp126 = "ssp126", 
                          prop_ssp585 = "ssp585")

ggplot(pivot, aes(y=prop, x=species, fill=ssp)) +
  geom_boxplot(outlier.shape=NA, fatten=0.8, lwd=0.3, width=0.5) +
  geom_point(aes(colour=ssp)) +
  facet_wrap(.~side) +
  # coord_flip() +
  geom_hline(yintercept=0, lty=2, lwd=0.2) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  scale_colour_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Habitat change (%)",title="Winter") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0,"cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))

# plot mean+errorbars
#------------------------
ggplot(data = pivot) + 
  stat_summary(
    mapping = aes(x = species, y = prop, colour=ssp),
    position=position_jitterdodge(dodge.width=0.6),
    fun.min = min,
    fun.max = max,
    fun = "mean") +
  facet_wrap(.~side) +
  scale_colour_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Habitat change (%)",title="a) Winter") +
  geom_hline(yintercept=0, lty=2, lwd=0.2) +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Habitat change (%)",title="a) Winter") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))








#########################################
# latitudinal change
# between contemporary and future period
#########################################
lat = area %>%
  select(-c(area,lon)) %>%
  group_by(species, side, model,run) %>%
  spread("ssp","lat") 
lat

# convert into kilometers
lat$diff_ssp585 = NA
lat$diff_ssp126 = NA
for (i in 1: nrow(lat)) { 
  lat$diff_ssp126[i] = trackDistance(x1=-60, 
                                     x2=-60,
                                     y1=lat$Present[i],
                                     y2=lat$ssp126[i],
                                     longlat=TRUE) 
  lat$diff_ssp585[i] = trackDistance(x1=-60, 
                                     x2=-60,
                                     y1=lat$Present[i],
                                     y2=lat$ssp585[i],
                                     longlat=TRUE) 
}
lat

lat$species = recode_factor(lat$species, 
                            Bw = "Bowhead",
                            Nar = "Narwhal")
lat -> lat_change
saveRDS(lat_change, file=paste0("./RDATA/6.Climatic_projections/Area/",
                                "Lat_change_Winter_3sp_6var_3GCMs.rds"))

#----------------
# plot
#----------------
pivot = lat[,c("model","species","side","diff_ssp126","diff_ssp585")] %>%
  pivot_longer(!c(model,species,side), 
               names_to = "ssp", values_to = "km") 
pivot
pivot$ssp = recode_factor(pivot$ssp, 
                          diff_ssp126 = "ssp126", 
                          diff_ssp585 = "ssp585")

ggplot(pivot, aes(y=km, x=species, fill=ssp)) +
  geom_boxplot(outlier.shape=NA, fatten=0.8, lwd=0.3, width=0.5) +
  facet_wrap(.~side) +
  # coord_flip() +
  scale_fill_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Latitudonal change (km)",title="") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))


# plot mean+errorbars
#------------------------
ggplot(data = pivot) +
  stat_summary(
    mapping = aes(x = species, y = km, colour=ssp),
    position=position_jitterdodge(dodge.width=0.6),
    fun.min = min,
    fun.max = max,
    fun = "mean") +
  facet_wrap(.~side) +
  scale_colour_manual(values=c("lightseagreen","firebrick3")) +
  labs(x="", y="Latitudinal change (km)",title="") +
  theme(strip.text.x = element_text(margin = margin(0,0,0,0, "cm")),
        panel.spacing = unit(0.1, "lines"),
        strip.background = element_rect(fill="steelblue4",size=0.2),
        strip.text = element_text(colour='white',size=10,face="bold",
                                  margin=margin(0.1,0.1,0.1,0.1,"cm")),
        axis.title = element_text(size=12,face="bold", hjust=0.5),
        panel.border = element_rect(colour="black",fill=NA,size=0.2),
        legend.key.size = unit(0.5,"line"),
        legend.key.width = unit(0.14,"cm"),
        legend.key.height = unit(0.3,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(0.1,'cm'),
        legend.margin=margin(t=-0.0, unit='cm'),
        legend.key = element_blank(),
        panel.spacing.x = unit(0.07, "lines"),
        panel.spacing.y = unit(0.07, "lines"),
        title = element_text(colour="black",size=13,face="bold"),
        plot.title=element_text(size=14, vjust=0, 
                                hjust=0, colour="black"),
        panel.background = element_blank(),
        panel.grid.major = element_line(colour="gray52", size=0.1))
