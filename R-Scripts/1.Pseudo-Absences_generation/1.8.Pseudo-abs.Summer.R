########################################################
####  pseudo-absences BOWHEADS NARWHALS and BELUGAS ####
####       absences outside env convex hull         ####
####    1 PCA per year / ID in Summer (Aug to Sep) 
####       resolution: 0.25 degrees
####  6 var: bathy + SST + dist + SSS + SSH + MLD   ####
########################################################

library(rgdal)
library(raster)
library(ggplot2)
library(fields)
library(rgeos)
library(trip)
library(ade4)
library(viridis)
library(doParallel)
library(foreach)
library(parallel)
library(dplyr)
library(data.table)
library(weanlingNES) # use function print(.n()) to identify when error with 1 element of the list



setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
rm(list=ls())

# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
model  = models[[1]]  # run teh script separately for each model






##############################################################
# Belugas and Bowheads WEST 
##############################################################
sp = c("Bel","Bw","Nar")
j  = "West"

#-------------------------------------------------
# load and prepare data for species i Est side
#-------------------------------------------------
system.time({   # 267 sec
  for (i in sp) { 
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    loc = readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",i,"_",j,
                         "_withoutNA_0.25deg.rds"))
    loc$id   = as.factor(loc$id)
    loc$id   = droplevels(loc$id)
    loc$yymm = substr(loc$dateTime,1,7) 
    loc$pres = 1
    loc$year = as.factor(as.numeric(loc$year))
    loc$year = droplevels(loc$year)
    loc$id   = droplevels(loc$id)
    loc = loc[,c("id","lon","lat","pres","year","month")]
    loc$month = as.character(as.numeric(loc$month))
    loc$month[loc$month=="8"] = "08"
    loc$month[loc$month=="9"] = "09"
    loc$month = as.factor(loc$month)
    loc$month = droplevels(loc$month)
    
    # remove ids with too few locations:
    # Nar West: "1999MM6.08", "2007.2007MM10.08", "2008.2008MM4.08", "1997.1997MM2.09"
    # "2016.deploy_37227.09", "2015.deploy_3965.09"
    #---------------------------------------------------------------------------------
    loc = loc[!(loc$id=="1999MM6" & loc$month=="08"),]
    loc = loc[!(loc$id=="2007MM10" & loc$month=="08"),]
    loc = loc[!(loc$id=="2008MM4" & loc$month=="08"),]
    loc = loc[!(loc$id=="1997MM2" & loc$month=="09"),]
    loc = loc[!(loc$id=="deploy_37227" & loc$month=="09"),]
    loc = loc[!(loc$id=="deploy_3965" & loc$month=="09"),]
    loc = loc[!(loc$id=="1999MM2" & loc$month=="08"),]
    loc = loc[!(loc$id=="1999MM3" & loc$month=="08"),]
    loc = loc[!(loc$id=="1999MM4" & loc$month=="08"),]
    loc = loc[!(loc$id=="1999MM5" & loc$month=="08"),] 
    loc = loc[!(loc$id=="1999MM7" & loc$month=="08"),] 
    
    
    #----------------------------------
    # remove IDs with less than 50 locs
    #-----------------------------------
    loc2 = loc %>% 
      group_by(id) %>% 
      filter(n() > 50)
    loc2$year  = droplevels(loc2$year)  # table(loc2$year)
    loc2$id    = droplevels(loc2$id)    # table(loc2$id)
    loc2$month = droplevels(loc2$month)
    
    
    #-----------------------------
    # split over each year and ID
    #-----------------------------
    year = split(loc2,list(loc2$year, loc2$id, loc2$month))  # length(year) head(abs)
    year2  <- Filter(nrow, year)                 # remove all empty tibbles
    year2  <- year2[sapply(year2, nrow)>50]      # remove ID with < 50 locs
    
    
    
    #-------------------------------------------------------
    # generate pseudo-absences over each year, month and ID
    #-------------------------------------------------------
    result = lapply(year2, function(x){
      print(.n())
      x$yymm = paste0(x$year,"-",x$month)
      d      = unique(x$yymm)
      x$id   = droplevels(x$id)
      x$year = droplevels(x$year)
      
      # load PCA objects
      #-------------------
      ENVspace = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                "/PCA/Summer/ENVspace_Summer_",model,"_",j,"_",d,".rds"))
      ENVspace = na.omit(ENVspace)
      ordi_pix = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                "/PCA/Summer/OrdiPix_Summer_",model,"_",j,"_",d,".rds"))
      ordi2    = brick(paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                              "/PCA/Summer/Ordi_Summer_",model,j,"_",d,".tif"))
      xy       = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                "/PCA/Summer/xy_rasterBrick_Summer_",model,"_",j,"_",d,".rds"))
      pts      = SpatialPointsDataFrame(data=ENVspace, coords=ENVspace[,c("Axis1","Axis2")])
      
      
      
      # spatialPoints of the individual
      #-------------------------------
      id = x[,c("id","lon","lat")]
      coordinates(id) = ~lon+lat
      
      
      # load env raster stack
      #-------------------------
      ras = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                           "/Env_raster/Summer/Env_raster_6var_",j,"_",model,"_",
                           d,".rds"))
      names(ras) = c("sst","ssh","sss","mld","bathy","dist")  
      
      
      # extract cell numbers
      #----------------------
      idx = raster::extract(ordi2,id,cellnumbers=T)[,"cells"]   # extract cell number at each whales loc
      
      
      # fix environmental constraint:
      # generate new object containing values of axis1 and axis 2
      # limit values between >quantiles(0.05) and < quantiles(0.99)
      #---------------------------------------------------------------
      occ = as.data.frame(values(ordi2)[idx,])   # 0.21 sec, dataframe containing 2 columns: Axis1, Axis2
      occ = na.omit(occ)
      
      
      # calculate convex hull of both axes
      #-----------------------------------
      names(occ) = c("Axis1","Axis2")
      coordinates(occ) = ~ Axis1+Axis2
      ch = gConvexHull(occ)   
      # plot(occ,axes=T)
      # plot(ch,add=T,border="green",lwd=2)
      
      
      # determine if locs inside convex hull (=0) or outside (=1)
      #-----------------------------------------------------------
      # extract convex hull values at each values of ordi
      val = over(pts,ch)
      ENVspace2 <- ENVspace
      ENVspace2$ConvE = val     
      ENVspace2$ConvE[ENVspace2$ConvE==1]     = 0    # inside
      ENVspace2$ConvE[is.na(ENVspace2$ConvE)] = 1    # outside, table(ENVspace2$ConvE)
      
      
      # convert back to spatial object
      #---------------------------------
      coordinates(ENVspace2) = ~Axis1+Axis2
      # plot(ENVspace2,add=T,col="red",pch=20)
      
      
      # identify locs outside convex hull in ordi
      #--------------------------------------------
      ordi3 <- ordi_pix
      ordi3@data <- data.frame(ordi3,ConvE=0)        # table(ordi3@data$ConvE)
      ordi3@data[which(ENVspace2@data$ConvE==1),"ConvE"] <- 1 # 1=locs outside convex hull,# table(ordi3@data$ConvE)
      ordi3 <- ordi3@data                          
      
      
      # extract locs outside environmental convex hull
      #-----------------------------------------------
      abs = ordi3[ordi3$ConvE==1,] 
      
      
      # generate pseudo-abs 10 times
      #------------------------------
      pseudo = data.frame("x"=0,"y"=0,"pres"=0,"run"=0,"year"=0,"month"=0,
                          "species"= 0,"side"=0,"id"=0)
      pseudo = abs[sample(1:nrow(abs),nrow(x)*10,replace=T),]
      
      # merge presence with absence locations
      #---------------------------------------
      pseudo$pres   = 0
      pseudo$run    = rep(c(1:10), each = nrow(x))
      pseudo$year   = unique(x$year)
      pseudo$month  = unique(x$month)
      pseudo$species= i
      pseudo$side   = j
      pseudo$id     = unique(x$id)
      
      x$species  = i
      x$side     = j
      colnames(x)[2] = "x"
      colnames(x)[3] = "y"
      x2 = data.frame(do.call("rbind", replicate(10, x, simplify=FALSE)))
      x2$run = rep(c(1:10), each = nrow(x))
      
      pseudo = rbind(pseudo[,c("x","y","pres","run","year","species","side","id","month")],
                     x2[,c("x","y","pres","run","year","species","side","id","month")])
      
      
      
      #--------------------------------
      # extract unscaled env variables 
      #--------------------------------
      pseudo$sst_U   = raster::extract(ras[[1]],pseudo[,c("x","y")])
      pseudo$ssh_U   = raster::extract(ras[[2]],pseudo[,c("x","y")])
      pseudo$sss_U   = raster::extract(ras[[3]],pseudo[,c("x","y")])
      pseudo$mld_U   = raster::extract(ras[[4]],pseudo[,c("x","y")])
      pseudo$bathy_U = raster::extract(ras[[5]],pseudo[,c("x","y")])
      pseudo$dist_U  = raster::extract(ras[[6]],pseudo[,c("x","y")])
      
      
      # extract nearest pixel when env value = NA
      #------------------------------------------------
      if(any(is.na(pseudo$sst_U))) {
        pseudo$sst_U[is.na(pseudo$sst_U)] = raster::extract(ras[[1]],pseudo[is.na(pseudo$sst_U),c("x","y")],
                                                            buffer=30000,     # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  } 
      
      if(any(is.na(pseudo$ssh_U))) {
        pseudo$ssh_U[is.na(pseudo$ssh_U)] = raster::extract(ras[[2]],pseudo[is.na(pseudo$ssh_U),c("x","y")],
                                                            buffer=30000, # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  }
      
      if(any(is.na(pseudo$sss_U))) {
        pseudo$sss_U[is.na(pseudo$sss_U)] = raster::extract(ras[[3]],pseudo[is.na(pseudo$sss_U),c("x","y")],
                                                            buffer=30000, # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  }
      
      if(any(is.na(pseudo$mld_U))) {
        pseudo$mld_U[is.na(pseudo$mld_U)] = raster::extract(ras[[4]],pseudo[is.na(pseudo$mld_U),c("x","y")],
                                                            buffer=30000, # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  }
      if(any(is.na(pseudo$bathy_U))) {
        pseudo$bathy_U[is.na(pseudo$bathy_U)] = raster::extract(ras[[5]],pseudo[is.na(pseudo$bathy_U),c("x","y")],
                                                                buffer=30000, # units in meters (30 km radius)
                                                                fun=function(x)mean(x,na.rm=TRUE))  }
      
      if(any(is.na(pseudo$dist_U))) {
        pseudo$dist_U[is.na(pseudo$dist_U)] = raster::extract(ras[[6]],pseudo[is.na(pseudo$dist_U),c("x","y")],
                                                              buffer=30000,   # units in meters (30 km radius)
                                                              fun=function(x)mean(x,na.rm=TRUE))  }
      
      
      
      return(pseudo)
    }) 
    final = data.table::rbindlist(result)
    final = as_tibble(final)
    
    # save absences for each species and each side
    #-----------------------------------------------
    saveRDS(final,file=paste0("./RDATA/4.Pseudo-abs/Summer/",model,"/",
                              "abs_Summer_Indiv_6var_",model,"_",i,"_", j,".rds"))
    gc()
  }
})

summary(final)















##############################################################
# Nar and Bw East - Summer
##############################################################
sp = c("Bw","Nar")
j  = "East"

#-------------------------------------------------
# load and prepare data for species i
#-------------------------------------------------
system.time({   # 2 min
  for (i in sp) { 
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    loc = readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",i,"_",j,
                         "_withoutNA_0.25deg.rds"))
    loc$id   = as.factor(loc$id)
    loc$id   = droplevels(loc$id)
    loc$pres = 1
    loc$year = as.factor(as.numeric(loc$year))
    loc$year = droplevels(loc$year)
    loc$id   = droplevels(loc$id)
    loc = loc[,c("id","lon","lat","pres","year","month")]
    loc$month = as.character(as.factor(loc$month))
    loc$month[loc$month=="8"] = "08"
    loc$month[loc$month=="9"] = "09"
    loc$month = as.factor(loc$month)
    loc$month = droplevels(loc$month)
    
    
    # issue when generating occ object (only NA)
    # because too much within the fjord so no env data associated in Aug
    # Nar East: "2012.deploy_21792.08", "2012.deploy_24638.08", "2016.deploy_37227.08"
    # "2016.deploy_37227.09", "2015.deploy_3965.09"
    #---------------------------------------------------------------------------------
    loc = loc[!(loc$id=="deploy_21792" & loc$month=="08"),]
    loc = loc[!(loc$id=="deploy_24638" & loc$month=="08"),]
    loc = loc[!(loc$id=="deploy_37227" & loc$month=="08"),]
    loc = loc[!(loc$id=="deploy_37227" & loc$month=="09"),]
    loc = loc[!(loc$id=="deploy_3965" & loc$month=="09"),]
    
    loc = loc[!(loc$id=="168433" & loc$month=="08"),]
    loc = loc[!(loc$id=="168437" & loc$month=="08"),]
    loc = loc[!(loc$id=="20160" & loc$month=="08"),]
    loc = loc[!(loc$id=="22849" & loc$month=="08"),]
    
    
    #----------------------------------
    # remove IDs with less than 50 locs
    #-----------------------------------
    loc2 = loc %>% 
      group_by(id) %>% 
      filter(n() > 50)
    loc2$year  = droplevels(loc2$year)  # table(loc2$year)
    loc2$id    = droplevels(loc2$id)    # table(loc2$id)
    loc2$month = droplevels(loc2$month)
    
    
    #-----------------------------
    # split over each year and ID
    #-----------------------------
    year   = split(loc2,list(loc2$year, loc2$id, loc2$month))  # length(year) head(abs)
    year2  <- Filter(nrow, year)                 # remove all empty tibbles
    year2  <- year2[sapply(year2, nrow)>50]      # remove ID with < 50 locs
    
    
    
    ########################################################
    # generate pseudo-absences over each year, month and ID
    ########################################################
    # system.time({   # 33 sec
    # select each year, month and ID
    #---------------------------------
    result = lapply(year2, function(x){
      print(.n())
      x$yymm = paste0(x$year,"-",x$month)
      d      = unique(x$yymm)
      x$id   = droplevels(x$id)
      x$year = droplevels(x$year)
      
      # load PCA objects
      #-------------------
      ENVspace = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                "/PCA/Summer/ENVspace_Summer_",model,"_",i,"_",j,"_",d,".rds"))
      ENVspace = na.omit(ENVspace)
      ordi_pix = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                "/PCA/Summer/OrdiPix_Summer_",model,"_",i,"_",j,"_",d,".rds"))
      ordi2    = brick(paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                              "/PCA/Summer/Ordi_Summer_",model,i,"_",j,"_",d,".tif"))
      xy       = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                "/PCA/Summer/xy_rasterBrick_Summer_",model,"_",i,"_",j,"_",d,".rds"))
      pts      = SpatialPointsDataFrame(data=ENVspace, coords=ENVspace[,c("Axis1","Axis2")])
      
      
      
      # spatialPoints of the individual
      #-------------------------------
      id = x[,c("id","lon","lat")]
      coordinates(id) = ~lon+lat
      
      
      # load env raster stack
      #-------------------------
      ras = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                           "/Env_raster/Summer/Env_raster_6var_",i,"_",j,"_",model,"_",
                           d,".rds"))
      names(ras) = c("sst","ssh","sss","mld","bathy","dist")  
      
      
      # extract cell numbers
      #----------------------
      idx = raster::extract(ordi2,id,cellnumbers=T)[,"cells"]   # extract cell number at each whales loc
      
      
      # fix environmental constraint:
      # generate new object containing values of axis1 and axis 2
      # limit values between >quantiles(0.05) and < quantiles(0.99)
      #---------------------------------------------------------------
      occ = as.data.frame(values(ordi2)[idx,])   # 0.21 sec, dataframe containing 2 columns: Axis1, Axis2
      occ = na.omit(occ)
      
      
      # calculate convex hull of both axes
      #-----------------------------------
      names(occ) = c("Axis1","Axis2")
      coordinates(occ) = ~ Axis1+Axis2
      ch = gConvexHull(occ)   
      # plot(occ,axes=T)
      # plot(ch,add=T,border="green",lwd=2)
      
      
      # determine if locs inside convex hull (=0) or outside (=1)
      #-----------------------------------------------------------
      # extract convex hull values at each values of ordi
      val = over(pts,ch)
      ENVspace2 <- ENVspace
      ENVspace2$ConvE = val     
      ENVspace2$ConvE[ENVspace2$ConvE==1]     = 0    # inside
      ENVspace2$ConvE[is.na(ENVspace2$ConvE)] = 1    # outside, table(ENVspace2$ConvE)
      
      
      # convert back to spatial object
      #---------------------------------
      coordinates(ENVspace2) = ~Axis1+Axis2
      # plot(ENVspace2,add=T,col="red",pch=20)
      
      
      # identify locs outside convex hull in ordi
      #--------------------------------------------
      ordi3 <- ordi_pix
      ordi3@data <- data.frame(ordi3,ConvE=0)        # table(ordi3@data$ConvE)
      ordi3@data[which(ENVspace2@data$ConvE==1),"ConvE"] <- 1 # 1=locs outside convex hull,# table(ordi3@data$ConvE)
      ordi3 <- ordi3@data                          
      
      
      # extract locs outside environmental convex hull
      #-----------------------------------------------
      abs = ordi3[ordi3$ConvE==1,] 
      
      
      # generate pseudo-abs 10 times
      #------------------------------
      pseudo = data.frame("x"=0,"y"=0,"pres"=0,"run"=0,"year"=0,"month"=0,
                          "species"= 0,"side"=0,"id"=0)
      pseudo = abs[sample(1:nrow(abs),nrow(x)*10,replace=T),]
      
      # merge presence with absence locations
      #---------------------------------------
      pseudo$pres   = 0
      pseudo$run    = rep(c(1:10), each = nrow(x))
      pseudo$year   = unique(x$year)
      pseudo$month  = unique(x$month)
      pseudo$species= i
      pseudo$side   = j
      pseudo$id     = unique(x$id)
      
      x$species  = i
      x$side     = j
      colnames(x)[2] = "x"
      colnames(x)[3] = "y"
      x2 = data.frame(do.call("rbind", replicate(10, x, simplify=FALSE)))
      x2$run = rep(c(1:10), each = nrow(x))
      
      pseudo = rbind(pseudo[,c("x","y","pres","run","year","species","side","id","month")],
                     x2[,c("x","y","pres","run","year","species","side","id","month")])
      
      
      
      #--------------------------------
      # extract unscaled env variables 
      #--------------------------------
      pseudo$sst_U   = raster::extract(ras[[1]],pseudo[,c("x","y")])
      pseudo$ssh_U   = raster::extract(ras[[2]],pseudo[,c("x","y")])
      pseudo$sss_U   = raster::extract(ras[[3]],pseudo[,c("x","y")])
      pseudo$mld_U   = raster::extract(ras[[4]],pseudo[,c("x","y")])
      pseudo$bathy_U = raster::extract(ras[[5]],pseudo[,c("x","y")])
      pseudo$dist_U  = raster::extract(ras[[6]],pseudo[,c("x","y")])
      
      
      # extract nearest pixel when env value = NA
      #------------------------------------------------
      if(any(is.na(pseudo$sst_U))) {
        pseudo$sst_U[is.na(pseudo$sst_U)] = raster::extract(ras[[1]],pseudo[is.na(pseudo$sst_U),c("x","y")],
                                                            buffer=30000,     # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  } 
      
      if(any(is.na(pseudo$ssh_U))) {
        pseudo$ssh_U[is.na(pseudo$ssh_U)] = raster::extract(ras[[2]],pseudo[is.na(pseudo$ssh_U),c("x","y")],
                                                            buffer=30000, # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  }
      
      if(any(is.na(pseudo$sss_U))) {
        pseudo$sss_U[is.na(pseudo$sss_U)] = raster::extract(ras[[3]],pseudo[is.na(pseudo$sss_U),c("x","y")],
                                                            buffer=30000, # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  }
      
      if(any(is.na(pseudo$mld_U))) {
        pseudo$mld_U[is.na(pseudo$mld_U)] = raster::extract(ras[[4]],pseudo[is.na(pseudo$mld_U),c("x","y")],
                                                            buffer=30000, # units in meters (30 km radius)
                                                            fun=function(x)mean(x,na.rm=TRUE))  }
      if(any(is.na(pseudo$bathy_U))) {
        pseudo$bathy_U[is.na(pseudo$bathy_U)] = raster::extract(ras[[5]],pseudo[is.na(pseudo$bathy_U),c("x","y")],
                                                                buffer=30000, # units in meters (30 km radius)
                                                                fun=function(x)mean(x,na.rm=TRUE))  }
      
      if(any(is.na(pseudo$dist_U))) {
        pseudo$dist_U[is.na(pseudo$dist_U)] = raster::extract(ras[[6]],pseudo[is.na(pseudo$dist_U),c("x","y")],
                                                              buffer=30000,   # units in meters (30 km radius)
                                                              fun=function(x)mean(x,na.rm=TRUE))  }
      
      
      
      return(pseudo)
      # }
    }) 
    final = data.table::rbindlist(result)
    final = as_tibble(final)
    final = final %>%
      filter(!is.na(sst_U))
    
    # save absences for each species 
    #---------------------------------
    saveRDS(final,file=paste0("./RDATA/4.Pseudo-abs/Summer/",model,"/",
                              "abs_Summer_Indiv_6var_",model,"_",i,"_", j,".rds"))
    gc()
  }
})

summary(final)













##############################################
# remove remaining NAs 
# (too close to shore to match env data grid)
##############################################

# Bel west
#-----------
abs <- readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,
                      "/abs_Summer_Indiv_6var_",model,"_Bel_West.rds"))
summary(abs)  


# Nar east
#-----------
abs <- readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,
                      "/abs_Summer_Indiv_6var_",
                      model,"_Nar_East.rds"))
summary(abs)  

# Bw east
#-----------
abs <- readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,
                      "/abs_Summer_Indiv_6var_",
                      model,"_Bw_East.rds"))
summary(abs) 

# Bw west
#-----------
abs <- readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,"/abs_Summer_Indiv_6var_",
                      model,
                      "_Bw_West.rds"))
summary(abs)  










#########################################################
# export pseudo-abs maps for each ID and Year
#########################################################
species = c("Bw","Nar")
side    = c("West","East")

north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada"
                      | north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Russia"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland",]
species = c("Bel_West","Bw_West","Bw_East","Nar_West","Nar_East")

#-------------------------------------------------
# load and prepare data for species i and side j
#-------------------------------------------------
system.time({   # 10 sec for both species and both sides without figs export
  for (i in unique(species)) {
      abs <- readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,
                            "/abs_Summer_Indiv_6var_",
                            model,"_",i,".rds"))
      abs = abs[abs$run==1,]
      
      #-------------      
      # export plot
      #-------------
      ggplot(shore, aes(long, lat)) +
        coord_map("azequidistant", xlim=c(range(abs$x)), ylim=range(abs$y)) + 
        geom_point(data=abs,aes(x,y,colour=as.factor(pres)), size=0.2,stroke=0) +
        geom_polygon(aes(group=group), fill="lightgrey",lwd=0) +
        scale_colour_manual(values=c("black","red")) +
        labs(y="Latitude", x="Longitude", title=paste0(i," Summer")) +
        facet_grid(month~year) +
        theme(legend.position = "none",
              panel.background = element_blank(),
              axis.title.y = element_text(size=7,color="black"),
              axis.title.x = element_text(size=7,color="black"),
              axis.text.x  = element_text(size=5,vjust=0.5,color="black"),
              axis.text.y  = element_text(size=6,color="black"),
              plot.margin  = unit(c(0.3,0.1,0.2,0.1), "cm"))
      
      ggsave(filename=paste0("./FIGURES/Pseudo-abs/Summer/Map_pseudo-abs_",
                             i,"_",model,"_Summer_0.25deg_6var.png"), 
             width=6,height=2,dpi=400,family="serif")
    
  }
})











