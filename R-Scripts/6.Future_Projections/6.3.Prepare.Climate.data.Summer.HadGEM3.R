##################################################
###       Climate models from CMIP6           ####
###    DATA PREPARATION before projections
###         model: HadGEM3-GC31-MM
###         variant label: r1i1p1f3
##################################################

library(ncdf4)   # package for netcdf manipulation
library(raster)  # package for raster manipulation
library(rgdal)   # package for geospatial analysis
library(ggplot2) # package for plotting
library(viridis)
library(tidyverse)
library(tidyquant)
library(ncdf4.helpers)


# choose the climate model:
model = "HadGEM3-GC31-MM"

# choose the scenario:
scenario = c("ssp126","ssp585")





####################################
###               tos          #####
####################################
variable = "tos"

#--------------------
system.time({    # 35 sec 
  for (ssp in scenario) { 
    tos2 = NULL
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable))
    
    
    for (f in 1:length(files)) { 
      
      # open each netcdf file
      #----------------------------
      nc = nc_open(files[f])
      t <- ncvar_get(nc, "time")
      dates <- nc.get.time.series(f=nc, time.dim.name="time")
      # extract coordinates
      lon <- as.vector(ncvar_get(nc, "longitude")) 
      lat <- as.vector(ncvar_get(nc, "latitude"))
      # extract variable
      var <- ncvar_get(nc, "tos") 
      
      # retain only dates for decades and summer
      dates  = as.character(dates)
      dates2 = data.frame(as.character(dates))
      colnames(dates2) = "val"
      dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                    collapse = '|')))
      dates2$year = substr(dates2$val, 1, 4)
      dates2 = dates2 %>%
        filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
      dates2 = dates2$val
      
      
      # open each layer and convert it to dataframe
      #---------------------------------------------
      for (i in unique(dates2)) { 
        var1 = as.vector(var[,,which(dates==i)])
        grid = as_tibble(cbind("lon"=lon, "lat"=lat, "val"=var1))
        grid = grid %>%
          filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
          mutate("year" = substr(i, 1, 4),
                 "month"= substr(i, 6, 7),
                 "var"  = variable) %>%
          filter(!is.na(var)) 
        
        # save tos2
        #----------
        tos2 = rbind(tos2, grid)
      }
    }
    tos2 = tos2[!is.na(tos2$val),]
    saveRDS(tos2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                             "/RDATA/6.Climatic_projections/",model,
                             "/tos_25km_",model,"_",ssp,"_2020-2100.rds"))
    rm(var, nc, grid, tos, var1)
  }
})
    
    
    
    
    
    
    
    
    
    
####################################
###             sos            #####
####################################
system.time({ 
  variable = "sos"
  for (ssp in scenario) { 
    sos2 = NULL
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable))
    
    for (f in 1:length(files)) { 
      
      # open each netcdf file
      #----------------------------
      nc = nc_open(files[f])
      dates <- nc.get.time.series(f=nc, time.dim.name="time")
      # extract coordinates
      lon <- as.vector(ncvar_get(nc, "longitude")) 
      lat <- as.vector(ncvar_get(nc, "latitude"))
      # extract variable
      var <- ncvar_get(nc, "sos") 
      
      # retain only dates for decades and summer
      dates  = as.character(dates)
      dates2 = data.frame(as.character(dates))
      colnames(dates2) = "val"
      dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                    collapse = '|')))
      dates2$year = substr(dates2$val, 1, 4)
      dates2 = dates2 %>%
        filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
      dates2 = dates2$val
      
      
      # open each layer and convert it to dataframe
      #---------------------------------------------
      for (i in unique(dates2)) { 
        var1 = as.vector(var[,,which(dates==i)])
        grid = as_tibble(cbind("lon"=lon, "lat"=lat,"val"=var1))
        grid = grid %>%
          filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
          mutate("year" = substr(i, 1, 4),
                 "month"= substr(i, 6, 7),
                 "var"  = variable) %>%
          filter(!is.na(var)) 
        
        # save sos2
        #----------
        sos2 = rbind(sos2, grid)
      }
    }
    sos2 = sos2[!is.na(sos2$val),]
    saveRDS(sos2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                             "/RDATA/6.Climatic_projections/",model,
                             "/sos_25km_",model,"_",ssp,"_2020-2100.rds"))
    
  }
})




    

####################################
###             zos            #####
####################################
variable = "zos"
system.time({ 
  for (ssp in scenario) { 
    zos2 = NULL
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable))
    for (f in 1:length(files)) { 
      
      # open each netcdf file
      #----------------------------
      nc = nc_open(files[f])
      dates <- nc.get.time.series(f=nc, time.dim.name="time")
      # extract coordinates
      lon <- as.vector(ncvar_get(nc, "longitude")) 
      lat <- as.vector(ncvar_get(nc, "latitude"))
      # extract variable
      var <- ncvar_get(nc, "zos") 
      
      # retain only dates for decades and summer
      dates = as.character(dates)
      dates2 = data.frame(as.character(dates))
      colnames(dates2) = "val"
      dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                    collapse = '|')))
      dates2$year = substr(dates2$val, 1, 4)
      dates2 = dates2 %>%
        filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
      dates2 = dates2$val
      
      
      # open each layer and convert it to dataframe
      #---------------------------------------------
      for (i in unique(dates2)) { 
        var1 = as.vector(var[,,which(dates==i)])
        grid = as_tibble(cbind("lon"=lon, "lat"=lat,"val"=var1))
        grid = grid %>%
          filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
          mutate("year" = substr(i, 1, 4),
                 "month"= substr(i, 6, 7),
                 "var"  = variable) %>%
          filter(!is.na(var)) 
        
        # save zos2
        #----------
        zos2 = rbind(zos2, grid)
      }
    }
    zos2 = zos2[!is.na(zos2$val),]
    saveRDS(zos2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                             "/RDATA/6.Climatic_projections/",model,
                             "/zos_25km_",model,"_",ssp,"_2020-2100.rds"))
    
    rm(sos,zos,dates,dates2,grid,var,var1,files,lat,lon,i,f,
       origin,nc,t,variable)
    
  }
})
      
      
      
      
      
      
      
      

####################################
###             mlotst         #####
####################################
variable = "mlotst"


#--------------------
system.time({    # 34 sec
  for (ssp in scenario) { 
    mlotst2 = NULL
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/HadGEM3-GC31-MM_25km/",variable))
    
    for (f in 1:length(files)) { 
      
      # open each netcdf file
      #----------------------------
      nc = nc_open(files[f])
      dates <- nc.get.time.series(f=nc, time.dim.name="time")
      # extract coordinates
      lon <- as.vector(ncvar_get(nc, "longitude")) 
      lat <- as.vector(ncvar_get(nc, "latitude"))
      # extract variable
      var <- ncvar_get(nc, "mlotst") 
      
      # retain only dates for decades and summer
      dates = as.character(dates)
      dates2 = data.frame(as.character(dates))
      colnames(dates2) = "val"
      dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                    collapse = '|')))
      dates2$year = substr(dates2$val, 1, 4)
      dates2 = dates2 %>%
        filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
      dates2 = dates2$val
      
      
      # open each layer and convert it to dataframe
      #---------------------------------------------
      for (i in unique(dates2)) { 
        var1 = as.vector(var[,,which(dates==i)])
        grid = as_tibble(cbind("lon"=lon, "lat"=lat,"val"=var1))
        grid = grid %>%
          filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
          mutate("year" = substr(i, 1, 4),
                 "month"= substr(i, 6, 7),
                 "var"  = variable) %>%
          filter(!is.na(var)) 
        
        # save mlotst2
        #----------
        mlotst2 = rbind(mlotst2, grid)
      }
    }
    mlotst2 = mlotst2[!is.na(mlotst2$val),]
    saveRDS(mlotst2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                                "/RDATA/6.Climatic_projections/",model,
                                "/mlotst_25km_",model,"_",ssp,"_2020-2100.rds"))
    
    rm(mlotst,dates,dates2,grid,var,var1,files,lat,lon,i,f,
       nc,t,variable)
    gc()
  }
})









#############################################################
# convert tibbles into rasters on a regular grid (0.25 deg)
#############################################################
sos <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                      "RDATA/6.Climatic_projections/",model,"/sos_25km_",model,"_",
                      ssp,"_2020-2100.rds"))
tos <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                      "RDATA/6.Climatic_projections/",model,"/tos_25km_",model,"_",
                      ssp,"_2020-2100.rds"))
zos <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                      "RDATA/6.Climatic_projections/",model,"/zos_25km_",model,"_",
                      ssp,"_2020-2100.rds"))
mlotst <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                      "RDATA/6.Climatic_projections/",model,"/mlotst_25km_",model,"_",
                      ssp,"_2020-2100.rds"))

tos$yymm = paste0(tos$year, "-", tos$month)
sos$yymm = paste0(sos$year, "-", sos$month)
zos$yymm = paste0(zos$year, "-", zos$month)
mlotst$yymm = paste0(mlotst$year, "-", mlotst$month)

tos = tos[!is.na(tos$val),]
sos = sos[!is.na(sos$val),]
zos = zos[!is.na(zos$val),]
mlotst = mlotst[!is.na(mlotst$val),]





###################
# Bel WEST side
###################
i = "Bel_West"
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")

# import bathy for masking land
#--------------------------------
env <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/",i,
                      "/Summer/Unscaled/rasterEnv_res0.25_Summer_",i,"_unscaled_1995-09.rds"))
# plot(env, col=viridis(64))
bathy = env[[1]]
names(bathy) = "layer"
w <- matrix(1, 3, 3)     # matrix for interpolation


#--------------------------------
# loop over each month and year
#---------------------------------
yymm = unique(tos$yymm)
system.time({  # 6 sec
  # loop over each date
  #------------------------
  for (d in unique(yymm)) { 
    
    ###########
    ## tos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = tos[tos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    tos_r = mask(ras2, bathy)
    
    
    ###########
    ## sos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = sos[sos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    sos_r = mask(ras2, bathy)
    
    
    ###########
    ## zos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = zos[zos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    zos_r = mask(ras2, bathy)
    
    
    ############
    ## mlotst ##
    ############
    # rasterize tibble over 0.25 deg grid
    sub = mlotst[mlotst$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    mlotst_r = mask(ras2, bathy)
    
    
    
    ##############################
    # aggregate raster layer
    ##############################
    proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]]) # sst,ssh,sss,mld,bathy,dist
    saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                              "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,
                              "/Env_proj_West_",model,"_",ssp,"_",d,".rds"))
  }
})





###################
# Bw EAST 
###################
i = "Bw_East"

# import bathy for masking land
#--------------------------------
env <- readRDS("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/Bw_East/Summer/Unscaled/rasterEnv_res0.25_Summer_Bw_East_unscaled_2017-08.rds")
bathy = env[[1]]
names(bathy) = "layer"


#--------------------------------
# loop over each month and year
#---------------------------------
yymm = unique(tos$yymm)
system.time({  # 15 sec
  # loop over each date
  #------------------------
  for (d in unique(yymm)) { 
    
    ###########
    ## tos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = tos[tos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    tos_r = mask(ras2, bathy)
    
    
    ###########
    ## sos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = sos[sos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    sos_r = mask(ras2, bathy)
    
    
    ###########
    ## zos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = zos[zos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    zos_r = mask(ras2, bathy)
    
    
    ############
    ## mlotst ##
    ############
    # rasterize tibble over 0.25 deg grid
    sub = mlotst[mlotst$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    mlotst_r = mask(ras2, bathy)
    
    
    ##############################
    # aggregate raster layer
    ##############################
    proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]])
    saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                              "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,
                              "/Env_proj_Bw_East_",model,"_",ssp,"_",d,".rds"))
  }
})








###################
# Nar EAST 
###################
i = "Nar_East"

# import bathy for masking land
#--------------------------------
env <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/",i,
                      "/Summer/Unscaled/rasterEnv_res0.25_Summer_",i,"_unscaled_2010-08.rds"))
bathy = env[[4]]
names(bathy) = "layer"


#--------------------------------
# loop over each month and year
#---------------------------------
yymm = unique(tos$yymm)
system.time({  # 8 sec
  # loop over each date
  #------------------------
  for (d in unique(yymm)) { 
    
    ###########
    ## tos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = tos[tos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    tos_r = mask(ras2, bathy)
    
    
    ###########
    ## sos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = sos[sos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    sos_r = mask(ras2, bathy)
    
    
    ###########
    ## zos ##
    ###########
    # rasterize tibble over 0.25 deg grid
    sub = zos[zos$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    zos_r = mask(ras2, bathy)
    
    ############
    ## mlotst ##
    ############
    # rasterize tibble over 0.25 deg grid
    sub = mlotst[mlotst$yymm==d,]
    ras = rasterize(sub[,c('lon','lat')], env[[4]], 
                    field=sub[,c("val")], fun=mean, na.rm=FALSE)
    # interpolation to fill the empty cells
    ras2 <- focal(ras, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
    # mask lands
    mlotst_r = mask(ras2, bathy)
    
    
    ##############################
    # aggregate raster layer
    ##############################
    proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]])
    saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                              "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,
                              "/Env_proj_Nar_East_",model,"_",ssp,"_",d,".rds"))
  }
})














########################################
# explo
########################################

#----------------------
# tos over time
#----------------------
tos = tos %>%
  mutate(side = ifelse(lon > (-47), "East", "West"))
pivot = tos[,c("val","year","month","side")] %>%
  pivot_longer(!c(month,year,side), 
               names_to = "var", values_to = "val")

ggplot(pivot, aes(x=year, y=val,colour=side)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y="deg C", x="Years",
       title = "Projections of tos",
       subtitle = "Summer (HadGEM3-GC31-MM, ssp585)") +
  facet_wrap(.~month, ncol=1) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/",model,
                       "/Boxplot_tos_years_",model,"_",ssp,
                       "_2020-2100_summer.png"),
       width=3.5,height=4.5,dpi=400,family="serif")

pivot %>%
  group_by(month,year) %>%
  summarize(mean=mean(val),
            sd=sd(val))




#----------------------
# sos over time
#----------------------
sos = sos %>%
  mutate(side = ifelse(lon > (-47), "East", "West"))

pivot = sos[,c("val","year","month","side")] %>%
  pivot_longer(!c(month,year,side), 
               names_to = "var", values_to = "val")

ggplot(pivot, aes(x=year, y=val, colour=side)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y="psu", x="Years",
       title = "Projections of sos",
       subtitle = "Summer (HadGEM3-GC31-MM, ssp585)") +
  facet_wrap(.~month, ncol=1) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/",model,
                       "/Boxplot_sos_years_",model,"_",ssp,
                       "_2020-2100_summer.png"),
       width=3.5,height=4.5,dpi=400,family="serif")

pivot %>%
  group_by(month,year) %>%
  summarize(mean=mean(val),
            sd=sd(val))



#----------------------
# sos over time
#----------------------
zos = zos %>%
  mutate(side = ifelse(lon > (-47), "East", "West"))

pivot = zos[,c("val","year","month","side")] %>%
  pivot_longer(!c(month,year,side), 
               names_to = "var", values_to = "val")
summary(pivot)

ggplot(pivot, aes(x=year, y=val, colour=side)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y="m", x="Years",
       title = "Projections of zos",
       subtitle = "Summer (HadGEM3-GC31-MM, ssp585)") +
  facet_wrap(.~month, ncol=1) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/",model,
                       "/Boxplot_zos_years_",model,"_",ssp,
                       "_2020-2100_summer.png"),
       width=3.5,height=4.5,dpi=400,family="serif")

pivot %>%
  group_by(month,year) %>%
  summarize(mean=mean(val),
            sd=sd(val))





#----------------
# plot maps
#----------------
month = unique(tos$month)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/RDATA/6.Climatic_projections/",model,"/"),
                   pattern=ssp)
files

system.time({
  for (f in 1:length(files)) { 
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/RDATA/6.Climatic_projections/",model,"/"))
    dat = readRDS(files[[f]])
    variable = substr(files[[f]],1,3)
    
    for(m in month) { 
      ggplot(dat[dat$month==m,],
             aes(x=lon, y=lat,colour=val)) +
        geom_point(size=0.3, stroke=0) +
        scale_color_viridis() +
        facet_wrap(.~year, ncol=3) +
        theme_tq() +
        labs(y="", x="",
             title = paste0("Projections of ",variable),
             subtitle = paste0("month ",m," (", model," ",ssp,")"))
      
      ggsave(filename=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                             "PAPER3/FIGURES/CLIMATO/GCM/",model,
                             "/Maps_proj/Map_proj_",variable,"_",model,
                             "_",ssp,"_2020-2100_",m,".png"),
             width=4,height=5,dpi=400,family="serif")
    }
  }
})



