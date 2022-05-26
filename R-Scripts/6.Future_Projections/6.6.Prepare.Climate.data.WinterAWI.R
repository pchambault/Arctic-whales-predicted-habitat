##################################################
###       Climate models from CMIP6           ####
###    DATA PREPARATION before projections
###            model: AWI-CM-1-1-MR           ####
###                   WINTER
##################################################

library(ncdf4)   # package for netcdf manipulation
library(raster)  # package for raster manipulation
library(rgdal)   # package for geospatial analysis
library(ggplot2) # package for plotting
library(viridis)
library(tidyverse)
library(tidyquant)
library(ncdf4.helpers)



# SSP585: Shared Socio-economic Pathway (worst scenario)
# spatial resolution: 25 km
# scale2 = function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

# choose the climate model:
model = "AWI-CM-1-1-MR"

# choose the scenario:
scenario = c("ssp126","ssp585")

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
sp = c("Bw_West","Bw_East","Nar_West","Nar_East")







####################################
###               tos          #####
####################################
system.time({    # 45 sec 
  for (ssp in scenario) { 
    variable = "tos"
    tos2  = NULL
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))
    
  for (f in 1:length(files)) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    dates <- nc.get.time.series(f=nc, time.dim.name="time")
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
    # extract variable
    var <- ncvar_get(nc, "tos") 
    
    # retain only dates for decades and summer
    dates  = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-12-","-01-","-02-","-03-"),
                                                 collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
    dates2 = dates2$val
    
    
    # open each layer and convert it to tibble
    #---------------------------------------------
    for (i in unique(dates2)) { 
      var1 = var[,which(dates==i)]
      grid = as_tibble(cbind("lon"=lon, "lat"=lat, "val"=var1))
      grid = grid %>%
        filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
        mutate("year" = substr(i, 1, 4),
               "month"= substr(i, 6, 7),
               "var"  = variable) %>%
        filter(!is.na(var)) 
      
      # save tos
      #----------
      tos2 = rbind(tos2, grid)
    }
  }
    tos2 = tos2[!is.na(tos2$val),]
    saveRDS(tos2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                             "/RDATA/6.Climatic_projections/",model,"/tos_25km_",
                             model,"_Winter_",ssp,"_2020-2100.rds"))
    rm(var, nc, grid, tos, var1)
    
  }
})









####################################
###             sos            #####
####################################
system.time({    # 40 sec 
  for (ssp in scenario) { 
    variable = "sos"
    sos2 = NULL
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))
    
    
  for (f in 1:length(files)) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    origin = paste0(substr(sub(".*_", "", files[f]), 1, 4),
                    "-1-1")
    dates = as.Date(t, origin=origin) # days since 2091-1-1 00:00:00
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
    # extract variable
    var <- ncvar_get(nc, "sos") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-12-","-01-","-02-","-03-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
    dates2 = dates2$val
    
    
    # open each layer and convert it to dataframe
    #---------------------------------------------
    for (i in unique(dates2)) { 
      var1 = var[,which(dates==i)]
      grid = as_tibble(cbind("lon"=lon, "lat"=lat,"val"=var1))
      grid = grid %>%
        filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
        mutate("year" = substr(i, 1, 4),
               "month"= substr(i, 6, 7),
               "var"  = variable) %>%
        filter(!is.na(var)) 
      
      # save sos
      #----------
      sos2 = rbind(sos2, grid)
    }
  }
    sos2 = sos2[!is.na(sos2$val),]
    saveRDS(sos2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                             "/RDATA/6.Climatic_projections/",model,"/sos_25km_",model,
                             "_Winter_",ssp,"_2020-2100.rds"))
  }
})







####################################
###             zos            #####
####################################
system.time({    # 45 sec
  for (ssp in scenario) { 
  variable = "zos"
  zos2 = NULL
  files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                            "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                     pattern=ssp)
  files
  setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
               "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))
  
  
  for (f in 1:length(files)) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    origin = paste0(substr(sub(".*_", "", files[f]), 1, 4),
                    "-1-1")
    dates = as.Date(t, origin=origin) # days since ...
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
    # extract variable
    var <- ncvar_get(nc, "zos") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-12-","-01-","-02-","-03-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
    dates2 = dates2$val
    
    
    # open each layer and convert it to dataframe
    #---------------------------------------------
    for (i in unique(dates2)) { 
      var1 = var[,which(dates==i)]
      grid = as_tibble(cbind("lon"=lon, "lat"=lat,"val"=var1))
      grid = grid %>%
        filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
        mutate("year" = substr(i, 1, 4),
               "month"= substr(i, 6, 7),
               "var"  = variable) %>%
        filter(!is.na(var)) 
      
      # save zos
      #----------
      zos2 = rbind(zos2, grid)
    }
  }
  saveRDS(zos2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                           "/RDATA/6.Climatic_projections/",model,"/zos_25km_",
                           model,"_Winter_",ssp,"_2020-2100.rds"))
  
  rm(sos,zos,dates,dates2,grid,var,var1,files,lat,lon,i,f,
     origin,nc,t,variable)
  }
})









####################################
###         mlotst             #####
####################################
system.time({    # 45 sec
  for (ssp in scenario) { 
    variable = "mlotst"
    mlotst2 = NULL
    files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                       pattern=ssp)
    files
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))
    
    
  for (f in 1:length(files)) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    origin = paste0(substr(sub(".*_", "", files[f]), 1, 4),
                    "-1-1")
    dates = as.Date(t, origin=origin) # days since ...
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
    # extract variable
    var <- ncvar_get(nc, "mlotst") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-12-","-01-","-02-","-03-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year %in% unique(year)[substr(unique(year),4,4) == "0"])
    dates2 = dates2$val
    
    
    # open each layer and convert it to dataframe
    #---------------------------------------------
    for (i in unique(dates2)) { 
      var1 = var[,which(dates==i)]
      grid = as_tibble(cbind("lon"=lon, "lat"=lat,"val"=var1))
      grid = grid %>%
        filter(lon>(-110) & lon<90 & lat >55 & lat<85) %>%
        mutate("year" = substr(i, 1, 4),
               "month"= substr(i, 6, 7),
               "var"  = variable) %>%
        filter(!is.na(var)) 
      
      # save mlotst
      #------------
      mlotst2 = rbind(mlotst2, grid)
    }
  }
    saveRDS(mlotst2,file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                                "/RDATA/6.Climatic_projections/",model,"/mlotst_25km_",
                                model,"_Winter_",ssp,"_2020-2100.rds"))
    
    rm(dates,dates2,grid,var,var1,files,lat,lon,i,f,
       origin,nc,t,variable)
  }
})












#############################################################
# convert tibbles into rasters on a regular grid (0.25 deg)
#############################################################
system.time({   # 103 sec
  for (ssp in scenario) { 
    sos <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                          "RDATA/6.Climatic_projections/",model,"/sos_25km_",model,"_Winter_",
                          ssp,"_2020-2100.rds"))
    tos <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                          "RDATA/6.Climatic_projections/",model,"/tos_25km_",model,"_Winter_",
                          ssp,"_2020-2100.rds"))
    zos <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                          "RDATA/6.Climatic_projections/",model,"/zos_25km_",model,"_Winter_",
                          ssp,"_2020-2100.rds"))
    mlotst <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                             "RDATA/6.Climatic_projections/",model,"/mlotst_25km_",model,
                             "_Winter_",ssp,"_2020-2100.rds"))
    tos$yymm = paste0(tos$year, "-", tos$month)
    sos$yymm = paste0(sos$year, "-", sos$month)
    zos$yymm = paste0(zos$year, "-", zos$month)
    mlotst$yymm = paste0(mlotst$year, "-", mlotst$month)
    
    
    
    
    
    
    
    ###################
    ##       WEST   ###
    ###################
    i = "Bw_West"
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    
    # import bathy for masking land
    #--------------------------------
    env <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/",
                          "/Bw_West/Summer/Unscaled/rasterEnv_res0.25_Summer_Bw_West_unscaled_2002-09.rds"))
    # plot(env, col=viridis(64))
    bathy = env[[1]]
    names(bathy) = "layer"
    w <- matrix(1, 3, 3)     # matrix for interpolation
    
    
    #--------------------------------
    # loop over each month and year
    #---------------------------------
    yymm = unique(tos$yymm)
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
      
      
      ###########
      ## mlotst ##
      ###########
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
      proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]]) # sss,ssh,sss,mld,bathy,dist
      saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                                "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,
                                "/Winter/Env_proj_West_",model,"_",ssp,"_",d,".rds"))
      saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                                "/RDATA/6.Climatic_projections/",model,"/Rasters/Nar_West",
                                "/Winter/Env_proj_Nar_West_",model,"_",ssp,"_",d,".rds"))
    }
    
    
    
    
    
    
    ###################
    # Bw EAST 
    ###################
    i = "Bw_East"
    
    # import bathy for masking land
    #--------------------------------
    env <- readRDS("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/Bw_East/Summer/Unscaled/rasterEnv_res0.25_Summer_Bw_East_unscaled_2018-08.rds")
    bathy = env[[1]]
    names(bathy) = "layer"
    
    
    #--------------------------------
    # loop over each month and year
    #---------------------------------
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
      
      
      ###########
      ## mlotst ##
      ###########
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
                                "/Winter/Env_proj_Bw_East_",model,"_",ssp,"_",d,".rds"))
    }
    
    
    
    
    
    
    
    
    ###################
    # Nar EAST 
    ###################
    i = "Nar_East"
    
    # import bathy for masking land
    #--------------------------------
    env <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/",i,
                          "/Summer/Unscaled/rasterEnv_res0.25_Summer_Nar_East_unscaled_2011-08.rds"))
    bathy = env[[4]]
    names(bathy) = "layer"
    
    
    #--------------------------------
    # loop over each month and year
    #---------------------------------
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
      
      
      ###########
      ## mlotst ##
      ###########
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
                                "/Winter/Env_proj_Nar_East_",model,"_",ssp,"_",d,".rds"))
    }
  }
})













########################################
######            explo            #####
########################################

# maps of tos from 2020-2100
#-----------------------------
sp = c("Bw_West","Bw_East","Nar_West","Nar_East")
month = c("-01","-02","-03","-12")

for (i in sp) { 
  for (m in month) { 
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                 "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,"/Winter"))
    r = do.call(stack, lapply(intersect(list.files(pattern=ssp),
                                        list.files(pattern=m)), readRDS))
    sub = raster::subset(r, grep("layer.1.", names(r), value = T))
    names(sub) = c("2020","2030","2040","2050","2060","2070","2080","2090","2100")
    
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3"))
    png(filename=paste0("./FIGURES/CLIMATO/GCM/",model,
                        "/Maps_tos/ProjStack_Winter_tos_2020-2100",m,"_",
                        i,"_",ssp,".png"),
        res=350, width=6, height=4.4,units="in")
    plot(sub, col = viridis(64),zlim=c(-2,10))
    dev.off()
  }
}


# maps of sos from 2020-2100
#-----------------------------
for (i in sp) { 
  for (m in month) { 
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                 "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,"/Winter"))
    r = do.call(stack, lapply(intersect(list.files(pattern=ssp),
                                        list.files(pattern=m)), readRDS))
    sub = raster::subset(r, grep("layer.3.", names(r), value = T))
    names(sub) = c("2020","2030","2040","2050","2060","2070","2080","2090","2100")
    
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3"))
    png(filename=paste0("./FIGURES/CLIMATO/GCM/",model,
                        "/Maps_sos/ProjStack_Winter_sos_2020-2100",m,"_",
                        i,"_",ssp,".png"),
        res=350, width=6, height=4.4,units="in")
    plot(sub, col = viridis(64),zlim=c(26,36))
    dev.off()
  }
}


# maps of zos from 2020-2100
#-----------------------------
for (i in sp) { 
  for (m in month) { 
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                 "/RDATA/6.Climatic_projections/",model,"/Rasters/",i,"/Winter"))
    r = do.call(stack, lapply(intersect(list.files(pattern=ssp),
                                        list.files(pattern=m)), readRDS))
    sub = raster::subset(r, grep("layer.2.", names(r), value = T))
    names(sub) = c("2020","2030","2040","2050","2060","2070","2080","2090","2100")
    
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3"))
    png(filename=paste0("./FIGURES/CLIMATO/GCM/",model,
                        "/Maps_zos/ProjStack_Winter_zos_2020-2100",m,"_",
                        i,"_",ssp,".png"),
        res=350, width=6, height=4.4,units="in")
    plot(sub, col = viridis(64)) # ,zlim=c(-1.5,0.5)
    dev.off()
  }
}





#----------------------
# tos over time
#----------------------
# tos <- readRDS("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/6.Climatic_projections/AWI-CM-1-1-MR/tos_25km_AWI-CM-1-1-MR_ssp585_2020-2100.rds")
tos = tos %>%
  mutate(side = ifelse(lon > (-47), "East", "West"))
pivot = tos[,c("val","year","month","side")] %>%
  pivot_longer(!c(month,year,side), 
               names_to = "var", values_to = "val")

ggplot(pivot, aes(x=year, y=val,colour=side)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y="deg C", x="Years",
       title = "Projections of tos",
       subtitle = paste0("Summer (",model,", ",ssp,")")) +
  facet_wrap(.~month, ncol=1) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/AWI-CM-1-1-MR/",
                       "Boxplot_tos_years_2020-2100_Winter_",
                       model,"_",ssp,".png"),
       width=3.5,height=4,dpi=400,family="serif")

sum = pivot %>%
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
       subtitle = paste0("Summer (",model,", ",ssp,")")) +
  facet_wrap(.~month, ncol=1) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/AWI-CM-1-1-MR/",
                       "Boxplot_sos_years_2020-2100_Winter_",
                       model,"_",ssp,".png"),
       width=3.5,height=4,dpi=400,family="serif")

pivot %>%
  group_by(month,year) %>%
  summarize(mean=mean(val),
            sd=sd(val))



#----------------------
# zos over time
#----------------------
zos = zos %>%
  mutate(side = ifelse(lon > (-47), "East", "West"))

pivot = zos[,c("val","year","month","side")] %>%
  pivot_longer(!c(month,year,side), 
               names_to = "var", values_to = "val")

ggplot(pivot, aes(x=year, y=val, colour=side)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y="m", x="Years",
       title = "Projections of zos",
       subtitle = paste0("Summer (",model,", ",ssp,")")) +
  facet_wrap(.~month, ncol=1) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/AWI-CM-1-1-MR/",
                       "Boxplot_zos_years_2020-2100_Winter_",
                       model,"_",ssp,".png"),
       width=3.5,height=4,dpi=400,family="serif")

pivot %>%
  group_by(month,year) %>%
  summarize(mean=mean(val),
            sd=sd(val))



#----------------------
# mlotst over time
#----------------------
mlotst = mlotst %>%
  mutate(side = ifelse(lon > (-47), "East", "West"))
unique(mlotst$side)

pivot = mlotst[,c("val","year","month","side")] %>%
  pivot_longer(!c(month,year,side), 
               names_to = "var", values_to = "val")
summary(pivot)

ggplot(pivot, aes(x=year, y=val, colour=side)) +
  geom_boxplot(outlier.shape = NA) +
  labs(y="(psu)m)", x="Years",
       title = "Projections of mlotst",
       subtitle = paste0("Summer (",model,", ",ssp,")")) +
  facet_wrap(.~month, ncol=1) +
  ylim(0,100) +
  scale_colour_brewer(palette = "Set2") +
  theme_tq() 
ggsave(filename=paste0("./FIGURES/CLIMATO/GCM/AWI-CM-1-1-MR/",
                       "Boxplot_mlotst_years_2020-2100_Winter_",
                       model,"_",ssp,".png"),
       width=3.5,height=4,dpi=400,family="serif")

pivot %>%
  group_by(month,year) %>%
  summarize(mean=mean(val),
            sd=sd(val))



#----------------
# plot maps
#----------------
month = unique(tos$month)
files = intersect(list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/RDATA/6.Climatic_projections/",model,"/"),
                   pattern=ssp),
                  list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                                    "PAPER3/RDATA/6.Climatic_projections/",model,"/"),
                             pattern="Winter"))
files

system.time({  # 56 sec
  for (f in 1:length(files)) { 
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                 "PAPER3/RDATA/6.Climatic_projections/",model,"/"))
    dat = readRDS(files[[f]])
    variable = substr(files[[f]],1,3)
    
    for(m in month) { 
      ggplot(dat[dat$month==m,],
             aes(x=lon, y=lat,colour=val)) +
        geom_point(size=0.4, stroke=0) +
        scale_color_viridis() +
        facet_wrap(.~year, ncol=3) +
        theme_tq() +
        labs(y="", x="",
             title = paste0("Projections of ",variable),
             subtitle = paste0("month ",m," (", model," ",ssp,")"))
      
      ggsave(filename=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                             "PAPER3/FIGURES/CLIMATO/GCM/",model,
                             "/Maps_proj/Map_proj_",variable,"_",model,
                             "_",ssp,"_2020-2100_Winter_",m,".png"),
             width=4,height=5,dpi=400,family="serif")
    }
  }
})

