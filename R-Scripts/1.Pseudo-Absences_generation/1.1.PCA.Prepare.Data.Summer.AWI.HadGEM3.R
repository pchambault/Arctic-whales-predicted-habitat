###################################################################
##### PCA for envtal background to generate pseudo-absences #######
#####                     SUMMER: Aug to Sept
#####     6 variables: mld, sst, sss, ssh, bathy, dist2shore
#####      envtal variables extracted form CMIP6 models
#####     2 GCMs: AWI-CM-1-1-MR and HadGEM3-GC31-MM
#####                 resolution: 0.25 deg, monthly
###################################################################


library(rgdal)
library(raster)
library(fields)
library(rgeos)
library(trip)
library(ade4)
library(ncdf4)
library(stringr)
library(viridis)
library(ggplot2) 
library(tidyverse)
library(tidyquant)
library(ncdf4.helpers)




# choose the climate model:
models = c("AWI-CM-1-1-MR", "HadGEM3-GC31-MM")
model = models[[1]]

# choose the species and locality of interest
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
sp = c("Bel_West","Bw_West","Bw_East","Nar_West","Nar_East")

# 4 dynamic variables: 
# - tos (sea surface temperature, in deg Celsius)
# - sos (sea surface salinity, in psu)
# - mlost (mixed layer depth, in m)
# - zos (sea surface height, in cm)

# 2 periods: historial (1850-2014) and contemporary (2015-2020)







####################################
###     tos  historical        #####
####################################
# choose the scenario: historial (1850-2014), ssp126: 2015-2100
scenario = c("historical","ssp126")
ssp      = scenario[1]

# choose the variable
variable = "tos"
tos2     = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 18 sec 
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
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                  collapse = '|')))
    dates2$year = as.numeric(substr(dates2$val, 1, 4))
    dates2 = dates2 %>%
      filter(year >= 1993)
    dates2 = dates2$val
    
    
    # open each layer and convert it to dataframe
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
      
    # save tos2
    #----------
    tos2 = rbind(tos2, tos)
    }
  }
})
dim(tos2)
summary(tos2$val)
unique(tos2$year)
unique(tos2$month)
rm(var, nc, grid, tos, var1)






####################################
###      sos  historical       #####
####################################
variable = "sos"
sos  = NULL
sos2 = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 16 sec 
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
    var <- ncvar_get(nc, "sos") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year >= 1993)
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
      sos = rbind(sos, grid)
    }
    
    # save sos2
    #----------
    sos2 = rbind(sos2, sos)
  }
})

summary(sos2$val)
unique(sos2$year)
unique(sos2$month)






####################################
###      zos historical        #####
####################################
variable = "zos"
zos  = NULL
zos2 = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 19 sec
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
    var <- ncvar_get(nc, "zos") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year >= 1993)
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
      zos = rbind(zos, grid)
    }
    
    # save zos2
    #----------
    zos2 = rbind(zos2, zos)
  }
})

unique(zos2$year)
unique(zos2$month)
rm(dates,dates2,grid,var,var1,files,lat,lon,i,f,nc,t,variable)






####################################
###       mlost historical    #####
####################################
variable = "mlotst"
mlotst   = NULL
mlotst2  = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 19 sec
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
    var <- ncvar_get(nc, "mlotst") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year >= 1993)
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
      mlotst = rbind(mlotst, grid)
    }
    
    # save zos2
    #----------
    mlotst2 = rbind(mlotst2, mlotst)
  }
})

unique(mlotst2$year)
unique(mlotst2$month)
rm(dates,dates2,grid,var,var1,files,lat,lon,i,f,nc,t,variable)

tos2 -> tos1
zos2 -> zos1
sos2 -> sos1
mlotst2 -> mlotst1
rm(mlotst2, tos2, sos2, zos2)














####################################
###  tos ssp126 (2015-2021)    #####
####################################
# choose the scenario: ssp126: 2015-2100
ssp   = scenario[2] 

# choose variable
variable = "tos"
tos   = NULL
tos2  = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 18 sec 
  for (f in 1:2) { 
    
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
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                  collapse = '|')))
    dates2$year = as.numeric(substr(dates2$val, 1, 4))
    dates2 = dates2 %>%
      filter(year <= 2020)
    dates2 = dates2$val
    
    
    # open each layer and convert it to dataframe
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
      tos = rbind(tos, grid)
    }
    
    # save tos2
    #----------
    tos2 = rbind(tos2, tos)
  }
})
dim(tos2)
summary(tos2$val)
unique(tos2$year)
unique(tos2$month)
rm(var, nc, grid, tos, var1)








####################################
###       sos ssp126           #####
####################################
variable = "sos"
sos  = NULL
sos2 = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 16 sec 
  for (f in 1:2) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    dates <- nc.get.time.series(f=nc, time.dim.name="time")
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
    # extract variable
    var <- ncvar_get(nc, "sos") 
    
    # retain only dates for decades and summer
    dates = as.character(dates)
    dates2 = data.frame(as.character(dates))
    colnames(dates2) = "val"
    dates2 = filter(dates2, str_detect(val, paste(c("-08-","-09-"),
                                                  collapse = '|')))
    dates2$year = substr(dates2$val, 1, 4)
    dates2 = dates2 %>%
      filter(year <= 2020)
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
      sos = rbind(sos, grid)
    }
    
    # save sos2
    #----------
    sos2 = rbind(sos2, sos)
  }
})

summary(sos2$val)
unique(sos2$year)
unique(sos2$month)








####################################
###       zos ssp126           #####
####################################
variable = "zos"
zos  = NULL
zos2 = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 19 sec
  for (f in 1:2) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    dates <- nc.get.time.series(f=nc, time.dim.name="time")
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
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
      filter(year <= 2020)
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
      zos = rbind(zos, grid)
    }
    
    # save zos2
    #----------
    zos2 = rbind(zos2, zos)
  }
})

unique(zos2$year)
unique(zos2$month)
rm(dates,dates2,grid,var,var1,files,lat,lon,i,f,
   nc,t,variable)






####################################
###        mlost ssp126        #####
####################################
variable = "mlotst"
mlotst  = NULL
mlotst2 = NULL

# import files of interest containing env data: 
# to be downloaded first on CMIP6 portal (https://esgf-node.llnl.gov/search/cmip6/)
files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                          "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable,"/"),
                   pattern=ssp)
files
setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
             "PAPER3/ENV.DATA/GCM/",model,"_25km/",variable))


#--------------------
system.time({    # 19 sec
  for (f in 1:2) { 
    
    # open each netcdf file
    #----------------------------
    nc = nc_open(files[f])
    t <- ncvar_get(nc, "time")
    dates <- nc.get.time.series(f=nc, time.dim.name="time")
    # extract coordinates
    lon <- as.vector(ncvar_get(nc, "lon")) 
    lat <- as.vector(ncvar_get(nc, "lat"))
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
      filter(year <= 2020)
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
      mlotst = rbind(mlotst, grid)
    }
    
    # save zos2
    #----------
    mlotst2 = rbind(mlotst2, mlotst)
  }
})

unique(mlotst2$year)
unique(mlotst2$month)
rm(dates,dates2,grid,var,var1,files,lat,lon,i,f,nc,t,variable)











#############################################################
# convert tibbles into rasters on a regular grid (0.25 deg)
#############################################################

# aggregate both periods for each variable
#-------------------------------------------
tos = rbind(tos1,tos2)
sos = rbind(sos1,sos2)
zos = rbind(zos1,zos2)
mlotst = rbind(mlotst1,mlotst2)

tos$yymm = paste0(tos$year, "-", tos$month)
sos$yymm = paste0(sos$year, "-", sos$month)
zos$yymm = paste0(zos$year, "-", zos$month)
mlotst$yymm = paste0(mlotst$year, "-", mlotst$month)

# save temporary files
saveRDS(mlotst, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                          "/RDATA/3.ACP/6variables/",model,"/mlotst_",model,".rds"))
saveRDS(tos, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                            "/RDATA/3.ACP/6variables/",model,"/tos_",model,".rds"))
saveRDS(sos, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                            "/RDATA/3.ACP/6variables/",model,"/sos_",model,".rds"))
saveRDS(zos, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                            "/RDATA/3.ACP/6variables/",model,"/zos_",model,".rds"))





###################
###    WEST     ###
###################
i = "West"
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")

# import bathy for masking land
#--------------------------------
env <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/",
                      "Bel_West/Summer/Unscaled/rasterEnv_res0.25_Summer_Bel_West",
                      "_unscaled_1995-09.rds"))
# plot(env, col=viridis(64))
bathy = env[[4]]
names(bathy) = "layer"
w <- matrix(1, 3, 3)     # matrix for interpolation


#--------------------------------
# loop over each month and year
#---------------------------------
yymm = unique(tos$yymm)
system.time({  # 21 sec
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
    proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]]) # sss,ssh,sss,mlotst,bathy,dist
    saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                              "/RDATA/3.ACP/6variables/",model,"/",i,
                              "/Env_raster/Summer/Env_raster_6var_West_",model,"_",d,".rds"))
  }
})





###################
###   Bw East   ###
###################
i = "Bw_East"
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")

# import bathy for masking land
#--------------------------------
env <- readRDS("./RDATA/3.ACP/0.25deg/Bw_East/Summer/Unscaled/rasterEnv_res0.25_Summer_Bw_East_unscaled_2017-08.rds")
bathy = env[[1]]
names(bathy) = "layer"

#--------------------------------
# loop over each month and year
#---------------------------------
system.time({  # 66 sec
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
    proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]]) # sss,ssh,sss,mlotst,bathy,dist
    saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                              "/RDATA/3.ACP/6variables/",model,"/",i,
                              "/Env_raster/Summer/Env_raster_6var_",i,"_",model,"_",d,".rds"))
  }
})





###################
###   Nar East  ###
###################
i = "Nar_East"
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")

# import bathy for masking land
#--------------------------------
env <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA/3.ACP/0.25deg/",i,
                      "/Summer/Unscaled/rasterEnv_res0.25_Summer_",i,
                      "_unscaled_2010-08.rds"))
bathy = env[[1]]
names(bathy) = "layer"

#--------------------------------
# loop over each month and year
#---------------------------------
system.time({  # 537 sec
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
    proj = stack(tos_r, zos_r, sos_r, mlotst_r, env[[4]], env[[5]]) # sss,ssh,sss,mlotst,bathy,dist
    saveRDS(proj, file=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                              "/RDATA/3.ACP/6variables/",model,"/",i,
                              "/Env_raster/Summer/Env_raster_6var_",i,"_",model,"_",d,".rds"))
  }
})







