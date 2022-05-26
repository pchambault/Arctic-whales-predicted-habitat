############################################################################
############################################################################
###### PCA on bowheads, narwhals, belugas both sides of Greenland in Summer
######        to combine env variables for pseudo-abs generation
######        spatial resolution of 0.25 decimal degrees
######          6 variables: bathy + SST + dist2shore + SSH + SSS + MLD
######                  SUMMER: Aug to Sep
######               using env data from CMIP6 model
############################################################################
############################################################################


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

# Nar refers to Narwhal, Bw to Bowhead and Bel to Beluga

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
rm(list=ls())


# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
model = models[[1]] # run the whole script for each model separately





##############################################################
# PCA for West side of Greenland
##############################################################
j = "West"

#-----------------------------
# calculate ACP for each year
#-----------------------------
system.time({   # 40 sec
  # select env data generated from script:
  # "1a.PCA.Prepare.Data.Summer_...R"
  setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
  files   = list.files(path=paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                   "/Env_raster/Summer/"),
                       pattern="Env_raster")
  
  for (f in 1:length(files)) { 
    setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                 "/RDATA/3.ACP/6variables/",model,"/",j,"/Env_raster/Summer/"))
    ras  = readRDS(files[[f]])
    d = gsub(".*[_]([^.]+)[.].*", "\\1", files[f])
    
    
    # extract coordinates of raster brick with orginal resolution
    # create the background for pseudo-absences
    #-------------------------------------------------------------
    xy   = coordinates(ras) 
    ras2 = values(ras)
    rownames(ras2) <- 1:nrow(ras2)  # generate cell numbers
    ras2 = na.omit(ras2) 
    xy   = xy[as.numeric(rownames(ras2)),] 
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    saveRDS(xy,file=paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                           "/PCA/Summer/xy_rasterBrick_Summer_",model,"_",
                           j,"_",d,".rds"))
    gc()
    
    # calculate ACP
    #-----------------
    ordi <- dudi.pca(ras2, nf=2, scannf=F) 
    ordi  = cbind(xy,ordi$li)          # add values of Axis1 and Axis2 to ordi
    ordi2 = rasterFromXYZ(ordi)        # RasterBrick: (x,y) coordinates of raster "axis 1" and raster "axis 2"
    names(ordi2) = c("Axis1","Axis2")  # RasterBrick
    writeRaster(ordi2,filename=paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                      "/PCA/Summer/Ordi_Summer_",model,
                                      j,"_",d,".tif"), 
                format="GTiff",overwrite=TRUE,
                options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
    gc()
    
    # convert ordi into SpatialPixelsDataFrame
    #-------------------------------------------
    ordi_pix <- as(ordi2,"SpatialPixelsDataFrame") # 2 columns: Axis1, Axis2
    ordi_pix@data$CellID   <- 1:nrow(ordi_pix)
    ENVspace <- ordi_pix@data    # dataframe with 3 columns: Axis1, Axis2, cellID
    saveRDS(ordi_pix,file=paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                 "/PCA/Summer/OrdiPix_Summer_", model,"_",
                                 j,"_",d,".rds"))
    saveRDS(ENVspace,file=paste0("./RDATA/3.ACP/6variables/",model,"/",j,
                                 "/PCA/Summer/ENVspace_Summer_",model,"_",
                                 j,"_",d, ".rds"))
    gc()
  }
})







##############################################################
# PCA for Narwhals and Bowheads East
##############################################################
j  = "East"
sp = c("Nar","Bw")

#-----------------------------
# calculate ACP for each year
#-----------------------------
system.time({   # 190 sec
  for (i in sp) { 
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    files   = list.files(path=paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                     "/Env_raster/Summer/"),
                         pattern="Env_raster")
    
    for (f in 1:length(files)) { 
      setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                   "/RDATA/3.ACP/6variables/",model,"/",i,"_",j,"/Env_raster/Summer/"))
      ras  = readRDS(files[[f]])
      d = gsub(".*[_]([^.]+)[.].*", "\\1", files[f])
      
      
      # extract coordinates of raster brick with orginal resolution
      # create the background for pseudo-absences
      #-------------------------------------------------------------
      xy   = coordinates(ras) 
      ras2 = values(ras)
      rownames(ras2) <- 1:nrow(ras2)  # generate cell numbers
      ras2 = na.omit(ras2) 
      xy   = xy[as.numeric(rownames(ras2)),] 
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      saveRDS(xy,file=paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                             "/PCA/Summer/xy_rasterBrick_Summer_",model,"_",i,"_",
                             j,"_",d,".rds"))
      gc()
      
      # calculate ACP
      #-----------------
      ordi <- dudi.pca(ras2, nf=2, scannf=F) 
      ordi  = cbind(xy,ordi$li)          # add values of Axis1 and Axis2 to ordi
      ordi2 = rasterFromXYZ(ordi)        # RasterBrick: (x,y) coordinates of raster "axis 1" and raster "axis 2"
      names(ordi2) = c("Axis1","Axis2")  # RasterBrick
      writeRaster(ordi2,filename=paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                        "/PCA/Summer/Ordi_Summer_",model,i,"_",
                                        j,"_",d,".tif"), 
                  format="GTiff",overwrite=TRUE,
                  options=c("INTERLEAVE=BAND","COMPRESS=LZW"))
      gc()
      
      # convert ordi into SpatialPixelsDataFrame
      #-------------------------------------------
      ordi_pix <- as(ordi2,"SpatialPixelsDataFrame") # 2 columns: Axis1, Axis2
      ordi_pix@data$CellID   <- 1:nrow(ordi_pix)
      ENVspace <- ordi_pix@data    # dataframe with 3 columns: Axis1, Axis2, cellID
      saveRDS(ordi_pix,file=paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                   "/PCA/Summer/OrdiPix_Summer_", model,"_",i,"_",
                                   j,"_",d,".rds"))
      saveRDS(ENVspace,file=paste0("./RDATA/3.ACP/6variables/",model,"/",i,"_",j,
                                   "/PCA/Summer/ENVspace_Summer_",model,"_",i,"_",
                                   j,"_",d, ".rds"))
      gc()
    }
  }
})










##############################################
# prepare dataset
##############################################

# import filtered data from Dryad
#------------------------------------
loc <- read.csv("./Locations_3species_West-East_Greenland.csv", sep=";") %>% 
  as_tibble %>%
  filter(month == 8 | month == 9)
loc$dateTime = as.POSIXct(strptime(loc$dateTime,format="%d/%m/%Y %H:%M"), tz="GMT")
loc
table(loc$species)
table(loc$month, loc$species)



# extract env data to remove locs without data
#-----------------------------------------------
library(maps)
model = "AWI-CM-1-1-MR"

## West ##
west = loc %>% filter(side == "West")
env <- readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/West/Env_raster/",
                      "Winter/Env_raster_6var_West_",model,"_1993-12.rds"))
west$env = raster::extract(env[[1]], as.matrix(west[,c("lon","lat")]))
summary(west$env)  
plot(lat~lon, west, pch=19, cex=0.2)
points(lat~lon, west[is.na(west$env),], col="red", pch=19, cex=0.2)
map(add=T)
west = west[!is.na(west$env),]

saveRDS(west[west$species=="Bel",], 
        "./RDATA/4.Pseudo-abs/Summer/Bel_West_withoutNA_0.25deg.rds")
saveRDS(west[west$species=="Bw",], 
        "./RDATA/4.Pseudo-abs/Summer/Bw_West_withoutNA_0.25deg.rds")
saveRDS(west[west$species=="Nar",], 
        "./RDATA/4.Pseudo-abs/Summer/Nar_West_withoutNA_0.25deg.rds")


## Bw East ##
bwE = loc %>% filter(side == "East" & species == "Bw")
env <- readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/Bw_East/Env_raster/",
                      "Winter/Env_raster_6var_Bw_East_",model,"_1993-02.rds"))
bwE$env = raster::extract(env[[1]], as.matrix(bwE[,c("lon","lat")]))
summary(bwE$env) 
plot(lat~lon, bwE, pch=19, cex=0.2)
points(lat~lon, bwE[is.na(bwE$env),], col="red", pch=19, cex=0.2)
map(add=T)
bwE = bwE[!is.na(bwE$env),]
saveRDS(bwE, "./RDATA/4.Pseudo-abs/Summer/Bw_East_withoutNA_0.25deg.rds")

## Nar East ##
narE = loc %>% filter(side == "East" & species == "Nar")
env <- readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/Nar_East/Env_raster/",
                      "Winter/Env_raster_6var_Nar_East_",model,"_1993-02.rds"))
narE$env = raster::extract(env[[1]], as.matrix(narE[,c("lon","lat")]))
summary(narE$env) 
nrow(narE[is.na(narE$env),]) / nrow(narE) * 100 
plot(lat~lon, narE, pch=19, cex=0.2)
points(lat~lon, narE[is.na(narE$env),], col="red", pch=19, cex=0.2)
map(add=T)
narE = narE[!is.na(narE$env),]
saveRDS(narE, "./RDATA/4.Pseudo-abs/Summer/Nar_East_withoutNA_0.25deg.rds")






