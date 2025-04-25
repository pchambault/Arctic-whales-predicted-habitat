#####################################################
#######   climatic projections from CMIP6      ######
#######      Winter, resolution: 0.25 deg      ######
####### 6 variables: MLD,SST+SSH+SSS+Bathy+dist #####
# 3 models: AWI-CM-1-1-MR,HadGEM3-GC31-MM,CNRM-CM6 #        
#####################################################

# https://pcmdi.llnl.gov/mips/cmip3/variableList.html#Table_O1e

library(gstat)
library(rgdal)
library(raster)
library(fields)
library(rgeos)
library(trip)
library(ade4)
library(ncdf4)
library(stringr)
library(viridis)
library(caret)
library(oce)
library(grDevices)
library(dplyr)


# select species and locality:
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
sp = c("Bw_West","Bw_East","Nar_West","Nar_East")

# choose the climate model:
models   = c("AWI-CM-1-1-MR","CNRM-CM6-1-HR","HadGEM3-GC31-MM")
# model = gcm[3]

# choose the scenario:
scenario = c("ssp126","ssp585")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}








###############################################################
####  stack projections for each run and the 10 decades    ####
###############################################################
system.time({  # 25 min
  for (model in models) { 
    for (ssp in scenario) {
      for (i in sp) {
        for (n in 1:10) { 
          # load best model
          #--------------------------
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          files = list.files(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                                    "/Best_Model/"), pattern="BestModel")
          setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                       "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,"/Best_Model/"))
          m = readRDS(files[[n]])
          
          
          #------------------------------
          # load env data over each date
          #------------------------------
          env = list.files(path=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA",
                                       "/6.Climatic_projections/",model,"/Rasters/",i,"/Winter/"),
                           pattern=ssp) 
          x <- stack()
          
          for (p in 1:length(env)) { 
            # import projections
            #-----------------------
            setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/RDATA",
                         "/6.Climatic_projections/",model,"/Rasters/",i,"/Winter"))
            proj <- readRDS(env[[p]])
            date  = substrRight(env[[p]], 11)
            date2 = substr(date,1,7)
            
            # convert raster into tibble
            #--------------------------------
            newdat = as_tibble(rasterToPoints(proj)) %>%
              dplyr::rename("sst_U" = layer.1, "ssh_U" = layer.2, "sss_U" = layer.3,
                            "mld_U" = layer.4.1,"bathy_U" = layer.4.2, "dist_U" = layer.5)
            newdat = newdat[!is.na(newdat$sss_U),]
            newdat = newdat[!is.na(newdat$dist_U),]
            
            
            # calculate future predictions 
            #------------------------------------
            pred_future <- caret::predict.train(m, type="prob", 
                                                newdat[,c(3:8)]) 
            newdat = as_tibble(cbind(newdat, "pred"=as.vector(pred_future$yes)))
            ras_future = rasterFromXYZ(newdat[,c("x","y","pred")])
            names(ras_future) = paste0("proj_",date2,"_run",n)
            # plot(ras_future, zlim=c(0,1), col=oceColorsDensity(64),cex.main=1)
            x <- stack(x ,ras_future)
          }
          
          # save raster stack
          #---------------------
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          saveRDS(x, file=paste0("./RDATA/6.Climatic_projections/",i,
                                 "/0.25deg/Winter/",model,"_",ssp,
                                 "/Proj_rasterStack_6var_Winter_",m$method,"_",
                                 model,"_",ssp,"_",i,
                                 "_run",n,".rds"))
          rm(x)
        }
      }
    }
  }
})










###########################################
## average projections over 10 runs/year ##
###########################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
year = as.character(seq(2020,2100,by=10))

system.time({  # 122 sec 
  for (model in models) { 
    for (ssp in scenario) {
      for (i in sp) {
        setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
        setwd(paste0("./RDATA/6.Climatic_projections/",i,
                     "/0.25deg/Winter/",model,"_",ssp))
        r = do.call(stack, lapply(list.files(pattern=ssp), readRDS))
        
        for (y in year) {
          rm(max,mean,sd,median)
          sub <- raster::subset(r, grep(y, names(r), value = T))
          
          # mean predictions over the 10 runs for each year
          #-------------------------------------------------
          mean <- calc(sub, fun=mean)
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          saveRDS(mean, file=paste0("./RDATA/6.Climatic_projections/",i,
                                    "/0.25deg/Winter/",model,"_",ssp,"/Average_6var/",
                                    "ProjMean_Winter_0.25deg_6var_10runs_bestAlgo_",
                                    i,"_",y,".rds"))
          
          png(filename=paste0("./FIGURES/CLIMATO/",i,
                              "/0.25deg/Winter/",model,"_",ssp,"/",
                              "ProjMean_Winter_0.25deg_6var_10runs_bestAlgo_",
                              i,"_",y,".png"),
              res=350,width=5, height=4.4,units="in")
          plot(mean, main=paste0("Mean Projections ", i,"\n ",y,
                                 " ",model," (",ssp,", best algo)"),
               zlim=c(0,1),col=viridis(64))
          dev.off()
          
          # max predictions over the 10 runs for each year
          #-----------------------------------------------
          max <- calc(sub, fun=max)
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          saveRDS(max, file=paste0("./RDATA/6.Climatic_projections/",i,
                                   "/0.25deg/Winter/",model,"_",ssp,"/Average_6var/",
                                   "ProjMax_Winter_0.25deg_6var_10runs_bestAlgo_",
                                   i,"_",y,".rds"))
          
          png(filename=paste0("./FIGURES/CLIMATO/",i,
                              "/0.25deg/Winter/",model,"_",ssp,"/",
                              "ProjMax_Winter_0.25deg_6var_10runs_bestAlgo_",
                              i,"_",y,".png"),
              res=350,width=5, height=4.4,units="in")
          plot(max, main=paste0("Max Projections ", i,"\n ",y,
                                " ",model," (",ssp,", best algo)"),
               zlim=c(0,1),col=viridis(64))
          dev.off()
          
          # median predictions over the 10 runs for each year
          #-----------------------------------------------
          median <- calc(sub, fun=median)
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          saveRDS(median, file=paste0("./RDATA/6.Climatic_projections/",i,
                                      "/0.25deg/Winter/",model,"_",ssp,"/Average_6var/",
                                      "ProjMedian_Winter_0.25deg_6var_10runs_bestAlgo_",
                                      i,"_",y,".rds"))
          
          png(filename=paste0("./FIGURES/CLIMATO/",i,
                              "/0.25deg/Winter/",model,"_",ssp,"/",
                              "ProjMedian_Winter_0.25deg_6var_10runs_bestAlgo_",
                              i,"_",y,".png"),
              res=350,width=5, height=4.4,units="in")
          plot(median, main=paste0("Median Projections ", i,"\n ",y,
                                   " ",model," (",ssp,", best algo)"),
               zlim=c(0,1),col=viridis(64))
          dev.off()
          
          
          # sd predictions over the 10 runs for each year
          #-----------------------------------------------
          sd <- calc(sub, fun=sd)
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          saveRDS(sd, file=paste0("./RDATA/6.Climatic_projections/",i,
                                  "/0.25deg/Winter/",model,"_",ssp,"/Average_6var/",
                                  "ProjSD_Winter_0.25deg_6var_10runs_bestAlgo_",
                                  i,"_",y,".rds"))
          
          png(filename=paste0("./FIGURES/CLIMATO/",i,
                              "/0.25deg/Winter/",model,"_",ssp,"/",
                              "ProjSD_Winter_0.25deg_6var_10runs_bestAlgo_",
                              i,"_",y,".png"),
              res=350,width=5, height=4.4,units="in")
          plot(sd, main=paste0("SD Projections ", i,"\n ",y,
                               " ",model," (",ssp,", best algo)"),
               zlim=c(0,1),col=viridis(64))
          dev.off()
        }
      }
      rm(sub,r)
    }
  }
})












########################################################
# export projections from 2020 to 2100 for each run
########################################################
library(tidyverse)
library(tidyquant)
library(oce)
north_map = map_data("world") %>% group_by(group)
shore     = north_map[north_map$region=="Canada" 
                      | north_map$region=="Greenland"
                      | north_map$region=="Norway"
                      | north_map$region=="Russia"
                      | north_map$region=="Sweden"
                      | north_map$region=="Denmark"
                      | north_map$region=="Finland"
                      | north_map$region=="Iceland",]


system.time({  #  73 sec
  for (model in models) { 
    for (ssp in scenario) {
      for (i in sp) {
        files = list.files(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                                  "/RDATA/6.Climatic_projections/",i,"/0.25deg/Winter/",
                                  model,"_",ssp), pattern="Proj")
        for (n in 1:10) { 
          setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                       "/RDATA/6.Climatic_projections/",i,"/0.25deg/Winter/",
                       model,"_",ssp))
          r = readRDS(files[n])
          
          
          png(filename=paste0("/Users/philippinechambault/Documents/POST-DOC/2021/",
                              "PAPER3/FIGURES/CLIMATO/",i,"/0.25deg/Winter/",model,
                              "_",ssp,"/runs/","Proj_Winter2020-2100_6var_bestAlgo_",
                              model,"_",ssp,"_run_",n,"_",i,".png"),
              res=350, width=5, height=4.4,units="in")
          plot(r, zlim=c(0,1), col=oceColorsDensity(64),cex.main=1)
          dev.off()
        }
      }
    }
  }
})





