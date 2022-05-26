####################################################
#######       CURRENT PREDICTIONS 
#######       spatial CV + caret          ##########
#######     0.25 degrees in WINTER        ##########
#######         from CMIP6 models
####### 6 var: mld, ssh, sss, sst, bathy + dist 
####################################################

library(caret)
library(raster)
library(viridis)
library(maps)
library(readr)
library(data.table)
library(CAST)
library(dplyr)
library(weanlingNES)
library(oce)
library(stringr)

# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")




##################################################
# BOWHEADS, NARWHALS WEST
# generate predictions for each algo, run and year
##################################################
system.time({ # 250 sec 
  species = c("Bw_West","Nar_West")
  
  for (model in models) { 
    #------------------------------
    # loop over each species
    #------------------------------
    for (i in species) { 
      # load and prepare data
      #---------------------------
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      abs = readRDS(paste0("./RDATA/4.Pseudo-abs/Winter/",model,
                           "/abs_Winter_Indiv_6var_",model,"_",i,".rds"))
      abs = abs[!is.na(abs$sst_U),]
      
      # remove IDs when less than 2 whales/year
      #----------------------------------------
      abs = abs %>% 
        group_by(year, month) %>% 
        filter(n_distinct(id) >= 2) 
      abs$id   = droplevels(abs$id)
      abs$year = droplevels(abs$year)
      
      # remove remaining NA values for env variables
      #----------------------------------------------
      abs = abs[,c("run","id","pres","year","species","side",
                   "sst_U","ssh_U","sss_U","mld_U",
                   "bathy_U","dist_U","month")]
      abs$year = as.numeric(as.character(abs$year))
      abs$yymm = paste0(abs$year,"-",abs$month)
      
      
      # loop over each run
      #--------------------
      stack <- stack()
      for (n in 1:10) {
        files = list.files(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                                  "/Best_Model/"), pattern="BestModel")
        setwd(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                     "/Best_Model/"))
        mod = readRDS(files[[n]])
        predictors = predictors(mod)
        
        # loop over each yymm
        #----------------------
        for (y in unique(abs$yymm)) {
          x = abs[abs$run==n & abs$yymm==y,]
          
          # import env raster for corresponding year
          #-----------------------------------------
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          ras = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",sub(".*_", "", i),
                               "/Env_raster/Winter/Env_raster_6var_",
                               sub(".*_", "", i),"_",model,"_",y,".rds"))
          newdat = data.frame(rasterToPoints(ras))
          colnames(newdat) = c("x","y","sst_U","ssh_U","sss_U",
                               "mld_U","bathy_U","dist_U")
          newdat = newdat[!is.na(newdat$bathy_U),]
          newdat = newdat[!is.na(newdat$dist_U),]
          newdat = newdat[!is.na(newdat$sst_U),]
          xy     = newdat[,c("x","y")]
          
          # generate predictions for year y
          #---------------------------------
          pred   = predict.train(mod, type="prob", 
                                 newdat[,predictors]) 
          df     = cbind(xy, "z"=pred[,"yes"])
          r      = rasterFromXYZ(df)
          names(r) = paste0(mod$method,"_",y,"_run",n)
          stack <- stack(stack, r)
          
        } # loop over each yymm
      }   # loop over each run
      saveRDS(stack,file=paste0("./RDATA/5.CARET/",i,
                                "/Predictions/Winter/",model,
                                "/Pred_Winter_6var_bestAlgo_",model,"_",
                                i,"_",mod$method,"_10runs.rds"))
    }       # loop over each species
  }
})
    
    
    
    
   



##################################################
# BOWHEADS and NARWHALS EAST
# generate predictions for each algo, run and year
##################################################
species = c("Bw_East","Nar_East")

#------------------------------
# loop over each species
#------------------------------
system.time({ # 270 sec (4 min) 
  for (model in models) { 
    for (i in species) { 
      # load and prepare data
      #---------------------------
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      abs = readRDS(paste0("./RDATA/4.Pseudo-abs/Winter/",model,
                           "/abs_Winter_Indiv_6var_",model,"_",i,".rds"))
      abs = abs[!is.na(abs$sst_U),]
      
      # remove IDs when less than 2 whales/year
      #----------------------------------------
      abs = abs %>% 
        group_by(year, month) %>% 
        filter(n_distinct(id) >= 2) 
      abs$id   = droplevels(abs$id)
      abs$year = droplevels(abs$year)
      
      # remove remaining NA values for env variables
      #----------------------------------------------
      abs = abs[,c("run","id","pres","year","species","side",
                   "sst_U","ssh_U","sss_U","mld_U",
                   "bathy_U","dist_U","month")]
      abs$year = as.numeric(as.character(abs$year))
      abs$yymm = paste0(abs$year,"-",abs$month)
      
      # load each run
      #---------------
      stack <- stack()
      for (n in 1:10) {
        files = list.files(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                                  "/Best_Model/"), pattern="BestModel")
        setwd(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                     "/Best_model/"))
        mod = readRDS(files[[n]])
        predictors = c("sst_U","ssh_U","sss_U","mld_U","bathy_U","dist_U")
        
        # load each yymm
        #-------------------
        for (y in unique(abs$yymm)) {
          x = abs[abs$run==n & abs$yymm==y,]
          
          # import env raster for corresponding year
          #-----------------------------------------
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          ras = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",i,
                               "/Env_raster/Winter/Env_raster_6var_",
                               i,"_",model,"_",y,".rds"))
          newdat = data.frame(rasterToPoints(ras))
          colnames(newdat) = c("x","y","sst_U","ssh_U","sss_U",
                               "mld_U","bathy_U","dist_U")
          newdat = newdat[!is.na(newdat$bathy_U),]
          newdat = newdat[!is.na(newdat$dist_U),]
          newdat = newdat[!is.na(newdat$sst_U),]
          xy     = newdat[,c("x","y")]
          
          # generate predictions for year y
          #---------------------------------
          pred   = predict.train(mod, type="prob", 
                                 newdat[,predictors]) 
          df     = cbind(xy, "z"=pred[,"yes"])
          r      = rasterFromXYZ(df)
          names(r) = paste0(mod$method,"_",y,"_run",n)
          stack <- stack(stack, r)
          
        } # loop over each yymm
      }   # loop over each run
      saveRDS(stack,file=paste0("./RDATA/5.CARET/",i,
                                "/Predictions/Winter/",model,
                                "/Pred_Winter_6var_bestAlgo_",model,"_",
                                i,"_",mod$method,"_10runs.rds"))
    }       # loop over each species
  }
})














##################################################
# export yearly predictions for each run
##################################################
system.time({  # 3 sec
  for (model in models) { 
    for (i in species) {
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      file = list.files(paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model), 
                        pattern="Pred_Winter_6var_bestAlgo")
      
      setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                   "/RDATA/5.CARET/",i,"/Predictions/Winter/",model))
      r    = readRDS(file[[1]])
      dat  = data.frame(str_split_fixed(names(r), "_", 3))
      algo = unique(dat$X1)
      dat$year  = substr(dat$X2, 1, 4)
      dat$month = substr(dat$X2, 6, 7)
      dat = as_tibble(dat)
      
      for (y in unique(dat$year)) {
        sub = raster::subset(r, grep(y, names(r), value = T))
        setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
        png(filename=paste0("./FIGURES/CARET/",i,
                            "/0.25deg/Predictions/Winter/",model,"/years_bestAlgo/",
                            "PredStack_bestAlgo_Winter_6var_10runs_year",y,"_",
                            i,"_",algo,".png"),
            res=350, width=6, height=4.4,units="in")
        plot(sub, col = oceColorsDensity(64), zlim=c(0,1))
        dev.off()
        rm(sub)
      }
    } 
  }
})
















####################################################
# export average predictions over all years
####################################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
species = c("Bw_West","Nar_West","Bw_East","Nar_East")
rm(max,mean,r,sd,cv,median)

system.time({   # 9 sec
  for (model in models) { 
    for (i in species) { 
      
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      file = list.files(paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model,"/"),
                        pattern="Pred_Winter_6var_bestAlgo")
      setwd(paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model))
      r = readRDS(file[[1]])
      
      # calculate max predictions
      max <- calc(r, fun=max)
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      saveRDS(max, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model,
                               "/Average_6var_bestAlgo/PredMax_ffs_bestAlgo_Winter_6var_",
                               model,"_",i,".rds"))
      
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Winter/",model,
                          "/bestAlgo/PredMax_ffs_bestAlgo_Winter_6var_",model,"_",
                          i,".png"),
          res=350,width=5, height=4.4,units="in")
      plot(max, col=oceColorsDensity(64),
           main=paste0(i,"\n Max (",model,")"),
           zlim=c(0,1))
      dev.off()
      
      
      # calculate average predictions
      mean <- calc(r, fun=mean)
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      saveRDS(mean, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model,
                                "/Average_6var_bestAlgo/PredMean_ffs_bestAlgo_Winter_6var_",
                                model,"_",i,".rds"))
      
      png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Winter/",model,
                          "/bestAlgo/PredMean_ffs_bestAlgo_Winter_6var_",model,"_",
                          i,".png"),
          res=350,width=5, height=4.4,units="in")
      plot(mean, col=oceColorsDensity(64),
           main=paste0(i," \n Mean (",model,")")
           ,zlim=c(0,1))
      dev.off()
      
      # calculate median predictions
      median <- calc(r, fun=median)
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      saveRDS(median, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model,
                                  "/Average_6var_bestAlgo/PredMedian_ffs_bestAlgo_Winter_6var_",
                                  model,"_",i,".rds"))
      
      png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Winter/",model,
                          "/bestAlgo/PredMedian_ffs_bestAlgo_Winter_6var_",model,"_",
                          i,".png"),
          res=350,width=5, height=4.4,units="in")
      plot(median, col=oceColorsDensity(64),
           main=paste0(i," \n Median (",model,")")
           ,zlim=c(0,1))
      dev.off()
      
      
      # calculate SD predictions
      sd <- calc(r, fun=sd)
      saveRDS(sd, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model,
                              "/Average_6var_bestAlgo/PredSD_ffs_bestAlgo_Winter_6var_",
                              model,"_",i,".rds"))
      
      png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Winter/",model,
                          "/bestAlgo/PredSD_ffs_bestAlgo_Winter_6var_",model,"_",
                          i,".png"),
          res=350,width=5, height=4.4,units="in")
      plot(sd, col=oceColorsDensity(64),
           main=paste0(i," \n SD (",model,")"))
      dev.off()
      
      
      # calculate CV predictions
      cv <- (sd/mean) * 100
      saveRDS(cv, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Winter/",model,
                              "/Average_6var_bestAlgo/PredCV_ffs_bestAlgo_Winter_6var_",
                              model,"_", i,".rds"))
      
      png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Winter/",model,
                          "/bestAlgo/PredCV_ffs_bestAlgo_Winter_6var_",model,"_",
                          i,".png"),
          res=350,width=5, height=4.4,units="in")
      plot(cv, col=oceColorsDensity(64),
           main=paste0(i," \n CV (",model,")"))
      dev.off()
      
      rm(max,mean,r,sd,cv,median)
    }
  }
})



