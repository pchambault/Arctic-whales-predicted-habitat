####################################################
####### PREDICTIONS : spatial CV + caret  ##########
#######     0.25 degrees in SUMMER        ##########
######         from CMIP6 models
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

setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")

# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
model = models[[1]]





##################################################
# BELUGAS, BOWHEADS, NARWHALS WEST
# generate predictions for each algo, run and year
##################################################
species = c("Bel_West","Bw_West","Nar_West")

#------------------------------
# loop over each species
#------------------------------
system.time({ # 1307 sec (21 min)
  for (i in species) { 
    # load and prepare data
    #---------------------------
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    abs = readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,
                         "/abs_Summer_Indiv_6var_",model,"_",i,".rds"))
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
    
    # loop over each algo
    #---------------------
    for (a in 1:7) {
      
      # loop over each run
      #--------------------
      stack <- stack()
      for (run in 1:10) {
        models = readRDS(paste0("./RDATA/5.CARET/",i,"/Summary_models/Summer/",model,
                                "/ffs_Summer_6var_",model,"_",i,"_run",run,".RDS"))
        mod = models[[a]]
        predictors = mod$selectedvars 
        
        # loop over each yymm
        #----------------------
        for (y in unique(abs$yymm)) {
          x = abs[abs$run==run & abs$yymm==y,]
          
          # import env raster for corresponding year
          #-----------------------------------------
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          ras = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",sub(".*_", "", i),
                               "/Env_raster/Summer/Env_raster_6var_",
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
          names(r) = paste0(mod$method,"_",y,"_run",run)
          stack <- stack(stack, r)
          
        } # loop over each yymm
      }   # loop over each run
      saveRDS(stack,file=paste0("./RDATA/5.CARET/",i,
                              "/Predictions/Summer/",model,
                              "/Pred_Summer_6var_",model,"_",
                              i,"_",mod$method,"_10runs.rds"))
    }     # loop over each algo
  }       # loop over each species
})









##################################################
# BOWHEADS and NARWHALS EAST
# generate predictions for each algo, run and year
##################################################
species = c("Bw_East","Nar_East")

#------------------------------
# loop over each species
#------------------------------
system.time({ # 1155 sec (19 min) 
  for (i in species) { 
    # load and prepare data
    #---------------------------
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    abs = readRDS(paste0("./RDATA/4.Pseudo-abs/Summer/",model,
                         "/abs_Summer_Indiv_6var_",model,"_",i,".rds"))
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
    
    # load each algo
    #-------------------  
    for (a in 1:7) {
      
      # load each run
      #---------------
      stack <- stack()
      for (run in 1:10) {
        models = readRDS(paste0("./RDATA/5.CARET/",i,"/Summary_models/Summer/",model,
                                "/ffs_Summer_6var_",model,"_",i,"_run",run,".RDS"))
        mod = models[[a]]
        predictors = mod$selectedvars 
        
        # load each yymm
        #-------------------
        for (y in unique(abs$yymm)) {
          x = abs[abs$run==run & abs$yymm==y,]
          
          # import env raster for corresponding year
          #-----------------------------------------
          setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
          ras = readRDS(paste0("./RDATA/3.ACP/6variables/",model,"/",i,
                               "/Env_raster/Summer/Env_raster_6var_",
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
          names(r) = paste0(mod$method,"_",y,"_run",run)
          stack <- stack(stack, r)
          
        } # loop over each yymm
      }   # loop over each run
      saveRDS(stack,file=paste0("./RDATA/5.CARET/",i,
                              "/Predictions/Summer/",model,
                              "/Pred_Summer_6var_",model,"_",
                              i,"_",mod$method,"_10runs.rds"))
    }     # loop over each algo
  }       # loop over each species
})



















##################################################
# export yearly predictions for each run
##################################################
library(stringr)
library(oce)
species = c("Bel_West","Bw_West","Bw_east","Nar_West","Nar_East")

system.time({  # 65 sec
  for (i in species) {
    files = list.files(paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model), 
                       pattern="Pred_Summer")
    
    for (n in 1:length(files)) { 
      setwd(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3/",
                   "/RDATA/5.CARET/",i,"/Predictions/Summer/",model))
      r = readRDS(files[[n]])
      dat  = data.frame(str_split_fixed(names(r), "_", 3))
      algo = unique(dat$X1)
      dat$year  = substr(dat$X2, 1, 4)
      dat$month = substr(dat$X2, 6, 7)
      dat = as_tibble(dat)
      
      for (y in unique(dat$year)) {
        sub = raster::subset(r, grep(y, names(r), value = T))
        setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
        png(filename=paste0("./FIGURES/CARET/",i,
                            "/0.25deg/Predictions/Summer/",model,"/years/",
                            "PredStack_Summer_6var_10runs_year",y,"_",
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
algo = c("gbm","rf","earth","gamboost","blackboost","glmboost","nnet")
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
rm(max,mean,r,sd,cv,median)

system.time({   # 26 sec
  for (i in species) { 
  #------------------------------
  # loop over each algo
  #------------------------------
  for (a in algo) {
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    file = list.files(paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model,"/"),
                       pattern=a)
    setwd(paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model))
    r = readRDS(file)
    
    # calculate max predictions
    max <- calc(r, fun=max)
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    saveRDS(max, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model,
                             "/Average_6var/PredMax_ffs_Summer_6var_",model,"_",
                             i,"_",a,".rds"))
    
    png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Summer/",model,
                        "/PredMax_ffs_Summer_6var_",model,"_",
                        i,"_",a,".png"),
        res=350,width=5, height=4.4,units="in")
    plot(max, col=oceColorsDensity(64),
         main=paste0(i," \n Max ",a," (",model,")"),
         zlim=c(0,1))
    dev.off()
    
    
    # calculate average predictions
    mean <- calc(r, fun=mean)
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    saveRDS(mean, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model,
                             "/Average_6var/PredMean_ffs_Summer_6var_",model,"_",
                             i,"_",a,".rds"))
    
    png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Summer/",model,
                        "/PredMean_ffs_Summer_6var_",model,"_",
                        i,"_",a,".png"),
        res=350,width=5, height=4.4,units="in")
    plot(mean, col=oceColorsDensity(64),
         main=paste0(i," \n Mean ",a," (",model,")"),
         zlim=c(0,1))
    dev.off()
    
    # calculate median predictions
    median <- calc(r, fun=median)
    setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
    saveRDS(median, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model,
                              "/Average_6var/PredMean_ffs_Summer_6var_",model,"_",
                              i,"_",a,".rds"))
    
    png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Summer/",model,
                        "/PredMedian_ffs_Summer_6var_",model,"_",
                        i,"_",a,".png"),
        res=350,width=5, height=4.4,units="in")
    plot(median, col=oceColorsDensity(64),
         main=paste0(i," \n Median ",a," (",model,")"),
         zlim=c(0,1))
    dev.off()
    
    
    # calculate SD predictions
    sd <- calc(r, fun=sd)
    saveRDS(sd, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model,
                              "/Average_6var/PredSD_Sffs_ummer_6var_",model,"_",
                              i,"_",a,".rds"))
    
    png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Summer/",model,
                        "/PredSD_ffs_Summer_6var_",model,"_",
                        i,"_",a,".png"),
        res=350,width=5, height=4.4,units="in")
    plot(sd, col=oceColorsDensity(64),
         main=paste0(i," \n SD ",a," (",model,")"))
    dev.off()
    
    
    # calculate CV predictions
    cv <- (sd/mean) * 100
    saveRDS(cv, file=paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model,
                              "/Average_6var/PredCV_ffs_Summer_6var_",model,"_",
                              i,"_",a,".rds"))
    
    png(filename=paste0("./FIGURES/CARET/",i,"/0.25deg/Predictions/Summer/",model,
                        "/PredCV_ffs_Summer_6var_",model,"_",
                        i,"_",a,".png"),
        res=350,width=5, height=4.4,units="in")
    plot(cv, col=oceColorsDensity(64),
         main=paste0(i," \n CV ",a," (",model,")"))
    dev.off()
    
    rm(max,mean,r,sd,cv,median)
   }
  }
})



