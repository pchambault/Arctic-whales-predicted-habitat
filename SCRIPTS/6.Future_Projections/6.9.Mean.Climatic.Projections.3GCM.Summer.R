####################################################
####### MEAN PREDICTIONS from the 3 GCMs  ##########   
#######        spatial CV + caret         ##########
#######     0.25 degrees in SUMMER        ##########
######         from CMIP6 models          ##########
####### 6 var: mld, ssh, sss, sst, bathy + dist ####
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

# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
model1 = models[[1]]
model2 = models[[2]]
model3 = models[[3]]

# choose scenario:
scenario = c("ssp126","ssp585")

# choose species:
species = c("Bel_West","Bw_West","Nar_West","Bw_East","Nar_East")







######################################
# current predictions
######################################
system.time({ # 0.5 sec 
  for (i in species) { 
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      m1 <- readRDS(paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model1,
                           "/Average_6var_bestAlgo/PredMean_ffs_bestAlgo_Summer_6var_",
                           model1,"_",i,".rds"))
      m2 <- readRDS(paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model2,
                           "/Average_6var_bestAlgo/PredMean_ffs_bestAlgo_Summer_6var_",
                           model2,"_",i,".rds"))
      m3 <- readRDS(paste0("./RDATA/5.CARET/",i,"/Predictions/Summer/",model3,
                           "/Average_6var_bestAlgo/PredMean_ffs_bestAlgo_Summer_6var_",
                           model3,"_",i,".rds"))
      
      # mean predictions
      #-------------------
      stack = stack(m1,m2,m3)
      mean <- calc(stack, fun=mean)
      saveRDS(mean, file=paste0("./RDATA/5.CARET/Mean_pred_3GCM/Summer/",
                                "PredMean_3GCMs_ffs_6var_Summer_",i,".rds"))
      
      # SD predictions
      #-------------------
      sd <- calc(stack, fun=sd)
      saveRDS(sd, file=paste0("./RDATA/5.CARET/Mean_pred_3GCM/Summer/",
                                "PredSD_3GCMs_ffs_6var_Summer_",i,".rds"))
      
      # CV predictions
      #----------------
      cv <- (sd/mean) * 100
      saveRDS(cv, file=paste0("./RDATA/5.CARET/Mean_pred_3GCM/Summer/",
                              "PredCV_3GCMs_ffs_6var_Summer_",i,".rds"))
      
     rm(mean,sd,cv,stack, m1, m2, m3)
  }
})








######################################
# future projections
######################################
system.time({ #  sec 
  for (ssp in scenario) {
    for (i in species) { 
      ## 2030 ##
      #---------
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      m1 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model1,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2030.rds"))
      m2 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model2,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2030.rds"))
      m3 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model3,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2030.rds"))
      
      # mean predictions
      stack = stack(m1,m2,m3)
      mean <- calc(stack, fun=mean)
      saveRDS(mean, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                                "ProjMean_3GCMs_ffs_6var_Summer2030_",i,"_",ssp,".rds"))
      
      # SD predictions
      sd <- calc(stack, fun=sd)
      saveRDS(sd, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                              "ProjSD_3GCMs_ffs_6var_Summer2030_",i,"_",ssp,".rds"))
      
      # CV predictions
      cv <- (sd/mean) * 100
      saveRDS(cv, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                              "ProjCV_3GCMs_ffs_6var_Summer2030_",i,"_",ssp,".rds"))
      rm(mean,sd,cv,m1,m2,m3,stack)
      
      
      ## 2060 ##
      #---------
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      m1 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model1,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2060.rds"))
      m2 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model2,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2060.rds"))
      m3 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model3,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2060.rds"))
      
      # mean predictions
      stack = stack(m1,m2,m3)
      mean <- calc(stack, fun=mean)
      saveRDS(mean, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                                "ProjMean_3GCMs_ffs_6var_Summer2060_",i,"_",ssp,".rds"))
      
      # SD predictions
      sd <- calc(stack, fun=sd)
      saveRDS(sd, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                              "ProjSD_3GCMs_ffs_6var_Summer2060_",i,"_",ssp,".rds"))
      
      # CV predictions
      cv <- (sd/mean) * 100
      saveRDS(cv, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                              "ProjCV_3GCMs_ffs_6var_Summer2060_",i,"_",ssp,".rds"))
      rm(mean,sd,cv,stack,m1, m2, m3)
      
      
      ## 2100 ##
      #---------
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      m1 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model1,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2100.rds"))
      m2 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model2,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2100.rds"))
      m3 <- readRDS(paste0("./RDATA/6.Climatic_projections/",i,"/0.25deg/Summer/",model3,
                           "_",ssp,"/Average_6var/",
                           "ProjMean_Summer_0.25deg_6var_10runs_bestAlgo_",i,"_2100.rds"))
      
      # mean predictions
      stack = stack(m1,m2,m3)
      mean <- calc(stack, fun=mean)
      saveRDS(mean, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                                "ProjMean_3GCMs_ffs_6var_Summer2100_",i,"_",ssp,".rds"))
      
      # SD predictions
      sd <- calc(stack, fun=sd)
      saveRDS(sd, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                              "ProjSD_3GCMs_ffs_6var_Summer2100_",i,"_",ssp,".rds"))
      
      # CV predictions
      cv <- (sd/mean) * 100
      saveRDS(cv, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                              "ProjCV_3GCMs_ffs_6var_Summer2100_",i,"_",ssp,".rds"))
      
      rm(mean,sd,cv,stack,m1, m2, m3)
    }
  }
})
