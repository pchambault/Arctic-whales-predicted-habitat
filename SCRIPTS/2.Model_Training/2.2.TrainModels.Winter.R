##################################################
#### NARWHALS BOWHEADS, BELUGAS GREENLAND ########
####    CARET - Machine learning algo     ########
####  pseudo-absence outside convexHull   ########
####       resolution=0.25 degrees        ########
####          spatial CV with ffs         ########
#### 6 variables: MLD,SST,SSH,SSS,bathy,dist #####
####          Winter: Dec to March
##################################################

library(CAST)
library(caret)
library(klaR)
library(kernlab)
library(nlme)
library(mgcv)
library(mboost)
library(import)
library(viridis)
library(C50)
library(earth)
library(arm)
library(raster)
library(maps)
library(MLeval)     # calculate AUC, sensitivity, specificity...
library(gridExtra)
library(gbm)
library(caTools)
library(corrplot)
library(RColorBrewer)
library(doParallel)
library(parallel)
library(foreach)
library(dplyr)


# ensemble      : RF, GBM, BRT
# regression    : GLM, GAM, MARS
# decision tree : CART, C5.0
# Support Vector Machine: SVM with Radial Basis Function Kernel
# neural network: perceptron, back-propagation, Hopfield Network


# select each species and locality
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
species = c("Bw_West","Nar_West","Bw_East","Nar_East")

# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
model = models[[1]]





######################################
# 1) loop over each species
######################################
cl <- parallel::makeCluster(5)
doParallel::registerDoParallel(cl)

system.time({ # 6257 sec (2h) with the mac with 7 algo
  for (i in species) { # 
    
    ######################################
    # 2) loop over each side
    ######################################
      # load and prepare data
      #---------------------------
      abs = readRDS(paste0("./RDATA/4.Pseudo-abs/Winter/",model,"/",
                           "abs_Winter_Indiv_6var_",model,"_",i,".rds"))
      
      # remove IDs when less than 2 whales/year
      #----------------------------------------
      abs = abs %>% 
        group_by(year, month) %>% 
        filter(n_distinct(id) >= 2) 
      abs$id   = droplevels(abs$id)
      abs = abs[!is.na(abs$sst_U),] 
      abs = abs[,c("run","id","x","y","pres","year","species","side","month",
                   "sst_U","ssh_U","sss_U","mld_U","bathy_U","dist_U")]
      abs$year = as.numeric(as.character(abs$year))
      
      
      
      
      ##########################################
      # 3) loop over each run (n) 
      ##########################################
      foreach(n=1:10) %dopar% {
        library(caret)
        library(nlme)
        library(mgcv)
        library(mboost)
        library(import)
        library(earth)
        library(arm)
        library(gridExtra)
        library(gbm)
        library(caTools)
        library(corrplot)
        library(RColorBrewer)
        library(CAST)
        library(tidyverse)
        library(dplyr)
      
        
        # select run
        #-----------
        data = abs[abs$run==n,]
        run  = unique(data$run)
        predictors <- c("sst_U","ssh_U","sss_U","mld_U","bathy_U","dist_U")
        data$pres = as.factor(as.numeric(data$pres))
        data$pres2 = "no"
        data$pres2[data$pres=="1"] = "yes"
        data$pres2 = as.factor(data$pres2)
        data = data[!is.na(data$dist_U),]
        data$index = seq(1, nrow(data), by = 1)
        
        
        #------------------------------------------
        # data partitioning (train, validation, test)
        # using spatial k fold cross validation 
        #------------------------------------------
        set.seed(10)
        indices <- CreateSpacetimeFolds(data, k=4,
                                        spacevar="id")
                                        # timevar="yymm")
        
        # split between train, validation and test
        #-------------------------------------------
        ind = data.frame(indices$index[[1]])
        colnames(ind) = "index"
        train = data %>%
          right_join(ind, by = "index") %>%
          dplyr::select(-c(index))
        
        ind = data.frame(indices$indexOut[[1]])
        colnames(ind) = "index"
        valid = data %>%
          right_join(ind, by = "index") %>%
          dplyr::select(-c(index))
        
        ind = data.frame(indices$indexOut[[3]])
        colnames(ind) = "index"
        test = data %>%
          right_join(ind, by = "index") %>%
          dplyr::select(-c(index))
        
        setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
        saveRDS(train, file=paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",
                                   model,"/split/train_Winter_6var_",model,"_",i,
                                   "_run",n,".rds"))
        saveRDS(valid, file=paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",
                                   model,"/split/valid_Winter_6var_",model,"_",i,
                                   "_run",n,".rds"))
        saveRDS(test, file=paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",
                                   model,"/split/test_Winter_6var_",model,"_",i,
                                  "_run",n,".rds"))
        
        
        # algo parametrization using k folds
        #-------------------------------------
        set.seed(10)
        indices <- CreateSpacetimeFolds(train, k=4,
                                        spacevar="id")
        ctrl = trainControl(method="cv",
                            number=4,
                            index=indices$index,
                            indexOut=indices$indexOut,
                            savePredictions=TRUE)
        
        
        
        
        
        
        ############################################################
        # evaluate aglorithms for each run
        ############################################################
        
        #------------------------
        # ensemble: RF, GBM, BRT
        #------------------------
        # Random Forest
        set.seed(10)  
        fit.rf <- ffs(train[,predictors],train$pres2,
                      preProc = c("center", "scale", "nzv"),
                      method="rf", ntree=50,
                      trControl=ctrl)
        
        # Gradient Boosting Machine
        set.seed(10)  
        fit.gbm <- ffs(train[,predictors],train$pres2, 
                       preProc = c("center", "scale", "nzv"),
                       method="gbm", 
                       trControl=ctrl)
        
        # Boosted Tree
        set.seed(10)  
        fit.bt = ffs(train[,predictors],train$pres2,
                     preProc = c("center", "scale", "nzv"),
                     method="blackboost", 
                     trControl=ctrl) 
        
        
        #-----------------------------
        # regression: GAM, MARS, GLM
        #-----------------------------
        # Multivariate Adaptive Regression Splines (MARS)
        set.seed(10)  
        fit.mars <- ffs(train[,predictors],train$pres2, 
                        preProc = c("center", "scale", "nzv"),
                        method="earth", 
                        trControl=ctrl)  
        
        # gam boost
        set.seed(10)  
        fit.gam <- ffs(train[,predictors],train$pres2,
                       preProc = c("center", "scale", "nzv"),
                       method="gamboost", 
                       trControl=ctrl)
        
        # glm boost
        set.seed(10)  
        fit.glm <- ffs(train[,predictors],train$pres2,
                       preProc = c("center", "scale", "nzv"),
                       method="glmboost", 
                       trControl=ctrl)
        
        
        #------------------
        # neural network
        #------------------
        set.seed(10)  
        fit.nnet <- ffs(train[,predictors],train$pres2,
                       preProc = c("center", "scale", "nzv"),
                       method="nnet", 
                       trControl=ctrl)
        
        
        #---------------------
        # Compare algorithms
        #---------------------
        results <- resamples(list(GBM  = fit.gbm, RF  = fit.rf,  BRT = fit.bt,
                                  MARS = fit.mars,GAM = fit.gam, GLM = fit.glm,
                                  NNET = fit.nnet))
        
        setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
        png(filename=paste0("./FIGURES/CARET/",i,"/Summary_Models/Winter/",
                            "Dotplot_spatialCV_ffs_Winter_6var_",model,"_",
                            i,"_run",n,".png"),
            res=350,width=5, height=5,units="in")
        print(dotplot(results))
        dev.off()
        
        
        saveRDS(results, file=paste0("./RDATA/5.CARET/",i,
                                     "/Summary_models/Winter/",model,
                                     "/Results_ffs_Winter_6var_",model,"_",
                                     i,"_run",n,".rds"))
        
        
        
        #---------------------
        # save model output
        #---------------------
        models <- list(GBM  = fit.gbm,  RF  = fit.rf,  BRT = fit.bt,
                       MARS = fit.mars, GAM = fit.gam, GLM = fit.glm,
                       NNET = fit.nnet)
        saveRDS(models, file=paste0("./RDATA/5.CARET/",i,
                                    "/Summary_models/Winter/",model,
                                    "/ffs_Winter_6var_",model,"_",
                                    i,"_run",n,".rds"))
        
      }    # loop over each run
  }        # loop over each species
  gc()
  stopCluster(cl)
  doParallel::registerDoParallel()
})









