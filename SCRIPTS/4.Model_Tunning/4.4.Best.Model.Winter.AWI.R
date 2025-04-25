##################################################
####          NARWHALS BOWHEADS GREENLAND ########
####    CARET - Machine learning algo     ########
####  pseudo-absence outside convexHull   ########
####       resolution=0.25 degrees        ########
####         spatial CV, ffs              ########
#### 6 variables: MLD,SST,SSH,SSS,bathy,dist #####
####          Winter: Dec to March          ######
####   tune the best algo for each species  ######
####          GCM: AWI-CM-1-1-MR
##################################################

library(CAST)
library(caret)
library(tidyverse)
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
# bayesian      : NB, BN, BBN
# decision tree : CART, C5.0


setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
species = c("Bw_West","Nar_West","Bw_East","Nar_East")

# choose the climate model:
model = "AWI-CM-1-1-MR"





##############################################
# find best algo for each spcies
##############################################
perf <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                       "/RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_",model,"_.rds"))
perf
pivot = perf %>%
  pivot_longer(!c(model,species,side,run), 
               names_to = "metric", values_to = "value") 

best = pivot %>% 
  group_by(species, side, metric, model) %>%
  filter(metric!="F1" & metric!="preval") %>%
  summarize(mean=mean(value),
            max=max(value),
            min=min(value),
            sd=sd(value))
best
best %>%
  group_by(metric,side,species) %>%
  filter(metric == "accur") %>%
  slice_max(mean)

best %>%
  group_by(metric,side,species) %>%
  filter(metric == "accur" & model!="rf") %>%
  slice_max(mean)

# Bw West: nnet
# Bw East: nnet
# Nar West: blackboost
# Nar East: nnet




















##########################################
#  Bowheads West: nnet
##########################################
i    = "Bw_West"
algo = "nnet"

grid <- expand.grid(
  decay = c(0.5, 1e-2, 1e-3, 1e-4),
  size = c(3,5,10,20))


#--------------------------------
# loop over each run (n) 
#--------------------------------
system.time({ # 330 sec (5 min) 
  cl <- parallel::makeCluster(5)
  doParallel::registerDoParallel(cl)
  
  foreach(n=1:10) %dopar% { 
    library(caret)
    library(klaR)
    library(kernlab)
    library(nlme)
    library(mgcv)
    library(mboost)
    library(import)
    library(C50)
    library(earth)
    library(arm)
    library(gridExtra)
    library(gbm)
    library(caTools)
    library(corrplot)
    library(RColorBrewer)
    library(CAST)
    
    #---------------------------------------
    # select run containing 80% of the data
    #---------------------------------------
    data <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                           "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,"/split/",
                           "train_Winter_6var_",model,"_",i,"_run",n,".rds"))
    
    #------------------------------------------
    # algo parametrization
    # spatial k fold cross validation 
    #------------------------------------------
    set.seed(10)
    indices <- CreateSpacetimeFolds(data, k=4,
                                    spacevar="id")
    ctrl = trainControl(method="cv",
                        number=4,
                        index=indices$index,
                        indexOut=indices$indexOut,
                        savePredictions=TRUE)
    
    
    #-----------------------------------------------------------
    # import best algo from train to extract selected variables
    #-----------------------------------------------------------
    ffs <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                          "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                          "/ffs_Winter_6var_",model,"_",i,"_run",n,".rds"))
    m = ffs[[7]]    
    predictors = m$selectedvars 
    
    
    #------------------------
    # run best algo
    #------------------------
    fit <- train(data[,predictors], data$pres2, 
                 preProc   = c("center", "scale", "nzv"),
                 method    = algo, 
                 tuneGrid  = grid,
                 trControl = ctrl)
    
    saveRDS(fit, file=paste0("./RDATA/5.CARET/",i,
                             "/Summary_models/Winter/",model,
                             "/Best_Model/BestModel_ffs_Winter_6var_",model,"_",
                             i,"_",m$method,"_run",n,".rds"))
  }    # loop over each run
  gc()
  stopCluster(cl)
  doParallel::registerDoParallel()
})











##########################################
#  Bowheads East: nnet
##########################################
i    = "Bw_East"
algo = "nnet"

grid <- expand.grid(
  decay = c(0.5, 1e-2, 1e-3, 1e-4),
  size = c(3,5,10,20))


#--------------------------------
# loop over each run (n) 
#--------------------------------
system.time({ # 60 sec
  cl <- parallel::makeCluster(5)
  doParallel::registerDoParallel(cl)
  
  foreach(n=1:10) %dopar% { 
    library(caret)
    library(klaR)
    library(kernlab)
    library(nlme)
    library(mgcv)
    library(mboost)
    library(import)
    library(C50)
    library(earth)
    library(arm)
    library(gridExtra)
    library(gbm)
    library(caTools)
    library(corrplot)
    library(RColorBrewer)
    library(CAST)
    
    #---------------------------------------
    # select run containing 80% of the data
    #---------------------------------------
    data <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                           "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,"/split/",
                           "train_Winter_6var_",model,"_",i,"_run",n,".rds"))
    
    #-----------------------------------------------------------
    # import best algo from train to extract selected variables
    #-----------------------------------------------------------
    ffs <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                          "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                          "/ffs_Winter_6var_",model,"_",i,"_run",n,".rds"))
    m = ffs[[7]]
    predictors = m$selectedvars 
    
    
    #------------------------------------------
    # algo parametrization
    # spatial k fold cross validation 
    #------------------------------------------
    set.seed(10)
    indices <- CreateSpacetimeFolds(data, k=4,
                                    spacevar="id")
    ctrl = trainControl(method="cv",
                        number=4,
                        index=indices$index,
                        indexOut=indices$indexOut,
                        savePredictions=TRUE)
    
    
    #------------
    # run algo
    #------------
    fit <- train(data[,predictors], data$pres2, 
                 preProc   = c("center", "scale", "nzv"),
                 method    = algo, 
                 tuneGrid  = grid,
                 trControl = ctrl)
    
    saveRDS(fit, file=paste0("./RDATA/5.CARET/",i,
                             "/Summary_models/Winter/",model,
                             "/Best_Model/BestModel_Winter_6var_",model,"_",
                             i,"_",m$method,"_run",n,".rds"))
  }    # loop over each run
  gc()
  stopCluster(cl)
  doParallel::registerDoParallel()
})











##########################################
#  Narwhals West: blackboost
##########################################
i    = "Nar_West"
algo = "blackboost"

grid <- expand.grid(
  mstop = c(50,100,150,200,250,300),
  maxdepth = c(1,2,3,4,5))


#--------------------------------
# loop over each run (n) 
#--------------------------------
system.time({ # 591 sec (10 min)
  cl <- parallel::makeCluster(5)
  doParallel::registerDoParallel(cl)
  
  foreach(n=1:10) %dopar% { 
    library(caret)
    library(klaR)
    library(kernlab)
    library(nlme)
    library(mgcv)
    library(mboost)
    library(import)
    library(C50)
    library(earth)
    library(arm)
    library(gridExtra)
    library(gbm)
    library(caTools)
    library(corrplot)
    library(RColorBrewer)
    library(CAST)
    
    #---------------------------------------
    # select run containing 80% of the data
    #---------------------------------------
    data <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                           "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,"/split/",
                           "train_Winter_6var_",model,"_",i,"_run",n,".rds"))
    
    #-----------------------------------------------------------
    # import best algo from train to extract selected variables
    #-----------------------------------------------------------
    ffs <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                          "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                          "/ffs_Winter_6var_",model,"_",i,"_run",n,".rds"))
    m = ffs[[3]]   # select blackboost
    predictors = m$selectedvars 
    
    #------------------------------------------
    # algo parametrization
    # spatial k fold cross validation 
    #------------------------------------------
    set.seed(10)
    indices <- CreateSpacetimeFolds(data, k=4,
                                    spacevar="id")
    ctrl = trainControl(method="cv",
                        number=4,
                        index=indices$index,
                        indexOut=indices$indexOut,
                        savePredictions=TRUE)
    
    #------------
    # run algo
    #------------
    fit <- train(data[,predictors], data$pres2, 
                 preProc   = c("center", "scale", "nzv"),
                 method    = algo, 
                 tuneGrid  = grid,
                 trControl = ctrl)
    
    saveRDS(fit, file=paste0("./RDATA/5.CARET/",i,
                             "/Summary_models/Winter/",model,
                             "/Best_Model/BestModel_ffs_Winter_6var_",model,"_",
                             i,"_",m$method,"_run",n,".rds"))
  }    # loop over each run
  gc()
  stopCluster(cl)
  doParallel::registerDoParallel()
})











##########################################
#  Narwhals East: nnet
##########################################
i    = "Nar_East"
algo = "nnet"

grid <- expand.grid(
  decay = c(0.5, 1e-2, 1e-3, 1e-4),
  size = c(3,5,10,20))


#--------------------------------
# loop over each run (n) 
#--------------------------------
system.time({ # 956 sec (1§ sec)
  cl <- parallel::makeCluster(5)
  doParallel::registerDoParallel(cl)
  
  foreach(n=1:10) %dopar% { 
    library(caret)
    library(klaR)
    library(kernlab)
    library(nlme)
    library(mgcv)
    library(mboost)
    library(import)
    library(C50)
    library(earth)
    library(arm)
    library(gridExtra)
    library(gbm)
    library(caTools)
    library(corrplot)
    library(RColorBrewer)
    library(CAST)
    
    #---------------------------------------
    # select run containing 80% of the data
    #---------------------------------------
    data <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                           "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,"/split/",
                           "train_Winter_6var_",model,"_",i,"_run",n,".rds"))
    
    #-----------------------------------------------------------
    # import best algo from train to extract selected variables
    #-----------------------------------------------------------
    ffs <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                          "/RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                          "/ffs_Winter_6var_",model,"_",i,"_run",n,".rds"))
    m = ffs[[7]]   
    predictors = m$selectedvars 
    
    
    #------------------------------------------
    # algo parametrization
    # spatial k fold cross validation 
    #------------------------------------------
    set.seed(10)
    indices <- CreateSpacetimeFolds(data, k=4,
                                    spacevar="id")
    ctrl = trainControl(method="cv",
                        number=4,
                        index=indices$index,
                        indexOut=indices$indexOut,
                        savePredictions=TRUE)
    
    #------------
    # run algo
    #------------
    fit <- train(data[,predictors], data$pres2, 
                 preProc   = c("center", "scale", "nzv"),
                 method    = algo, 
                 tuneGrid  = grid,
                 trControl = ctrl)
    
    saveRDS(fit, file=paste0("./RDATA/5.CARET/",i,
                             "/Summary_models/Winter/",model,
                             "/Best_Model/BestModel_Winter_6var_",model,"_",
                             i,"_",m$method,"_run",n,".rds"))
  }    # loop over each run
  gc()
  stopCluster(cl)
  doParallel::registerDoParallel()
})











