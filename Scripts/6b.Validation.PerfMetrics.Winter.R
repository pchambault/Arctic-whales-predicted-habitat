####################################################
####### perf metrics: spatial CV + caret     #######
#######         0.25 degrees in WINTER       #######
#######   6 variables: mld,sst,ssh,sss,bathy,dist
#######      narwhals, bowheads, belugas
#######         ffs + spatial CV + 7 algo    #######
####################################################


library(CAST)
library(caret)
library(raster)
library(viridis)
library(maps)
library(readr)
library(data.table)
library(CAST)
library(dplyr)
library(ggplot2)
library(reshape)
library(tidyquant)
library(dplyr)
library(tidyverse)


# select species and locality
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
species = c("Bw_West","Nar_West","Bw_East","Nar_East")

# choose the climate model:
models = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
# model = models[[1]]





######################################
# 0) loop over each model
######################################
system.time({ # 341 sec (6 min)
  for (model in models) { 
    
    ######################################
    # 1) loop over each species
    ######################################
    for (i in unique(species)) {
      ############################
      # 2) loop over each run
      ############################  
      perf_MOD = data.frame()
      
      #------------------------------
      # select run and test dataset
      #------------------------------
      for (n in 1:10){ 
        valid = readRDS(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                               "/split/valid_Winter_6var_",model,"_",i,"_run",n,".rds"))
        
        #------------------------------
        # select the model of run n
        #------------------------------
        m = readRDS(paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",model,
                           "/ffs_Winter_6var_",model,"_",i,"_run",n,".rds"))
        
        
        
        #############################################################
        # 3) loop over each algo 
        #############################################################       
        for (a in 1:length(m)){   # 7 algorithms
          mod = m[[a]]
          
          # predict on the validation dataset
          #------------------------------------
          pred   = caret::predict.train(mod, type="raw",#"prob", 
                                        valid[,c("sst_U","ssh_U","sss_U",
                                                 "mld_U","bathy_U","dist_U")]) 
          predictions = cbind(valid, "pred"=as.factor(pred))
          
          
          
          # extract performance metrics
          #-------------------------------
          df  = data.frame("accur"=0,"sensi"=0,"speci"=0,"F1"=0,"preval"=0,
                           "precision"=0,"recall"=0, "tss"=0,"model"=0,
                           "run"=0,"species"=0,"side"=0) 
          # confusion matrix
          conf = confusionMatrix(data=predictions$pred,
                                 reference=predictions$pres2,
                                 positive="yes")
          mat  = as.data.frame(conf$byClass)
          df$sensi  = mat[1,1]
          df$speci  = mat[2,1]
          df$precision = mat[5,1]
          df$recall = mat[6,1]
          df$F1     = mat[7,1]
          df$preval = mat[8,1]
          df$accur  = as.numeric(conf$overall[1])
          df$tss     = df$sensi + df$speci - 1
          df$model   = mod$method
          df$run     = n
          df$species = sub("_.*", "", i)  # retain string before "_"
          df$side    = sub(".*_", "", i)  # retain string after "_"
          
          # save metrics
          perf_MOD = rbind(perf_MOD,df)
          perf_MOD = as_tibble(perf_MOD)
          
        }  # loop over each algo
      }    # loop over each run
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      saveRDS(perf_MOD, file=paste0("./RDATA/5.CARET/",i,"/Summary_models/Winter/",
                                    model,"/PerfMOD_Caret_Winter_7algo_6var_",model,"_",
                                    i,".rds"))
    }
  }
})






######################################
# explo perf metrics
######################################
perf_nar_west <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/",
                                "2021/PAPER3/RDATA/5.CARET/Nar_West/Summary_models/",
                                "Winter/",model,"/PerfMOD_Caret_Winter_7algo_6var_",
                                model,"_Nar_West.rds"))
perf_nar_east <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/",
                                "2021/PAPER3/RDATA/5.CARET/Nar_East/Summary_models/",
                                "Winter/",model,"/PerfMOD_Caret_Winter_7algo_6var_",
                                model,"_Nar_East.rds"))
perf_bw_west <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/",
                               "2021/PAPER3/RDATA/5.CARET/Bw_West/Summary_models/",
                               "Winter/",model,"/PerfMOD_Caret_Winter_7algo_6var_",
                               model,"_Bw_West.rds"))
perf_bw_east <- readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/",
                               "2021/PAPER3/RDATA/5.CARET/Bw_East/Summary_models/",
                               "Winter/",model,"/PerfMOD_Caret_Winter_7algo_6var_",
                               model,"_Bw_East.rds"))

perf = rbind(perf_nar_west,perf_nar_east,perf_bw_west,perf_bw_east)
per  = as_tibble(perf)
saveRDS(perf, file=paste0("./RDATA/5.CARET/Performance/",
                          "/PerfMOD_Winter_7algo_6var_2sp_",model,"_",".rds"))

pivot = perf %>%
  pivot_longer(!c(model,species,side,run), 
               names_to = "metric", values_to = "value") 


## Bowheads  ##
#--------------
ggplot(pivot[pivot$species=="Bw" & !(pivot$metric=="precision")
             & !(pivot$metric=="preval") & !(pivot$metric=="recall"),], 
       aes(x=model, y=value, fill=model)) +
  geom_boxplot(outlier.shape = NA,lwd=0.2) +
  labs(y="", x="", title = "Bowheads") +
  facet_grid(side~metric) +
  ylim(0,1) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = 0.5, lwd=0.2, lty=2) +
  theme_tq() +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(size=5,color="black"),
        axis.title.x = element_text(size=5,color="black"),
        axis.text.x  = element_text(size=5,angle=90,vjust=0.5,hjust=0.5,color="black"),
        axis.text.y  = element_text(size=5,color="black"))

ggsave(filename=paste0("./FIGURES/CARET/Performance/Winter",
                       "/Boxplot_Winter_perf_10runs_7algo_spatialCV_6var_",
                       model,"_Bw.png"),
       width=5,height=3,dpi=400,family="serif")



## Narwhals  ##
#--------------
ggplot(pivot[pivot$species=="Nar" & !(pivot$metric=="precision")
             & !(pivot$metric=="preval") & !(pivot$metric=="recall"),], 
       aes(x=model, y=value, fill=model)) +
  geom_boxplot(outlier.shape = NA,lwd=0.2) +
  labs(y="", x="", title = "Narwhals", subtitle = "") +
  facet_grid(side~metric) +
  ylim(0,1) +
  scale_fill_brewer(palette = "Set2") +
  geom_hline(yintercept = 0.5, lwd=0.2, lty=2) +
  theme_tq() +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(size=5,color="black"),
        axis.title.x = element_text(size=5,color="black"),
        axis.text.x  = element_text(size=5,angle=90,vjust=0.5,hjust=0.5,color="black"),
        axis.text.y  = element_text(size=5,color="black"))

ggsave(filename=paste0("./FIGURES/CARET/Performance/Winter",
                       "/Boxplot_Winter_perf_10runs_7algo_spatialCV_6var_",
                       model,"_Nar.png"),
       width=5,height=3,dpi=400,family="serif")













#############################################
# find best model for each species and side
#############################################
pivot %>% # best model: RF
  group_by(species, side) %>%
  filter(metric == "accur") %>%
  slice_max(value)

pivot %>% 
  filter(pivot$model!="rf") %>% 
  group_by(species, side) %>%
  filter(metric == "accur") %>%
  slice_max(value)


best = pivot %>% 
  filter(metric!="F1" & metric!="preval" 
         & metric!="precision" & metric!="recall") %>%
  group_by(species, side, metric, model) %>%
  summarize(mean=mean(value),
            max=max(value),
            min=min(value),
            sd=sd(value))
best
best %>%
  group_by(metric,side,species) %>%
  slice_max(mean) %>%
  filter(metric == "accur")







