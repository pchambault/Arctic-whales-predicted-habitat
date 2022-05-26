########################################
####  SI Table: models performance  ####
########################################

library(tidyverse)
library(ggplot2)
library(gridExtra)



bel <- readRDS("./RDATA/5.CARET/VarImp/Predictors_3GCMs_10runs_Bel_West.rds")
bel = as_tibble(bel)
bel



#-------------------------
# load summer performance
#-------------------------
m1 <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Summer_7algo_6var_3sp_AWI-CM-1-1-MR_.rds")
m1$gcm = "AWI-CM-1-1-MR"
m2 <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Summer_7algo_6var_3sp_CNRM-CM6-1-HR_.rds")
m2$gcm = "CNRM-CM6-1-HR"
m3 <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Summer_7algo_6var_3sp_HadGEM3-GC31-MM_.rds")
m3$gcm = "HadGEM3-GC31-MM"

perf_summer = rbind(m1,m2,m3)
perf_summer = perf_summer %>%
  select(-c(F1,preval,precision,recall,tss)) %>%
  mutate(season = "Summer")


#-------------------------
# load winter performance
#-------------------------
m1 <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_AWI-CM-1-1-MR_.rds")
m1$gcm = "AWI-CM-1-1-MR"
m2 <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_CNRM-CM6-1-HR_.rds")
m2$gcm = "CNRM-CM6-1-HR"
m3 <- readRDS("./RDATA/5.CARET/Performance/PerfMOD_Winter_7algo_6var_2sp_HadGEM3-GC31-MM_.rds")
m3$gcm = "HadGEM3-GC31-MM"

perf_winter = rbind(m1,m2,m3)
perf_winter = perf_winter %>%
  select(-c(F1,preval,precision,recall,tss)) %>%
  mutate(season = "Winter")

perf = rbind(perf_summer, perf_winter)
perf
sum = perf %>%
  group_by(model, species, side, gcm, season) %>%
  summarise(mean_acc = mean(accur),
            sd_acc   = sd(accur),
            mean_sensi = mean(sensi),
            sd_sensi   = sd(sensi),
            mean_speci = mean(speci),
            sd_speci   = sd(speci))

sum = sum %>% 
  mutate(model = recode(model, 
                        blackboost = "brt",
                        earth = "mars"),
         species = recode(species, 
                          Bel = "Beluga",
                          Bw = "Bowhead", 
                          Nar = "Narwhal"))
sum = sum %>%
  arrange(season, species, side)
sum = sum[,c(5,2:4,1,6:11)]
sum
str(sum)

sum = sum %>%
  mutate(mean_acc = signif(mean_acc, 2),
         sd_acc = signif(sd_acc, 2),
         mean_sensi = signif(mean_sensi, 2),
         sd_sensi = signif(sd_sensi, 2),
         mean_speci = signif(mean_speci, 2),
         sd_speci = signif(sd_speci, 2))
sum
names(sum)

sum$accuracy    = paste0(sum$mean_acc,"±", signif(sum$sd_acc),2)
sum$sensitivity = paste0(sum$mean_sensi,"±", signif(sum$sd_sensi),2)
sum$specificity = paste0(sum$mean_speci,"±", signif(sum$sd_speci),2)
sum = sum %>%
  select(-c(mean_acc, sd_acc, mean_sensi, sd_sensi,
            mean_speci, sd_speci)) %>%
  rename("algorithm" = model)
names(sum)= c("Season","Species","Side","GCM","Algorithm",
              "Accuracy","Sensitivity","Specificity")
sum
library(data.table)
fwrite(sum, "./SI.Table.PerfModels.csv")


#------------------------------------------------------
# pivot table wider to have the 7 algorithm in 1 row
#------------------------------------------------------
names(sum)
acc = sum %>%
  dplyr::select(-c(Sensitivity, Specificity)) %>%
  group_by(Season, Species, Side, GCM, Algorithm) %>%
  spread("Algorithm","Accuracy")
acc$Metric = "Accuracy"

sensi = sum %>%
  dplyr::select(-c(Accuracy, Specificity)) %>%
  group_by(Season, Species, Side, GCM, Algorithm) %>%
  spread("Algorithm","Sensitivity")
sensi$Metric = "Sensitivity"

speci = sum %>%
  dplyr::select(-c(Accuracy, Sensitivity)) %>%
  group_by(Season, Species, Side, GCM, Algorithm) %>%
  spread("Algorithm","Specificity")
speci$Metric = "Specificity"

perf = rbind(acc, sensi, speci)
names(perf)
perf = perf[,c(1:4,12,5:11)]
perf
perf = perf[order(perf$Species),]
perf = perf[order(perf$Side),]


fwrite(perf, "./SI.Table.PerfModels.Wider.csv")

