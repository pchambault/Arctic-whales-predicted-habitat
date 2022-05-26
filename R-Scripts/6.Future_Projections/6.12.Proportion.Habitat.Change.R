##############################################################
####### calculate habitat loss, gain, no change     ##########
#######             SUMMER & WINTER                 ##########
#######         for each species and locality
##############################################################

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(png)
library(smoothr)


models   = c("AWI-CM-1-1-MR", "CNRM-CM6-1-HR", "HadGEM3-GC31-MM")
scenario = c("ssp126","ssp585")
proba    = 0.5  # favorable habitat when probability of presence > 0.5



#########################################
#########     SUMMER          ###########
#########################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
species = c("Bel_West","Bw_West","Bw_East","Nar_West","Nar_East")
season  = "Summer"


#-----------------------------------------------------------------------------
# stack rasters of gain, loss and no change for the 3 GCMs for each scenario
#-----------------------------------------------------------------------------
system.time({    # 1 sec
  for (i in species) {
    for (ssp in scenario) { 
        #----------------------------------------
        # import rasters for present and 2100
        #----------------------------------------
        present = readRDS(paste0("./RDATA/5.CARET/Mean_pred_3GCM/",season,"/",
                                 "PredMean_3GCMs_ffs_6var_",season,"_",i,".rds"))
        
        proj  = readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Summer/",
                               "/ProjMean_3GCMs_ffs_6var_Summer2100_",i,"_",ssp,".rds"))
        
        #-----------------------------------------
        # calculate loss between present and 2100
        #-----------------------------------------
        present[present<proba] = NA
        proj[proj<proba] = NA
        
        loss = mask(present, mask=proj, inverse=T)
        same = mask(proj, mask=present)
        gain = mask(proj, mask=present, inverse=T)
        
        #----------------------------------------------------
        # identify and remove patches of disconnected cells
        #----------------------------------------------------
        n = 8
        clumpz <- clump(loss, directions=n, gaps=F)
        f <- data.frame(freq(clumpz))  # frequency table of clumps
        excludeID <- f$value[which(f$count <= n)] # put these into a vector of clump ID's to be removed
        loss[clumpz %in% excludeID] <- NA # exclude these
        
        clumpz <- clump(same, directions=n, gaps=F)
        f <- data.frame(freq(clumpz)) 
        excludeID <- f$value[which(f$count <= n)]
        same[clumpz %in% excludeID] <- NA 
        
        clumpz <- clump(gain, directions=n, gaps=F)
        f <- data.frame(freq(clumpz)) 
        excludeID <- f$value[which(f$count <= n)]
        gain[clumpz %in% excludeID] <- NA
        
        
        hab_change <- stack(gain,loss,same)
        saveRDS(hab_change, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                                        "/Hab_Change/Stack/HabChange_Mean_Stack_",
                                        season,"_3GCMs_",ssp,"_",i,".rds") )
      } # loop over each scenario
    }   # loop over each species
})






#-----------------------------------------
# convert into polygon and then tibble
#-----------------------------------------
system.time({    # 2 sec
  for (i in species) {
    hab_ssp   = NULL
    for (ssp in scenario) {
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      r = readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                         "/RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/Stack/HabChange_Mean_Stack_Summer_3GCMs_",
                         ssp,"_",i,".rds"))
      names(r) = c("gain","loss","same")
      gain = r[[1]]
      loss = r[[2]]
      same = r[[3]]

      
      #-------------------------------------
      # convert to polygon and reshape
      #-------------------------------------
      ## habitat loss ##
      if (all(is.na(getValues(loss)))) {
        loss = NULL} else{
          pol = rasterToPolygons(loss,dissolve=T,na.rm=T)
          pol <- aggregate(pol,dissolve=T)
          pol <- smooth(pol, method = "ksmooth", smoothness=5) # smooth polygon
          loss        <- fortify(pol, region = "id")
          loss$hab    = "Loss"
        }

      ## habitat gain  ##
      if (all(is.na(getValues(gain)))) {
        gain = NULL} else{
          pol = rasterToPolygons(gain,dissolve=T,na.rm=T)
          pol <- aggregate(pol,dissolve=T)
          pol <- smooth(pol, method = "ksmooth", smoothness=5)
          gain        <- fortify(pol, region = "id")
          gain$hab    = "Gain"
        }

      ## no change in habitat ##
      if (all(is.na(getValues(same)))) {
        same = NULL} else{
          pol = rasterToPolygons(same,dissolve=T,na.rm=T)
          pol <- aggregate(pol,dissolve=T)
          pol <- smooth(pol, method = "ksmooth" ,smoothness=5)
          same        <- fortify(pol, region = "id")
          same$hab    = "No change"
        }

      hab = as_tibble(rbind(same, gain, loss)) %>%
        mutate("sp"   = sub("_.*", "", i),
               "side" = sub(".*_", "", i),
               "ssp"  = ssp)

      hab_ssp = rbind(hab_ssp, hab)
      saveRDS(hab_ssp, file=paste0("./RDATA/6.Climatic_projections/",
                                   "Mean_pred_3GCM/Hab_Change/",
                                   "HabChange_Mean_3GCMs_",season,"_",i,
                                   "_",ssp,".rds") )

    } # loop over scenario
  }   # loop over species
})















#########################################
#########     Winter          ###########
#########################################
setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
species = c("Bw_West","Bw_East","Nar_West","Nar_East")
season  = "Winter"
proba   = 0.5


#-----------------------------------------------------------------------------
# stack rasters of gain, loss and no change for the 3 GCMs for each scenario
#-----------------------------------------------------------------------------
system.time({    # 1 sec
  for (i in species) {
    for (ssp in scenario) { 
      #----------------------------------------
      # import rasters for present and 2100
      #----------------------------------------
      present = readRDS(paste0("./RDATA/5.CARET/Mean_pred_3GCM/",season,"/",
                               "PredMean_3GCMs_ffs_6var_",season,"_",i,".rds"))
      
      proj  = readRDS(paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/Winter/",
                             "/ProjMean_3GCMs_ffs_6var_Winter2100_",i,"_",ssp,".rds"))
      
      #-----------------------------------------
      # calculate loss between present and 2100
      #-----------------------------------------
      present[present<proba] = NA
      proj[proj<proba] = NA
      
      loss = mask(present, mask=proj, inverse=T)
      same = mask(proj, mask=present)
      gain = mask(proj, mask=present, inverse=T)
      
      #----------------------------------------------------
      # identify and remove patches of disconnected cells
      #----------------------------------------------------
      n = 8
      clumpz <- clump(loss, directions=n, gaps=F)
      f <- data.frame(freq(clumpz))  # frequency table of clumps
      excludeID <- f$value[which(f$count <= n)] # put these into a vector of clump ID's to be removed
      loss[clumpz %in% excludeID] <- NA # exclude these
      
      clumpz <- clump(same, directions=n, gaps=F)
      f <- data.frame(freq(clumpz)) 
      excludeID <- f$value[which(f$count <= n)]
      same[clumpz %in% excludeID] <- NA 
      
      clumpz <- clump(gain, directions=n, gaps=F)
      f <- data.frame(freq(clumpz)) 
      excludeID <- f$value[which(f$count <= n)]
      gain[clumpz %in% excludeID] <- NA
      
      
      hab_change <- stack(gain,loss,same)
      saveRDS(hab_change, file=paste0("./RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                                      "/Hab_Change/Stack/HabChange_Mean_Stack_",
                                      season,"_3GCMs_",ssp,"_",i,".rds") )
    } # loop over each scenario
  }   # loop over each species
})






#-----------------------------------------
# convert into polygon and then tibble
#-----------------------------------------
system.time({    # 2 sec
  for (i in species) {
    hab_ssp   = NULL
    for (ssp in scenario) {
      setwd("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3")
      r = readRDS(paste0("/Users/philippinechambault/Documents/POST-DOC/2021/PAPER3",
                         "/RDATA/6.Climatic_projections/Mean_pred_3GCM/",
                         "Hab_Change/Stack/HabChange_Mean_Stack_Winter_3GCMs_",
                         ssp,"_",i,".rds"))
      names(r) = c("gain","loss","same")
      gain = r[[1]]
      loss = r[[2]]
      same = r[[3]]
      
      
      #-------------------------------------
      # convert to polygon and reshape
      #-------------------------------------
      ## habitat loss ##
      if (all(is.na(getValues(loss)))) {
        loss = NULL} else{
          pol = rasterToPolygons(loss,dissolve=T,na.rm=T)
          pol <- aggregate(pol,dissolve=T)
          pol <- smooth(pol, method = "ksmooth", smoothness=5) # smooth polygon
          loss        <- fortify(pol, region = "id")
          loss$hab    = "Loss"
        }
      
      ## habitat gain ##
      if (all(is.na(getValues(gain)))) {
        gain = NULL} else{
          pol = rasterToPolygons(gain,dissolve=T,na.rm=T)
          pol <- aggregate(pol,dissolve=T)
          pol <- smooth(pol, method = "ksmooth", smoothness=5)
          gain        <- fortify(pol, region = "id")
          gain$hab    = "Gain"
        }
      
      ## no change in habitat ##
      if (all(is.na(getValues(same)))) {
        same = NULL} else{
          pol = rasterToPolygons(same,dissolve=T,na.rm=T)
          pol <- aggregate(pol,dissolve=T)
          pol <- smooth(pol, method = "ksmooth" ,smoothness=5)
          same        <- fortify(pol, region = "id")
          same$hab    = "No change"
        }
      
      hab = as_tibble(rbind(same, gain, loss)) %>%
        mutate("sp"   = sub("_.*", "", i),
               "side" = sub(".*_", "", i),
               "ssp"  = ssp)
      
      hab_ssp = rbind(hab_ssp, hab)
      saveRDS(hab_ssp, file=paste0("./RDATA/6.Climatic_projections/",
                                   "Mean_pred_3GCM/Hab_Change/",
                                   "HabChange_Mean_3GCMs_",season,"_",i,
                                   "_",ssp,".rds") )
      
    } # loop over scenario
  }   # loop over species
})
