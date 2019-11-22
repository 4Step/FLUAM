# Replicate FLUAM Logic 
# Purpose: To understand FLUAM Logic and ability to replicate outside CPP code
# TODO:: portions of the code shall be developed as R-Package with RCPP calls

library(data.table)
library(tidyverse)
library(openxlsx)
library(Rcpp)


start_time <- Sys.time()

# Read control file
ctl_file   <- "Parameter/FLUAM_ControlFile.xlsx"

# Input files
path       <- "M:/Models/StateWide/TSM_Legacy/FLUAM"
skim_file  <- "Input/LOAD_BIGDUMP.TXT"
taz_file   <- "Input/d_TAZ_2015_In.csv"
dc_file    <- "Input/DensityConstraints.csv"
gc_file    <- "Input/Growth_15to20.csv"
links_file <- "Input/d_Links.txt"
nodes_file <- "Input/d_Nodes.txt"


# Output File
out_file    <- "Output/FLUAM_Output.xlsx"
fratar_file <- "Output/d_FratarInput.txt"

# ctl_file   <- "FLUAM_ControlFile_15_20.txt"
#-------------------------------------------------------------------------------
# Read Control Files
#-------------------------------------------------------------------------------
# Set working dir
setwd(path)

# Read CTL parameters
df_ctl    <- read.xlsx(ctl_file)

ctl       <- setNames(as.list(df_ctl$Value), df_ctl$Key)

resDecile <- df_ctl %>% 
             filter(Type == 'resAccessCoef') %>%
             mutate(Value = as.numeric(Value)) %>%
             pull(Value)

nonResDecile <- df_ctl %>% 
             filter(Type == 'nonRAccessCoef') %>%
             mutate(Value = as.numeric(Value)) %>%
             pull(Value)                  

resDenDecile <- df_ctl %>% 
             filter(Type == 'resDenCoef') %>%
             mutate(Value = as.numeric(Value)) %>%
             pull(Value)  

# FRATAR Area Type
fratEMPFact <- df_ctl %>% 
             filter(Type == 'fratEmpFact') %>%
             mutate(Value = as.numeric(Value)) %>%
             pull(Value)

#-------------------------------------------------------------------------------
# Read Input Files
#-------------------------------------------------------------------------------


# Review new skim (6461)
dt_skim <- fread(skim_file)
setnames(dt_skim, c("I", "J", "value"))

# Limit Skims to match TAZ (internal zones)
df_taz <- fread(taz_file)
df_taz <- df_taz[, 1:19]
  
# Subset II skim
max_taz <- max(df_taz$TAZ)
if(max(dt_skim$I) | max(dt_skim$J) > max_taz){
    dt_skim2 <- df_skim2 <- dt_skim[I <= max_taz & J <= max_taz & value > 0 , ]
}

# read Growth Center File
df_gc <- fread(gc_file, header = T)
setnames(df_gc, c("growthCenter", "Control_HH", "Control_EMP", "GC_Name"))  

# read density constraints File
df_dc <- fread(dc_file) 
setnames(df_dc, c("TAZ", "housingDensityConstraint", "employmentDensityConstraint"))        

# read network files
df_link <- fread(links_file)
df_node <- fread(nodes_file)

#-------------------------------------------------------------------------------
# Accessbility Logic
#-------------------------------------------------------------------------------
# find the closest 100 zones for each OTAZ based on time (skim)
dt_closest_TAZ <- dt_skim2[order(I, value)][, .SD[c(1:100)], by = I]

# adjusted travel time
ttAdj <- as.numeric(df_ctl$Value[df_ctl$Key == 'ctl.7'])
dt_closest_TAZ <- dt_closest_TAZ[ , adjTime := (value ^ ttAdj)]


# Append Destination TAZ Housing & Emp
df_taz_he <- df_taz[, c("TAZ", "housing", "employment")]
setnames(df_taz_he, "TAZ", "J")

setkey(dt_closest_TAZ, "J")
setkey(df_taz_he, "J")
dt_closest_TAZ <- merge(dt_closest_TAZ, df_taz_he, all.x = T)
setkey(dt_closest_TAZ, NULL)
setkey(df_taz_he, NULL)

# Weight AdjTT by destination HH
setnames(dt_closest_TAZ, "I", "TAZ")
dt_closest_TAZ[order(TAZ)]
dt_closest_TAZ <- dt_closest_TAZ[ , adjTime := (value ^ ttAdj)]
dt_closest_TAZ <- dt_closest_TAZ[value == 0 , adjTime := 0]

# setkey(dt_closest_TAZ, "TAZ")
# Average Accessibility measures (this is a bug in landuse.cpp where denominatior used in average is 100 + 1)
avgTTime               <- dt_closest_TAZ[order(TAZ), 
                                         .(avgTTime = sum(value) / 101), 
                                         by = TAZ] 

avgAdjTTime            <- dt_closest_TAZ[order(TAZ), 
                                         .(avgAdjTTime = round(sum(adjTime) / 101, 6)), 
                                         by = TAZ] 

avgAdjTTimeWgtByHH     <- dt_closest_TAZ[order(TAZ), 
                                         .(avgAdjTTimeWgtByHH = round(sum(adjTime * housing) / 101, 6)), 
                                         by = TAZ] 

avgAdjTTimeWgtByEmp    <- dt_closest_TAZ[order(TAZ), 
                                         .(avgAdjTTimeWgtByEmp = round(sum(adjTime * employment) / 101, 6)), 
                                         by = TAZ]

avgAdjTTimeWgtByHH_Emp <- dt_closest_TAZ[order(TAZ), 
                                         .(avgAdjTTimeWgtByHH_Emp = round( sum(adjTime * (employment + housing)) / 101, 6)), 
                                         by = TAZ] 

setkey(df_taz, "TAZ")
setkey(avgTTime, "TAZ")
setkey(avgAdjTTime, "TAZ")
setkey(avgAdjTTimeWgtByHH, "TAZ")
setkey(avgAdjTTimeWgtByEmp, "TAZ")
setkey(avgAdjTTimeWgtByHH_Emp, "TAZ")

df_taz <- merge(df_taz, avgTTime, by = "TAZ", all.x = T )
df_taz <- merge(df_taz, avgAdjTTime, by = "TAZ", all.x = T )
df_taz <- merge(df_taz, avgAdjTTimeWgtByHH, by = "TAZ", all.x = T )
df_taz <- merge(df_taz, avgAdjTTimeWgtByEmp, by = "TAZ", all.x = T )
df_taz <- merge(df_taz, avgAdjTTimeWgtByHH_Emp, by = "TAZ", all.x = T )

#-------------------------------------------------------------------------------
# Scale Accessibility - 99th Percentile
#-------------------------------------------------------------------------------
nInDistrict <- df_taz[, .N, by = DOTDistrict]
nInDistrict <- nInDistrict[, t99 := (N - (0.99 * (N-1)))]
nInDistrict <- nInDistrict[, nFor99th := as.integer(t99 + 0.5) + 2]
nInDistrict <- nInDistrict[, t99_Int := as.integer(t99)]
nInDistrict <- nInDistrict[, t99_mid := as.integer((nFor99th + t99_Int) / 2)]

# 99th percentile 
df_taz2 <- df_taz[, th99th := -1 * (avgAdjTTimeWgtByHH_Emp + 3)]
df_taz2 <- df_taz2[order(th99th), id := seq_len(.N), by = DOTDistrict]

setkey(df_taz2, "DOTDistrict")
setkey(nInDistrict, "DOTDistrict")

df_taz2 <- merge(df_taz2, nInDistrict, x.all = T)
# df_taz2 <- df_taz2[id == t99_Int,  the99th := th99th, by = DOTDistrict]
# df_taz2 <- df_taz2[id == nFor99th,  the99th := th99th, by = DOTDistrict]
df_taz2 <- df_taz2[id == t99_mid,  the99th := th99th, by = DOTDistrict]
df_taz2 <- df_taz2[!is.na(the99th), list(DOTDistrict, the99th)]
df_taz2 <- df_taz2[ , the99th := -1 * the99th]

# Append 99th percentile to df_taz data
setkey(df_taz, "DOTDistrict")
df_taz <- merge(df_taz, df_taz2, x.all = T)
df_taz[, c("id", "th99th") := NULL]

# Scale Accessibility
df_taz[ , accessScaled := 0]
setkey(df_taz,NULL)
df_taz[avgAdjTTimeWgtByHH_Emp > 0 , 
       accessScaled := pmin(1, log(avgAdjTTimeWgtByHH_Emp + 3) / log(the99th))]

#-------------------------------------------------------------------------------
# Categorical Access
#-------------------------------------------------------------------------------
# Function to compute categorial access
computeCategorical <- function(vec_access){

  cnt <- 0
  
  accessCategorical <- lapply(1:length(vec_access), function(cnt) {
    cnt <<- cnt + 1
    
    access       <- vec_access[cnt]
  
    nAbove <- length(vec_access[vec_access > access])
    nBelow <- length(vec_access[vec_access < access])
    
    nTotal <- nAbove + nBelow
    
    # There are some differences between cpp & r-script type casts
    # accessCategorical <- min(10,  as.integer(nBelow / nTotal * 10) + 1)
    accessCategorical <- min(10,  floor(nBelow / nTotal * 10) + 1)
    
  }) 
  return(unlist(accessCategorical))
}

dt_taz2 <- df_taz[ , accessCategorical := computeCategorical(accessScaled), by = DOTDistrict]

# fwrite(dt_taz2, "df_taz2.csv", row.names = F)

#-------------------------------------------------------------------------------
# calculateDistToIntersections
#-------------------------------------------------------------------------------
# Define link class
arterial_class <- c(20:30)
ramp_class     <- c(70:79)
highToll_class <- c(10:19, 91:96)

df_link[ LINKGRP1 %in% arterial_class, type := 'arterial']
df_link[ LINKGRP1 %in% ramp_class, type := 'ramp']
df_link[ LINKGRP1 %in% highToll_class, type := 'high_tolls']

df_link2 <- df_link[, c("A", "type")]
df_link2 <- unique(df_link2)
setnames(df_link2, c("N", "A_Type"))
setkey(df_link2, "N")
setkey(df_node, "N")
df_node <- merge(df_node, df_link2, x.all = T)

df_link2 <- df_link[, c("B", "type")]
df_link2 <- unique(df_link2)
setnames(df_link2, c("N", "B_Type"))
setkey(df_link2, "N")
df_node <- merge(df_node, df_link2, x.all = T)

# Add type as Ramp = 1, Arterial = 2 and Both = 3
zone_nodes <- df_node[ N <= 6424, c("N", "X", "Y")]
zone_nodes <- distinct(zone_nodes)

df_node <- df_node[A_Type %in% c("arterial", "ramp") & B_Type %in% c("arterial", "ramp") , 
                   type := "arterial"]
df_node <- df_node[A_Type == "ramp" || B_Type == "ramp" , 
                   type := "ramp"]

other_nodes <- df_node[ N > 6424 & A_Type %in% c("arterial", "ramp") & B_Type %in% c("arterial", "ramp"), c("N", "X", "Y")]

#-------------------------------------------------------------------------------
# R & CPP: finding closest zone is computationally intensive
#-------------------------------------------------------------------------------
# C++ function to compute distances and get nodes
sourceCpp("source_code_new/compute_distance.cpp")

other_N    <- other_nodes$N
other_X    <- other_nodes$X
other_Y    <- other_nodes$Y

# Arterial Distance
# x <- pdistC(vec_x, vec_y, other_X, other_Y, other_N)

cnt <- 0
dist_xy <- do.call(rbind, lapply(1:nrow(zone_nodes), function(cnt){
  cnt <<- cnt + 1
     vec_zones  <- zone_nodes$N[cnt]
     vec_x      <- zone_nodes$X[cnt]
     vec_y      <- zone_nodes$Y[cnt] 
     
     # This function is from compute_distance.cpp file
     x <- pdistC(vec_x, vec_y, other_X, other_Y, other_N)
     xy <- do.call(cbind, list(x[[1]], x[[2]]))
     return(xy)
}))

# dist_xy <- as.data.frame(dist_xy)
dist_xy <- dist_xy %>% 
           as.data.frame() %>%
           mutate(TAZ = seq(1:nrow(dist_xy))) %>%
           select(TAZ, distToArterial = V1, ArterialNode = V2)

setDT(dist_xy)
setkey(dist_xy, "TAZ")
setkey(dt_taz2, "TAZ")
dt_taz2 <- merge(dt_taz2, dist_xy, x.all = T)

# Ramp Distances are not properly computed due to missing type in the data
RmpDist <- sqrt(sqrt(99999/ 1609.344) ) + 0.1

#-------------------------------------------------------------------------------
# CalculateLandConsumption
#-------------------------------------------------------------------------------
# Compute variables
df_taz3 <- dt_taz2 %>% 
           setDF() %>%
           mutate(distToRamp = RmpDist,
                  devAcres =  resDeveloped + nonresDeveloped,
                  availableAcres = pmax(1, totalAcres - devAcres - undevelopableAcres),
                  percentDeveloped = ifelse(devAcres + availableAcres > 0 & availableAcres > 1 , 
                                            devAcres / ( devAcres + availableAcres),
                                            1),
                  nonresPercentDeveloped = ifelse(devAcres > 0, 
                                                  nonresDeveloped / devAcres,
                                                  0),
                  accessChange = ifelse(cur_AdjTTimeWgtByHH_Emp > 0, 
                                        pmin(1,(avgAdjTTimeWgtByHH_Emp - cur_AdjTTimeWgtByHH_Emp) / cur_AdjTTimeWgtByHH_Emp), 1 ),
                  resDensity = ifelse(resDeveloped > 0,
                                      pmax(0, log( (housing / resDeveloped) + 0.01) ), 
                                      0),
                  llache = log(avgAdjTTimeWgtByHH_Emp + 3),
                  boolUGB = ifelse(growthBoundary == 0, 1, 0),
                  boolToCoast5M = ifelse(distToCoast <= 5, 1, 0),
                  boolAcre1 = ifelse(resAvailableAcres < 1, 1, 0), 
                  boolAcre1k = ifelse(resAvailableAcres > 1000, 1, 0), 
                  decile = as.numeric(resDecile [accessCategorical]),
                  decile2 = as.numeric(nonResDecile [accessCategorical]),
                  decile3 = as.numeric(resDenDecile [accessCategorical]) 
                  )

# Compute residential land consumption (initial before iterating)
df_taz3 <- df_taz3 %>% 
           mutate(resCons = as.numeric(ctl$resConstantCoef) +                                   
                      as.numeric(ctl$resGrowBoundaryCoef) * boolUGB +                     
                      as.numeric(ctl$resDistToCoast) * pmin(10, distToCoast) +
                      as.numeric(ctl$resAccessChangeCoef) * accessChange +                
                      as.numeric(ctl$resNonResUseCoef) * nonresPercentDeveloped +         
                      as.numeric(ctl$resCoastalTAZCoef) * boolToCoast5M +                            
                      as.numeric(ctl$resDevelopmentCoef) * percentDeveloped +                             
                      as.numeric(ctl$resNonResCoastalCoef) * boolToCoast5M * nonresPercentDeveloped +    
                      decile +                                      
                      as.numeric(ctl$resSmallVacantCoef) * boolAcre1+                        
                      as.numeric(ctl$resLargeVacantCoef) * boolAcre1k ,
                 resCons =  exp(resCons),
                 landConsuptionHH = resCons / (1 + resCons)
           )

# Compute non-residential land consumption
df_taz3 <- df_taz3 %>% 
           mutate(nonresCon = as.numeric(ctl$nonresConstantCoef) +                                   
                      as.numeric(ctl$nonresGrowBoundaryCoef) * boolUGB +                     
                      as.numeric(ctl$nonresDistToRampCoef) * distToRamp +          
                      as.numeric(ctl$nonresAccessChangeCoef) * accessChange +                
                      decile2,
                 nonresCon =  exp(nonresCon),
                 landConsuptionEmp = nonresCon / (1 + nonresCon)
           )


# Compute Residential Density
df_taz3 <- df_taz3 %>% 
           mutate(landDensityHH = as.numeric(ctl$d_resConstantCoef) + 
        						as.numeric(ctl$d_resDevelopmentDensCoef) * resDensity + 
        						as.numeric(ctl$d_resAccessChangeCoef) * accessChange + 
        						as.numeric(ctl$d_resSmallVacantCoef) *  boolAcre1 +
        						as.numeric(ctl$d_resLargeVacantCoef) *  boolAcre1k +
        						decile3)

# Compute non-Residential Density
df_taz3 <- df_taz3 %>% 
           mutate(landDensityEmp = as.numeric(ctl$d_nonresConstantCoef) + 
        						as.numeric(ctl$d_nonresAccessCoef) * llache )

#-------------------------------------------------------------------------------
# CalculateNewHousing
#-------------------------------------------------------------------------------
# Append density constraints
df_taz4 <- df_taz3 %>%
           left_join(df_dc, by = "TAZ") 

# keep track of landConsuptions
iter_lcHH <-list()
iter_HH  <- list()
iter     <- 0
converge <- FALSE

while(!converge & iter < 20) {
  
  iter = iter + 1
  print(paste("HH allocation iteration: ", iter))

  # Compute raw HH allocation
  df_temp <- df_taz4 %>% 
             mutate(resDensity = ifelse(resDeveloped > 0, housing / resDeveloped, 0),
                    resVac3    = resAvailableAcres,
                    expDensity = exp(landDensityHH),
                    rsgRd2     = ifelse(resDensity / expDensity < 20 & totalAcres < 1000 ,  
                                    pmax( expDensity, resDensity), 
                                    expDensity ),
                    rsgRd2     = ifelse(housingDensityConstraint != 1 & rsgRd2 > housingDensityConstraint,
                                    housingDensityConstraint, 
                                    rsgRd2),
                    HHAllocated = resVac3 * rsgRd2 * landConsuptionHH
                    )
  # Iteration 
  iter_lcHH[[iter]]   <- df_temp$landConsuptionHH
  iter_HH[[iter]]   <- df_temp$HHAllocated
  
  # df_temp %>% filter(growthCenter == 20) %>% head()
  
  # Compute raw HH allocation
  DRIHHbyGrowthCenter <- df_temp %>%
                         group_by(growthCenter) %>%
                         summarise(DRIHHbyGrowthCenter = sum(DRI_Housing),
                                   HHbyGrowthCenter = sum(HHAllocated)) %>%
                         left_join(df_gc, by = "growthCenter") %>%
                         mutate(ScaleFactorForGrowthCenter = ifelse( HHbyGrowthCenter > 0, 
                                     (Control_HH - DRIHHbyGrowthCenter) / HHbyGrowthCenter, 0)) %>%
                         select(growthCenter, ScaleFactorForGrowthCenter,  Control_HH)
                                   
  # Scale by growth centers
  df_temp <- df_temp %>% 
             left_join(DRIHHbyGrowthCenter, by = "growthCenter") %>%
             select(-Control_HH) %>%
             mutate(resFactoredCons = pmin(1, landConsuptionHH * ScaleFactorForGrowthCenter),
                    landConsuptionHH = resFactoredCons,          # Key to support iteration
                    resFactoredDensity = rsgRd2,
                    HHAllocated = resVac3 * resFactoredCons * resFactoredDensity,
                    HHTotal = housing + HHAllocated + DRI_Housing
                    ) %>%
             select(-ScaleFactorForGrowthCenter)                # remove it from 
  
  # df_temp %>% filter(growthCenter == 20) %>% head()
  
  # Check for convergence
  DRIHHbyGrowthCenter <- df_temp %>%
                         select(growthCenter, DRI_Housing, HHAllocated) %>%
                         group_by(growthCenter) %>%
                         summarise(HH_model = sum(DRI_Housing + HHAllocated)) %>%
                         left_join(df_gc, by = "growthCenter") %>%
                         mutate(diff = abs(HH_model - Control_HH),
                                converge = ifelse(diff > 100, -100, 1))
  
  converge <- min(DRIHHbyGrowthCenter$converge) == 1 
  
  # If converged get all data
  if(converge || iter == 20) {
    
    newfields <- c("TAZ", "landConsuptionHH", "resVac3",
                    "expDensity", "rsgRd2",  "HHAllocated",  "resFactoredCons",                 
                    "resFactoredDensity", "HHTotal" )
    
    df_temp <- df_temp %>% 
               select(newfields)
    
    df_taz4 <- df_taz4 %>%
             select(-landConsuptionHH) %>%
             left_join(df_temp, by = "TAZ")
    
    break
  } else{
      # only add iteration data
      df_temp <- df_temp %>%
             select(TAZ, landConsuptionHH)
  
     # remove last iteration landuse
     df_taz4 <- df_taz4 %>%
             select(-landConsuptionHH) %>%
             left_join(df_temp, by = "TAZ")
    
  }
  
} 


#-------------------------------------------------------------------------------
# CalculateNewHousing
#-------------------------------------------------------------------------------
# keep track of landConsuptions
iter_lcEMP <-list()
iter_EMP  <- list()
iter      <- 0
converge  <- FALSE
while(!converge & iter < 20) {
  
  iter = iter + 1
  print(paste("EMP allocation iteration: ", iter))
  
  # Append density constraints
  df_temp <- df_taz4 %>%
             mutate(nonresDensity = ifelse(nonresDeveloped + 1 > 0, 
                                           employment / (nonresDeveloped + 1),
                                           0),
                    empVacLand = nonresAvailableAcres,
                    rsg_rd     = pmax(0, landDensityEmp),
                    density    = ifelse(totalAcres > 1000 , 
                                        ifelse(nonresDensity > 50, rsg_rd, pmax(5, nonresDensity)),
                                        pmax(5, pmax(nonresDensity, rsg_rd))),
                    density    = ifelse(employmentDensityConstraint != 1000 & density > employmentDensityConstraint,
                                        employmentDensityConstraint, density),
                    EmpAllocated = landConsuptionEmp * density * empVacLand
                   )
  
  # Iteration 
    iter_lcEMP[[iter]]   <- df_temp$landConsuptionEmp
    iter_EMP[[iter]]     <- df_temp$EmpAllocated
  
  # Compute raw HH allocation
    DRIEMPbyGrowthCenter <- df_temp %>%
                           group_by(growthCenter) %>%
                           summarise(DRIEmpbyGrowthCenter = sum(DRI_Employment),
                                     EmpbyGrowthCenter = sum(EmpAllocated)) %>%
                           left_join(df_gc, by = "growthCenter") %>%
                           mutate(ScaleFactorForGrowthCenter = ifelse( EmpbyGrowthCenter > 0, 
                                       pmax(0, (Control_EMP - DRIEmpbyGrowthCenter) / EmpbyGrowthCenter), 
                                       0)) %>%
                           select(growthCenter, ScaleFactorForGrowthCenter,  Control_EMP)  
  
    
  # Scale by growth centers
  df_temp <- df_temp %>% 
               left_join(DRIEMPbyGrowthCenter, by = "growthCenter") %>%
               select(-Control_EMP) %>%
               mutate(nonresFactoredCons = pmin(1, landConsuptionEmp * ScaleFactorForGrowthCenter),
                      landConsuptionEmp = nonresFactoredCons,          # Key to support iteration
                      nonresFactoredDensity = density,
                      EmpAllocated = empVacLand * nonresFactoredCons * nonresFactoredDensity,
                      EmpTotal = employment + EmpAllocated + DRI_Employment
                      ) %>%
               select(-ScaleFactorForGrowthCenter)                     # remove it from 
  
  # Check for convergence
  DRIEMPbyGrowthCenter <- df_temp %>%
                         select(growthCenter, DRI_Employment, EmpAllocated) %>%
                         group_by(growthCenter) %>%
                         summarise(EMP_model = sum(DRI_Employment + EmpAllocated)) %>%
                         left_join(df_gc, by = "growthCenter") %>%
                         mutate(diff = abs(EMP_model - Control_EMP),
                                converge = ifelse(diff > 100, -100, 1))
  
   converge <- min(DRIEMPbyGrowthCenter$converge) == 1 
  
  # If converged get all data
  if(converge || iter == 20) {
    
    newfields <- c("TAZ", "landConsuptionEmp", "nonresFactoredDensity",                 
                   "nonresFactoredCons", "EmpAllocated", "EmpTotal" )
    
    df_temp <- df_temp %>% 
               select(newfields)
    
    df_taz4 <- df_taz4 %>%
             select(-landConsuptionEmp) %>%
             left_join(df_temp, by = "TAZ")
    
    break
  } else{
      # only add iteration data
      df_temp <- df_temp %>%
             select(TAZ, landConsuptionEmp)
  
     # remove last iteration landuse
     df_taz4 <- df_taz4 %>%
             select(-landConsuptionEmp) %>%
             left_join(df_temp, by = "TAZ")
    
  }

}

#-------------------------------------------------------------------------------
# FRATAR
#-------------------------------------------------------------------------------
df_taz5 <- df_taz4 %>%
           mutate(boolUrbanArea = ifelse(areaType == 2, 1, 0),
                  baseTrips =	as.numeric(ctl$fratConstant) + 
							                  fratEMPFact[areaType] * employment  +
							                  as.numeric(ctl$fratHHFact) * housing  + 
							                  as.numeric(ctl$fratUrbanArea)* boolUrbanArea,
                  futureTrips =	as.numeric(ctl$fratConstant) + 
							                  fratEMPFact[areaType] * EmpTotal  +
							                  as.numeric(ctl$fratHHFact) * HHTotal  + 
							                  as.numeric(ctl$fratUrbanArea)* boolUrbanArea,
                  growth = futureTrips / baseTrips
                  )

# Format for Tranplan 
df_fratar <- df_taz5 %>%
             mutate(i = 1,
                    fo = "FO",
                    ftaz = stringr::str_pad(TAZ, width = 4, side = "left", pad = "0"),
                    fgc = stringr::str_pad(round((growth + 0.005) * 100, 0), width = 6, side = "left", pad = "0")) %>%
             select(fo, ftaz, i, fgc)

# Write FRATAR File
write.table(df_fratar, fratar_file, sep = " ", quote = FALSE,
            row.names = F, col.names = F)

#-------------------------------------------------------------------------------
# Export data
excel_data <- list("year_2020" = df_taz5)
write.xlsx(excel_data, out_file)


end_time <- Sys.time()
end_time - start_time





