# Functions to calculate accessibility

#-------------------------------------------------------------------------------
# Accessbility Measures
#-------------------------------------------------------------------------------
computeAccessibility <- function(dt_skim2, df_ctl, df_taz){
  # find the closest 100 zones for each OTAZ based on time (skim)
  dt_closest_TAZ <- dt_skim2[order(I, value)][, .SD[c(1:100)], by = I]
  
  # adjusted travel time
  ttAdj <- as.numeric(df_ctl$Value[df_ctl$Key == 'ctl.7'])
  dt_closest_TAZ <- dt_closest_TAZ[ , adjTime := (value ^ ttAdj)]
  
  # Append Destination TAZ Housing & Emp
  df_taz_he <- df_taz[, c("TAZ", "housing", "employment")] %>% 
               setDT()
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
                                           .(avgTTime = sum(value) / 100), 
                                           by = TAZ] 
  
  avgAdjTTime            <- dt_closest_TAZ[order(TAZ), 
                                           .(avgAdjTTime = round(sum(adjTime) / 100, 6)), 
                                           by = TAZ] 
  
  avgAdjTTimeWgtByHH     <- dt_closest_TAZ[order(TAZ), 
                                           .(avgAdjTTimeWgtByHH = round(sum(adjTime * housing) / 100, 6)), 
                                           by = TAZ] 
  
  avgAdjTTimeWgtByEmp    <- dt_closest_TAZ[order(TAZ), 
                                           .(avgAdjTTimeWgtByEmp = round(sum(adjTime * employment) / 100, 6)), 
                                           by = TAZ]
  
  avgAdjTTimeWgtByHH_Emp <- dt_closest_TAZ[order(TAZ), 
                                           .(avgAdjTTimeWgtByHH_Emp = round( sum(adjTime * (employment + housing)) / 100, 6)), 
                                           by = TAZ] 
  
  return(avgAdjTTimeWgtByHH_Emp)
  
  # setkey(df_taz, "TAZ")
  # setkey(avgTTime, "TAZ")
  # setkey(avgAdjTTime, "TAZ")
  # setkey(avgAdjTTimeWgtByHH, "TAZ")
  # setkey(avgAdjTTimeWgtByEmp, "TAZ")
  # setkey(avgAdjTTimeWgtByHH_Emp, "TAZ")
  
  # df_taz <- merge(df_taz, avgTTime, by = "TAZ", all.x = T )
  # df_taz <- merge(df_taz, avgAdjTTime, by = "TAZ", all.x = T )
  # df_taz <- merge(df_taz, avgAdjTTimeWgtByHH, by = "TAZ", all.x = T )
  # df_taz <- merge(df_taz, avgAdjTTimeWgtByEmp, by = "TAZ", all.x = T )
  # df_taz <- merge(df_taz, avgAdjTTimeWgtByHH_Emp, by = "TAZ", all.x = T )
  
}


#-------------------------------------------------------------------------------
# Scale Accessibility - 99th Percentile
#-------------------------------------------------------------------------------
getScaledAccessibility <- function(df_taz){
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
  df_taz2 <- df_taz2[id == t99_mid,  the99th := th99th, by = DOTDistrict]
  df_taz2 <- df_taz2[!is.na(the99th), list(DOTDistrict, the99th)]
  df_taz2 <- df_taz2[ , the99th := -1 * the99th]
  
  # Append 99th percentile to df_taz data
  setkey(df_taz, "DOTDistrict")
  df_taz <- merge(df_taz, df_taz2, x.all = T)
  df_taz[, c("id", "th99th") := NULL]
  
  # Scale Accessibility
  df_taz[ , accessScaled := 0]
  
  setkey(df_taz, NULL)
  df_taz[avgAdjTTimeWgtByHH_Emp > 0 , 
         accessScaled := pmin(1, log(avgAdjTTimeWgtByHH_Emp + 3) / log(the99th))]
  
  return(df_taz)
}


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




