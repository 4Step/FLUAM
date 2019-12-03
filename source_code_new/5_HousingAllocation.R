#-------------------------------------------------------------------------------
# CalculateNewHousing
# runType (1 = allocate or -1 = deallocate)
#-------------------------------------------------------------------------------
allocateHousing <- function(df_taz3, df_gc, excludeDRI, includeDev, runType){
  # Append density constraints

  # keep track of landConsuptions
  iter_lcHH <-list()
  iter_HH  <- list()
  iter     <- 0
  converge <- FALSE
  
  while(!converge & iter < max_iter ) {
    
    iter = iter + 1
    print(paste("HH allocation iteration: ", iter))
    
    #---------------------------------------------------------------------------
    # Compute Land ("vacant" or "redev + vacant" to be used for allocation
    if(includeDev){
      # The entire zone can be re-developed (required in counties like Broward 
      # where available land is insufficient to accommodate the entire BEBR growth )
      # FLUAM 2.1 uses existing density but after allocation, we don't compute consumed land
      # Thus it creates higher density
      df_temp <- df_taz3 %>% 
                 # Includes "redevelopment"
                 mutate( resDensity = ifelse(resDeveloped > 0, 
                                             housing / (resDeveloped + resAvailableAcres), 
                                             0),
                         resVac3    = resAvailableAcres + resDeveloped)
    } else {
        df_temp <- df_taz3 %>% 
                 # Excludes "redevelopment"
                 mutate(resDensity = ifelse(resDeveloped > 0, 
                                            housing / resDeveloped, 
                                            0),
                        resVac3    = resAvailableAcres)
    }
    #---------------------------------------------------------------------------
    # Compute raw HH allocation
    df_temp <- df_temp %>%
                mutate(
                       expDensity = exp(landDensityHH),
                       rsgRd2     = ifelse(resDensity / expDensity < 20 & totalAcres < 1000 ,  
                                           pmax( expDensity, resDensity), 
                                           expDensity ),
                       rsgRd2     = ifelse(housingDensityConstraint != 1 & rsgRd2 > housingDensityConstraint,
                                           housingDensityConstraint, 
                                           rsgRd2),
                       landConsuptionHH = ifelse(runType < 0 & housing == 0, 0, landConsuptionHH),
                       HHAllocated = resVac3 * rsgRd2 * landConsuptionHH,
                       HHAllocated = ifelse(DRI_Housing > 0, 0, HHAllocated),
                       HHAllocated = ifelse(runType < 0 & HHAllocated  > housing, 0, HHAllocated)
                )
    # Iteration 
    iter_lcHH[[iter]]   <- df_temp$landConsuptionHH
    iter_HH[[iter]]     <- df_temp$HHAllocated
    
    # df_temp %>% filter(growthCenter == 20) %>% head()
    
    # Compute raw HH allocation
    DRIHHbyGrowthCenter <- df_temp %>%
      group_by(growthCenter) %>%
      summarise(DRIHHbyGrowthCenter = sum(DRI_Housing),
                HHbyGrowthCenter = sum(HHAllocated)) %>%
      left_join(df_gc, by = "growthCenter") %>%
      mutate(ScaleFactorForGrowthCenter = case_when(
                 HHbyGrowthCenter > 0 & !excludeDRI ~ ((Control_HH - DRIHHbyGrowthCenter) / HHbyGrowthCenter),
                 HHbyGrowthCenter > 0 & excludeDRI ~ (Control_HH  / HHbyGrowthCenter),
                 TRUE ~ 0)
             ) %>%
      select(growthCenter, ScaleFactorForGrowthCenter,  Control_HH)
    
    # Scale by growth centers
    df_temp <- df_temp %>% 
      left_join(DRIHHbyGrowthCenter, by = "growthCenter") %>%
      mutate(resFactoredCons = pmin(1, landConsuptionHH * ScaleFactorForGrowthCenter),
             # Key to support iteration
             landConsuptionHH = resFactoredCons,          
             resFactoredDensity = rsgRd2,
             HHAllocated = ifelse(Control_HH > 0, 
                                  resVac3 * resFactoredCons * resFactoredDensity,
                                  0),
             HHAllocated = ifelse(DRI_Housing > 0 , 0, HHAllocated),
             HHAllocated = ifelse(runType < 0 & HHAllocated  > housing, 0, HHAllocated),
             HHTotal = housing + runType * (HHAllocated + DRI_Housing)
      ) %>%
      select(-ScaleFactorForGrowthCenter, -Control_HH)                
    
    # Check for convergence
    DRIHHbyGrowthCenter <- df_temp %>%
      select(growthCenter, DRI_Housing, HHAllocated) %>%
      group_by(growthCenter) %>%
      summarise(HH_model = ifelse(excludeDRI, 
                                  sum(HHAllocated),
                                  sum(DRI_Housing + HHAllocated))) %>%
      left_join(df_gc, by = "growthCenter") %>%
      mutate(diff =  abs(Control_HH - HH_model),
             converge = ifelse(diff > 100, -100, 1))
    
    converge <- min(DRIHHbyGrowthCenter$converge) == 1 
    
    # If converged get all data
    if(converge || iter == max_iter) {
      
      newfields <- c("TAZ", "landConsuptionHH", "resVac3",
                     "expDensity", "rsgRd2",  "HHAllocated",  "resFactoredCons",                 
                     "resFactoredDensity", "HHTotal" )
      
      df_temp <- df_temp %>% 
        select(newfields)
      
      df_taz3 <- df_taz3 %>%
        select(-landConsuptionHH) %>%
        left_join(df_temp, by = "TAZ")
      
      break
    } else{
      
      # only add iteration data
      df_temp <- df_temp %>%
        select(TAZ, landConsuptionHH)
      
      # remove last iteration landuse
      df_taz3 <- df_taz3 %>%
        select(-landConsuptionHH) %>%
        left_join(df_temp, by = "TAZ")
      
    }
    
  } 
  
  # Write iterim consumption files
  if(debug) {
    if(includeDev){
        debug_hh_lc_file <- paste0("Output/Debug_",next_year,"_HH_RevDev_Land_Consupution.xlsx")
        debug_hh_file    <- paste0("Output/Debug_",next_year,"_HH_RevDev_Allocated.xlsx")
    } else{
        debug_hh_lc_file <- paste0("Output/Debug_",next_year,"_HH_Land_Consupution.xlsx")
        debug_hh_file    <- paste0("Output/Debug_",next_year,"_HH_Allocated.xlsx")
    }
    
    names(iter_lcHH) <- paste0("Iter-", c(1:length(iter_lcHH)))
    df_iter_lcHH     <- data.frame(TAZ = df_temp$TAZ, iter_lcHH)
    
    names(iter_HH) <- paste0("Iter-", c(1:length(iter_HH)))
    df_iter_HH       <- data.frame(TAZ = df_temp$TAZ, iter_HH)
    
    write.xlsx(df_iter_lcHH, debug_hh_lc_file) 
    write.xlsx(df_iter_HH, debug_hh_file) 
  }
  
  reta <- list(df_taz4 = df_taz3, DRIHHbyGrowthCenter = DRIHHbyGrowthCenter)
  return(reta)
}

