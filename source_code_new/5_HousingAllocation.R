#-------------------------------------------------------------------------------
# CalculateNewHousing
#-------------------------------------------------------------------------------
allocateHousing <- function(df_taz3, df_gc){
  # Append density constraints

  # keep track of landConsuptions
  iter_lcHH <-list()
  iter_HH  <- list()
  iter     <- 0
  converge <- FALSE
  
  while(!converge & iter < max_iter ) {
    
    iter = iter + 1
    print(paste("HH allocation iteration: ", iter))
    
    # Compute raw HH allocation
    df_temp <- df_taz3 %>% 
      mutate(resDensity = ifelse(resDeveloped > 0, housing / resDeveloped, 0),
             resVac3    = resAvailableAcres,
             expDensity = exp(landDensityHH),
             rsgRd2     = ifelse(resDensity / expDensity < 20 & totalAcres < 1000 ,  
                                 pmax( expDensity, resDensity), 
                                 expDensity ),
             rsgRd2     = ifelse(housingDensityConstraint != 1 & rsgRd2 > housingDensityConstraint,
                                 housingDensityConstraint, 
                                 rsgRd2),
             HHAllocated = resVac3 * rsgRd2 * landConsuptionHH,
             HHAllocated = ifelse(DRI_Housing > 0, 0, HHAllocated)
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
             HHAllocated = ifelse(DRI_Housing > 0, 0, HHAllocated),
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
    if(converge || iter == max_iter) {
      
      newfields <- c("TAZ", "landConsuptionHH", "resVac3",
                     "expDensity", "rsgRd2",  "HHAllocated",  "resFactoredCons",                 
                     "resFactoredDensity", "HHTotal" )
      
      df_temp <- df_temp %>% 
        select(newfields)
      
      df_taz4 <- df_taz3 %>%
        select(-landConsuptionHH) %>%
        left_join(df_temp, by = "TAZ")
      
      break
    } else{
      # only add iteration data
      df_temp <- df_temp %>%
        select(TAZ, landConsuptionHH)
      
      # remove last iteration landuse
      df_taz4 <- df_taz3 %>%
        select(-landConsuptionHH) %>%
        left_join(df_temp, by = "TAZ")
      
    }
    
  } 
  
  reta <- list(df_taz4 = df_taz4, DRIHHbyGrowthCenter = DRIHHbyGrowthCenter)
  return(reta)
}

