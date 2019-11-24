#-------------------------------------------------------------------------------
# CalculateNewEmployment
#-------------------------------------------------------------------------------

allocateEmployment <- function(df_taz4, df_gc) {
  # keep track of landConsuptions
  iter_lcEMP <-list()
  iter_EMP  <- list()
  iter      <- 0
  converge  <- FALSE
  while(!converge & iter < max_iter) {
    
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
             EmpAllocated = landConsuptionEmp * density * empVacLand,
             EmpAllocated = ifelse(DRI_Employment > 0, 0, EmpAllocated)
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
             EmpAllocated = ifelse(DRI_Employment > 0, 0, EmpAllocated),
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
    if(converge || iter == max_iter) {
      
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
  ret <- list(df_taz4 = df_taz4, DRIEMPbyGrowthCenter = DRIEMPbyGrowthCenter)
  return(ret)
}


