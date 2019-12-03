#-------------------------------------------------------------------------------
# CalculateNewEmployment
#-------------------------------------------------------------------------------

allocateEmployment <- function(df_taz4, df_gc, excludeDRI, includeDev, runType) {
  
  # keep track of landConsuptions
  iter_lcEMP <-list()
  iter_EMP  <- list()
  iter      <- 0
  converge  <- FALSE
  while(!converge & iter < max_iter) {
    
    iter = iter + 1
    print(paste("EMP allocation iteration: ", iter))
    
    #---------------------------------------------------------------------------
    # Compute Land ("vacant" or "redev + vacant" to be used for allocation
    if(includeDev){
      # The entire zone can be re-developed (required in counties like Broward 
      # where available land is insufficient to accommodate the entire BEBR growth )
      # FLUAM 2.1 uses existing density but after allocation, we don't compute consumed land
      # Thus it creates higher density
      df_temp <- df_taz4 %>%
                 mutate(nonresDensity = ifelse(nonresDeveloped + 1 > 0, 
                                               employment / (nonresDeveloped + nonresAvailableAcres + 1), 
                                               0),
                        empVacLand = nonresAvailableAcres + nonresDeveloped)
    } else {
      df_temp <- df_taz4 %>%
                 mutate(nonresDensity = ifelse(nonresDeveloped + 1 > 0, 
                                              employment / (nonresDeveloped + 1), # FLUAM 2.1
                                              0),
                        empVacLand = nonresAvailableAcres)
    }
    
    #---------------------------------------------------------------------------
    # Append density constraints
    df_temp <- df_temp %>%
                mutate(
                       rsg_rd     = pmax(0, landDensityEmp),
                       density    = ifelse(totalAcres > 1000 , 
                                           ifelse(nonresDensity > 50, rsg_rd, pmax(5, nonresDensity)),
                                           pmax(5, pmax(nonresDensity, rsg_rd))),
                       density    = ifelse(employmentDensityConstraint != 1000 & density > employmentDensityConstraint,
                                           employmentDensityConstraint, density),
                       landConsuptionEmp = ifelse(runType < 0 & employment == 0, 0, landConsuptionEmp),
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
      mutate(
             # ScaleFactorForGrowthCenter = ifelse( EmpbyGrowthCenter > 0,
             #                                      pmax(0, (Control_EMP - DRIEmpbyGrowthCenter) / EmpbyGrowthCenter),
             #                                      0)
              ScaleFactorForGrowthCenter = case_when(
                 EmpbyGrowthCenter > 0 & !excludeDRI ~ ((Control_EMP - DRIEmpbyGrowthCenter) / EmpbyGrowthCenter),
                 EmpbyGrowthCenter > 0 & excludeDRI ~ (Control_EMP  / EmpbyGrowthCenter),
                 TRUE ~ 0)) %>%
      select(growthCenter, ScaleFactorForGrowthCenter,  Control_EMP)  
    
    
    # Scale by growth centers
    df_temp <- df_temp %>% 
      left_join(DRIEMPbyGrowthCenter, by = "growthCenter") %>%
      select(-Control_EMP) %>%
      mutate(nonresFactoredCons = pmin(1, landConsuptionEmp * ScaleFactorForGrowthCenter),
             # Key to support iteration
             landConsuptionEmp = nonresFactoredCons,          
             nonresFactoredDensity = density,
             EmpAllocated = empVacLand * nonresFactoredCons * nonresFactoredDensity,
             EmpAllocated = ifelse(DRI_Employment > 0, 0, EmpAllocated),
             EmpAllocated = ifelse(runType < 0 & EmpAllocated  > employment, 0, EmpAllocated),
             EmpTotal = employment + runType * (EmpAllocated + DRI_Employment)
      ) %>%
      select(-ScaleFactorForGrowthCenter)                     # remove it from 
    
    # Check for convergence
    DRIEMPbyGrowthCenter <- df_temp %>%
      select(growthCenter, DRI_Employment, EmpAllocated) %>%
      group_by(growthCenter) %>%
      summarise(EMP_model = ifelse(excludeDRI, 
                                  sum(EmpAllocated),
                                  sum(DRI_Employment + EmpAllocated))) %>%
      left_join(df_gc, by = "growthCenter") %>%
      mutate(diff = abs(Control_EMP - EMP_model),
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
  
  # Write iterim consumption files
  if(debug) {
    if(includeDev){
        debug_emp_lc_file <- paste0("Output/Debug_",next_year,"_EMP_ReDev_Land_Consupution.xlsx")
        debug_emp_file    <- paste0("Output/Debug_",next_year,"_EMP_ReDev_Allocated.xlsx")
    } else{
        debug_emp_lc_file <- paste0("Output/Debug_",next_year,"_EMP_Land_Consupution.xlsx")
        debug_emp_file    <- paste0("Output/Debug_",next_year,"_EMP_Allocated.xlsx")
    }

    
    names(iter_lcEMP) <- paste0("Iter-", c(1:length(iter_lcEMP)))
    df_iter_lcEMP    <- data.frame(TAZ = df_temp$TAZ, iter_lcEMP)
    
    names(iter_EMP) <- paste0("Iter-", c(1:length(iter_EMP)))
    df_iter_EMP       <- data.frame(TAZ = df_temp$TAZ, iter_EMP)
    
    write.xlsx(df_iter_lcEMP, debug_emp_lc_file) 
    write.xlsx(df_iter_EMP, debug_emp_file) 
  }
  
  
  ret <- list(df_taz4 = df_taz4, DRIEMPbyGrowthCenter = DRIEMPbyGrowthCenter)
  return(ret)
}


