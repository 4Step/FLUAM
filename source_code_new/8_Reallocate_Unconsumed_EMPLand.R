
#-------------------------------------------------------------------------------
# Function to compute consumed land
#-------------------------------------------------------------------------------
# Computes land consumed from emp and res densitites
# Computes new "avaialable" and "developed" res / non-res land
computeNewAvailableLand <- function(df){
  
  temp <- df %>%
          mutate(
                 # Compute allocated HH, EMP land consumed
                 empSpentLand = ifelse(nonresFactoredDensity > 0, EmpAllocated / nonresFactoredDensity, 0),
                 hhSpentLand  = ifelse(resFactoredDensity > 0, HHAllocated  / resFactoredDensity, 0),
                 
                 # Compute net remaining land as new 
                 resAvailableAcres0 = resAvailableAcres,
                 nonresAvailableAcres0 = nonresAvailableAcres,
                 resAvailableAcres_net    = pmax(0,resAvailableAcres - hhSpentLand),
                 nonresAvailableAcres_net = pmax(nonresAvailableAcres - empSpentLand,0),
                 totalAvailable       = resAvailableAcres_net + nonresAvailableAcres_net,
                 
                 # Update developed land
                 resDeveloped         = resDeveloped + hhSpentLand,
                 nonresDeveloped      = nonresDeveloped + empSpentLand
          ) %>%
          group_by(growthCenter) %>%
          mutate(resDevShare1 = ifelse(sum(resDeveloped + nonresDeveloped) > 0,
                                        sum(resDeveloped) / sum(resDeveloped + nonresDeveloped),
                                        0)
                   ) %>%
          ungroup()  %>% 
          mutate(
                 resAvailableAcres    = totalAvailable * resDevShare1,
                 nonresAvailableAcres = totalAvailable - resAvailableAcres,
                 # 
                 # # Check for missing area (this is allocated hh, emp developed land)
                 new_total_check      = resAvailableAcres +
                                        nonresAvailableAcres +
                                        resDeveloped +
                                        nonresDeveloped +
                                        undevelopableAcres 	+
                                        AgriculturalAcres,
                 total_area_check     = totalAcres - new_total_check
          )
  
  # check error (total land area diff over +/- 1)
  check_error <- abs(sum(temp$total_area_check))
  
  if(check_error > 1){
    temp <- temp %>%
            mutate(
                 # # Allocated missing area as developed by same shares at county or zone
                 resDeveloped         = resDeveloped + pmax(total_area_check * resDevShare1, 0),
                 nonresDeveloped      = nonresDeveloped + pmax(total_area_check *  (1 - resDevShare1), 0),
                 new_total_check      = resAvailableAcres +
                                        nonresAvailableAcres +
                                        resDeveloped +
                                        nonresDeveloped +
                                        undevelopableAcres 	+
                                        AgriculturalAcres,
                 # Recheck and if there are any assign it to non-res available
                 total_area_check     = totalAcres - new_total_check,
                 nonresAvailableAcres = pmax(nonresAvailableAcres + total_area_check, 0) 
            )
  }

  
  return(temp)
}


#-------------------------------------------------------------------------------
# Reallocate employment land to residential after intial allocation  
#-------------------------------------------------------------------------------
# Re-allocates un-used EMP land for residential development
 assign_unused_EMPLand <- function(df, unallocatedCounties, useAgriLand){   
       temp <- df %>% 
                mutate(
                 # Unmet counties, use left-over emp available land for res-consumption
                 resAvailableAcres1 = ifelse(growthCenter %in% unallocatedCounties,
                                             resAvailableAcres_net + nonresAvailableAcres_net,
                                             resAvailableAcres_net),
                 nonresAvailableAcres1 = ifelse(growthCenter %in% unallocatedCounties,
                                                0,
                                             nonresAvailableAcres_net),
                 
                 nonresAvailableAcres  = nonresAvailableAcres1,
                 resAvailableAcres     = resAvailableAcres1,
                 
                 # If still unmet, use agriculture land towards available residential consumption
                 # IF demand is so high the agriculture land gets converted at higher rate (use a higher rate of 5%)
                 extraAgriLand = ifelse(growthCenter %in% unallocatedCounties & useAgriLand, 
                                            (rate / 100) * AgriculturalAcres, 
                                            0),
                 AgriculturalAcres = ifelse(growthCenter %in% unallocatedCounties & useAgriLand, 
                                            AgriculturalAcres - extraAgriLand,
                                            AgriculturalAcres),
                 
                 # use all of "extra" converted Agriculture land towards residential development
                 resAvailableAcres = ifelse(growthCenter %in% unallocatedCounties & useAgriLand, 
                                            resAvailableAcres + extraAgriLand, 
                                            resAvailableAcres)
                 )
  return(temp)
}
