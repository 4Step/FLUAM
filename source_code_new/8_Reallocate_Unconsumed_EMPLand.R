
#-------------------------------------------------------------------------------
# Function to compute consumed land
#-------------------------------------------------------------------------------
# Computes land consumed from emp and res densitites
# Computes new "avaialable" and "developed" res / non-res land
computeNewAvailableLand <- function(df_taz5a, Agri_res_noRes_Flag){
  
  temp <- df_taz5a %>%
          mutate(
                 # Consumed 
                 empSpentLand = EmpAllocated * nonresFactoredDensity,
                 hhSpentLand1 = HHAllocated  * resFactoredDensity,
                 
                 # Net remaining
                 resAvailableAcres_net1 = pmax(0, resAvailableAcres - hhSpentLand),
                 totalAvailable = resAvailableAcres_net1 + nonresAvailableAcres1,
                 
                 # Total developed (note there was also hhSpendLand, see computeRemainingLand)
                 # hhSpentLand = After first round of hh allocation, emp was allocated and the net emp land for 
                 # used to allocated "unmet" demand, thus hhSpentLand1 shows the further 
                 # land consumption by "unmet" demand
                 resDeveloped = resDeveloped + hhSpentLand + hhSpentLand1,
                 nonresDeveloped = nonresDeveloped + empSpentLand 
                 )
  
  # Split total Available into res  & non-res available (TAZ based)
  if(Agri_res_noRes_Flag == 1){
    temp <- temp %>%
            mutate(resDevShare2 = ifelse((resDeveloped + nonresDeveloped) > 0,
                                        resDeveloped / (resDeveloped + nonresDeveloped),
                                        0)
                   )
  } 
  
  # Split total Available into res  & non-res available (Countywide)
 if(Agri_res_noRes_Flag == 2){
    temp <- temp %>%
            group_by(growthCenter) %>%
            mutate(resDevShare2 = ifelse(sum(resDeveloped + nonresDeveloped) > 0,
                                        sum(resDeveloped) / sum(resDeveloped + nonresDeveloped),
                                        0)
                   ) %>%
            ungroup()
  } 
  
  # Split total Available into res & non-res available ()
  temp <- temp %>% 
          mutate(resAvailableAcres    = totalAvailable * resDevShare2,
                 nonresAvailableAcres = totalAvailable - resAvailableAcres)
  
  return(temp)
}


#-------------------------------------------------------------------------------
# Compute available land for residential and employment after intial allocation
#-------------------------------------------------------------------------------
computeRemainingLand <- function(df_taz4, unallocatedCounties, useAgriLand){
  
  # Compute allocated land and convert remaining agriculture land as available res
  temp <- df_taz4 %>%
          mutate(
                 # Compute allocated HH, EMP land consumed
                 empSpentLand = EmpAllocated * nonresFactoredDensity,
                 hhSpentLand = HHAllocated  * resFactoredDensity,
                 
                 # Compute net remaining land
                 resAvailableAcres0 = resAvailableAcres,
                 nonresAvailableAcres0 = nonresAvailableAcres,
                 resAvailableAcres_net = pmax(0,resAvailableAcres - hhSpentLand),
                 nonresAvailableAcres_net = pmax(nonresAvailableAcres - empSpentLand,0),
                 
                 # Unmet counties, use left-over emp available land for res-consumption
                 resAvailableAcres1 = ifelse(growthCenter %in% unallocatedCounties,
                                             resAvailableAcres_net + nonresAvailableAcres_net,
                                             resAvailableAcres_net),
                 nonresAvailableAcres1 = ifelse(growthCenter %in% unallocatedCounties,
                                                0,
                                             nonresAvailableAcres_net),
                 
                 nonresAvailableAcres  = nonresAvailableAcres1,
                 resAvailableAcres = resAvailableAcres1,
                 
                 # If still unmet, use agriculture land towards available residential consumption
                 # IF demand is so high the agriculture land gets converted at higher rate (use a higher rate of 5%)
                 extraAgriLand = ifelse(growthCenter %in% unallocatedCounties & useAgriLand, 
                                            (5 / 100) * AgriculturalAcres, 
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
