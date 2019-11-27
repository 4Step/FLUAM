#-------------------------------------------------------------------------------
# # Compute Land Consumption variables
#-------------------------------------------------------------------------------
prepareLCVariables <- function(dt_taz2, ctl){
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
                               # FLUAM 2.1 uses existing density but after allocation, we don't compute consumed land
                               # Thus it creates higher density
                               # pmax(0, log( (housing / resDeveloped ) + 0.01) ), 
                               pmax(0, log( (housing / (resDeveloped + availableAcres)) + 0.01) ), 
                               0),
           llache = log(avgAdjTTimeWgtByHH_Emp + 3),
           boolUGB = ifelse(growthBoundary == 1, 1, 0),
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
  
  return(df_taz3)
}




