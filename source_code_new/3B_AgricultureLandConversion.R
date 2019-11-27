# convert agriculture land to res and non-res available
convertAgriculture <- function(dt_taz2, next_year, curr_year, rate, Agri_res_noRes_Flag){
  
  # This is always computed from the base land
  statewide_farmLand <- dt_taz2[ , sum(AgriculturalAcres)] * (rate / 100) * (next_year - curr_year)
  
  dt_taz2 <- dt_taz2[ , "resDevShare" := 0]
  if(Agri_res_noRes_Flag == 1){
      dt_taz2 <- dt_taz2[(resDeveloped + nonresDeveloped) > 0, 
                         "resDevShare" :=  resDeveloped / (resDeveloped + nonresDeveloped)]
  }
  
  if(Agri_res_noRes_Flag == 2){
    dt_taz2 <- dt_taz2[sum(resDeveloped + nonresDeveloped) > 0, 
                         "resDevShare" :=  sum(resDeveloped) / sum(resDeveloped + nonresDeveloped), by = growthCenter ]
  }

  dt_taz2 <- dt_taz2[, "convertedAgrLand" := pmin(AgriculturalAcres,
                                                 statewide_farmLand * accessCategorical / sum(accessCategorical))]
  
  dt_taz2 <- dt_taz2[, "convtResLand" := convertedAgrLand *  resDevShare]
  dt_taz2 <- dt_taz2[, "convtNonResLand" := convertedAgrLand - convtResLand]
  
  dt_taz2 <- dt_taz2[, "convtResLand" := convertedAgrLand *  resDevShare]
  dt_taz2 <- dt_taz2[, "convtNonResLand" := convertedAgrLand - convtResLand]
  
  # Update existing values
  dt_taz2 <- dt_taz2[, AgriculturalAcres := AgriculturalAcres - convertedAgrLand]
  dt_taz2 <- dt_taz2[, resAvailableAcres := resAvailableAcres + convtResLand]
  dt_taz2 <- dt_taz2[, nonresAvailableAcres := nonresAvailableAcres + convtNonResLand]
  
  return(dt_taz2)
}





