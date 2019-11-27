
#-------------------------------------------------------------------------------
# # Compute Land Consumption Summary
#-------------------------------------------------------------------------------
# "9_Summary_Land_Consumption.R"

summariseLandByCategory <- function(df){
  # Coutywide land category summary
  lc_county <- df %>% 
      group_by(growthCenter) %>%
      summarise_at( vars("housing", "employment", "resAvailableAcres","nonresAvailableAcres",	 
                         "resDeveloped" , "nonresDeveloped" ,
                         "undevelopableAcres", "AgriculturalAcres") , sum) %>%
      ungroup()
  
   # Statewide land category summary
   lc_state <- df %>% 
      summarise_at( vars("housing", "employment", "resAvailableAcres","nonresAvailableAcres",	 
                         "resDeveloped" , "nonresDeveloped" ,
                         "undevelopableAcres", "AgriculturalAcres") , sum)
   
   ret <- list(lc_state = lc_state, lc_county = lc_county)
   return(ret)
}
  
