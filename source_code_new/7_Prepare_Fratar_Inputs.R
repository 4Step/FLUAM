#-------------------------------------------------------------------------------
# FRATAR
#-------------------------------------------------------------------------------
# Function to FRATAR trips
computeFRATAR <- function(df_taz4, runType){
  df_taz5 <- df_taz4 %>%
    mutate(boolUrbanArea = ifelse(areaType == 2, 1, 0),
           baseTrips     = as.numeric(ctl$fratConstant) + 
                           fratEMPFact[areaType] * employment  +
                           fratHHFact[areaType] * housing  + 
                           as.numeric(ctl$fratUrbanArea)* boolUrbanArea ,
           baseTrips     = baseTrips * scale_fac,
           futureTrips   = as.numeric(ctl$fratConstant) + 
                           fratEMPFact[areaType] * EmpTotal  +
                           fratHHFact[areaType] * HHTotal  + 
                           as.numeric(ctl$fratUrbanArea)* boolUrbanArea ,
           futureTrips   = futureTrips * scale_fac,
           growth        = futureTrips / baseTrips
    )
  
  # For MPO model
  if(runType < 0){
     df_taz5 <-  df_taz5 %>%
                 mutate(growth =  baseTrips / futureTrips)
  }
  
  return(df_taz5)
}

#-------------------------------------------------------------------------------
# Function to write fratar grwoth files
writeFRATARInput <- function(df, fratar_file, ext_Stn_file, next_year){

  # Format II Trips for Tranplan 
  df_fratar <- df %>%
    mutate(i = 1,
           fo = "FO",
           ftaz = stringr::str_pad(TAZ, width = 4, side = "left", pad = "0"),
           fgc = stringr::str_pad(round((growth + 0.005) * 100, 0), width = 7, side = "left", pad = "0"),
           id = paste(fo, ftaz, i, fgc, sep = " ")) %>%
    # select(fo, ftaz, i, fgc) %>%
     select(id) 
   
   
  # Get External Stn Growth Factors
  df_ext <- read.xlsx(ext_Stn_file) %>%
            select(id = paste0("Year.",next_year))
  
  df_fratar <- rbind(df_fratar, df_ext)
    
  # Write FRATAR File
  write.table(df_fratar, fratar_file, sep = " ", quote = FALSE,
              row.names = F, col.names = F) 
}
#-------------------------------------------------------------------------------

# TODO: This is a separate file for next iteration
# Function to export outputs
# exportFRATARTrips <- function(df_taz4, df_taz5, taz_fields, hhConverge, empConverge, out_file){
#   
#   # keep the standard input fields 
#   df_taz6 <- df_taz5 %>%
#              select(-housing, -employment) %>%
#              rename(housing = HHTotal, employment = EmpTotal) %>%
#              select(taz_fields)
#   
#   # Export data
#   excel_data <- list("TAZ_Data" = df_taz6,
#                      "debug" = df_taz4,
#                      "debug_reallocate" = df_taz5,
#                      "hhConverge" = hhConverge, 
#                      "empConverge" = empConverge,
#                      "hhconverge2" = hhConverge2)
#   write.xlsx(excel_data, out_file) 
# }





