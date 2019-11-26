#-------------------------------------------------------------------------------
# FRATAR
#-------------------------------------------------------------------------------
# Function to FRATAR trips
computeFRATAR <- function(df_taz4){
  df_taz5 <- df_taz4 %>%
    mutate(boolUrbanArea = ifelse(areaType == 2, 1, 0),
           baseTrips =	as.numeric(ctl$fratConstant) + 
             fratEMPFact[areaType] * employment  +
             as.numeric(ctl$fratHHFact) * housing  + 
             as.numeric(ctl$fratUrbanArea)* boolUrbanArea,
           futureTrips =	as.numeric(ctl$fratConstant) + 
             fratEMPFact[areaType] * EmpTotal  +
             as.numeric(ctl$fratHHFact) * HHTotal  + 
             as.numeric(ctl$fratUrbanArea)* boolUrbanArea,
           growth = futureTrips / baseTrips
    )
  
  return(df_taz5)
}

#-------------------------------------------------------------------------------
# Function to write fratar grwoth files
writeFRATARInput <- function(df_taz5, fratar_file){
  # Format for Tranplan 
  df_fratar <- df_taz5 %>%
    mutate(i = 1,
           fo = "FO",
           ftaz = stringr::str_pad(TAZ, width = 4, side = "left", pad = "0"),
           fgc = stringr::str_pad(round((growth + 0.005) * 100, 0), width = 6, side = "left", pad = "0")) %>%
    select(fo, ftaz, i, fgc)
  
  # Write FRATAR File
  write.table(df_fratar, fratar_file, sep = " ", quote = FALSE,
              row.names = F, col.names = F) 
}
#-------------------------------------------------------------------------------

# TODO: This is a separate file for next iteration
# Function to export outputs
exportFRATARTrips <- function(df_taz5, taz_fields, hhConverge, empConverge, out_file){
  
  # keep the standard input fields 
  df_taz6 <- df_taz5 %>%
             select(-housing, -employment) %>%
             rename(housing = HHTotal, employment = EmpTotal) %>%
             select(taz_fields)
  
  # Export data
  excel_data <- list("TAZ_Data" = df_taz6,
                     "debug" = df_taz5,
                     "hhConverge" = hhConverge, 
                     "empConverge" = empConverge )
  write.xlsx(excel_data, out_file) 
}





