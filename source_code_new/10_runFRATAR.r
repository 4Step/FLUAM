# This IPF is designed to scale seed trip tables (output of ODME) with zonal cap.
# 
# The scaling factors (aka growth factors) are computed based on existing and allocated landuse
# 
# The criticism of this approach is base landuse and ODME seed need to be correlated and
# were found to be less correlated. Due to this lack of dependancey on landuse, the tripends by zone 
# were required to be artifically capped (ex: 80,000 trips per zone). Ideally this would be handled
# via hh, emp density maximization where each zone is delinated based on number of HH or 
# pracitical capacity (density) and not in trip tables. NextGen is going to replace all of this.
# In the meantime reluctantly adherirng to this out-dated practice. 
#
#
# This is a post processor to FLUAM
# Theoritically FLUAM outputs are zonal hh, emp and the next three steps are:
# 1. apply trip generation rates to produce zonal trip ends
# 2. compare base and future year trip ends to compute growth factors
# 3. apply IPF to scale ODME trip table with the growth factors
#
# Three Sets:
# Set-A, 
# Set-B uses a different setting to read trips and growth summary
# Set-C although similar to set-A, growth factors in the output files are in 
#  decending order due to deallocation (meaning 2020 carries 2025 growth factors instead of 2020)
#
#-------------------------------------------------------------------------------

# C++ function to run iterative proportional fitting with capping
sourceCpp("source_code_new/compute_IPF.cpp")

# User settings???
path       <- "M:/Models/StateWide/TSM_Legacy/FLUAM"
Years      <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)

# To run set- A, set-B, set-C (mutually exclusively)
set_A  <-  FALSE
set_B  <-  FALSE
set_C  <-  TRUE

scenarios <- c(set_A, set_B, set_C)
scenNames <- c("set_A", "set_B", "set_C")

# Cap trip ends
capTripEnds  <- 80000
ipf_max_iter <- 20

# These input files do not change
ODME_TT_file   <- "Input/base_data/adjusted_seed.csv"
taz_pd_file    <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
ext_Stn_file   <- "Input/controlTotals/External_Stns_GrowthFactors.xlsx"

# Ouput summary file
summary_out_file <- "Output/IPF_Summary.xlsx"

# year          <- 2020
start_time <- Sys.time()

# Ouput tripend summary data
excel_data <- list()  


# Process for Set-C (2040)
# TODO: Move this outside this process
if(set_C){
   
   taz_data <- read.xlsx(paste0("Output/Set_C_Summary.xlsx"))
   taz_data_2040 <- read.xlsx(paste0("Output/2040_FLUAM_Output.xlsx")) %>%
                    select(TAZ, areaType)
   
   # reformat data
   taz_data <- taz_data %>% 
               left_join(taz_data_2040, by = "TAZ") %>%
               gather(var, value, -TAZ, -growthCenter, -areaType) %>%
               separate(var, c("yyyy", "var"), sep = "_") %>%
               spread(var, value)
   
   # compute trips and growth rates
    taz_data <- taz_data %>%   
                mutate(boolUrbanArea = ifelse(areaType == 2, 1, 0),
                 Trips =	as.numeric(ctl$fratConstant) + 
                   fratEMPFact[areaType] * EMP  +
                   fratHHFact[areaType] * HH  + 
                   as.numeric(ctl$fratUrbanArea)* boolUrbanArea) 
    
    # Compute growth factors
    set_c_gfac <- taz_data %>%
           mutate(yyyy = ifelse(is.na(yyyy), 0, yyyy)) %>%
           select(-EMP, -HH, -boolUrbanArea) %>%
           spread(yyyy, Trips) %>%
           replace(is.na(.),0) %>%
           mutate(g2020 = ifelse( `2015` > 0 , `2020` / `2015`, 1),
                  g2025 = ifelse( `2020` > 0 , `2025` / `2020`, 1),
                  g2030 = ifelse( `2025` > 0 , `2030` / `2025`, 1),
                  g2035 = ifelse( `2030` > 0 , `2035` / `2030`, 1),
                  g2040 = ifelse( `2035` > 0 , `2040` / `2035`, 1),
                  g2045 = ifelse( `2040` > 0 , `2045` / `2040`, 1),
                  g2050 = ifelse( `2045` > 0 , `2050` / `2045`, 1)) %>%
           select(TAZ, g2020, g2025, g2030, g2035, g2040, g2045, g2050)
}


for(y in 1:length(Years)){
  
  #-----------------------------------------------------------------------------
  # Loop Settings
  year      <- Years[y]

  
  # outputs
  log_file     <- paste0("Output/",year,"_IPF.log")
  ipf_mat_file <- paste0("Output/",year,"_mat.csv")
  
  # Get growth factors
  df_ext    <- read.xlsx(ext_Stn_file, sheet = "ext_growthrates") %>%
               select(TAZ, growth = paste0(year))
  
  # select seed table
  if(year == 2020){
    seedTT_file <- ODME_TT_file
  }else{
    prev_year <- Years[y-1]
    seedTT_file <- paste0("Output/",prev_year,"_mat.csv")
  }
  
  # Read FLUAM Growth Factors
  # SET-A growth factors
  # if(!runMPO_tables){
    
  if(set_A){
      read_newdata  <- paste0("Output/",year,"_FLUAM_Output.xlsx") 
      sheetName     <- "debug_reallocate"
      df_int        <- read.xlsx(read_newdata, sheet = sheetName) 

    # For new zones (no hh in base year), growth fac is NA, flag such zones
    df_int_growth <- df_int %>% 
                     select(TAZ, growth) %>%
                     mutate(growth = ifelse(is.na(growth), -9999, growth),
                            growth = ifelse(growth > 2, -9999, growth)) 
    
    # Use new trip end totals for such zones
    df_int_tripends <- df_int %>% 
                       mutate(futureTrips = ifelse(is.na(growth), futureTrips, 0)) %>%
                       # mutate(futureTrips =  0) %>%
                       select(TAZ, futureTrips)
    
    # for final summary
    trip_summary <- df_int %>% 
                    select(TAZ, HH = HHTotal, EMP = EmpTotal, tripGen = futureTrips, growth)
  }
  
  # SET-B growth factors
  # Read MPO based Growth Factors
  if(set_B){
    read_newdata  <- paste0("Output/summary_MPO_Output.xlsx")
    sheetName     <- "growthFac"
    
    # Get growth factors
    df_int_growth <- read.xlsx(read_newdata, sheet = sheetName) %>%
                     select(TAZ, growth = paste0("g",year))
    
    # read data for summary
    df_int <- read.xlsx(read_newdata, sheet = paste0(year)) %>%
              select(TAZ, HH, EMP, tripGen = Trips) %>%
              left_join(df_int_growth, by = "TAZ")
    
    # Blank vector of zeros (IPF function requirement due to above line 61 & 66)
    df_int_tripends <- df_int %>%
                       mutate(futureTrips = 0) %>%
                       select(TAZ, futureTrips)
    
    # for final summary
    trip_summary <- df_int 
    
  }
  
  # SET - C processing for growth factors
  if(set_C){
    
    # For new zones (no hh in base year), growth fac is NA, flag such zones
    df_int_growth <- set_c_gfac %>% 
                     select(TAZ, growth = paste0("g", year)) %>%
                     mutate(growth = ifelse(is.na(growth), -9999, growth),
                            growth = ifelse(growth > 2, -9999, growth)) 
    
    # for final summary
    trip_summary <- taz_data %>% 
                    filter(yyyy == year) %>%
                    select(TAZ, HH, EMP, tripGen = Trips) %>%
                    left_join(df_int_growth, by = "TAZ")

    # Use new trip end totals for such zones
    df_int_tripends <- trip_summary %>% 
                       mutate(futureTrips = ifelse(is.na(growth), tripGen, 0)) %>%
                       select(TAZ, futureTrips)
    
  }
  
  #-----------------------------------------------------------------------------
  # run IPF
  print(paste("Running IPF for     ", year))
  print(paste("IPF seed table is   ", seedTT_file))
  print(paste("IPF output table is ", ipf_mat_file))
  print(paste("IPF log file is     ", log_file)) 
  print(paste("Scenario Name       ", scenNames[scenarios]))
  print("-----------------------------------------")
  
  # Read seed table
  if(y == 1) {
    dt_seedTT <- fread(seedTT_file)
  } else {
    rm(dt_seedTT)
    # use a copy or the same from memory ? 
    # dt_seedTT <- copy(newMat)
    dt_seedTT <- newMat
    
  }
  
  df_growth <- rbind(df_int_growth, df_ext)
  max_taz   <- max(df_growth$TAZ)

  # Print log file
  sink(log_file)
  
  # Run FRATAR (2D IPF)
  ret <- runIPF(dt_seedTT, df_growth$growth, df_int_tripends$futureTrips, capTripEnds, ipf_max_iter)
  
  # reset to console
  sink()

  #-----------------------------------------------------------------------------
  # Write summaries
  
  # write new balanced matrix
  newMat <- ret$newMat %>% setDT()
  newMat <- newMat[, Trip := round(Trip,0)]
  fwrite(newMat, ipf_mat_file, col.names = FALSE)
  
  # Final output
  rowsum <- ret$rowsum
  colsum <- ret$colsum
  IPF_tripends <- data.frame(TAZ = df_growth$TAZ, IPF_rowsum = rowsum, IPF_colsum = colsum)
  
  trip_summary <- trip_summary %>% 
                  left_join(IPF_tripends, by = "TAZ")
  
  # save summary
  excel_data[[y]] <- trip_summary
  
}


# Summary of trips
names(excel_data) <- Years

for(y in 1: length(Years)){
  temp_total <- excel_data[[y]] %>% summarise_at(vars(HH, tripGen, IPF_rowsum), sum)
  temp_total <- cbind(Years[y], temp_total)
  if(y == 1){
    trp_total <- temp_total
  } else{
     trp_total <- rbind(trp_total, temp_total)
  }
}

trp_total <- trp_total %>%
             mutate(rate_gen = round(tripGen / HH,2),
                    rate_ipf = round(IPF_rowsum / HH,2))  

excel_data[["trip_summary"]] <- trp_total

# Write output trip end summary
write.xlsx(excel_data, summary_out_file) 



end_time <- Sys.time()
runTime <- round(end_time - start_time, 2)
units <- attr(runTime, "units")
print("*******************************************************")
print(paste("IPF Run Time  :", runTime, units))




