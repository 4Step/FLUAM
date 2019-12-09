# This IPF is designed to scale seed trip tables (output of ODME)
# 
# The scaling factors (aka growth factors) are computed based on existing and allocated landuse
# 
# The criticism of this approach is base landuse and ODME seed need to be correlated and
# were found to be less correlated. Due to this lack of dependancey, the tripends by zone 
# were required to be artifically capped (ex: 80,000 trips per zone). Ideally this would be handled
# via hh, emp density maximization where each zone is delinated based on number of HH or 
# pracitical capacity (density)

# This is a post processor to FLUAM
# Theoritically FLUAM outputs are zonal hh, emp and the next three steps are:
# 1. apply trip generation rates to produce zonal trip ends
# 2. compare base and future year trip ends to compute growth factors
# 3. apply IPF to scale ODME trip table with the growth factors
#
#-------------------------------------------------------------------------------

# C++ function to run iterative proportional fitting with capping
sourceCpp("source_code_new/compute_IPF.cpp")

# User settings???
path       <- "M:/Models/StateWide/TSM_Legacy/FLUAM"
Years      <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)

# To run MPO (set- B)
runMPO_tables <- FALSE

# Cap trip ends
capTripEnds  <- 80000
ipf_max_iter <- 20

# These input files do not change
ODME_TT_file   <- "Input/base_data/adjusted_seed.csv"
taz_pd_file    <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
ext_Stn_file   <- "Input/controlTotals/External_Stns_GrowthFactors.xlsx"

# Ouput summary file
summary_out_file <- "Output/IPF_Ssummary.xlsx"

# year          <- 2020
start_time <- Sys.time()

# Ouput tripend summary data
excel_data <- list()  

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
  if(!runMPO_tables){
    read_newdata  <- paste0("Output/",year,"_FLUAM_Output.xlsx") 
    sheetName     <- "debug_reallocate"
    df_int        <- read.xlsx(read_newdata, sheet = sheetName)
    
    # For new zones (no hh in base year), growth fac is NA, flag such zones
    df_int_growth <- df_int %>% 
                     select(TAZ, growth) %>%
                     mutate(growth = ifelse(is.na(growth), -9999, growth)) 
    
    # Use new trip end totals for such zones
    df_int_tripends <- df_int %>% 
                       mutate(futureTrips = ifelse(is.na(growth), futureTrips, 0)) %>%
                       # mutate(futureTrips =  0) %>%
                       select(TAZ, futureTrips)
    
    # for final summary
    trip_summary <- df_int %>% 
                    select(TAZ, HH = HHTotal, EMP = EmpTotal, tripGen = futureTrips, growth)
  }
  
  # Read MPO based Growth Factors
  if(runMPO_tables){
    read_newdata  <- paste0("Output/summary_MPO_Output.xlsx")
    sheetName     <- "growthFac"
    
    # Get growth factors
    df_int_growth <- read.xlsx(read_newdata, sheet = sheetName) %>%
                     select(TAZ, growth = paste0("g",year))
    
    # read data for summary
    df_int <- read.xlsx(read_newdata, sheet = year) %>%
              select(TAZ, HH, EMP, tripGen = Trips) %>%
              left_join(df_int_growth, by = "TAZ")
    
    # Blank vector of zeros (IPF function requirement due to above line 61 & 66)
    df_int_tripends <- df_int %>%
                       mutate(futureTrips = 0) %>%
                       select(TAZ, futureTrips)
    
  }
  
  #-----------------------------------------------------------------------------
  # run IPF
  print(paste("Running IPF for     ", year))
  print(paste("IPF seed table is   ", seedTT_file))
  print(paste("IPF output table is ", ipf_mat_file))
  print(paste("IPF log file is     ", log_file)) 
  print(paste("run MPO triptables  ", runMPO_tables))
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
  fwrite(newMat, ipf_mat_file)
  
  # Final output
  rowsum <- ret$rowsum
  colsum <- ret$colsum
  IPF_tripends <- data.frame(TAZ = df_growth$TAZ, IPF_rowsum = rowsum, IPF_colsum = colsum)
  
  trip_summary <- trip_summary %>% 
                  left_join(IPF_tripends, by = "TAZ")
  
  # save summary
  excel_data[[y]] <- trip_summary
  
}

# Write output trip end summary
# names(excel_data) <- Years
write.xlsx(excel_data, summary_out_file) 

end_time <- Sys.time()
end_time - start_time

