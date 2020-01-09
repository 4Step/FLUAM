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
library(data.table)
library(dplyr)
library(openxlsx)
library(Rcpp)
library(stringr)

# C++ function to run iterative proportional fitting with capping
sourceCpp("source_code_new/compute_IPF.cpp")

# User settings???
path       <- "M:/Models/StateWide/TSM_Legacy/FLUAM"
Years      <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)

# To run set- A, set-D (mutually exclusively)
#one of ("set_A", "set_D)

runScenario <- "Set_B"

# Cap trip ends
capTripEnds  <- 80000
ipf_max_iter <- 20

# These input files do not change
ODME_TT_file   <- "Input/base_data/adjusted_seed3.csv"
taz_pd_file    <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
ext_Stn_file   <- "Input/controlTotals/External_Stns_GrowthFactors.xlsx"

# Ouput summary file
summary_out_file <- "Output/IPF_Summary.xlsx"

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
  # df_ext    <- read.xlsx(ext_Stn_file, sheet = "ext_growthrates") %>%
  df_ext    <- read.xlsx(ext_Stn_file, sheet = "NB_CostalConnector (v9)") %>%
               select(TAZ, growth = paste0(year))
  
  # select seed table
  if(year == 2020){
    seedTT_file <- ODME_TT_file
  }else{
    prev_year <- Years[y-1]
    seedTT_file <- paste0("Output/",prev_year,"_mat.csv")
  }
  
  # Read FLUAM Growth Factors
  read_newdata  <- paste0("Output/",runScenario,"_Summary.xlsx")
   
  # Read growth factors
  df_growth <- read.xlsx(read_newdata, sheet = "growthFac") %>%
                   select(TAZ, growth = paste0("g",year))
  
   # Read hh, emp data for summary
  df_hh_EMP <- read.xlsx(read_newdata, sheet = "TAZ_Data") %>%
               select(TAZ, HH = paste0(year, "_HH"), EMP = paste0(year, "_EMP"))
  
  # Read Trips 
  df_int <- read.xlsx(read_newdata, sheet = "taz_trips") %>%
            select(TAZ, futureTrips = paste0(year)) %>%
            left_join(df_hh_EMP, by = "TAZ") %>%
            left_join(df_growth, by = "TAZ") 
  
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
  
  # Summary
  trip_summary <- df_int %>% 
                  select(TAZ, HH, EMP, tripGen = futureTrips, growth)
  
  
  #-----------------------------------------------------------------------------
  # run IPF
  print(paste("Running IPF for     ", year))
  print(paste("IPF seed table is   ", seedTT_file))
  print(paste("IPF output table is ", ipf_mat_file))
  print(paste("IPF log file is     ", log_file)) 
  print(paste("Scenario Name       ", runScenario))
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
print(Sys.time())
print(paste("IPF Run Time  :", runTime, units))




