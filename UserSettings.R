# This is the only file where user sets specifies the model specification
# RunDir :  0  -> Do not run
#        :  1  -> Increment Growth from previous year (ex: 2020 gets built on top of 2015)
#        : -1  -> Decrement Growth from future year (ex: 2035 gets built from 2040)
# For RunDir = -1 the model calculations are run exactly the same way except for two exceptions:
# 1. Starting HH, EMP are future year and the accessibilities for both years are weighted by future year 
# 2. Second, 

# Libraries
library(data.table)
library(tidyverse)
library(openxlsx)
library(Rcpp)
library(stringr)

# Input files
path       <- "M:/Models/StateWide/TSM_Legacy/FLUAM"

Years      <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)
runFLUAM   <- c(0,       1,    1,    1,    1,    1,    1,    1)
runMPO     <- c(0,      -1,   -1,   -1,   -1,    0,    1,    1) 

useMPO_Controls <- FALSE

# DRI specification
# 0 = provided are DRI totals 
# 1 = provided are DRI increments from last 5 years
global_Flag = 0  

# Agriculture land to res / non-res vacant land
# 1 = res/ non-res based on  2015 zonal developed ratio
# 2 = res/ non-res based on  2015 countywide developed ratio  
Agri_res_noRes_Flag = 2
rate = 1.5              

# Read control file
ctl_file     <- "Parameter/FLUAM_Properties.csv"

# 2015 SE data developed from Parcel Data
# taz_pd_file  <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
taz_pd_file  <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"

gc_file      <- "Input/controlTotals/FLUAM_Growth_Controls.xlsx"
dc_file      <- "Input/base_data/new_density_constraints_at_TAZ_v2.csv"
DRI_file     <- "Input/controlTotals/FLUAM_DRI.xlsx"
ext_Stn_file <- "Input/controlTotals/External_Stns_GrowthFactors.xlsx"

mpo_base_file <- "Input/base_data/2040_TAZ_data_based_on_MPO_Models.xlsx"

# tripGen_facs <- "Input/base_data/TripGen_kFactors.csv"
tripGen_facs <- "Input/base_data/TripGen_scaleFactors.csv"


# Max iterations 
max_iter <- 20

# To write land consumption & HH, EMP by iteration
debug    <- FALSE

#-------------------------------------------------------------------------------
# Run FLUAM for each year
start_time <- Sys.time()

if(!useMPO_Controls){
  
  # use "sub" variables to support MPO_Control Settings
  RunDir       <- runFLUAM
  sub_runs     <- RunDir
  sub_Years    <- Years
    
  for(i in 2:length(Years)){
  # for(i in 2:2){
    print("*******************************************************")
    print(paste("Computing FLUAM :", Years[i]))
    source("source_code_new/FLUAM.R")
  }
}


#-------------------------------------------------------------------------------
# For MPO data 
if(useMPO_Controls){
  
 # Years      <- c(2020, 2025, 2030, 2035, 2040, 2045, 2050)
 # RunDir     <- c(  -1,   -1,   -1,   -1,    0,    1,    1) 
 Years      <- Years[2:length(Years)]
 RunDir     <- runMPO[2:length(runMPO)]
 
 # Use DRI from Increment tab (it's unwanted complexity to change sign and compute deltas)
 global_Flag <- 1
 
 # break point (currently set to use only one break point)
 pos <- which(RunDir == 0)
 
 # decrement
 sub_years1  <- Years[pos:1]
 sub_run1    <- RunDir[pos:1]  
 
 # increment
 sub_years2  <- Years[pos:length(Years)]
 sub_run2    <- RunDir[pos:length(RunDir)]  
 
 # array of runs
 nest_years <- list(sub_years1, sub_years2)
 nest_runs  <- list(sub_run1, sub_run2)
 
 for(n in 1:length(nest_years)){
 # for(n in 2:2){
   sub_Years <- nest_years[[n]]
   sub_runs  <- nest_runs[[n]]

    for(i in 2:length(sub_Years)){
    # for(i in 2:2){
      print("*******************************************************")
      print(paste("Computing FLUAM :", sub_Years[i]))
      
      # Use MPO model 2040 TAZ data as starting point
      if(sub_Years[i-1] == 2040){
         source_taz_file <- read.xlsx(mpo_base_file, sheet = "base_data")
         target_file     <- paste0("Output/2040_FLUAM_Output.xlsx")
         excel_data      <- list("TAZ_Data" = source_taz_file)
         write.xlsx(excel_data, target_file) 
      }
      
      source("source_code_new/FLUAM.R")
      
      if(sub_Years[i] == 2020 & sub_runs[i] == -1){
        source("source_code_new/compute_growth_MPO_Deallocation_2020.R")
      }
    }
   
 }
  
}

#-------------------------------------------------------------------------------
end_time <- Sys.time()
runTime <- round(end_time - start_time, 2)
units <- attr(runTime, "units")
print("*******************************************************")
print(paste("FLUAM Run Time  :", runTime, units))


#-------------------------------------------------------------------------------


