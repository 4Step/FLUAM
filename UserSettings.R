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

# start_time <- Sys.time()

# Input files
path       <- "M:/Models/StateWide/TSM_Legacy/FLUAM"
Years      <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)
RunDir     <- c(0,       1,    1,    1,    1,    1,    1,    1)

useMPO_Controls <- FALSE
# If we are using MPO controls then use the below setting
# RunDir     <- c(0,     1,    1,    -1,    -1,    0,    1,    1)

# DRI specification
# 0 = provided are DRI totals 
# 1 = provided are DRI increments from last 5 years
global_Flag = 0  

# Agriculture land to res / non-res vacant land
# 1 = res/ non-res based on  2015 zonal developed ratio
# 2 = res/ non-res based on  2015 countywide developed ratio  
Agri_res_noRes_Flag = 2
rate = 1.5 # 3 percent / year - should be county specific

# Read control file
ctl_file     <- "Parameter/FLUAM_Properties.csv"

# 2015 SE data developed from Parcel Data
# taz_pd_file  <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
taz_pd_file  <- "Input/base_data/2015_TAZ_data_based_on_ParcelData_Nov1.xlsx"

gc_file      <- "Input/controlTotals/FLUAM_Growth_Controls.xlsx"
dc_file      <- "Input/base_data/new_density_constraints_at_TAZ_v2.csv"
# dc_file      <- "reference/FLUAM_2_1_DensityConstraints.csv"
DRI_file     <- "Input/controlTotals/FLUAM_DRI.xlsx"
ext_Stn_file <- "Input/controlTotals/External_Stns_GrowthFactors.xlsx"

# Max iterations 
max_iter <- 20

#-------------------------------------------------------------------------------
# Run FLUAM for each year
start_time <- Sys.time()

for(i in 2:length(Years)){
# for(i in 2:2){
  print("*******************************************************")
  print(paste("Computing FLUAM :", Years[i]))
  source("source_code_new/FLUAM.R")
}

end_time <- Sys.time()
end_time - start_time

#-------------------------------------------------------------------------------
# For MPO data 
if(useMPO_Controls){
}
Years      <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050)
RunDir     <- c(0,     -1,    -1,   -1,   -1,    0,    1,    1)

# (breakdown by anchor RunDir == 0)
anchors <- Years[which(RunDir == 0)]
# for()

#-------------------------------------------------------------------------------


