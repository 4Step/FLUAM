# Builds "Fratar Input Growth" rates based on regional models' SE data
# Data:
#   1. Regional Models
#   2. CFRPM - Interim Years
#   3. FLUAM for Non-Regional Model Area

# Process:
#   Interpolation - from regional model years
#   Replacement for CFRPM TAZs
#   Substitute TAZ data for Non-Regional Areas from FLUAM
#   Trip Conversion (same logic as un FLUAM)
#   Growth Factor (ratio of successive years, 
#       for all regional model zones, this should be same as linear interpolation growth rate)
#   Summarise marginals by county and compute increments

library(tidyverse)
library(stringr)
library(openxlsx)


#------------------------------------------------------------------------------- 
# List of Inputs
#------------------------------------------------------------------------------- 
  # List of CFRPM data files (CFRPM zones)
  se_dir  <- "Input/CFRPM"
  
  # CFRPM model files description (fields to use)
  about_SE_files <- "About_these_datasets.xlsx"
  
  # CFRPM Model Data (output interim years, TSM legacy zones)
  cfrpm_data   <- "Output/cfrpm_interim_years.csv"
  
  #----------------------------------------------------------------------------- 
  # Regional Model Data (by TSM legacy zones)
  mpo_data     <- "Input/FL_MPO_SE_Data.xlsx"
  mpo_sheet    <- "Processed_MPO"
  
  # FLUAM Data (TSM legacy zones)
  fluam_data   <- "Input/Set_A_Summary.xlsx"
  fluam_sheet  <- "TAZ_Data"
  
  # DRI Input (TSM legacy zones)
  DRI_data     <- "Input/FLUAM_DRI.xlsx"
  DRI_sheet    <- "DRI_Increment"
  
  
  # Manually check and keep the zones that are in more than one model
  # Delete these duplicates
  delete_taz <- c( "Central 2390",
      "non-MPO 5620",
      "non-MPO 3095",
      "D1 4242",
      "D1 4297",
      "D1 4300")
  
  #-----------------------------------------------------------------------------
  # FRATAR Inputs
  ext_Stn_file <- "Input/External_Stns_GrowthFactors.xlsx"
  ctl_file     <- "Input/FLUAM_Properties.csv"
  
  # 2015 Parcel Data (TSM legacy zones)
  base_data    <- "Input/2015_TAZ_data_based_on_ParcelData_Nov1.xlsx"
  base_sheet   <- "base_data"


#------------------------------------------------------------------------------- 
# Run program
#------------------------------------------------------------------------------- 
skipInterpolation <- FALSE
  
  if(!skipInterpolation){
    # Compile CFRPM data
    source("script/compile_CFRPM_Interim_Years_to_TSM.R")
    
    # Interpolate between the regional model years
    source("script/Interpolate.R")
  }

  # Consolidate regional model, CFRPM, FLUAM, DRI, External data towards FRATAR Inputs
  source("script/Build_Fratar_files.R")
  
  # Develop County level growth factors
  source("script/develop_county_growth.R")
  
#------------------------------------------------------------------------------- 
