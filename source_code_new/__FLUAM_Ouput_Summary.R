# POST - PROCESSOR
# Get Summary of FLUAM Ouputs (run-outside FLUAM model)
library(openxlsx)
library(tidyverse)

out_dir     <- "Output"
out_file    <- "Set_C_Summary.xlsx"

base_file   <- "Input/base_data/2015_TAZ_data_based_on_ParcelData_Nov1.xlsx"

# Get list of FLUAM outputs 
fluam_files <- list.files(out_dir, pattern = ".xlsx")

# Read base data
base_data   <- read.xlsx(base_file, sheet = "base_data") %>%
               select(TAZ, growthCenter, `2015_HH` = housing, `2015_EMP` = employment)

# Loop by year get allocated HH, EMP by TAZ
for(f in 1:length(fluam_files )){
  fileName <- fluam_files[f]
  year <- as.integer(strsplit(fileName, "_")[[1]][1])
  
  temp <- read.xlsx(paste(out_dir, fluam_files[f], sep = "/"), sheet = "TAZ_Data") %>%
        select(TAZ, housing, employment)
                    # HHAllocated, resFactoredDensity,
                    # EmpAllocated, nonresFactoredDensity,
                    # resAvailableAcres, nonresAvailableAcres,
                    # resDeveloped, nonresDeveloped,
                    # AgriculturalAcres, undevelopableAcres,
                    # totalAcres) %>% 
  colnames(temp) <- c("TAZ", paste0(year, "_HH"),  paste0(year, "_EMP"))  
  
  base_data <- base_data %>%
               left_join(temp, by = "TAZ")
  
}

# Summary
  county_summary <- base_data %>%
                    select(-TAZ) %>%
                    gather(var, value, - growthCenter) %>%
                    group_by(growthCenter, var) %>%
                    summarise(val = sum(value)) %>%
                    spread(var,  val)

# Write Output
excel_data <- list("TAZ_Data" = base_data,
                   "county_summary" = county_summary
                   )

write.xlsx(excel_data, paste(out_dir, out_file, sep = "/")) 


