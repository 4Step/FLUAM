# POST - PROCESSOR
# Get Summary of FLUAM Ouputs (run-outside FLUAM model)
library(openxlsx)
# library(tidyverse)

out_dir     <- "Output"

useMPO_Controls <- FALSE

df_kfac <- read.csv(tripGen_facs)

if(!useMPO_Controls){
  
   base_file   <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
   
   # Read parcel 2015 data
   base_data   <- read.xlsx(base_file, sheet = "base_data") %>%
                 select(TAZ, growthCenter, areaType, `2015_HH` = housing, `2015_EMP` = employment)
   out_file    <- "Set_A_Summary.xlsx"
} 

if(useMPO_Controls){
   
   base_file   <- "Input/base_data/2040_TAZ_data_based_on_MPO_Models.xlsx"
   
   # Read mpo 2015 base data
   base_data   <- read.xlsx(base_file, sheet = "mpo_fluam_data") %>%
                  select(TAZ, growthCenter, areaType, `2015_HH`, `2015_EMP`)

   out_file    <- "Set_C_Summary.xlsx"
}


# Get list of FLUAM outputs 
fluam_files <- list.files(out_dir, pattern = ".xlsx")


# check and delete previous file
check_outfile <- paste(out_dir, fluam_files[(fluam_files %in% out_file)], sep = "/")
if (file.exists(check_outfile)) {
  unlink(check_outfile)
  fluam_files <- fluam_files[!(fluam_files %in% out_file)]
}

# Loop by year get allocated HH, EMP by TAZ
for(f in 1:length(fluam_files )){
  
  fileName <- fluam_files[f]
  year <- as.integer(strsplit(fileName, "_")[[1]][1])
  
  temp <- read.xlsx(paste(out_dir, fluam_files[f], sep = "/"), sheet = "TAZ_Data") %>%
          select(TAZ, housing, employment)

  colnames(temp) <- c("TAZ", paste0(year, "_HH"),  paste0(year, "_EMP"))  
  
  base_data <- base_data %>%
               left_join(temp, by = "TAZ")
  
}

# Summary
county_summary <- base_data %>%
                  select(-TAZ, -areaType) %>%
                  gather(var, value, -growthCenter) %>%
                  group_by(growthCenter, var) %>%
                  summarise(val = sum(value)) %>%
                  spread(var,  val)

  
# Compute Trip Generation
taz_data <- base_data %>% 
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

# Scale trips to match 
taz_data <- taz_data %>%
           left_join(df_kfac, by = "growthCenter") %>%
           mutate(Trips = Trips *scale_fac)

# Compute growth factors
taz_gfac <- taz_data %>%
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
              g2050 = ifelse( `2045` > 0 , `2050` / `2045`, 1))   

# Keep computed, scaled trips
taz_trips <- taz_gfac %>%
            select(TAZ, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`)

# Make sure we don't have negative growth at county level
taz_gfac <- taz_gfac %>%
            select(TAZ, g2020, g2025, g2030, g2035, g2040, g2045, g2050) %>%
            mutate(g2020 = ifelse(g2020 < 1, 1, g2020))

# Write Output
excel_data <- list("TAZ_Data" = base_data,
                   "county_summary" = county_summary,
                   "taz_trips" = taz_trips,
                   "growthFac" = taz_gfac)

write.xlsx(excel_data, paste(out_dir, out_file, sep = "/")) 


