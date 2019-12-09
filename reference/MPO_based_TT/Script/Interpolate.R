library(openxlsx)
library(tidyverse)

# mpo_data     <- "Input/FL_MPO_SE_Data.xlsx"
# mpo_sheet    <- "Processed_MPO"
# 
# fluam_data   <- "Input/Set_A_Summary.xlsx"
# fluam_sheet  <- "TAZ_Data"
# 
# 
# DRI_data     <- "Input/FLUAM_DRI.xlsx"
# DRI_sheet    <- "DRI_Increment"
# 
# cfrpm_data   <- "Output/cfrpm_interim_years.csv"
# 
# base_data    <- "Input/2015_TAZ_data_based_on_ParcelData_Nov1.xlsx"
# base_sheet   <- "base_data"


# Read cfrpm data 
df_cfrpm    <-  read.csv(cfrpm_data)

# read mpo data based hh, emp
df_m <- read.xlsx(mpo_data, sheet = mpo_sheet)

# Read FLUAM Outputs (Set-A, allocated HH, EMP)
df_p <- read.xlsx(fluam_data, sheet = fluam_sheet)

# Read DRI increments
df_d <- read.xlsx(DRI_data, sheet = DRI_sheet)

# Read Parcel data
df_b <- read.xlsx(base_data, sheet = base_sheet)

# Manually check and keep the zones that are in more than one model
# Delete these duplicates
# delete_taz <- c( "Central 2390",
#     "non-MPO 5620",
#     "non-MPO 3095",
#     "D1 4242",
#     "D1 4297",
#     "D1 4300")

#------------------------------------------------------------------------------- 
# FUNCTIONS
#------------------------------------------------------------------------------- 
# function to summarise model data
getModelSummary <- function(data_f){
  
  models_sum <- data_f %>% 
              group_by(Model, Year) %>%
              summarise_at(c("TOT_DU", "TOT_POP", "TOT_EMP"), sum)
  
  total <- models_sum %>% 
           ungroup() %>% 
           mutate(Year = ifelse(Year <= 2015, 2015, 2045)) %>% 
           group_by(Year) %>% 
           summarise_if(is.numeric, sum) %>% 
           mutate(Model = 'Total') 
  
  models_sum <- bind_rows(models_sum, total) %>%
           arrange(Year, Model) %>%
           gather(var, value, -Model, -Year) %>%
           mutate(id = paste(Year, var, sep = "-")) %>%
           select(-Year, -var) %>%
           spread(id, value)
}

# function to interpolate
interpolate <- function(b_val, f_val, b_year, f_year, c_year){
  n = b_year - c_year
  int = b_val + (n * (b_val - f_val) / (f_year - b_year))
  round(int,0)
}

#------------------------------------------------------------------------------- 
# FUNCTIONS
#------------------------------------------------------------------------------- 
df_m2 <- df_m %>% 
          mutate(`2020` =  interpolate(base_val, futu_val, 
                                       base_year, futu_year, 2020),
                 `2025` =  interpolate(base_val, futu_val, 
                                       base_year, futu_year, 2025),
                 `2030` =  interpolate(base_val, futu_val, 
                                       base_year, futu_year, 2030),
                 `2035` =  interpolate(base_val, futu_val, 
                                       base_year, futu_year, 2035),
                 `2040` =  interpolate(base_val, futu_val, 
                                       base_year, futu_year, 2040),
                 `2050` =  interpolate(base_val, futu_val, 
                                       base_year, futu_year, 2050)) %>%
          mutate(`2050` = ifelse(`2050` < 0, round(futu_val, 0), `2050`))

df_m2 <- df_m2 %>%
         select(-base_year, -futu_year, -base_val, -futu_val, -TSM_NextGen) %>%
         gather(year, valume, -TAZ, -Model , -var, -TSM_Legacy, -County)

# Create a cross-walk for CFRPM zones
df_m2_cfrpm <- df_m2 %>%
               filter(Model == "Central") %>%
               select(TAZ, Model, TSM_Legacy, County) %>%
               distinct()

df_m_wo_cfrpm <- df_m2 %>%
                 filter(!(Model %in% c("Central", "non-MPO")))

# List of non-mpo zones
nonMPO_Zones <- df_m2 %>%
                filter(Model == "non-MPO") %>%
                select(TSM_Legacy) %>%
                distinct() %>%
                pull(TSM_Legacy)

#-------------------------------------------------------------------------------   
df_cfrpm2 <- df_cfrpm %>%
             gather(year, valume, -TAZ, -Model , -var) %>%
             mutate(year = gsub("X","", year),
                    Model = as.character(Model)) 
             
# Append TSM data
df_cfrpm3 <- df_cfrpm2 %>%
             left_join(df_m2_cfrpm, by = c("TAZ", "Model"))


# Replace CFRPM from the MPO interpolated data
df_mpo <-  bind_rows(df_m_wo_cfrpm, df_cfrpm3) %>%
           filter(!is.na(TSM_Legacy) & var != "TOT_POP" & Model != "non-MPO")
#------------------------------------------------------------------------------- 
# Process DRI data
df_d2 <- df_d %>%
         select(-`Total35y.DRI_HH`, -`Total35y.DRI_EMP`, -housing, -employment) %>%
         gather(var, value, -TAZ) %>%
         separate(var, c("year", "var"), sep = "\\.") %>%
         separate(var, c("DRI", "var"), sep = "_") %>%
         select(TAZ, year, var, value)

#------------------------------------------------------------------------------- 
# Build TSM data
# Get FLAUM for nonMPO zones
df_nonMPO <- df_p %>%
             filter(TAZ %in% nonMPO_Zones) %>%
             select(-growthCenter) %>%
             gather(var, value, -TAZ) %>%
             separate(var, c("year", "var"), sep = "_")

df_mpo2 <- df_mpo %>%
          mutate(var2 = case_when(var == "TOT_DU" ~ "HH",
                                  var == "TOT_EMP" ~ "EMP")) %>%
          select(TAZ = TSM_Legacy, year, var = var2, value = valume ) 
          
df_tsm <- bind_rows(df_mpo2, df_nonMPO, df_d2) %>%
          group_by( TAZ, year, var) %>%
          summarise(val = sum(value)) %>%
          spread(var, val)
          
#------------------------------------------------------------------------------- 
# Read Area Type
df_b2 <- df_b %>%
         select(TAZ, areaType) %>%
         full_join(df_tsm, by = "TAZ")

write.csv(df_b2,  "Output/df_b2.csv", row.names = F)

# write this data out
df_b3 <- df_b2 %>%
         gather(var, value, -TAZ, -areaType, -year) %>%
         # filter(!is.na(year)) %>%
         mutate(id = paste(year, var, sep = "_")) %>%
         select(-var, -year) %>%
         spread(id, value) %>%
         replace(is.na(.), 0) %>%
         select(-NA_EMP, -NA_HH)

write.csv(df_b3,  "Output/Regional_Model_FLUAM_SE_data.csv", row.names = F)

#------------------------------------------------------------------------------- 





