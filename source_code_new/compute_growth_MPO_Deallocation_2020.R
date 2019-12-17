
# Read 2020 data (de-allocated from 2025)
df_2020  <- read.xlsx("Output/2020_FLUAM_Output.xlsx", sheet = "TAZ_Data") %>%
            select(TAZ, growthCenter, EmpTotal = employment, HHTotal = housing)

# MPO 2015 data (this is different from Parcel Data)
df_2015_file <- "Input/base_data/2040_TAZ_data_based_on_MPO_Models.xlsx"

df_kfac <- read.csv(tripGen_facs)

df_2020 <- df_2020 %>%
           left_join(df_kfac, by = "growthCenter")

# Read 2015 data from
df_2015   <- read.xlsx(df_2015_file, 
                       sheet = "mpo_fluam_data") %>%
             select(TAZ, areaType, housing = `2015_HH`, employment = `2015_EMP`)		

df_15to20 <- df_2015 %>% left_join(df_2020, by = "TAZ") 

df_taz6   <- computeFRATAR(df_15to20, 1)

print("Writing 2020 Outputs...")
fratar_file <- paste0("Output/2020_FratarInput.txt")

# writeFRATARInput(df_taz6, fratar_file, ext_Stn_file, 2020)

