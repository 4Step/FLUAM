
# Read 2020 data (de-allocated from 2025)
df_2020  <- read.xlsx("Output/2020_FLUAM_Output.xlsx", sheet = "TAZ_Data") %>%
            select(TAZ, EmpTotal = employment, HHTotal = housing)

# Read 2015 data from
df_2015   <- read.xlsx(taz_pd_file, sheet = "base_data") %>%
             select(TAZ, areaType, housing, employment)

df_15to20 <- df_2015 %>% left_join(df_2020, by = "TAZ") 

df_taz6   <- computeFRATAR(df_15to20, 1)

print("Writing 2020 Outputs...")
fratar_file <- paste0("Output/2020_FratarInput.txt")

writeFRATARInput(df_taz6, fratar_file, ext_Stn_file, 2020)

