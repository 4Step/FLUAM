# Write out county level increments

# reformat HH, EMP by year
df_b2a <- df_b2 %>%
          select(-areaType) %>%
          mutate(year = ifelse(is.na(year), 0, year)) %>%
          gather(var, value, -TAZ, -year) %>%
          mutate(id = paste(year, var, sep = ".")) %>%
          select(-year, -var) %>%
          spread(id, value) %>%
          select(-`0.HH`, -`0.EMP`) %>%
          replace(is.na(.),0) 

sumFields <- colnames(df_b2a)[-1]
  
# Add County information
df_county <- df_p %>%
             select(TAZ, growthCenter) %>%
             left_join(df_b2a, by = "TAZ") %>%
             group_by(growthCenter) %>%
             summarise_at(sumFields, sum)

retainFields <- sumFields[-1:-2]

# Develop increments
df_county_increments <- df_county %>%
                         mutate(`2050.EMP` = `2050.EMP` - `2045.EMP`,
                                `2050.HH`  = `2050.HH`  - `2045.HH`,
                                `2045.EMP` = `2045.EMP` - `2040.EMP`,
                                `2045.HH`  = `2045.HH`  - `2040.HH`,
                                `2040.EMP` = `2040.EMP` - `2035.EMP`,
                                `2040.HH`  = `2040.HH`  - `2035.HH`,
                                `2035.EMP` = `2035.EMP` - `2030.EMP`,
                                `2035.HH`  = `2035.HH`  - `2030.HH`,
                                `2030.EMP` = `2030.EMP` - `2025.EMP`,
                                `2030.HH`  = `2030.HH`  - `2025.HH`,
                                `2025.EMP` = `2025.EMP` - `2020.EMP`,
                                `2025.HH`  = `2025.HH`  - `2020.HH`,
                                `2020.EMP` = `2020.EMP` - `2015.EMP`,
                                `2020.HH`  = `2020.HH`  - `2015.HH`)


df_county_increments %>% colSums()

df_county_increments <- df_county_increments %>%
                    select(growthCenter, retainFields)

write.csv(df_county_increments, "Output/MPO_County_Marginal_Growth.csv", row.names = F)


