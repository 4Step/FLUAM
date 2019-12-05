# Update Parcel Data Land Categories
# From Justin's review of State Parks, 
# some 300 zones should be re-catagorized as undevelopable

wd <- "M:/Models/StateWide/TSM_Legacy/FLUAM"
state_park_zones_file <- "reference/FINAL_Conservation_TAZs.xlsx"

df_base  <-  read.xlsx(taz_pd_file, sheet = "base_data")
taz_list <- df_ext <- read.xlsx(state_park_zones_file) %>% pull(TAZ)


df_base <- df_base %>%
           mutate(flag = ifelse(TAZ %in% taz_list, 1, 0),
                  undevelopableAcres = ifelse(flag == 1, 
                                               undevelopableAcres + 
                                               resAvailableAcres +
                                               nonresAvailableAcres +
                                               AgriculturalAcres,
                                              undevelopableAcres),
                  resAvailableAcres = ifelse(flag == 1, 
                                              0,
                                              resAvailableAcres),
                  nonresAvailableAcres = ifelse(flag == 1, 
                                            0,
                                            nonresAvailableAcres),
                  AgriculturalAcres = ifelse(flag == 1, 
                                            0,
                                            AgriculturalAcres)
                  )


write.csv(df_base, "reference/reCategorized_landuse.csv", row.names = F)



