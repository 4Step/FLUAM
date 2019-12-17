
#-------------------------------------------------------------------------------
# Reads input data files
#-------------------------------------------------------------------------------
# Read CTL parameters
# ctl_file  <- "Input/FLUAM_Properties.csv"
df_ctl    <- read.csv(ctl_file, stringsAsFactors = FALSE)
ctl       <- setNames(as.list(df_ctl$Value), df_ctl$Key)

# function to get FRATAR constants by Area Type
fratEMPFact <- df_ctl %>% 
             filter(Type == 'fratEmpFact') %>%
             mutate(Value = as.numeric(Value)) %>%
             pull(Value)

fratHHFact <- df_ctl %>% 
               filter(Type == 'fratHHFact') %>%
               mutate(Value = as.numeric(Value)) %>%
               pull(Value)
#------------------------------------------------------------------------------- 
# IF we skip the first two steps then read thier ouput
if(skipInterpolation){
  df_b2 <- read.csv("Output/df_b2.csv")
}

# Compute Trips 
df_taz <- df_b2 %>%
mutate(boolUrbanArea = ifelse(areaType == 2, 1, 0),
       Trips =	as.numeric(ctl$fratConstant) + 
         fratEMPFact[areaType] * EMP  +
         fratHHFact[areaType] * HH  + 
         as.numeric(ctl$fratUrbanArea)* boolUrbanArea
)

# Vectorize "Column Format" Trips by Year
df_taz2 <- df_taz %>%
           mutate(year = ifelse(is.na(year), 0, year)) %>%
           select(-EMP, -HH, -boolUrbanArea) %>%
           spread(year, Trips) %>%
           select(-`0`) %>%
           replace(is.na(.),0)

# Compute growth factors
df_taz2 <- df_taz2 %>%
           mutate(g2020 = ifelse( `2015` > 0 , `2020` / `2015`, 1),
                  g2025 = ifelse( `2020` > 0 , `2025` / `2020`, 1),
                  g2030 = ifelse( `2025` > 0 , `2030` / `2025`, 1),
                  g2035 = ifelse( `2030` > 0 , `2035` / `2030`, 1),
                  g2040 = ifelse( `2035` > 0 , `2040` / `2035`, 1),
                  g2045 = ifelse( `2040` > 0 , `2045` / `2040`, 1),
                  g2050 = ifelse( `2045` > 0 , `2050` / `2045`, 1)) 

# Save sumamry files for IPF
df_taz2_tripEnds <- df_taz2 %>% 
            select(TAZ, `2015`, `2020`, `2025`, `2030`, `2035`, `2040`, `2045`, `2050`)

df_taz2_gcFac <- df_taz2 %>% 
            select(TAZ, g2020, g2025, g2030, g2035, g2040, g2045, g2050)


df_taz2 <- df_taz2 %>%
           select(TAZ, g2020, g2025, g2030, g2035, g2040, g2045, g2050) %>%
           gather(year, growth, -TAZ) %>%
           mutate(year = as.numeric(gsub("g","", year)),
                   i = 1,
                   fo = "FO",
                   ftaz = stringr::str_pad(TAZ, width = 4, side = "left", pad = "0"),
                   fgc = stringr::str_pad(round((growth + 0.005) * 100, 0), width = 7, side = "left", pad = "0"),
                   id = paste(fo, ftaz, i, fgc, sep = " ")) %>%
           select(year, id) 


#-------------------------------------------------------------------------------
# Get External Stn Growth Factors
df_ext     <- read.xlsx(ext_Stn_file) 
excel_data <- list()
years      <- unique(df_taz2$year)

# Add base year data
excel_data[["2015"]] <- df_taz %>%
                        filter(year == 2015)


for(y in 1:length(years)){
  
  # To write tranplan based FRATAR input files
  if(writeTrnPLnFratarInput){
    fratar_file <- paste0("Output/",years[y],"_FratarInput.txt") 
    
    df_ext_y <- df_ext%>%
                select(id = paste0("Year.",years[y]))
    
    df_taz_y <- df_taz2 %>%
                filter(year == years[y]) %>%
                select(id)
    
    df_fratar <- rbind(df_taz_y, df_ext_y)
      
    # Write FRATAR File
    write.table(df_fratar, fratar_file, sep = " ", quote = FALSE,
                row.names = F, col.names = F)
    }
  
  # Not required by for reference
  df_taz_trip <- df_taz %>%
                 filter(year == years[y]) 
  
  excel_data[[as.character(years[y])]] <- df_taz_trip

 
}


# Append trip ends
excel_data[["tripGen"]]   <- df_taz2_tripEnds
excel_data[["growthFac"]] <- df_taz2_gcFac


out_file <- paste0("Output/summary_MPO_Output.xlsx")

# write.csv(df_taz_trip, paste0("Output/",years[y],"_Output.csv"))
write.xlsx(excel_data, out_file) 

#------------------------------------------------------------------------------- 


