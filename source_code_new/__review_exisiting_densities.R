library(openxlsx)
library(tidyverse)

# setwd("M:/Models/StateWide/TSM_Legacy/workdir/taz_data_review")
parcel_data  <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
parcel_sheet <- "base_data"

mpo_data     <- "reference/FL_MPO_SE_Data.xlsx"
mpo_sheet    <- "Processed_MPO"
#-------------------------------------------------------------------------------
# Get TAZ HH and EMP 
#-------------------------------------------------------------------------------
# read parcel data based hh, emp
# df_p <- read.xlsx(parcel_data, sheet = parcel_sheet)

df_p2 <- df_p %>%
         mutate(res_density = ifelse( resDeveloped > 0,
                                 round( housing /  resDeveloped, 3), 
                                 0), 
                nonRes_density = ifelse( nonresDeveloped >0 , 
                                 round(  employment/ nonresDeveloped, 3),
                                 0),
                metro = case_when(County %in% c("Hillsborough", "Pinellas") ~ "Tampa",
                                  County %in% c("Duval", "St. Johns") ~ "Jacksonville",
                                  County %in% c("Orange", "Seminole") ~ "Orlando",
                                  County %in% c("Miami-Dade", "Broward", "Palm Beach") ~ "SouthFlorida",
                                  TRUE ~ "Other")) %>%
         select(TSM_TAZ, County, metro, HH = housing, EMP = employment, 
                res_area = resDeveloped,  nonRes_area = nonresDeveloped, 
                res_density, nonRes_density)


write.csv(df_p2, "Output/parcel_data_densities.csv", row.names = F)

# Compute densities for zones with at least an acre or more
resDen <- df_p2 %>%
         filter(metro != "Other" & res_density > 0 & res_area > 1) %>%
         group_by(metro, County) %>%
         summarise_at(vars(res_density), list(min = min, max = max, mean = mean))
  
nonResDen <- df_p2 %>%
         filter(metro != "Other" & nonRes_density > 0 & nonRes_area > 1) %>%
         group_by(metro, County) %>%
         summarise_at(vars(nonRes_density), list(min = min, max = max, mean = mean))

write.csv(rbind(resDen, nonResDen), "Output/metro_density.csv", row.names = F)


#-------------------------------------------------------------------------------
# Update with New Density Constraints  
#-------------------------------------------------------------------------------
# New density constraints
# path_dc_data <- "M:/Models/StateWide/TSM_Legacy/workdir/taz_data_review"
# new_den_const  <- "Input/base_data/gen max densitys by area type.xlsx"
# denConst_sheet <- "reformat"

old_constraints <- "reference/FLUAM_2_1_DensityConstraints.csv"

# path_dc_data2 <- "M:/Models/StateWide/TSM_Legacy/FLUAM/Input/base_data"
new_den_const  <- "reference/gen max densitys by area type.xlsx"
denConst_sheet2 <- "reformat2"

# TAZ data
# path_taz_data <- "M:/Models/StateWide/TSM_Legacy/FLUAM/Input/base_data"
taz_file  <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
taz_sheet <- "base_data"

# Read new max densities
df_dc  <- read.xlsx(new_den_const, sheet = denConst_sheet2) %>% 
          select(-at, -County)
    
default_const <- df_dc %>% 
        filter(growthCenter == 0) %>% 
        select(-growthCenter)

newconst <- df_dc %>% 
            filter(growthCenter != 0) %>%
            mutate(EMP = ifelse(is.na(EMP), -1, EMP),
                   HH = ifelse(is.na(HH), -1, HH))
      
# Get zonal data to update
df_taz <- read.xlsx(taz_file, sheet = taz_sheet)

# Read old contraints
df_old <- read.csv(old_constraints) %>%
          rename(housingDensityConstraint = `Res Density`,  employmentDensityConstraint = `Non-Res Density`)

df_taz <- df_taz %>%
          left_join(df_old, by = "TAZ")
          select(TAZ, growthCenter, "areaType" , "housingDensityConstraint",  "employmentDensityConstraint")

# Update density data
df_taz2 <- df_taz %>%
          left_join(default_const , by = "areaType") %>%
          rename(new_dc_hh = HH, new_dc_emp = EMP) %>%
          left_join(newconst , by = c("growthCenter", "areaType")) %>%
          mutate(new_dc_hh = ifelse(is.na(HH), new_dc_hh, HH),
                 new_dc_emp = ifelse(is.na(EMP), new_dc_emp, EMP)) %>%
          mutate(new_dc_hh = ifelse(new_dc_hh == -1, housingDensityConstraint, new_dc_hh),
                 new_dc_emp = ifelse(new_dc_emp == -1, employmentDensityConstraint, new_dc_emp)) %>%
          select(TAZ, growthCenter, areaType , 
                 housingDensityConstraint = new_dc_hh, 
                 employmentDensityConstraint = new_dc_emp)

write.csv(df_taz2, "Input/base_data/new_density_constraints_at_TAZ.csv", row.names = F)

#-------------------------------------------------------------------------------
# For 6 problem counties - use MPO max thresholds
#-------------------------------------------------------------------------------
df_dc <- read.csv("Input/base_data/new_density_constraints_at_TAZ.csv")
df_p  <- read.xlsx(parcel_data, sheet = parcel_sheet) %>%
         select(TAZ, resDeveloped)

mpo_data     <- "reference/FL_MPO_SE_Data.xlsx"
mpo_sheet    <- "Processed_MPO"

# read mpo data based hh, emp
df_m <- read.xlsx(mpo_data, sheet = mpo_sheet)

# Manually check and keep the zones that are in more than one model
# Delete these duplicates
delete_taz <- c( "Central 2390",
    "non-MPO 5620",
    "non-MPO 3095",
    "D1 4242",
    "D1 4297",
    "D1 4300")

# Get hh, emp
df_m2 <- df_m %>%
        filter(var != "TOT_POP") %>%
        group_by(TSM_Legacy, Model, var) %>%
        summarise(data = sum(`2045`)) %>%
        spread(var, data) %>%
        rename(MPO_HH = TOT_DU, MPO_EMP = TOT_EMP) %>% 
        ungroup() %>%
          mutate(del_taz = paste(Model, TSM_Legacy)) %>%
          filter(!(del_taz %in% delete_taz)) 

problem_counties <- c(6, 50, 52, 60)

# Join both parcel data and mpo data (side by side)
df_pm <- df_p %>% 
         left_join(df_dc, by = "TAZ") %>%
         left_join(df_m2, by = c("TAZ" = "TSM_Legacy")) %>%
         mutate(max_dc_HH = ifelse(!is.na(MPO_HH) & resDeveloped > 0 & growthCenter %in% problem_counties, 
                                   MPO_HH / resDeveloped, housingDensityConstraint),
                max_dc_HH = pmax(max_dc_HH, housingDensityConstraint)) %>% 
  rename(old_density = housingDensityConstraint, housingDensityConstraint = max_dc_HH) %>% 
  select(TAZ, growthCenter , areaType, housingDensityConstraint, employmentDensityConstraint )

# check
df_pm %>% filter(growthCenter %in% problem_counties) %>% View()
         
write.csv(df_pm, "Input/base_data/new_density_constraints_at_TAZ_v2.csv", row.names = F)









