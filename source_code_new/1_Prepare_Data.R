# Reads input data files
#-------------------------------------------------------------------------------
# Read CTL parameters
# df_ctl    <- read.csv(ctl_file, stringsAsFactors = FALSE)
# ctl       <- setNames(as.list(df_ctl$Value), df_ctl$Key)

# function to extract residential deciles
getResDecile <- function(df_ctl){
  resDecile <- df_ctl %>% 
    filter(Type == 'resAccessCoef') %>%
    mutate(Value = as.numeric(Value)) %>%
    pull(Value)
  
  return(resDecile)
}

# function to extract non-residential deciles
getNonResDecile <- function(df_ctl){
  nonResDecile <- df_ctl %>% 
               filter(Type == 'nonRAccessCoef') %>%
               mutate(Value = as.numeric(Value)) %>%
               pull(Value)   
  
  return(nonResDecile)
}

# function to extract density deciles
getDensityDecile <- function(df_ctl){
  resDenDecile <- df_ctl %>% 
               filter(Type == 'resDenCoef') %>%
               mutate(Value = as.numeric(Value)) %>%
               pull(Value)  
  
  return(resDenDecile)
}

# function to get FRATAR constants by Area Type
getFratarConst <- function(df_ctl){
  fratEMPFact <- df_ctl %>% 
               filter(Type == 'fratEmpFact') %>%
               mutate(Value = as.numeric(Value)) %>%
               pull(Value)
  return(fratEMPFact)
}

#-------------------------------------------------------------------------------
# read parcel data based zonal data (same for all years, except HH, EMP)
loadSEData <- function(taz_pd_file, year){
  
  # load base file
  # taz_pd_file <- "Input/base_data/2015_TAZ_data_based_on_ParcelData.xlsx"
  df_pd       <- read.xlsx(taz_pd_file, sheet = "base_data")
  
  if(year != 2015) {
    
    SE_file <- paste0("Output/",year, "_FLUAM_Output.xlsx") 
    
    df_se   <- read.xlsx(SE_file, sheet = "TAZ_Data") %>%
      # select(TAZ,  housing = HHTotal, employment = EmpTotal)
      select(TAZ,  housing, employment)
    
    # Overwrite base values with computed values
    df_taz <- df_pd %>% 
      select(-housing, -employment) %>%
      left_join(df_se, by = "TAZ")
    
    # Res / Non-Res developed land remains same for all years (something to address in the next version)
    # There is a way to convert under redevelopment but still that conversion rate 
    # remains same between the years
  } else{
    df_taz <- df_pd
  }
  
  return(df_taz)
}


#-------------------------------------------------------------------------------
# Function to load skims
loadSkim <- function(skim_year, max_taz) {
  skim_file <- paste0("Input/networks_skims/", skim_year, "-LOAD_BIGDUMP.TXT")
  dt_skim <- fread(skim_file)
  setnames(dt_skim, c("I", "J", "value"))
  
  # keep only II skim
  if(max(dt_skim$I) | max(dt_skim$J) > max_taz){
    dt_skim <- dt_skim[I <= max_taz & J <= max_taz & value > 0 , ]
  }
  
  return(dt_skim)
}

#-------------------------------------------------------------------------------
# Read network link data 
loadLinks <- function(year){
  link_file <- paste0("Input/networks_skims/d_Links_",year, "_Rev.txt")
  dt_link <- fread(link_file)
  dt_link <- dt_link[ , list(A = V1, B = V2, FacType = V8)]
  
  return(dt_link)
}

# Read network node data
loadNodes <- function(year){
  node_file <- paste0("Input/networks_skims/d_Nodes_",year, "_Rev.txt")
  dt_node <- fread(node_file)
  setnames(dt_node, c("N", "X", "Y"))
}

#-------------------------------------------------------------------------------
# Read Growth Center File
getGrowthTarget <- function(gc_file, useMPO_Controls, growth_year){
  # Switch between MPO and BEBR data
  if(useMPO_Controls){ 
    sheetName <- "MPO_Increment"
  } else {
    sheetName <- "BEBR_Increments"
  }
  df_gc   <- read.xlsx(gc_file, sheet = sheetName)
  
  # Select (assumes MPO data is also like this)
  df_gc <- df_gc %>% 
    select(County, paste(growth_year, "HH", sep ="."), paste(growth_year, "EMP", sep ="."))
  
  colnames(df_gc) <- c("growthCenter", "Control_HH", "Control_EMP")
  
  return(df_gc)
}

#-------------------------------------------------------------------------------
# Read density thresholds
# # read density constraints File
getDensityThresholds <- function(dc_file){
  dt_dc <- fread(dc_file)
  
  df_dc <- dt_dc[, c("TAZ", "housingDensityConstraint", "employmentDensityConstraint")] %>%
           setDF()
  
  return(df_dc)
}

# Overwrite TAZ densities
updateDensityThresholds <- function(df_taz, df_dc){
  df_taz <- df_taz %>% 
            # select(-housingDensityConstraint, -employmentDensityConstraint) %>%
            left_join(df_dc, by = "TAZ") 
  
  return(df_taz)
  
}

#-------------------------------------------------------------------------------
# Read DRI Information
getDRIs <- function(DRI_file, curr_year, next_year, runType, global_Flag){
    
    # default (runType = 1)
    prev_DRI_year  = curr_year
    next_DRI_year  = next_year
        
    if(runType == -1){
      prev_DRI_year  = next_year
      next_DRI_year  = curr_year
    } 
  
    # Supplied includes current year data and thus compute increment
    if(global_Flag == 0){
      
      df_DRI   <- read.xlsx(DRI_file, sheet = "DRIs")
      
      if(curr_year == 2015){
        
        df_DRI <- df_DRI %>%
                mutate(DRI_Housing = get(paste(next_year, "DRI_HH", sep =".")) -
                                     `2015.Parcel_HH`,
                       DRI_Employment = get(paste(next_year, "DRI_EMP", sep =".")) - 
                                        `2015.Parcel_EMP`
                       ) %>%
                select(TAZ, DRI_Housing, DRI_Employment)
        
      } else {
        df_DRI <- df_DRI %>%
                mutate(DRI_Housing = get(paste(next_DRI_year, "DRI_HH", sep =".")) -
                                     get(paste(prev_DRI_year, "DRI_HH", sep =".")),
                       DRI_Employment = get(paste(next_DRI_year , "DRI_EMP", sep =".")) - 
                                        get(paste(prev_DRI_year, "DRI_EMP", sep ="."))
                       ) %>%
                select(TAZ, DRI_Housing, DRI_Employment)     

      }
      
    } else{
      # Supplied are increments and use the deltas directly
      df_DRI   <- read.xlsx(DRI_file, sheet = "DRI_Increment")
      
      df_DRI <- df_DRI %>%
             select(TAZ, 
                    DRI_Housing = paste(next_DRI_year, "DRI_HH", sep ="."), 
                    DRI_Employment = paste(next_DRI_year, "DRI_EMP", sep ="."))
      
      # colnames(df_DRI) <- c("TAZ", "DRI_Housing", "DRI_Employment")
    } 
    
  return(df_DRI)
}

# Append DRI
appendDRI <- function(df_taz, df_DRI){
  df_taz <- df_taz %>% 
    left_join(df_DRI, by = "TAZ") %>%
    mutate(DRI_Housing = ifelse(is.na(DRI_Housing), 0, DRI_Housing),
           DRI_Employment = ifelse(is.na(DRI_Employment), 0, DRI_Employment))
  
  return(df_taz)
  
}

