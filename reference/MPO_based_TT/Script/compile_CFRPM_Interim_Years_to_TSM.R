# TItle: MPO model data Extractor

# About: 
# Tool is used to extract MPO model data from all available models

# Usage:
# MPO_Data > SE_Data           : All MPO data is dumped here
# "About_these_datasets.xlsx"  : This file lists the information about the MPO 
#                                data files, their assosiation to Models and 
#                                what data and fields needs to be extracted..
# Author: Amar Sarvepalli 
# Data  : 07-17-2019
#------------------------------------------------------------------------------- 
# User Settings
#------------------------------------------------------------------------------- 
library(tidyverse)
library(foreign)
library(openxlsx)

# List of MPO data files
# se_dir  <- "Input/CFRPM"

# zonal crosswalk (MPO -> Legacy, County)
# mpo_legacy_cw <- "crosswalk/MPO_to_Legacy_v2.dbf"
# mpo_legacy_cw <- "crosswalk/MPO_Legacy_NextGen_v2.dbf"
# 
# # output
# excel_out    <- "outputs/FL_MPO_SE_Data.xlsx"

start_time  <- Sys.time()
#------------------------------------------------------------------------------- 
# create a model info table
# about_SE_files <- "About_these_datasets.xlsx"
about          <- openxlsx::read.xlsx(paste(se_dir, about_SE_files, sep = "/"))

# remove files that are not in use
about_file_usage <- about %>% 
                    filter(File_Use > 0) %>%
                    pull(File_Name) 

# SE data 
files <- list.files(se_dir,recursive = TRUE)

files <- files[files %in% about_file_usage]
# Notes: (why can't just use the listed files in "about" instead of searching for all files ?)

# Show listed files that doesn't exist
about_file_usage[!(about_file_usage %in% files)]

#------------------------------------------------------------------------------- 
# A process to identify DU, POP, EMP, HM, OCC, GQ, SCH and COL fields
getFieldList <- function(dataframe, str_field){
  dataframe %>%
    pull(str_field) %>%
    paste(collapse = ",") %>%
    str_split(",") %>%
    unlist() %>%
    str_trim() %>%
    unique() 
}

# List of all fields
hp_listed_fields <- about %>% 
  getFieldList ("POP_Fields")

emp_listed_fields <- about %>% 
  getFieldList ("EMP_Fields")

enr_listed_fields <- about %>% 
  getFieldList ("EDU_Fields")

vis_listed_fields <- about %>% 
  getFieldList ("VIS_Fields")

gq_listed_fields <- about %>% 
  getFieldList ("GQ_Fields")

# Separate fields by sub-types
hh_key  <- hp_listed_fields[grep("DU|UNIT|HOME|HH|hh", hp_listed_fields)]
pop_key <- hp_listed_fields[grep("POP|Pop|pop|MALE", hp_listed_fields)]
emp_key <- emp_listed_fields[grep("TOT_EMP|TOTEMP|EMPTOT|EMP_TOT|emp_total|Emp|tot_emp",
                                  emp_listed_fields)]
sch_key <- enr_listed_fields[grep("K12|Kto8|9to12|9_12|K_8|SCHENR|SCHOOL",
                                     enr_listed_fields)]
col_key <- enr_listed_fields[grep("college|UNIVERSITY|POST|HIEDUC", 
                                     enr_listed_fields)]
hmr_key <- vis_listed_fields[!grepl("OCC|NA",vis_listed_fields)]
hmo_key <- vis_listed_fields[grep("OCC|HM_POC",vis_listed_fields)]
gq_key  <- gq_listed_fields[!grepl("NA",gq_listed_fields)]

#------------------------------------------------------------------------------- 
# Get file extension 
for (f in 1:length(files)){
  ext <- strsplit(files[f], "\\.") [[1]][2]
  file_name <- paste(se_dir, files[f], sep = "/")
  
  # TODO: use switch 
  # identify extension and read the data      
  if(ext %in% c("dbf", "DBF")) {df <- read.dbf(file_name)}
  if(ext %in% c("csv", "CSV")) {df <- read.csv(file_name)}
  if(ext %in% c("xlsx")) {df <- read.xlsx(file_name)}
  
  # Get information about this file
  about_file <- about %>% filter(File_Name == files[f])
  
  # Get model data year
  # year_field  <- str_trim(str_split(about_file["Year"], ",")[[1]])
  #-----------------------------------------------------------------------------
  # special case for SERPM Data where EMP, EDU, VIS data is at MAZ level
  taz_field  <- str_trim(str_split(about_file["TAZ_Fields"], ",")[[1]])
  
  if(length(taz_field) > 1){
    taz_field <- "TAZ"
    df <- df %>% 
      group_by(TAZ) %>%
      summarise_all(funs(sum))
    
  }
  
  #-----------------------------------------------------------------------------
  # Get POP_data
  if(!is.na(about_file["POP_Use"]) & about_file["POP_Use"] == 1){
    
    # Get list of fields to use from data
    pop_fields <- str_trim(str_split(about_file["POP_Fields"], ",")[[1]])
    # taz_field  <- str_trim(str_split(about_file["TAZ_Fields"], ",")[[1]])
    
    # Get data
    df_pop <- df %>% 
              select(taz_field, pop_fields) %>%
              mutate(TOT_DU  = rowSums(select(., one_of(hh_key))),
                     TOT_POP = rowSums(select(., one_of(pop_key))),
                     Model   = unlist(about_file["Model"][1]),
                     Year    = as.integer(unlist(about_file["Year"][1]))) %>%
              select(Model, Year, TAZ = taz_field, TOT_DU, TOT_POP)
    
    # transpose
    df_pop <- df_pop %>%
              gather(field, value, -TAZ, -Model, -Year)
  # }
  } else {
    df_pop <- data.frame(Model = c("Dummy","Dummy"),
                         Year  = c(0,0),
                         TAZ   = c(0,0),
                         field = c("TOT_DU", "TOT_POP"),
                         value = c(0,0))
  }
  
  #-----------------------------------------------------------------------------  
  # Get EMP data
  if(!is.na(about_file["EMP_Use"]) & about_file["EMP_Use"] == 1){
    
    # Get list of fields to use from data
    emp_fields <- str_trim(str_split(about_file["EMP_Fields"], ",")[[1]])
    # taz_field  <- str_trim(str_split(about_file["TAZ_Fields"], ",")[[1]])
    
    # Get data
    df_emp <- df %>% 
              select(taz_field, emp_fields) %>%
              mutate(TOT_EMP  = rowSums(select(., one_of(emp_key))),
                     Model    = unlist(about_file["Model"][1]),
                     Year    = as.integer(unlist(about_file["Year"][1]))) %>%
              select(Model, Year, TAZ = taz_field, TOT_EMP)
    
    # transpose
    df_emp <- df_emp %>%
              gather(field, value, -TAZ, -Model, -Year)
    
    # Append data
    # df_temp <- rbind(df_pop, df_emp)
  } else {
    df_emp <- data.frame(Model = c("Dummy"),
                         Year  = c(0),
                         TAZ   = c(0),
                         field = c("TOT_EMP"),
                         value = c(0))
  }
  
  #-----------------------------------------------------------------------------
  # Get Enrollment data
  if(!is.na(about_file["EDU_Use"]) & about_file["EDU_Use"] == 1){
    
    # Get list of fields to use from data
    enr_fields <- str_trim(str_split(about_file["EDU_Fields"], ",")[[1]])
    # taz_field  <- str_trim(str_split(about_file["TAZ_Fields"], ",")[[1]])
    
    # Get data
    df_enr <- df %>% 
              select(taz_field, enr_fields) %>%
              mutate(TOT_SCH  = rowSums(select(., one_of(sch_key))),
                     TOT_COL  = rowSums(select(., one_of(col_key))),
                     Model    = unlist(about_file["Model"][1]),
                     Year     = as.integer(unlist(about_file["Year"][1]))) %>%
              select(Model, Year, TAZ = taz_field, TOT_SCH, TOT_COL)
    
    # transpose
    df_enr <- df_enr %>%
              gather(field, value, -TAZ, -Model, -Year)
    
    # Append data
    # df_temp <- rbind(df_temp, df_enr)
  } else {
    df_enr <- data.frame(Model = c("Dummy","Dummy"),
                         Year  = c(0,0),
                         TAZ = c(0,0),
                         field = c("TOT_SCH", "TOT_COL"),
                         value = c(0,0))
  }
  
  
  #-----------------------------------------------------------------------------
  # Get Visitor data
  if(!is.na(about_file["VIS_Use"]) & about_file["VIS_Use"] == 1){
    
    # Get list of fields to use from data
    vis_fields <- str_trim(str_split(about_file["VIS_Fields"], ",")[[1]])
    # taz_field  <- str_trim(str_split(about_file["TAZ_Fields"], ",")[[1]])
    
    # Get data
    df_vis <- df %>% 
              select(taz_field, vis_fields) %>%
              mutate(HM_ROOMS = rowSums(select(., one_of(hmr_key))),
                     HM_OCC   = rowSums(select(., one_of(hmo_key))),
                     Model    = unlist(about_file["Model"][1]),
                     Year     = as.integer(unlist(about_file["Year"][1]))) %>%
              select(Model, Year, TAZ = taz_field, HM_ROOMS, HM_OCC)
    
    # transpose
    df_vis <- df_vis %>%
              gather(field, value, -TAZ, -Model, -Year)
    
    # Append data
    # df_temp <- rbind(df_temp, df_vis)
  } else {
    df_vis <- data.frame(Model = c("Dummy","Dummy"),
                         Year  = c(0, 0),
                         TAZ   = c(0,0),
                         field = c("HM_ROOMS", "HM_OCC"),
                         value = c(0,0))
  }
  #-----------------------------------------------------------------------------  
  # Get Group Quarter data
  if(!is.na(about_file["GQ_Use"]) & about_file["GQ_Use"] == 1){
    
    # Get list of fields to use from data
    gq_fields <- str_trim(str_split(about_file["GQ_Fields"], ",")[[1]])
    # taz_field  <- str_trim(str_split(about_file["TAZ_Fields"], ",")[[1]])
    
    # Get data
    df_gq <- df %>% 
              select(taz_field, gq_fields) %>%
              mutate(TOT_GQ = rowSums(select(., one_of(gq_key))),
                     Model  = unlist(about_file["Model"][1]),
                     Year   = as.integer(unlist(about_file["Year"][1]))) %>%
              select(Model, Year, TAZ = taz_field, TOT_GQ)
    
    # transpose
    df_gq <- df_gq %>%
              gather(field, value, -TAZ, -Model, -Year)
    
    # Append data
    # df_temp <- rbind(df_temp, df_gq)
  } else {
    df_gq <- data.frame(Model = c("Dummy"),
                        Year  = c(0),
                        TAZ   = c(0),
                        field = c("TOT_GQ"),
                        value = c(0))
  }   
  
  df_temp <- rbind(df_pop, df_emp, df_enr, df_vis, df_gq)
  
  #-----------------------------------------------------------------------------
  # Append all models
  if(f == 1){
    df_all <- df_temp
  } else {
    df_all <- rbind(df_all, df_temp)
  }
  
}
#------------------------------------------------------------------------------- 
# Transpose 
df_all <- df_all %>%
            group_by(TAZ, Model, Year, field) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            spread(field, value) %>%
            replace(is.na(.),0) %>%
            filter(Model != "Dummy" | TAZ != 0)

# select variables to keep
df_mpo <- df_all %>%
          select(TAZ, Model, Year, TOT_DU, TOT_POP, TOT_EMP)

#------------------------------------------------------------------------------- 
# Append Legacy, County zones
#------------------------------------------------------------------------------- 
# Read two files
# df_cw  <- read.dbf(mpo_legacy_cw, as.is = T)
# 
# # recode model names
# df_cw <- df_cw %>% 
#          filter(is.na(preference)) %>%
#          filter(Model != "Central_v7") %>%
#          mutate(Model = ifelse(Model == "nonMPO", "non-MPO", Model),
#                 Model = ifelse(Model == "TBRPM", "TampaBay", Model)) %>%
#          select(TAZ = N, Model, TSM_Legacy, TSM_NextGen = TSM_NextGe, County = COUNTY_NAM)
#          

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
# Interpolate Data
#------------------------------------------------------------------------------- 
# Get marginals by model
df_mpo %>% getModelSummary() %>% View()

# Interpolate 2015 and 2045 data
df_mpo2 <- df_mpo %>%
          filter(TAZ > 0) %>%
          gather(var, value, -TAZ, -Model, -Year) %>%
          spread(Year, value) 

df_itp <- df_mpo2 %>%   
          mutate(`2050` =  interpolate(`2040`, `2045`, 
                                       2040, 2045, 2050),
                 `2050` =  ifelse(`2050` < 0, round(`2045`, 0), `2050`))

write.csv(df_itp, "Output/cfrpm_interim_years.csv", row.names = F)

#------------------------------------------------------------------------------- 


