# Main code
# Prepares data
# Computes accessibility measures

#-------------------------------------------------------------------------------
# Allocate or Deallocate
#-------------------------------------------------------------------------------
# Know the direction to increment or decrement
runType   <- RunDir[i]
if(runType == 1){
    curr_year    <- Years[i-1]
    next_year    <- Years[i]
    growth_year  <- Years[i]
} else{
    curr_year    <- Years[i+1]
    next_year    <- Years[i]
    growth_year  <- Years[i+1]
}

#-------------------------------------------------------------------------------
# Read Input Data
#-------------------------------------------------------------------------------
print("Reading Inputs (skims)...")

# Read control file
df_ctl    <- read.csv(ctl_file, stringsAsFactors = FALSE)
ctl       <- setNames(as.list(df_ctl$Value), df_ctl$Key)

# Read input files, skims, control totals
source("source_code_new/1_Prepare_Data.R")

# get decile constants in arrays
resDecile    <- getResDecile(df_ctl)
nonResDecile <- getNonResDecile(df_ctl)
resDenDecile <- getDensityDecile(df_ctl)
fratEMPFact  <- getFratarConst(df_ctl)
  
# Load SE data
df_taz  <- loadSEData(taz_pd_file, curr_year)
max_taz <- max(df_taz$TAZ)

# list of field names
taz_fields <- colnames(df_taz)

# Read current & future year skims
dt_cskim <- loadSkim(curr_year, max_taz)
dt_nskim <- loadSkim(next_year, max_taz)

# Get future year links and nodes
dt_link <- loadLinks(next_year) 
dt_node <- loadNodes(next_year) 

# Get growth controls
df_gc <- getGrowthTarget(gc_file, useMPO_Controls, growth_year)

# Density Constraints
df_dc <- getDensityThresholds(dc_file) 

# Read DRI data
df_DRI  <- getDRIs(DRI_file, curr_year, next_year, runType, global_Flag)
   
# Update density constraints
df_taz <- updateDensityThresholds(df_taz, df_dc)

# append DRI 
df_taz <- appendDRI(df_taz, df_DRI)

#-------------------------------------------------------------------------------
# Land Consumption Summary 
#-------------------------------------------------------------------------------
source("source_code_new/9_Summary_Land_Consumption.R")
before  <- summariseLandByCategory(df_taz)

df_taz  <- recomputeAvailableShares(df_taz, Agri_res_noRes_Flag) 
before2 <- summariseLandByCategory(df_taz)

#-------------------------------------------------------------------------------
# Compute accessibilities
#-------------------------------------------------------------------------------
print("Computing Accessibilities...")

# Load Accessibility Function
source("source_code_new/2_Compute_Accessibilities.R")

# current year accessibility
cur_AdjTTimeWgtByHH_Emp <- computeAccessibility(dt_cskim, df_ctl, df_taz)
setnames(cur_AdjTTimeWgtByHH_Emp, c("TAZ","cur_AdjTTimeWgtByHH_Emp"))

# next year accessibility
avgAdjTTimeWgtByHH_Emp  <- computeAccessibility(dt_nskim, df_ctl, df_taz)

# Append 
setDT(df_taz)
setkey(df_taz, "TAZ")
setkey(avgAdjTTimeWgtByHH_Emp, "TAZ")
setkey(cur_AdjTTimeWgtByHH_Emp, "TAZ")

df_taz <- merge(df_taz, cur_AdjTTimeWgtByHH_Emp, by = "TAZ", all.x = T )
df_taz <- merge(df_taz, avgAdjTTimeWgtByHH_Emp, by = "TAZ", all.x = T )

# Scale accessibility
df_taz <- getScaledAccessibility(df_taz)

# Compute accessibility category
dt_taz2 <- df_taz[ , accessCategorical := computeCategorical(accessScaled), by = DOTDistrict]

#-------------------------------------------------------------------------------
# calculateDistToIntersections
#-------------------------------------------------------------------------------
print("Computing Distance to Closest Ramp...")
# Load intersection distance Function
source("source_code_new/3_Network_Distances.R")

# C++ function to compute distances and get nodes
sourceCpp("source_code_new/compute_distance.cpp")

# Compute nearest ramp distances
other_nodes <- getRampNodes(dt_link, dt_node, max_taz)
zone_nodes  <- getCentroids(dt_node, max_taz)
dt_taz2     <- getRampDistance(dt_taz2, zone_nodes, other_nodes)
  
#-------------------------------------------------------------------------------
# Agriculture Land Conversion & Calculate Land Consumption
#-------------------------------------------------------------------------------
print("Computing Agri.Land Conversion...")
source("source_code_new/3B_AgricultureLandConversion.R")
dt_taz2 <- convertAgriculture(dt_taz2, next_year, curr_year, rate, Agri_res_noRes_Flag)

print("Computing Land Conumption...")  
source("source_code_new/4_LandConspution_Variables.R")
includeDev <- FALSE  # adds existing developed land towards redevelopment
df_taz3 <- prepareLCVariables(dt_taz2, ctl, includeDev)

#-------------------------------------------------------------------------------
# Allocate HH & EMP 
#-------------------------------------------------------------------------------
print("Allocating Housing...")

source("source_code_new/5_HousingAllocation.R")
excludeDRI <- FALSE  # excludes DRI from CountyGrowth Target


ret        <- allocateHousing(df_taz3, df_gc, excludeDRI, includeDev)
df_taz4    <- ret$df_taz4
hhConverge <- ret$DRIHHbyGrowthCenter %>%
              select(growthCenter, Control_HH, HH_model, unMet_HH = diff, HH_Flag = converge)

print("Allocating Employment...")
source("source_code_new/6_EmploymentAllocation.R")
ret         <- allocateEmployment(df_taz4, df_gc, excludeDRI, includeDev)
df_taz4     <- ret$df_taz4
empConverge <- ret$DRIEMPbyGrowthCenter %>%
              select(growthCenter, Control_EMP, EMP_model, unMet_EMP = diff, EMP_Flag = converge)

#-------------------------------------------------------------------------------
# Recheck Available Land
#-------------------------------------------------------------------------------
# Get "unallocated" units as "new" targets
df_gc2 <- hhConverge %>% 
          left_join(empConverge, by = "growthCenter") %>%
          mutate(Control_HH = ifelse(unMet_HH > 100, unMet_HH, 0),
                 Control_EMP = ifelse(unMet_EMP > 100, unMet_EMP, 0)) %>%
          select(growthCenter, Control_HH, Control_EMP)
        
# Counties with "unmet" demand
# unallocatedCounties <- df_gc2 %>% filter(Control_HH > 0) %>% pull(growthCenter)
unmet_HH_Counties   <- df_gc2 %>% filter(Control_HH > 0) %>% pull(growthCenter)
unmet_EMP_Counties  <- df_gc2 %>% filter(Control_EMP > 0) %>% pull(growthCenter)

# Choose whether to re-run or not
if(length(unmet_HH_Counties) > 0 ) {
  reRun_HH_for_UnMetCounties   <- TRUE
} else{
  reRun_HH_for_UnMetCounties   <- FALSE
}

if(length(unmet_EMP_Counties) > 0 ) {
  reRun_EMP_for_UnMetCounties   <- TRUE
} else{
  reRun_EMP_for_UnMetCounties   <- FALSE
}


# Choose if we want to use "un-consumed" EMP land towards HH 
# This is applicable only if the EMP for the county is allocated but not HHs
useEMPLand_for_resDev <- FALSE 

if(useEMPLand_for_resDev){  
  print("Re-allocating Housing after employment..") 
  
  # compute allocated land and move any remaining land & agriculture land into
  source("source_code_new/8_Reallocate_Unconsumed_EMPLand.R")
  
  df_taz4a <- df_taz4 %>% 
              mutate(HHAllocated1 = HHAllocated, 
                     HHTotal1 = HHTotal)
  
  df_taz3b <- computeRemainingLand(df_taz4a, unallocatedCounties, FALSE)
  
} else{
  
  df_taz3b <- df_taz4 %>% 
              mutate(HHAllocated1 = HHAllocated, 
                     HHTotal1 = HHTotal,
                     EmpAllocated1 = EmpAllocated,
                     EmpTotal = EmpTotal)
}

#-------------------------------------------------------------------------------
# run Allocate "Unmet" demand towards "Redevlopment"
#-------------------------------------------------------------------------------
# Run this for only counties that are not converges 
# (in other words, reallocated demand to redevelopment zones)

# Allocated unmet EMP towards redevelopment
if(reRun_HH_for_UnMetCounties){
    
    print("Re-allocating 'Unmet' Housing towards redevelopment...") 
    excludeDRI <- TRUE  # excludes DRI from CountyGrowth Target
    includeDev <- TRUE  # adds existing developed land towards redevelopment
    
    df_taz3c   <- df_taz3b %>% 
                  select(colnames(df_taz3), HHAllocated1)
    
    df_taz3c    <- prepareLCVariables(df_taz3c, ctl, includeDev)
    
    ret        <- allocateHousing(df_taz3c, df_gc2, excludeDRI, includeDev)
    
    df_taz5    <- ret$df_taz4 %>%
                  mutate(HHTotal = pmax(0, HHAllocated1 + HHAllocated + housing))
    
    hhConverge2 <- ret$DRIHHbyGrowthCenter
} 

# Allocated unmet EMP towards redevelopment
if(reRun_EMP_for_UnMetCounties){
  
    excludeDRI <- TRUE  # excludes DRI from CountyGrowth Target
    includeDev <- TRUE  # adds existing developed land towards redevelopment
    
    print("Re-allocating 'Unmet' Employment towards redevelopment...")
    df_taz3     <- prepareLCVariables(dt_taz2, ctl, includeDev)
    
    ret         <- allocateEmployment(df_taz5, df_gc, excludeDRI, includeDev)
    df_taz5     <- ret$df_taz4
    empConverge2  <- ret$DRIEMPbyGrowthCenter
}

# Merge two versions together
if(reRun_HH_for_UnMetCounties | reRun_EMP_for_UnMetCounties){  
  
  # Append EMP fields from before
  empAllocatedFields <- colnames(df_taz4)
  hhAllocatedFields <- colnames(df_taz5)
  
  addFields <- empAllocatedFields[!(empAllocatedFields %in% hhAllocatedFields)]
  df_taz4b <- df_taz4 %>% select(TAZ, addFields)
  
  df_taz5 <- df_taz5 %>%
            left_join(df_taz4b, by = "TAZ")
  
  # Compute final land categories ()
  df_taz3d <- df_taz3b %>% 
              select(TAZ, empSpentLand, hhSpentLand, 
                     resAvailableAcres0,  nonresAvailableAcres0,
                     resAvailableAcres_net, nonresAvailableAcres_net,
                     resAvailableAcres1, nonresAvailableAcres1, extraAgriLand)
    
  df_taz5a <- df_taz5 %>%
              left_join(df_taz3d, by = "TAZ")
  
  df_taz5b <- computeNewAvailableLand(df_taz5a, Agri_res_noRes_Flag)

} else {
      df_taz5b <- df_taz3b
      hhConverge2 <- "Total HH Demand is met in hhConverge and so no reallocation is needed"
      empConverge2 <- "Total EMP Demand is met in empConverge and so no reallocation is needed"
}

#-------------------------------------------------------------------------------
# FRATAR
#-------------------------------------------------------------------------------
print("Computing Trips / Fratar Inputs...")
source("source_code_new/7_Prepare_Fratar_Inputs.R")
df_taz6 <- computeFRATAR(df_taz5b)

print("Writing Outputs...")
fratar_file <- paste0("Output/",next_year,"_FratarInput.txt")  
writeFRATARInput(df_taz6, fratar_file, ext_Stn_file, next_year)

df_taz7 <- df_taz6 %>%
           select(-housing, -employment) %>%
           rename(housing = HHTotal, employment = EmpTotal) %>%
           select(taz_fields)

#-------------------------------------------------------------------------------
# Land Consumption Summary 
#-------------------------------------------------------------------------------
after  <- summariseLandByCategory(df_taz7)

# compare state-wide
x1 <- before2$lc_state - before$lc_state
x2 <- after$lc_state - before2$lc_state
y <- rbind(before$lc_state, before2$lc_state, x1, after$lc_state,  x2) 

stateSum <- data.frame(Year = c(curr_year, curr_year,  "Adj_Available", next_year, "Diff"), y)

w1 <- before$lc_county
w2 <- after$lc_county 
x1 <- w2 - w1
names(w1) <- paste(curr_year, names(w1), sep = ".")
names(w2) <- paste(next_year, names(w2), sep = ".")
names(x1) <- paste("Diff", names(w2), sep = ".")
countySum <- cbind(w1, w2, x1) %>% 
             as.data.frame()
#-------------------------------------------------------------------------------
# Excel Output
#-------------------------------------------------------------------------------
out_file <- paste0("Output/",next_year,"_FLUAM_Output.xlsx")
# out_file <- paste0("Output/",next_year, "_", curr_year,"_FLUAM_Output.xlsx")
# exportFRATARTrips(df_taz4, df_taz6, taz_fields, hhConverge, empConverge, out_file)
# keep the standard input fields 

# Export data
excel_data <- list("TAZ_Data" = df_taz7,
                   "debug" = df_taz4,
                   "debug_reallocate" = df_taz6,
                   "hhConverge" = hhConverge, 
                   "empConverge" = empConverge,
                   "hhconverge2" = hhConverge2,
                   "empconverge2" = empConverge2,
                   "SummaryCounty" = countySum, 
                   "SummaryFL" = stateSum
                   )

write.xlsx(excel_data, out_file) 
#-------------------------------------------------------------------------------





