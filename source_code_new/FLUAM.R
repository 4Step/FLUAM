# Main code
# Prepares data
# Computes accessibility measures

#-------------------------------------------------------------------------------
# Allocate or Deallocate
#-------------------------------------------------------------------------------
# Know the direction to increment or decrement
runType      <- sub_runs[i]
curr_year    <- sub_Years[i-1]
next_year    <- sub_Years[i]

if(runType == 1){
    growth_year  <- sub_Years[i]
} else{
    growth_year  <- sub_Years[i-1]
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
resDecile       <- getResDecile(df_ctl)
nonResDecile    <- getNonResDecile(df_ctl)
resDenDecile    <- getDensityDecile(df_ctl)
fratarConstant  <- getFratarConst(df_ctl)

fratEMPFact  <- fratarConstant$fratEMPFact
fratHHFact   <- fratarConstant$fratHHFact

# Load SE data
df_taz  <- loadSEData(taz_pd_file, curr_year)
max_taz <- max(df_taz$TAZ)


# if(debug_taz > 0) {
#   print("Line 46")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }

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

# if(debug_taz > 0) {
#   print("Line 81")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }
#-------------------------------------------------------------------------------
# Land Consumption Summary 
#-------------------------------------------------------------------------------
source("source_code_new/9_Summary_Land_Consumption.R")
before  <- summariseLandByCategory(df_taz)

df_taz  <- recomputeAvailableShares(df_taz, Agri_res_noRes_Flag) 
before2 <- summariseLandByCategory(df_taz)

# if(debug_taz > 0) {
#   print("Line 98")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }
#-------------------------------------------------------------------------------
# Compute accessibilities
#-------------------------------------------------------------------------------
print("Computing Accessibilities...")

# Load Accessibility Function
source("source_code_new/2_Compute_Accessibilities.R")

# if(debug_taz > 0) {
#   print("Line 114")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }

# current year accessibility
temp_taz <- copy(df_taz)
cur_AdjTTimeWgtByHH_Emp <- computeAccessibility(dt_cskim, df_ctl, temp_taz)
setnames(cur_AdjTTimeWgtByHH_Emp, c("TAZ","cur_AdjTTimeWgtByHH_Emp"))

# if(debug_taz > 0) {
#   print("Line 127")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }

# next year accessibility
temp_taz <- copy(df_taz)
avgAdjTTimeWgtByHH_Emp  <- computeAccessibility(dt_nskim, df_ctl, temp_taz)

# Append 
setDT(df_taz)
setkey(df_taz, "TAZ")
setkey(avgAdjTTimeWgtByHH_Emp, "TAZ")
setkey(cur_AdjTTimeWgtByHH_Emp, "TAZ")



df_taz <- merge(df_taz, cur_AdjTTimeWgtByHH_Emp, by = "TAZ", all.x = T )
df_taz <- merge(df_taz, avgAdjTTimeWgtByHH_Emp, by = "TAZ", all.x = T )
setkey(df_taz, NULL)

# if(debug_taz > 0) {
#   print("Line 128")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }

# Scale accessibility
df_taz <- getScaledAccessibility(df_taz)

# if(debug_taz > 0) {
#   print("Line 133")
#   df_taz  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }

# Compute accessibility category
dt_taz2 <- df_taz[ , accessCategorical := computeCategorical(accessScaled), by = DOTDistrict]

# if(debug_taz > 0) {
#   print("Line 172")
#   dt_taz2  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }
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

# if(debug_taz > 0) {
#   print("Line 195")
#   dt_taz2  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# }  
#-------------------------------------------------------------------------------
# Agriculture Land Conversion & Calculate Land Consumption
#-------------------------------------------------------------------------------
print("Computing Agri.Land Conversion...")
source("source_code_new/3B_AgricultureLandConversion.R")

# Don't run for deallocation (assume the 2040 base density / land is )
if(runType > 0){
  dt_taz2 <- convertAgriculture(dt_taz2, next_year, curr_year, rate, Agri_res_noRes_Flag)
}

if(debug_taz > 0) {
  print("Line 178")
  dt_taz2  %>% 
    filter(TAZ == debug_taz) %>% 
    select(debug_fields) %>%
    as.data.frame() %>% 
    print()
} 

print("Computing Land Conumption...")  
source("source_code_new/4_LandConspution_Variables.R")
includeDev <- FALSE  # adds existing developed land towards redevelopment
df_taz3 <- prepareLCVariables(dt_taz2, ctl, includeDev)

# if(debug_taz > 0) {
#   print("Line 192")
#   df_taz3  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# } 
#-------------------------------------------------------------------------------
# Allocate HH & EMP 
#-------------------------------------------------------------------------------
source("source_code_new/5_HousingAllocation.R")

# excludes DRI from CountyGrowth Target
excludeDRI <- FALSE  

print("Allocating Housing...")
ret        <- allocateHousing(df_taz3, df_gc, excludeDRI, includeDev, runType)
df_taz4    <- ret$df_taz4
hhConverge <- ret$DRIHHbyGrowthCenter %>%
              select(growthCenter, Control_HH, HH_model, unMet_HH = diff, HH_Flag = converge)

print("Allocating Employment...")
source("source_code_new/6_EmploymentAllocation.R")
ret         <- allocateEmployment(df_taz4, df_gc, excludeDRI, includeDev, runType)
df_taz4     <- ret$df_taz4
empConverge <- ret$DRIEMPbyGrowthCenter %>%
               select(growthCenter, Control_EMP, EMP_model, unMet_EMP = diff, EMP_Flag = converge)

# if(debug_taz > 0) {
#   print("Line 221")
#   df_taz4  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame()%>% 
#     print()
# } 
#-------------------------------------------------------------------------------
# Recheck Available Land
#-------------------------------------------------------------------------------
# Get "unallocated" units as "new" targets
df_gc2 <- hhConverge %>% 
          left_join(empConverge, by = "growthCenter") %>%
          mutate(Control_HH = ifelse(abs(unMet_HH) > 100, unMet_HH, 0),
                 Control_EMP = ifelse(abs(unMet_EMP) > 100, unMet_EMP, 0)) %>%
          select(growthCenter, Control_HH, Control_EMP)
        
# Counties with "unmet" demand
# unallocatedCounties <- df_gc2 %>% filter(Control_HH > 0) %>% pull(growthCenter)
unmet_HH_Counties   <- df_gc2 %>% filter(Control_HH > 0) %>% pull(growthCenter)
unmet_EMP_Counties  <- df_gc2 %>% filter(Control_EMP > 0) %>% pull(growthCenter)

unallocatedCounties <- unique(c(unmet_HH_Counties, unmet_EMP_Counties))

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
                     HHTotal1 = HHTotal,
                     EmpAllocated1 = EmpAllocated,
                     EmpTotal = EmpTotal)
  
  df_taz3b <- assign_unused_EMPLand(df_taz4a, unallocatedCounties, FALSE)
  
} else{
  
  df_taz3b <- df_taz4 

}

#-------------------------------------------------------------------------------
# run Allocate "Unmet" demand towards "Redevlopment"
#-------------------------------------------------------------------------------
# Run this for only counties that are not converges 
# (in other words, reallocated demand to redevelopment zones)

# Allocated unmet HH & EMP towards redevelopment
if(reRun_HH_for_UnMetCounties || reRun_EMP_for_UnMetCounties) {
    
    excludeDRI <- TRUE  # excludes DRI from CountyGrowth Target
    includeDev <- TRUE  # adds existing developed land towards redevelopment
    
    # crave out unConverged counties
    df_unmet <- df_taz4 %>% 
                filter(growthCenter %in% unallocatedCounties) %>%
                mutate(housing0 = housing,
                       employment0 = employment,
                       housing = HHTotal, 
                       employment = EmpTotal,
                       HHAllocated1 = HHAllocated,  
                       EmpAllocated1 = EmpAllocated) %>%
                select(colnames(df_taz3), 
                       housing0, employment0, 
                       HHAllocated1, EmpAllocated1)
    
    df_gcSel <- df_gc2 %>%
                filter(growthCenter %in% unallocatedCounties)
    
    print("Re-allocating 'Unmet' Housing towards 'redevelopment'...") 
    df_taz3c    <- prepareLCVariables(df_unmet, ctl, includeDev)
    
    ret         <- allocateHousing(df_taz3c, df_gcSel, excludeDRI, includeDev, runType)
    df_taz4HH   <- ret$df_taz4
    hhConverge2 <- ret$DRIHHbyGrowthCenter%>%
                   select(growthCenter, Control_HH, HH_model, unMet_HH = diff, HH_Flag = converge)

    print("Re-allocating 'Unmet' Employment towards redevelopment...")
    ret           <- allocateEmployment(df_taz4HH, df_gcSel, excludeDRI, includeDev, runType)
    df_taz4EMP    <- ret$df_taz4
    empConverge2  <- ret$DRIEMPbyGrowthCenter%>%
                     select(growthCenter, Control_EMP, EMP_model, unMet_EMP = diff, EMP_Flag = converge)
    
    
    # Check total housing and employment here
    check_reallocation <- df_taz4EMP %>%
                  group_by(growthCenter) %>%
                  summarise_at(vars(housing0, employment0, 
                                    HHAllocated1, HHAllocated, HHTotal,
                                    EmpAllocated1, EmpAllocated, EmpTotal), sum)
    
    df_taz5  <- df_taz4EMP %>%
                  mutate(HHAllocated2 = HHAllocated,
                         EmpAllocated2 = EmpAllocated,
                         HHAllocated = HHAllocated1 + HHAllocated2,
                         EmpAllocated = EmpAllocated1 + EmpAllocated2,                          
                         housing = housing0,
                         employment = employment0,
                         HHTotal = case_when(runType > 0 ~ pmax(0, HHAllocated + housing),
                                             runType < 0 ~ pmax(0, housing - HHAllocated),
                                             TRUE ~ 0),
                         # EmpTotal = pmax(0, EmpAllocated + employment),
                         EmpTotal = case_when(runType > 0 ~ pmax(0, EmpAllocated + employment),
                                              runType < 0 ~ pmax(0, employment - EmpAllocated),
                                              TRUE ~ 0)
                         ) %>% 
                 select(-housing0, -employment0)
    
    # Merge First allocation and Re-allocated data sets
    # colnames(df_taz5)[!(colnames(df_taz5)%in% colnames(df_met))]
    df_met <- df_taz4 %>% 
              filter(!(growthCenter %in% unallocatedCounties)) %>%
              mutate(HHAllocated1 = HHAllocated,
                     EmpAllocated1 = EmpAllocated,
                     HHAllocated2 = 0,
                     EmpAllocated2 = 0)
    
    df_taz5a <- bind_rows(df_met, df_taz5) %>%
                arrange(TAZ)
    
} else {
      df_taz5a <- df_taz3b
      hhConverge2 <- "Total HH Demand is met in hhConverge and so no reallocation is needed"
      empConverge2 <- "Total EMP Demand is met in empConverge and so no reallocation is needed"
}

# if(debug_taz > 0) {
#   print("Line 369")
#   df_taz5a  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# } 

#-------------------------------------------------------------------------------
# Compute land availability 
#-------------------------------------------------------------------------------
# Recalculate land consumed by allocated development
source("source_code_new/8_Reallocate_Unconsumed_EMPLand.R")

# Don't run for deallocation (assume the 2040 base density / land is )
if(runType > 0){
  df_taz5b <- computeNewAvailableLand(df_taz5a)
} else{
  df_taz5b <- df_taz5a
}
  
#-------------------------------------------------------------------------------
# FRATAR
#-------------------------------------------------------------------------------
print("Computing Trips / Fratar Inputs...")
source("source_code_new/7_Prepare_Fratar_Inputs.R")

# read k-factors for trip gen (ODME to hh, emp didn't match well)
df_kfac <- read.csv(tripGen_facs)

df_taz5b <- df_taz5b %>%
           left_join(df_kfac, by = "growthCenter")

df_taz6 <- computeFRATAR(df_taz5b, runType)

print("Writing Outputs...")
fratar_file <- ifelse(runType ==1, 
                       paste0("Output/",next_year,"_FratarInput.txt") ,
                       paste0("Output/",curr_year,"_FratarInput.txt"))

# Turn on only to run FRATAR in tranplan
# writeFRATARInput(df_taz6, fratar_file, ext_Stn_file, next_year)

df_taz7 <- df_taz6 %>%
           select(-housing, -employment) %>%
           rename(housing = HHTotal, employment = EmpTotal) %>%
           select(taz_fields)

# if(debug_taz > 0) {
#   print("Line 418")
#   df_taz5a  %>% 
#     filter(TAZ == debug_taz) %>% 
#     select(debug_fields) %>%
#     as.data.frame() %>% 
#     print()
# } 
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





