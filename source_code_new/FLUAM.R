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
    curr_year    <- Years[i]
    next_year    <- Years[i+1]
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
print("Computing Agri.Land Conversion & Land Conumption...")

source("source_code_new/3B_AgricultureLandConversion.R")
dt_taz2 <- convertAgriculture(dt_taz2, next_year)
  
source("source_code_new/4_LandConspution_Variables.R")
df_taz3 <- prepareLCVariables(dt_taz2, ctl)

#-------------------------------------------------------------------------------
# Allocate HH & EMP 
#-------------------------------------------------------------------------------
print("Allocating Housing...")

source("source_code_new/5_HousingAllocation.R")
ret        <- allocateHousing(df_taz3, df_gc)
df_taz4    <- ret$df_taz4
hhConverge <- ret$DRIHHbyGrowthCenter

print("Allocating Employment...")
source("source_code_new/6_EmploymentAllocation.R")
ret         <- allocateEmployment(df_taz4, df_gc)
df_taz4     <- ret$df_taz4
empConverge <- ret$DRIEMPbyGrowthCenter

#-------------------------------------------------------------------------------
# FRATAR
#-------------------------------------------------------------------------------
print("Computing Trips / Fratar Inputs...")
source("source_code_new/7_Prepare_Fratar_Inputs.R")
df_taz5 <- computeFRATAR(df_taz4)

print("Writing Outputs...")
fratar_file <- paste0("Output/",next_year,"_FratarInput.txt")  
writeFRATARInput(df_taz5, fratar_file)

out_file <- paste0("Output/",next_year,"_FLUAM_Output.xlsx")
exportFRATARTrips(df_taz5, taz_fields, hhConverge, empConverge, out_file)

#-------------------------------------------------------------------------------





