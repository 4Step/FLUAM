
# C++ function to run iterative proportional fitting with capping
sourceCpp("source_code_new/compute_IPF.cpp")

year          <- 2020
seedTT_file   <- "Input/base_data/adjusted_seed.csv"
taz_pd_file   <- "Input/base_data/2015_TAZ_data_based_on_ParcelData_Nov1.xlsx"
ext_Stn_file  <- "Input/controlTotals/External_Stns_GrowthFactors.xlsx"


# Read seed table
dt_seedTT <- fread(seedTT_file)

# read growth factors
read_newdata  <-  paste0("Output/",year,"_FLUAM_Output.xlsx")

# Get growth factors
df_ext <- read.xlsx(ext_Stn_file, sheet = "ext_growthrates") %>%
          select(TAZ, growth = paste0(year))

df_int <- read.xlsx(read_newdata, sheet = "debug_reallocate")

# For new zones (no hh in base year), growth fac is NA, flag such zones
df_int_growth <- df_int %>% 
                 select(TAZ, growth) %>%
                 mutate(growth = ifelse(is.na(growth), -9999, growth))

# Use new trip end totals for such zones
df_int_tripends <- df_int %>% 
                   mutate(futureTrips = ifelse(is.na(growth), futureTrips, 0)) %>%
                   select(TAZ, futureTrips)

df_growth <- rbind(df_int_growth, df_ext)

max_taz   <- max(df_growth$TAZ)

# Check seed row / col sums
# rowSum = dt_seedTT[, sum(Trip), by = I]
# colSum = dt_seedTT[, sum(Trip), by = J]
# fwrite(rowSum, "seed_rowSum.csv")
# fwrite(colSum, "seed_colSum.csv")

# Run fratar
# 
# # Get Seed Marginals
# seedMarginals <- getMarginals(dt_seedTT, max_taz)
# s_x           <- seedMarginals$rowSum
# s_y           <- seedMarginals$colSum
# 
# # # Get seed matrix
# ret       <- getMatrix(dt_seedTT, max_taz)
# write.csv(ret, "seed_mat.csv")

# # Apply growth factors
# df_data <- left_join(df_growth, df_int_tripends, by = "TAZ") %>%
#            mutate(seed_row = s_x,
#                   seed_col = s_y,
#                   target_row = ifelse(growth != -9999, s_x * growth, futureTrips),
#                   target_col = ifelse(growth != -9999, s_y * growth, futureTrips))

# get matrix row/col sums
# rcSums        <- getMatRCSums(seedMat)
# r_x <- rcSums$rowSum
# r_y <- rcSums$colSum


start_time <- Sys.time()
# Print log file
sink("fratar.log")

# Run FRATAR (2D IPF)
ret <- runIPF(dt_seedTT, df_growth$growth,df_int_tripends$futureTrips, 80000, 20)
# write.csv(ret, "new_mat.csv")
sink()
end_time <- Sys.time()
end_time - start_time

# write new balanced matrix
newMat <- ret$newMat %>% setDT()
newMat <- newMat[, Trip := round(Trip,0)]
fwrite(newMat, "new_mat.csv")

# Final output
rowsum <- ret$rowsum
colsum <- ret$colsum
IPF_tripends <- cbind(df_growth$TAZ, rowsum, colsum)

write.csv(IPF_tripends, "IPF_tripends.csv", row.names = F)



# rowSum = newMat[, sum(Trip), by = I]
# colSum = newMat[, sum(Trip), by = J]
# fwrite(rowSum, "rowSum.csv")
# fwrite(colSum, "colSum.csv")

# Interim values
# x <- ret$row_seed
# y <- ret$col_seed
# x0 <- ret$row_target
# y0 <- ret$col_target
# x1 <- ret$rowsum1
# y1 <- ret$colsum1



# allY <- cbind(y, y0, y1, y2)
# allX <- cbind(x, x0, x1, x2)

# write.csv(allX, "rowSum.csv")
# write.csv(allY, "colSum.csv")



# ret <- getMarginals(dt_seedTT$I, dt_seedTT$J, dt_seedTT$Trip, 6424)





