# Fratar trips
# Estimate new Regression Equations
# Run IPF with seed 
library(plotly)


#-------------------------------------------------------------------------------
# Get Seed Trip Table and Marginals
#-------------------------------------------------------------------------------
# Function to get trip ends by TAZ
getSeedTT <- function(seedTT_file){
  
  dt_seedTT <- fread(seedTT_file)
  max_taz <- max(max(dt_seedTT$I), max(dt_seedTT$J))
    
  dt_seedTT <- dt_seedTT[I <= max_taz & J <= max_taz, ]
  otrips <- dt_seedTT[, sum(Trip), by = I]
  dtrips <- dt_seedTT[, sum(Trip), by = J]
  
  setnames(otrips, c("TAZ", "OrigTrips"))
  setnames(dtrips, c("TAZ", "DestTrips"))
  setkey(otrips, "TAZ")
  setkey(dtrips, "TAZ")
  
  taz <- data.table(TAZ = c(1:max_taz))
  setkey(taz, "TAZ")
  
  taz <- merge(taz, otrips, all.x = T)
  taz <- merge(taz, dtrips, all.x = T)
  
  seedTrips <- taz %>% 
         setDF() %>%
         mutate(OrigTrips = ifelse(is.na(OrigTrips), 0, OrigTrips),
                DestTrips = ifelse(is.na(DestTrips), 0, DestTrips),
                seedTrips = (OrigTrips + DestTrips)/2) %>%
         select(TAZ, seedTrips)
  
  ret <- list(dt_seedTT = dt_seedTT, seedTrips = seedTrips)
  return(ret)
}

#-------------------------------------------------------------------------------
# Estimate Trip Generation Rates
#-------------------------------------------------------------------------------
seedTT_file   <- "Input/base_data/adjusted_seed.csv"
taz_pd_file   <- "Input/base_data/2015_TAZ_data_based_on_ParcelData_Nov1.xlsx"

# Get Seed trip table and tripends by zone 
ret <- getSeedTT(seedTT_file)
df_SeedTrips <- ret$seedTrips
dt_seedTT    <- ret$dt_seedTT

# Read base year data
df_pd  <- read.xlsx(taz_pd_file, sheet = "base_data") %>%
          select(TAZ, growthCenter, areaType, housing, employment)

# Get internal zonal trips
df_SeedInternal <- df_SeedTrips %>% 
                   filter(TAZ <= 6424)

df_data <- df_pd %>%
           left_join(df_SeedInternal, by = "TAZ") %>%
           mutate(boolUrban = ifelse(areaType ==2 , 1, 0),
                  hh1 = ifelse(areaType == 1, housing, 0),
                  hh2 = ifelse(areaType == 2, housing, 0),
                  hh3 = ifelse(areaType == 3, housing, 0),
                  emp1 = ifelse(areaType == 1, employment, 0),
                  emp2 = ifelse(areaType == 2, employment, 0),
                  emp3 = ifelse(areaType == 3, employment, 0)
                  ) %>%
           filter(seedTrips < 50000)

# Estimate 
mod <- lm(data = df_data, 
                 seedTrips ~  hh1 + hh2 + hh3 +  emp1 + emp2 + emp3 -1)
                 # seedTrips ~  housing + emp1 + emp2 + emp3 -1) 

# Add old and new estimated results (next to seed) 
df_data <- df_data %>%
           mutate(empFact = case_when(areaType == 1 ~ fratEMPFact[1], 
                                            areaType == 2 ~ fratEMPFact[2], 
                                            areaType == 3 ~ fratEMPFact[3]),
                  hhFact = case_when(areaType == 1 ~ fratHHFact[1], 
                                            areaType == 2 ~ fratHHFact[2], 
                                            areaType == 3 ~ fratHHFact[3]),
                  estNew = mod$fitted.values,
                  estOld = ctl$fratConstant + 
                                 housing * hhFact +
                                 employment * empFact +
                                 ctl$fratUrbanArea * boolUrban) %>%
          mutate(diff_old = round(estOld - seedTrips, 0),
                 diff_new = ifelse(seedTrips > 0, round( mod$residuals, 0), 0),
                 kp_fac = estNew + diff_new)

# Ideally we should not have zone based factors, but to match ODME trips based on hh, emp we need to add residuals back (as k-factors)
df_kfactors <- df_data %>% select(TAZ, k_fac = diff_new)

# Summarise differences by county
sumCounty <- df_data %>%
             group_by(growthCenter) %>%
             summarise_at(vars(diff_old, diff_new), list(min, max, mean, sum))

colnames(sumCounty) <- c("County",
                         paste(rep(c("old", "new"), 4),rep(c("min", "max", "mean", "net"),each = 2)))

# Summarise trip by county
tripsByCounty <- df_data %>%
             group_by(growthCenter) %>%
             summarise_at(vars(seedTrips, estNew), sum) %>%
             mutate(scale_fac = seedTrips / estNew)

# Get r-squared for old estimator
mod_old <- lm(data = df_data, seedTrips ~  estOld)


# plot existing and new estimated trips
scatter_plot <- plot_ly(data = df_data, x = ~seedTrips) %>%
      add_trace(y = ~estNew, name = "new", mode = "markers") %>%
      add_trace(y = ~estOld, name = "old", mode = "markers") %>%
      layout(annotations = list(
              text = paste0("<b>FLUAM 2.1 </b> r.squared: ",
                      round(summary(mod_old)$r.squared,4), "<br>",
                      "<b>re-estimated </b> r.squared: ",
                      round(summary(mod)$r.squared,4)),
              x = 60000, y = 100000,showarrow=FALSE ))


sink("reference/TripGeneration/lm.txt")
print(summary(mod))
sink()

x1 <- summary(mod)$coefficients
var <- rownames(x1)
x <- cbind(var, x1)

# df_data$estNew
excel_data <- list("data" = df_data,
                   "estimators" = x,
                   "County_Summary" = sumCounty,
                   "k-factor" = df_kfactors,
                   "scale_factor" = tripsByCounty)

write.xlsx(excel_data, "reference/TripGeneration/TripRate_Estimation.xlsx") 


#-------------------------------------------------------------------------------



  
  
