#-------------------------------------------------------------------------------
# calculateDistToIntersections
#-------------------------------------------------------------------------------
# function to ramp nodes 
getRampNodes <- function(df_link, df_node, max_taz){
  arterial_class <- c(20:30)
  ramp_class     <- c(70:79)
  highToll_class <- c(10:19, 91:96)
  
  df_link[ FacType %in% arterial_class, type := 'arterial']
  df_link[ FacType %in% ramp_class, type := 'ramp']
  df_link[ FacType %in% highToll_class, type := 'high_tolls']
  
  df_link2 <- df_link[, c("A", "type")]
  df_link2 <- unique(df_link2)
  setnames(df_link2, c("N", "A_Type"))
  setkey(df_link2, "N")
  setkey(df_node, "N")
  df_node <- merge(df_node, df_link2, x.all = T)
  
  df_link2 <- df_link[, c("B", "type")]
  df_link2 <- unique(df_link2)
  setnames(df_link2, c("N", "B_Type"))
  setkey(df_link2, "N")
  df_node <- merge(df_node, df_link2, x.all = T)
  
  df_node <- df_node[A_Type %in% c("arterial", "ramp") & B_Type %in% c("arterial", "ramp") , 
                     type := "arterial"]
  df_node <- df_node[A_Type == "ramp" || B_Type == "ramp" , 
                     type := "ramp"]
  
  # other_nodes <- df_node[ N > 6424 & A_Type %in% c("arterial", "ramp") & B_Type %in% c("arterial", "ramp"), c("N", "X", "Y")]
  other_nodes <- df_node[ N > max_taz & A_Type == "ramp" & B_Type %in% c("arterial", "high_tolls"), c("N", "X", "Y")]
  
  return(other_nodes)
}


# function to get centroids
getCentroids <- function(df_node, max_taz){
  # Add type as Ramp = 1, Arterial = 2 and Both = 3
  zone_nodes <- df_node[ N <= max_taz, c("N", "X", "Y")]
  zone_nodes <- distinct(zone_nodes)
  return(zone_nodes)
}


#-------------------------------------------------------------------------------
# C++ function to compute distances and get nodes
#-------------------------------------------------------------------------------
# 
# sourceCpp("source_code_new/compute_distance.cpp")

#-------------------------------------------------------------------------------
# R & CPP: finding closest zone is computationally intensive
#-------------------------------------------------------------------------------

getRampDistance <- function(dt_taz, zone_nodes, other_nodes){
  other_N    <- other_nodes$N
  other_X    <- other_nodes$X
  other_Y    <- other_nodes$Y
  
  # Arterial Distance
  # x <- pdistC(vec_x, vec_y, other_X, other_Y, other_N)
  
  cnt <- 0
  dist_xy <- do.call(rbind, lapply(1:nrow(zone_nodes), function(cnt){
    cnt <<- cnt + 1
    vec_zones  <- zone_nodes$N[cnt]
    vec_x      <- zone_nodes$X[cnt]
    vec_y      <- zone_nodes$Y[cnt] 
    
    # This function is from compute_distance.cpp file
    x <- pdistC(vec_x, vec_y, other_X, other_Y, other_N)
    xy <- do.call(cbind, list(x[[1]], x[[2]]))
    return(xy)
  }))
  
  # dist_xy <- as.data.frame(dist_xy)
  dist_xy <- dist_xy %>% 
    as.data.frame() %>%
    mutate(TAZ = seq(1:nrow(dist_xy))) %>%
    select(TAZ, RmpDist = V1, RampNode = V2)
  
  setDT(dist_xy)
  setkey(dist_xy, "TAZ")
  setkey(dt_taz2, "TAZ")
  dt_taz <- merge(dt_taz, dist_xy, x.all = T)
  
  return(dt_taz)
}

# Ramp Distances are not properly computed due to missing type in the data
# RmpDist <- sqrt(sqrt(99999/ 1609.344) ) + 0.1
