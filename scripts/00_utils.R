# ==============================================================================
# SCRIPT: 00_utils.R
# PURPOSE: Helper functions
# ==============================================================================

# Calculates the haversine distance (km) between two Lat/Long points
haversine_km <- function(lat1, long1, lat2, long2) {
  radius_earth <- 6371 # radius of the Earth in km
  dist_lat <- (lat2 - lat1) * pi / 180
  dist_long <- (long2 - long1) * pi / 180
  
  a <- sin(dist_lat / 2) ^ 2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dist_long / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  return(radius_earth * c)
}