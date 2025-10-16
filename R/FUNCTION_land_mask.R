land_mask <- function(lon, lat, coast_to_land, coast_to_sea, eqfilter) {
  set.seed(0)
  points <- cbind(lon, lat)
  points <- as.data.frame(na.omit(points))
  pts <- st_as_sf(points, coords = 1:2, crs = 4326)
  points$over_land <- FALSE
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  points$over_land <- !is.na(as.numeric(st_intersects(pts, world)))
  points$eqfilter <- eqfilter

  points$dist_to_coastline <- raster::extract(distancetocoast::distance_to_coastline_10, cbind(lon, lat)) / 1000

  points$remove <- FALSE
  points$remove[points$dist_to_coastline > coast_to_land & points$over_land %in% TRUE & points$eqfilter == 1] <- TRUE
  points$remove[points$dist_to_coastline > coast_to_sea & points$over_land %in% FALSE & points$eqfilter == 1] <- TRUE

  output <- points$remove
  return(output)
}
