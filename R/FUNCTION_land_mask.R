land_mask <- function(lon, lat, coast_to_land, coast_to_sea, eqfilter) {
  trytest <- 1
  trytest <- tryCatch(library(distancetocoast), error = function(err) NA)
  if (length(trytest[trytest %in% "distancetocoast"]) != 1) {
    devtools::install_github("mdsumner/distancetocoast")
  }
  library(distancetocoast)
  library(rnaturalearth)

  set.seed(0)
  points <- cbind(lon, lat)
  points <- as.data.frame(na.omit(points))
  pts <- st_as_sf(points, coords = 1:2, crs = 4326)
  points$over_land <- FALSE
  world <- ne_countries(scale = "medium", returnclass = "sf")
  points$over_land <- !is.na(as.numeric(st_intersects(pts, world)))
  points$eqfilter <- eqfilter

  points$dist_to_coastline <- raster::extract(distance_to_coastline_10, cbind(lon, lat)) / 1000

  points$remove <- F
  points$remove[points$dist_to_coastline > coast_to_land & points$over_land %in% TRUE & points$eqfilter == 1] <- T
  points$remove[points$dist_to_coastline > coast_to_sea & points$over_land %in% FALSE & points$eqfilter == 1] <- T

  output <- points$remove
  return(output)
}
