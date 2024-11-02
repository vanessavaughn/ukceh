## shapefile

library(sf)
library(ncdf4)
library(raster)
library(ggplot2)


shapefile <- '/Users/vanes/Downloads/ukceh/World_Countries_(Generalized)/World_Countries__Generalized_.shp'
gdf <- st_read(shapefile)


#uk index
uk_index <- which(gdf$COUNTRY == 'United Kingdom')
print(uk_index)


#emep
emepfile<- '1960_UK_conc.nc'

# Read NetCDF file
nc<- nc_open ("1960_UK_conc.nc")
print(nc)

pm25 <- "SURF_ug_PM25_rh50"  # Replace with the actual variable name
emep <- raster::brick(emepfile, varname = pm25)

gdf <- st_transform(gdf, crs = crs(emep))

for (ind in uk_index) {
  shp <- gdf[ind, ] 
if (st_is_empty(shp)) {
  next
}
  
shp_raster <- raster::mask(emep, as(st_geometry(shp), "Spatial"))  # Mask NetCDF data with the shapefile
sum_value <- cellStats(shp_raster, stat = 'sum')
print(paste("Sum for index", ind, ":", sum_value))
}

for (ind in uk_index) {
  shp <- gdf[ind, ]  # Select the row for the given index
  roi <- raster::mask(emep, shp)  # Mask NetCDF data with the shapefile
  
  # Compute sum
  sum_value <- cellStats(roi, stat = 'sum')
  print(sum_value)
}

# Plotting the last ROI
plot(roi)
