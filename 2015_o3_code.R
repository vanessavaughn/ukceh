#3/7/24
#2015 O3 data

###install packages
  install.packages("ncdf4")
  install.packages("tidync")
  install.packages("maps")
  install.packages("mapsdata")
  install.packages("rworldmap")
  install.packages("raster")
  install.packages("sf")
  install.packages("terra")
  install.packages("lubridate")
  library(tidync)
  library(ncdf4)
  library(tidyverse)
  library(maps)
  library(rworldmap)
  library(dplyr)
  library(ggplot2)
  library(raster)
  library(sf)
  library(terra)
  library(lubridate)
  library(openair)

###read 2015 sample data
  sample2015<- tidync("2015_UK_conc.nc")
  print(sample2015)
  nc_file<- nc_open("2015_UK_conc.nc")
  class(nc_file)
#inspecting data
    ncmeta::nc_grids("2015_UK_conc.nc")
    ncmeta::nc_vars("2015_UK_conc.nc")

###making dataframe from nc
#get longitude from file
  lon <- ncvar_get(nc_file, "lon")
  nlon<- dim(lon)  #check length or dimension
  head(lon)        #print first few columns
  lon_matrix <- matrix(lon, nrow = 369, ncol = 447)
  lon_vector<- as.vector(lon_matrix)
#get latitude from file
  lat<- ncvar_get(nc_file, "lat")
  nlat<- dim(lat)
  head(lat)
  lat_matrix <- matrix(lat, nrow = 369, ncol = 447)
  lat_vector<- as.vector(lat_matrix)
#get time from file
  tt<- ncvar_get(nc_file, "time")
  units<- ncatt_get(nc_file, "time", "units")
  ntt<-dim(tt)
  head(tt)
  print(c(nlon, nlat, ntt))
  o3 <- "SURF_ppb_O3" 
#Pull the o3 data
  o3.array1 <- ncvar_get(nc_file, o3)
#Get other information for the data
  o31<- ncatt_get(nc_file, o3, "long_name")
  o3units1<- ncatt_get(nc_file, o3, "units")
  fillvalue<- ncatt_get(nc_file, o3, "_FillValue")
#Check dimension of o3 data
  dim(o3.array1)
#(ncdf4) automatically converts Fillvalues to NA
  nc_close(nc_file)
#Convert o3 into vector form
  o3.vec.long1<- as.vector(o3.array1)
#Check length of vector 
  length(o3.vec.long1)
#Convert o3_final into data.frame format
  o3nc_df<- data.frame(lon=lon_vector, lat=lat_vector, SURF_ppb_O3= o3.vec.long1)
  names(o3nc_df) <- c("longitude", "latitude", "SURF_ppb_O3")
  
###Get openair site data
 
  #filter sites for o3 and pm2.5 
  o3_sites<- importMeta(source= "aurn", all=TRUE, year="2015") %>%
    filter(variable %in% c('O3'))
  
  all_sites <- unique(o3_sites$code)
 
  o3sites_values<- importAURN(site = all_sites, year = 2015, pollutant =
                                "o3", hc = FALSE)

  
  
  #avg by code
  o3sites_values<- o3sites_values %>%
    group_by(code)%>% summarize(o3=mean(o3, na.rm=TRUE))
  o3sites_values_coords <- merge(o3sites_values, o3_sites, by="code", all.x=TRUE)
  
  
### make o3 nc df into raster
  #making df into raster
  ext <- ext(min(o3nc_df$lon), max(o3nc_df$lon), min(o3nc_df$lat), max(o3nc_df$lat))
  res <- c(.06, .06)
  r <- rast(ext = ext, res = res, crs = "EPSG:4326")
  points <- vect(o3nc_df, geom = c("lon", "lat"), crs = "EPSG:4326")
  r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
  r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
  names(r_df) <- c("longitude", "latitude", "SURF_ppb_O3")

###read_emep 
  #arctic polar stereographic crs: EPSG:3995

  ozone<- 0.51
o3sites_values_coords<- o3sites_values_coords%>%
  mutate(o3=o3*ozone)

ggplot()+


  
### Plot
 ggplot() +
    geom_raster(data = r_df, aes(x = longitude, y = latitude, fill = SURF_ppb_O3)) +
    geom_point(data = o3sites_values_coords, aes(x = longitude, y = latitude, color = o3), size=2) +
   scale_fill_viridis_c(name = "O3 ppb", limits=c(5, 40))+
   scale_color_viridis_c( name = "O3 ppb", limits=c(5,40), guide="none") +
   coord_fixed() +
   labs(x = "Longitude", y = "Latitude") +
   theme_minimal()


print(o3_2015_plot)
