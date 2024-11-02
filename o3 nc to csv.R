####netcdf to csv#######



#####get longitude from file#####
lon <- ncvar_get(nc_file, "lon")
nlon<- dim(lon)  ##check length or dimension###
head(lon)        ##print first few columns##
lon_matrix <- matrix(lon, nrow = 369, ncol = 447)
lon_vector<- as.vector(lon_matrix)
#####get latitude from file#####
lat<- ncvar_get(nc_file, "lat")
nlat<- dim(lat)
head(lat)
lat_matrix <- matrix(lat, nrow = 369, ncol = 447)
lat_vector<- as.vector(lat_matrix)
######Get time or dates########
tt<- ncvar_get(nc_file, "time")
units<- ncatt_get(nc_file, "time", "units")
ntt<-dim(tt)
head(tt)
print(c(nlon, nlat, ntt))
o3 <- "SURF_ppb_O3" 
####Pull the o3 data####
fal.array1 <- ncvar_get(nc_file, o3)
###Get other information for the data###
o31<- ncatt_get(nc_file, o3, "long_name")
o3units1<- ncatt_get(nc_file, o3, "units")
fillvalue<- ncatt_get(nc_file, o3, "_FillValue")
###Check dimension of albedo data####
dim(fal.array1)
##(ncdf4) automatically converts Fillvalues to NA######
nc_close(nc_file)
######Convert o3 into vector form#######
o3.vec.long1<- as.vector(fal.array1)
####Check length of vector 
length(o3.vec.long1)
####Create a matrix with longitude*latitude*time#######
tlonlat1<- as.matrix(expand.grid(lon_matrix, lat_matrix, tt))
#####bind o3 vector with matrix (lat/lon/time) by row######
o3_final <- data.frame(cbind(tlonlat1, o3.vec.long1))
####Rename headers of the column######
names(o3_final)<- c( "longitude", "latitude","date", "o3")
#####Convert fal_final into data.frame format#####
o3nc_df<- data.frame(lon=lon_vector, lat=lat_vector, SURF_ppb_O3= o3.vec.long1)
  
  
  as.data.frame(o3_final)

write.csv(o3_df, "C:/Users/vanes/Downloads/ukceh/o3.csv", row.names=FALSE)





  #making df into raster
ext <- ext(min(o3nc_df$lon), max(o3nc_df$lon), min(o3nc_df$lat), max(o3nc_df$lat))
res <- c(.06, .06)
r <- rast(ext = ext, res = res, crs = "EPSG:4326")
points <- vect(o3nc_df, geom = c("lon", "lat"), crs = "EPSG:4326")
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
plot(r, main="data")
plot(r)
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df) <- c("longitude", "latitude", "SURF_ppb_O3")


#find locations with greatest o3 concentrations
top_o3_locations<- o3_df %>%
  top_n(500, wt=o3)

#merging aurn and nc df
sites_points_df<- o3_sites %>%
  left_join(r_df %>% dplyr::select(longitude, latitude, SURF_ppb_O3), by = c("longitude", "latitude"))

names(o3nc_df)<- c("longitude", "latitude", "SURF_ppb_O3")
o3_merge<- o3nc_df %>%
  left_join(o3_sites %>% dplyr::select(latitude, longitude, code),
            by=c("latitude", "longitude"))
ggplot() +
  geom_raster(data = r_df, aes(x = longitude, y = latitude, fill = SURF_ppb_O3)) +
  geom_point(data = o3sites_df, aes(x = longitude, y = latitude, color = SURF_ppb_O3), size = 3) +
  scale_fill_viridis_c(name = "O3 Value")+
  scale_color_gradient(low = "blue", high = "red", name = "O3 Value") +
  coord_fixed() +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()

x_value <- 57.15736
y_value <- o3nc_df$longitude[o3nc_df$latitude == x_value]
print(y_value)

## filter for hotspots
#convert to sf objects to match up coords
o3sites_df<- o3_sites[,c("code","longitude", "latitude")]

sf_o3 <- st_as_sf(o3nc_df, coords = c("longitude", "latitude"), crs = 4326)
sf_sites <- st_as_sf(o3sites_df, coords = c("longitude", "latitude"), crs = 4326)
# Find the nearest O3 point for each site
site_indices <- st_nearest_feature(sf_sites, sf_o3)
# Extract the corresponding O3 values from the indices
o3sites_df$SURF_ppb_O3 <- sf_o3$SURF_ppb_O3[site_indices]
