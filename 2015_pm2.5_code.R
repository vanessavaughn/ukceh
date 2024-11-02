#5/7/24
#2015 PM 2.5 Data

library(openair)
###read 2015 sample data
sample2015<- tidync("2015_UK_conc.nc")
print(sample2015)
nc_file<- nc_open("2015_UK_conc.nc")
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
pm25 <- "SURF_ug_PM25_rh50" 
#Pull the PM 2.5 data
pm25.array1 <- ncvar_get(nc_file, pm25)
#Get other information for the data
pm251<- ncatt_get(nc_file, pm25, "long_name")
pm25units1<- ncatt_get(nc_file, pm25, "units")
fillvalue<- ncatt_get(nc_file, pm25, "_FillValue")
#Check dimension of pm 2.5 data
dim(pm25.array1)
#(ncdf4) automatically converts Fillvalues to NA
nc_close(nc_file)
#Convert pm 2.5 into vector form
pm25.vec.long1<- as.vector(pm25.array1)
#Check length of vector 
length(pm25.vec.long1)
#Convert to data.frame format
pm25nc_df<- data.frame(lon=lon_vector, lat=lat_vector, SURF_ug_PM25_rh50= pm25.vec.long1)
names(pm25nc_df) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

###Get openair site data
sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3', 'PM2.5','PM25'))
dplyr::glimpse(sites)
print(sites)
#filter sites for o3 and pm2.5 
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
write.csv(o3_sites, 'o3_sites.csv')
pm_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('PM2.5'))
pm_sites<- importMeta(source= "aurn", all=TRUE, year="2015") %>%
  filter(variable %in% c('PM2.5', 'PM25'))
pmsites_values<- importAURN(site = all_sites, year = 2015, pollutant =
                "pm2.5", hc = FALSE)
all_sites <- unique(pm_sites$code)
#avg by code
pmsites_values<- pmsites_values %>%
  group_by(code)%>% summarize(pm2.5=mean(pm2.5, na.rm=TRUE))
pmsites_values_coords <- merge(pmsites_values, pm_sites, by="code", all.x=TRUE)

### make pm25 nc df into raster
#making df into raster
ext <- ext(min(pm25nc_df$lon), max(pm25nc_df$lon), min(pm25nc_df$lat), max(pm25nc_df$lat))
res <- c(.06, .06)
r <- rast(ext = ext, res = res, crs = "EPSG:4326")
points <- vect(pm25nc_df, geom = c("longitude", "latitude"), crs = "EPSG:4326")
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

###read_emep= function(), proxy=F

## filter for hotspots
#convert to sf objects to match up coords
pm25sites_df<- pm_sites[,c("code","longitude", "latitude")]
sf_pm25 <- st_as_sf(pm25nc_df, coords = c("longitude", "latitude"), crs = 4326)
sf_sites <- st_as_sf(pm25sites_df, coords = c("longitude", "latitude"), crs = 4326)
# Find the nearest O3 point for each site
site_indices <- st_nearest_feature(sf_sites, sf_pm25)
# Extract the corresponding pm 2.5 values from the indices
pm25sites_df$SURF_ug_PM25_rh50 <- sf_pm25$SURF_ug_PM25_rh50[site_indices]

### Plot
pm_sites<- na.omit(pm_sites)
ggplot() +
  geom_raster(data = r_df, aes(x = longitude, y = latitude, fill = SURF_ug_PM25_rh50)) +
  geom_point(data = pmsites_values_coords ,aes(x = longitude, y = latitude, color = pm2.5)) +
  scale_fill_viridis_c(name = "PM 2.5 Value", limits = c(3, 16))+
  scale_color_viridis_c( name = "PM 2.5 Value", limits = c(3, 16), guide = "none") +
  coord_fixed() +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()
print(pm25_2015_plot)
