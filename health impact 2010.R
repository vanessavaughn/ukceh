#health impact


#import pm 2000 data
pm_2010 <- read_emep("2010_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2010<- as.data.frame(pm_2010, xy=TRUE, na.rm=TRUE)
names(pm_2010)<- c('longitude', 'latitude', 'pm')
pm_2010$pm <- as.numeric(pm_2010$pm)

popnc_2010 <- read_emep("gbr_pd_2010_1km_UNadj.nc", "EPSG:4326", var = '', dims = c('lon', 'lat'), proxy = TRUE)

#calculating RR


#define the RR equation function
# Define the corrected relative risk function in R
cal_rr <- function(pm, theta, a, u, v) {
  {z <- pm - 2.4
  numerator <- theta * log((z / a) + 1)
  denominator <- exp(1 + exp(-((z - u) / v)))
  rr <- numerator / denominator
  return(rr)
  }
}

ages <- unique(`NCD+LRI_GEMM`$age.range)
# Loop over each age group and calculate rr
for (age.range in ages) {
  # Filter NCD+LRI_GEMM for the current age group
  age_specific_data <- `NCD+LRI_GEMM` %>%
    filter(age.range == !!age.range) 
  # Add rr for the current age group to pm_2020
  pm_2010 <- pm_2010 %>%
    mutate(!!paste0("rr_age_", age.range) := cal_rr(pm,
                                                    theta = age_specific_data$theta, 
                                                    a = age_specific_data$a, 
                                                    u = age_specific_data$u, 
                                                    v = age_specific_data$v))
}

## import population data
LRI<- X2010lri
NCD<- X2010ncd


names(United_Kingdom_2010)<- c("age.range", "pop")
United_Kingdom_2010$pop<- (United_Kingdom_2010$pop)/100

ncd_pop$val <- as.numeric(ncd_pop$val)


## join pop and bm by age range
lri_pop <- LRI %>%
  inner_join(United_Kingdom_2010, by = "age.range") 
ncd_pop <- NCD %>%
  inner_join(United_Kingdom_2010, by = "age.range")
## multiply bm and pop
lri_pop<- lri_pop %>%
  mutate(product = val * pop)
lri_pop<- lri_pop %>%
  mutate(product_u = upper * pop)
lri_pop<- lri_pop %>%
  mutate(product_l = lower * pop)

ncd_pop<- ncd_pop %>%
  mutate(product = val * pop)
ncd_pop<- ncd_pop %>%
  mutate(product_u = upper * pop)
ncd_pop<- ncd_pop %>%
  mutate(product_l = lower * pop)



#fix names
names(pm_2010)<-c('longitude', 'latitude', 'pm', 'rr_25.29', 'rr_30.34', 'rr_35.39','rr_40.44','rr_45.49','rr_50.54','rr_55.59','rr_60.64','rr_65.69', 'rr_70.74', 'rr_75.79', 'rr_80plus')
ncd_pop$age.range <- c('rr_25.29', 'rr_30.34', 'rr_35.39','rr_40.44','rr_45.49','rr_50.54','rr_55.59','rr_60.64','rr_65.69', 'rr_70.74', 'rr_75.79', 'rr_80plus')

lri_pop$age.range <- c('rr_25.29', 'rr_30.34', 'rr_35.39','rr_40.44','rr_45.49','rr_50.54','rr_55.59','rr_60.64','rr_65.69', 'rr_70.74', 'rr_75.79', 'rr_80plus')


#########multiplying
for(i in 1:nrow(ncd_pop)) {
  # Extract the age range and corresponding value
  age_range <- ncd_pop$age.range[i]
  multiplier <- ncd_pop$product[i]
  
  # Check if there is a column in pm_2000 that matches the age range
  if(age_range %in% colnames(pm_2010)) {
    # Multiply the entire column in pm_2020 by the ncd_pop value and create a new column
    new_col_name <- paste0("ncd_", age_range)
    pm_2010[[new_col_name]] <- pm_2010[[age_range]] * multiplier
  }
}

# Now repeat the process for lri_pop
for(i in 1:nrow(lri_pop)) {
  # Extract the age range and corresponding value
  age_range <- lri_pop$age.range[i]
  multiplier <- lri_pop$product[i]
  
  # Check if there is a column in pm_2000 that matches the age range
  if(age_range %in% colnames(pm_2010)) {
    # Multiply the entire column in pm_2000 by the lri_pop value and create a new column
    new_col_name <- paste0("lri_", age_range)
    pm_2010[[new_col_name]] <- pm_2010[[age_range]] * multiplier
  }
}
##############################

####multiply by pop nc
popnc_2010 <- read_emep("gbr_pd_2010_1km_UNadj.nc", "EPSG:4326", var = 'Band1', dims = c('lon', 'lat'), proxy = TRUE)

popnc_2010<- as.data.frame(popnc_2010, xy=TRUE, na.rm=TRUE)
popnc_2010<- na.omit(popnc_2010)
names(popnc_2010) <- c("longitude", "latitude", "pop")
#making df into raster
ext <- ext(min(popnc_2010$longitude), max(popnc_2010$longitude), min(popnc_2010$latitude), max(popnc_2010$latitude))
res <- c(.01, .01)
r <- rast(ext = ext, res = res)
points <- vect(popnc_2010, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "pop", fun = "mean")
popnc_2010<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(popnc_2010) <- c("longitude", "latitude", "pop")

install.packages("FNN")
library(FNN)

# Combine latitude and longitude into a matrix
coords_pm <- as.matrix(pm_2010[, c("latitude", "longitude")])
coords_popnc <- as.matrix(popnc_2010[, c("latitude", "longitude")])

# Find the nearest neighbors
nn <- get.knnx(coords_popnc, coords_pm, k = 1)

# Add the nearest population values to pm_2000
pm_2010$closest_pop <- popnc_2010$pop[nn$nn.index]


cols_to_multiply <- 17:40 

# Multiply each specified column by the 'multiplier' column and replace original values
pm_2010[, cols_to_multiply] <- pm_2010[, cols_to_multiply] * pm_2010$closest_pop


####################################

#adding ncd and lri

for(i in 1:nrow(ncd_pop)) {
  # Extract the age range
  age_range <- ncd_pop$age.range[i]
  
  # Construct the column names for ncd_* and lri_* for this age range
  ncd_col <- paste0("ncd_", age_range)
  lri_col <- paste0("lri_", age_range)
  
  # Check if both ncd_* and lri_* columns exist in pm_2000
  if(ncd_col %in% colnames(pm_2010) && lri_col %in% colnames(pm_2010)) {
    # Sum the values in the ncd_* and lri_* columns
    total_col_name <- paste0("dm_", age_range)
    pm_2010[[total_col_name]] <- pm_2010[[ncd_col]] + pm_2010[[lri_col]]
  }
}


###################
##plotting 25-29
deltam_2010_25.29<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_25.29')

ext <- ext(min(deltam_2010_25.29$longitude), max(deltam_2010_25.29$longitude), min(deltam_2010_25.29$latitude), max(deltam_2010_25.29$latitude))
res <- c(.01, .01)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_25.29, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_25.29", fun = "mean")
deltam_2010_25.29<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_25.29) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_25.29 <- st_as_sf(deltam_2010_25.29, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
deltam_2010_25.29<- st_intersection(deltam_2010_25.29, uk)


custom_breaks <- c(0,1,5,10,25, 50,100, 500,1000,2000)
custom_colors <- rainbow(length(custom_breaks) - 1)
custom_limits <- c(0, 2000)

#plot
m2529plot<- ggplot() +
  geom_sf(data = deltam_2010_25.29, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Premature Deaths 25-29") +
  ggtitle("Premature Deaths ages 25-29")+
  theme(legend.position = "none")
print(m2529plot)

#####################################################

##plotting 30-34
deltam_2010_30.34<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_30.34')

#making df into raster
ext <- ext(min(deltam_2010_30.34$longitude), max(deltam_2010_30.34$longitude), min(deltam_2010_30.34$latitude), max(deltam_2010_30.34$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_30.34, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_30.34", fun = "mean")
deltam_2010_30.34<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_30.34) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_30.34 <- st_as_sf(deltam_2010_30.34, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_30.34<- st_intersection(deltam_2010_30.34, uk)



#plot
m3034plot<- ggplot() +
  geom_sf(data = deltam_2010_30.34, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 30-34") +
  ggtitle("Premature Deaths ages 30-34")+
  theme(legend.position="none")
print(m3034plot)

###########################################
##plotting 35-39
deltam_2010_35.39<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_35.39')

#making df into raster
ext <- ext(min(deltam_2010_35.39$longitude), max(deltam_2010_35.39$longitude), min(deltam_2010_35.39$latitude), max(deltam_2010_35.39$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_35.39, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_35.39", fun = "mean")
deltam_2010_35.39<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_35.39) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_35.39 <- st_as_sf(deltam_2010_35.39, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_35.39<- st_intersection(deltam_2010_35.39, uk)


#plot
m3539plot<- ggplot() +
  geom_sf(data = deltam_2010_35.39, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 35-39") +
  theme(legend.position="none")+
  ggtitle("Premature Deaths ages 35-39")
print(m3539plot)
#####################################################

##plotting 40-44
deltam_2010_40.44<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_40.44')

#making df into raster
ext <- ext(min(deltam_2010_40.44$longitude), max(deltam_2010_40.44$longitude), min(deltam_2010_40.44$latitude), max(deltam_2010_40.44$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_40.44, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_40.44", fun = "mean")
deltam_2010_40.44<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_40.44) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_40.44 <- st_as_sf(deltam_2010_40.44, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
deltam_2010_40.44<- st_intersection(deltam_2010_40.44, uk)


#plot
m4044plot<- ggplot() +
  geom_sf(data = deltam_2010_40.44, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  theme(legend.position="none")+
  labs(color = "Δ M age 40-44") +
  ggtitle("Premature Deaths ages 40-44")
print(m4044plot)

###########################################
##plotting 45-49
deltam_2010_45.49<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_45.49')

#making df into raster
ext <- ext(min(deltam_2010_45.49$longitude), max(deltam_2010_45.49$longitude), min(deltam_2010_45.49$latitude), max(deltam_2010_45.49$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_45.49, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_45.49", fun = "mean")
deltam_2010_45.49<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_45.49) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_45.49 <- st_as_sf(deltam_2010_45.49, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_45.49<- st_intersection(deltam_2010_45.49, uk)

#plot
m4549plot<- ggplot() +
  geom_sf(data = deltam_2010_45.49, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 45-49") +
  theme(legend.position="none")+
  ggtitle("Premature Deaths ages 45-49")
print(m4549plot)

#####################################################

##plotting 50-54
deltam_2010_50.54<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_50.54')

#making df into raster
ext <- ext(min(deltam_2010_50.54$longitude), max(deltam_2010_50.54$longitude), min(deltam_2010_50.54$latitude), max(deltam_2010_50.54$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_50.54, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_50.54", fun = "mean")
deltam_2010_50.54<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_50.54) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_50.54 <- st_as_sf(deltam_2010_50.54, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
deltam_2010_50.54<- st_intersection(deltam_2010_50.54, uk)




#plot
m5054plot<- ggplot() +
  geom_sf(data = deltam_2010_50.54, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  theme(legend.position="none")+
  labs(color = "Δ M age 50-54") +
  ggtitle("Premature Deaths ages 50-54")
print(m5054plot)
###########################################
##plotting 55-59
deltam_2010_55.59<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_55.59')

#making df into raster
ext <- ext(min(deltam_2010_55.59$longitude), max(deltam_2010_55.59$longitude), min(deltam_2010_55.59$latitude), max(deltam_2010_55.59$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_55.59, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_55.59", fun = "mean")
deltam_2010_55.59<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_55.59) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_55.59 <- st_as_sf(deltam_2010_55.59, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_55.59<- st_intersection(deltam_2010_55.59, uk)

#plot
m5559plot<- ggplot() +
  geom_sf(data = deltam_2010_55.59, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 55-59") +
  theme(legend.position="none")+
  ggtitle("Premature Deaths ages 55-59")
print(m5559plot)

#####################################################

##plotting 60-64
deltam_2010_60.64<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_60.64')

#making df into raster
ext <- ext(min(deltam_2010_60.64$longitude), max(deltam_2010_60.64$longitude), min(deltam_2010_60.64$latitude), max(deltam_2010_60.64$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_60.64, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_60.64", fun = "mean")
deltam_2010_60.64<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_60.64) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_60.64 <- st_as_sf(deltam_2010_60.64, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
deltam_2010_60.64<- st_intersection(deltam_2010_60.64, uk)



#plot
m6064plot<- ggplot() +
  geom_sf(data = deltam_2010_60.64, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  theme(legend.position="none")+
  labs(color = "Δ M age 60-64") +
  ggtitle("Premature Deaths ages 60-64")
print(m6064plot)
###########################################
##plotting 65-69
deltam_2010_65.69<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_65.69')

#making df into raster
ext <- ext(min(deltam_2010_65.69$longitude), max(deltam_2010_65.69$longitude), min(deltam_2010_65.69$latitude), max(deltam_2010_65.69$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_65.69, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_65.69", fun = "mean")
deltam_2010_65.69<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_65.69) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_65.69 <- st_as_sf(deltam_2010_65.69, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_65.69<- st_intersection(deltam_2010_65.69, uk)


#plot
m6569plot<- ggplot() +
  geom_sf(data = deltam_2010_65.69, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 65-69") +
  theme(legend.position="none")+
  ggtitle("Premature Deaths ages 65-69")
print(m6569plot)

#####################################################


##plotting 70-74
deltam_2010_70.74<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_70.74')

#making df into raster
ext <- ext(min(deltam_2010_70.74$longitude), max(deltam_2010_70.74$longitude), min(deltam_2010_70.74$latitude), max(deltam_2010_70.74$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_70.74, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_70.74", fun = "mean")
deltam_2010_70.74<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_70.74) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_70.74 <- st_as_sf(deltam_2010_70.74, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
deltam_2010_70.74<- st_intersection(deltam_2010_70.74, uk)



#plot
m7074plot<- ggplot() +
  geom_sf(data = deltam_2010_70.74, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  theme(legend.position="none")+
  labs(color = "Δ M age 70-74") +
  ggtitle("Premature Deaths ages 70-74")
print(m7074plot)

###########################################
##plotting 75-79
deltam_2010_75.79<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_75.79')

#making df into raster
ext <- ext(min(deltam_2010_75.79$longitude), max(deltam_2010_75.79$longitude), min(deltam_2010_75.79$latitude), max(deltam_2010_75.79$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_75.79, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_75.79", fun = "mean")
deltam_2010_75.79<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_75.79) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_75.79 <- st_as_sf(deltam_2010_75.79, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_75.79<- st_intersection(deltam_2010_75.79, uk)



#plot
m7579plot<- ggplot() +
  geom_sf(data = deltam_2010_75.79, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 75-79") +
  theme(legend.position="none")+
  ggtitle("Premature Deaths ages 75-79")
print(m7579plot)
###########################################
##plotting 80plus
deltam_2010_80<- pm_2010 %>%
  dplyr::select('longitude', 'latitude', 'dm_rr_80plus')

#making df into raster
ext <- ext(min(deltam_2010_80$longitude), max(deltam_2010_80$longitude), min(deltam_2010_80$latitude), max(deltam_2010_80$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(deltam_2010_80, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "dm_rr_80plus", fun = "mean")
deltam_2010_80<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(deltam_2010_80) <- c("longitude", "latitude", "delta_M")

#making df into sf
deltam_2010_80 <- st_as_sf(deltam_2010_80, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
deltam_2010_80<- st_intersection(deltam_2010_80, uk)



max(deltam_2020_80$delta_M)

#plot
ggplot() +
  geom_sf(data = deltam_2010_80, aes(color=delta_M))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  theme_minimal()+
  scale_color_gradientn(
    colors = rev(custom_colors),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  labs(color = "Δ M age 80+") +
  theme(legend.position = "none",
        legend.text = element_text(angle = 45),
        legend.key.height = unit(4, "cm") )+
  ggtitle("Premature Deaths ages 80+")
print(m80plot)
#####################################################

#######bar graph
bp_2010<- data.frame(age.range= c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+"),
                     pd= c(sum(pm_2010$ncd_rr_25.29), sum(pm_2010$ncd_rr_30.34),sum(pm_2010$ncd_rr_35.39), sum(pm_2010$ncd_rr_40.44),sum(pm_2010$ncd_rr_45.49), sum(pm_2010$ncd_rr_50.54),sum(pm_2010$ncd_rr_55.59), sum(pm_2010$ncd_rr_60.64),sum(pm_2010$ncd_rr_65.69), sum(pm_2010$ncd_rr_70.74),sum(pm_2010$ncd_rr_75.79), sum(pm_2010$ncd_rr_80plus), sum(pm_2010$lri_rr_25.29), sum(pm_2010$lri_rr_30.34),sum(pm_2010$lri_rr_35.39), sum(pm_2010$lri_rr_40.44),sum(pm_2010$lri_rr_45.49), sum(pm_2010$lri_rr_50.54),sum(pm_2010$lri_rr_55.59), sum(pm_2010$lri_rr_60.64),sum(pm_2010$lri_rr_65.69), sum(pm_2010$lri_rr_70.74),sum(pm_2010$lri_rr_75.79), sum(pm_2010$lri_rr_80plus)),
                     disease=rep(c("NCD", "LRI"), each=12))

options(scipen = 999)
ggplot(data=bp_2010, aes(x=age.range,y=pd, fill=disease))+
  geom_bar(stat="identity", color="black", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  labs(x= "Age Range",y="Premature Deaths", title= "2010")+
  scale_y_continuous(limits=c(0,2000000)) + 
  theme_minimal()
