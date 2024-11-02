## decades plots - o3
library(ggplot2)
library(viridis)
library(scales)
library(sf)
library(dplyr)
library(tidync)
library(stars)
library(terra)
library(rnaturalearth)

##import data - 1960

o3_1960 <- read_emep("1960_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_1960<- as.data.frame(o3_1960, xy=TRUE, na.rm=TRUE)
o3_1960$SURF_ppb_O3 <- as.numeric(o3_1960$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_1960$x), max(o3_1960$x), min(o3_1960$y), max(o3_1960$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_1960, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_1960<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_1960) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_1960_sf <- st_as_sf(o3_1960, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
o3_1960_sf_uk<- st_intersection(o3_1960_sf, uk)
max(o3_1960$SURF_ppb_O3)

custom_breaks <- c(0, 10, 20, 30, 40)
custom_colors <- rainbow(length(custom_breaks) - 1)
custom_limits <- c(0, 40)




#plot 1960
o3_1960_plot<- ggplot() +
  geom_sf(data = o3_1960_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1960 O3")

print(o3_1960_plot)

###########################################################################

##import data - 1970

o3_1970 <- read_emep("1970_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_1970<- as.data.frame(o3_1970, xy=TRUE, na.rm=TRUE)
o3_1970$SURF_ppb_O3 <- as.numeric(o3_1970$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_1970$x), max(o3_1970$x), min(o3_1970$y), max(o3_1970$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_1970, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_1970<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_1970) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_1970_sf <- st_as_sf(o3_1970, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_1970_sf_uk<- st_intersection(o3_1970_sf, uk)


#plot 1970
o3_1970_plot<- ggplot() +
  geom_sf(data = o3_1970_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  theme(legend.position = "none")+
  ggtitle("1970 O3")
print(o3_1970_plot)

###########################################################################

##import data - 1980

o3_1980 <- read_emep("1980_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_1980<- as.data.frame(o3_1980, xy=TRUE, na.rm=TRUE)
o3_1980$SURF_ppb_O3 <- as.numeric(o3_1980$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_1980$x), max(o3_1980$x), min(o3_1980$y), max(o3_1980$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_1980, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_1980<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_1980) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_1980_sf <- st_as_sf(o3_1980, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_1980_sf_uk<- st_intersection(o3_1980_sf, uk)


#plot 1980
o3_1980_plot<- ggplot() +
  geom_sf(data = o3_1980_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  theme(legend.position = "none")+
  ggtitle("1980 O3")

print(o3_1980_plot)


###########################################################################

##import data - 1990

o3_1990 <- read_emep("1990_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_1990<- as.data.frame(o3_1990, xy=TRUE, na.rm=TRUE)
o3_1990$SURF_ppb_O3 <- as.numeric(o3_1990$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_1990$x), max(o3_1990$x), min(o3_1990$y), max(o3_1990$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_1990, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_1990<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_1990) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_1990_sf <- st_as_sf(o3_1990, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_1990_sf_uk<- st_intersection(o3_1990_sf, uk)


#plot 1990
o3_1990_plot<- ggplot() +
  geom_sf(data = o3_1990_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  theme(legend.position = "none")+
  ggtitle("1990 O3")
print(o3_1990_plot)


###########################################################################

##import data - 2000

o3_2000 <- read_emep("2000_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_2000<- as.data.frame(o3_2000, xy=TRUE, na.rm=TRUE)
o3_2000$SURF_ppb_O3 <- as.numeric(o3_2000$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_2000$x), max(o3_2000$x), min(o3_2000$y), max(o3_2000$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_2000, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_2000<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_2000) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_2000_sf <- st_as_sf(o3_2000, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_2000_sf_uk<- st_intersection(o3_2000_sf, uk)


#plot 2000
o3_2000_plot<- ggplot() +
  geom_sf(data = o3_2000_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  theme(legend.position = "none")+
  ggtitle("2000 O3")
print(o3_2000_plot)


###########################################################################

##import data - 2010

o3_2010 <- read_emep("2010_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_2010<- as.data.frame(o3_2010, xy=TRUE, na.rm=TRUE)
o3_2010$SURF_ppb_O3 <- as.numeric(o3_2010$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_2010$x), max(o3_2010$x), min(o3_2010$y), max(o3_2010$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_2010, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_2010<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_2010) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_2010_sf <- st_as_sf(o3_2010, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_2010_sf_uk<- st_intersection(o3_2010_sf, uk)
max(o3_2010_sf_uk$SURF_ppb_O3)

#plot 2010
o3_2010_plot<- ggplot() +
  geom_sf(data = o3_2010_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  theme(legend.position = "none")+
  ggtitle("2010 O3")

print(o3_2010_plot)


###########################################################################

##import data - 2020

o3_2020 <- read_emep("2020_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

o3_2020<- as.data.frame(o3_2020, xy=TRUE, na.rm=TRUE)
o3_2020$SURF_ppb_O3 <- as.numeric(o3_2020$SURF_ppb_O3)


#making df into raster
ext <- ext(min(o3_2020$x), max(o3_2020$x), min(o3_2020$y), max(o3_2020$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_2020, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_2020<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_2020) <- c("longitude", "latitude", "SURF_ppb_O3")

#making df into sf
o3_2020_sf <- st_as_sf(o3_2020, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_2020_sf_uk<- st_intersection(o3_2020_sf, uk)
max(o3_2020_sf_uk$SURF_ppb_O3)

#plot 2020
o3_2020_plot<- ggplot() +
  geom_sf(data = o3_2020_sf_uk, aes(color=SURF_ppb_O3))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2020 O3")
print(o3_2020_plot)

###########################################################################################
#difference 2020-1960

# Merge data frames by lat and lon
o3_2020_1960 <- merge(o3_1960, o3_2020, by = c("latitude", "longitude"))

# Calculate the difference in o3 values
o3_2020_1960$o3_difference <- o3_2020_1960$SURF_ppb_O3.y - o3_2020_1960$SURF_ppb_O3.x
o3_2020_1960 <- subset(o3_2020_1960, select = -SURF_ppb_O3.y)
o3_2020_1960 <- subset(o3_2020_1960, select = -SURF_ppb_O3.x)
names(o3_2020_1960) <- c("y", "x", "o3_difference")


#making df into raster
ext <- ext(min(o3_2020_1960$longitude), max(o3_2020_1960$longitude), min(o3_2020_1960$latitude), max(o3_2020_1960$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(o3_2020_1960, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "o3_difference", fun = "mean")
o3_2020_1960<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_2020_1960) <- c("longitude", "latitude", "o3_difference")

#making df into sf
o3_2020_1960_sf <- st_as_sf(o3_2020_1960, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_2020_1960_sf_uk<- st_intersection(o3_2020_1960_sf, uk)
min(o3_2020_1960_sf_uk$o3_difference)


custom_breaks <- c(1,5,10, 15,20)
custom_colors <- rainbow(length(custom_breaks) - 1)
custom_limits <- c(1, 20)

#plot 2020
o3_2020_1960_plot<- ggplot() +
  geom_sf(data = o3_2020_1960_sf_uk, aes(color=o3_difference))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "Δ O3 ppb") +
  ggtitle("Δ O3 from 1960-2020 ")
print(o3_2020_1960_plot)



###########################################################################################



#panel of plots
combined_o3_plots<- grid.arrange(o3_1960_plot,o3_1970_plot,o3_1980_plot,o3_1990_plot,o3_2000_plot,o3_2010_plot,o3_2020_plot,o3_2020_1960_plot, ncol = 4, nrow = 2)
print(combined_o3_plots)