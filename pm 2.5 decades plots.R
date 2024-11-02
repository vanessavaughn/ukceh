## decades plots - pm 2.5

library(ggplot2)
library(viridis)
library(scales)
library(sf)
devtools::install_github("ropensci/rnaturalearthhires")
library(tidync)
install.packages("devtools")
library(devtools)
library(gridExtra)
##import data - 1960

pm_1960 <- read_emep("1960_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1960<- as.data.frame(pm_1960, xy=TRUE, na.rm=TRUE)
pm_1960$SURF_ug_PM25_rh50 <- as.numeric(pm_1960$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1960$x), max(pm_1960$x), min(pm_1960$y), max(pm_1960$y))
res <- c(.01, .01)
r <- rast(ext = ext, res = res)
points <- vect(pm_1960, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1960<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1960) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

plot(pm_1960)

#making df into sf
pm_1960_sf <- st_as_sf(pm_1960, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
pm_1960_sf_uk<- st_intersection(pm_1960_sf, uk)
max(pm_1960$SURF_ug_PM25_rh50)

custom_breaks <- c(2,4,6,8, 10)
custom_colors <- rainbow((length(custom_breaks) - 1))
custom_limits<- c(1, 10)


#plot 1960
pm_1960_plot<- ggplot() +
  geom_sf(data = pm_1960_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1960 PM 2.5")+theme(legend.position="none")
print(pm_1960_plot)

###########################################################################

##import data - 1970

pm_1970 <- read_emep("1970_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1970<- as.data.frame(pm_1970, xy=TRUE, na.rm=TRUE)
pm_1970$SURF_ug_PM25_rh50 <- as.numeric(pm_1970$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1970$x), max(pm_1970$x), min(pm_1970$y), max(pm_1970$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_1970, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1970<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1970) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1970_sf <- st_as_sf(pm_1970, coords = c("longitude", "latitude"), crs = 4326)



uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
pm_1970_sf_uk<- st_intersection(pm_1970_sf, uk)
max(pm_1970$SURF_ug_PM25_rh50)

custom_breaks <- c(1, 10, 20,30, 40, 50,60)
custom_colors <- rainbow((length(custom_breaks) - 1))
custom_limits<- c(1, 60)


#plot 1970
pm_1970_plot<- ggplot() +
  geom_sf(data = pm_1970_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1970 PM 2.5")+
  theme(legend.position="none")
print(pm_1970_plot)

###########################################################################

##import data - 1980

pm_1980 <- read_emep("1980_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1980<- as.data.frame(pm_1980, xy=TRUE, na.rm=TRUE)
pm_1980$SURF_ug_PM25_rh50 <- as.numeric(pm_1980$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1980$x), max(pm_1980$x), min(pm_1980$y), max(pm_1980$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_1980, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1980<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1980) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1980_sf <- st_as_sf(pm_1980, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
pm_1980_sf_uk<- st_intersection(pm_1980_sf, uk)


custom_breaks <- c(1, 20,40,60,80, 100)
custom_colors <- rainbow((length(custom_breaks) - 1))
custom_limits<- c(1, 100)


#plot 1980
pm_1980_plot<- ggplot() +
  geom_sf(data = pm_1980_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1980 PM 2.5")+
  theme(legend.position="none")
print(pm_1980_plot)
###########################################################################

##import data - 1990

pm_1990 <- read_emep("1990_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1990<- as.data.frame(pm_1990, xy=TRUE, na.rm=TRUE)
pm_1990$SURF_ug_PM25_rh50 <- as.numeric(pm_1990$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1990$x), max(pm_1990$x), min(pm_1990$y), max(pm_1990$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_1990, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1990<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1990) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1990_sf <- st_as_sf(pm_1990, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
pm_1990_sf_uk<- st_intersection(pm_1990_sf, uk)


#plot 1990
pm_1990_plot<- ggplot() +
  geom_sf(data = pm_1990_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1990 PM 2.5")+
  theme(legend.position="none")
print(pm_1990_plot)


###########################################################################

##import data - 2000

pm_2000 <- read_emep("2000_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2000<- as.data.frame(pm_2000, xy=TRUE, na.rm=TRUE)
pm_2000$SURF_ug_PM25_rh50 <- as.numeric(pm_2000$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_2000$x), max(pm_2000$x), min(pm_2000$y), max(pm_2000$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_2000, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_2000<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_2000) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_2000_sf <- st_as_sf(pm_2000, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
pm_2000_sf_uk<- st_intersection(pm_2000_sf, uk)

#plot 2000
pm_2000_plot<- ggplot() +
  geom_sf(data = pm_2000_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  ) +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("2000 PM 2.5")+
  theme(legend.position="none")
print(pm_2000_plot)


###########################################################################

##import data - 2010

pm_2010 <- read_emep("2010_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2010<- as.data.frame(pm_2010, xy=TRUE, na.rm=TRUE)
pm_2010$SURF_ug_PM25_rh50 <- as.numeric(pm_2010$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_2010$x), max(pm_2010$x), min(pm_2010$y), max(pm_2010$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_2010, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_2010<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_2010) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_2010_sf <- st_as_sf(pm_2010, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk


pm_2010_sf_uk<- st_intersection(pm_2010_sf, uk)


#plot 2010
pm_2010_plot<- ggplot() +
  geom_sf(data = pm_2010_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  ) +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("2010 PM 2.5")+
  theme(legend.position="none")
print(pm_2010_plot)





###########################################################################

##import data - 2020

pm_2020 <- read_emep("2020_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2020<- as.data.frame(pm_2020, xy=TRUE, na.rm=TRUE)
pm_2020$SURF_ug_PM25_rh50 <- as.numeric(pm_2020$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_2020$x), max(pm_2020$x), min(pm_2020$y), max(pm_2020$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_2020, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_2020<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_2020) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_2020_sf <- st_as_sf(pm_2020, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
pm_2020_sf_uk<- st_intersection(pm_2020_sf, uk)
max(pm_2020_sf_uk$SURF_ug_PM25_rh50)

pm_2020_plot<- ggplot() +
  geom_sf(data = pm_2020_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("2020 PM 2.5")
print(pm_2020_plot)


###########################################################################################
#difference 2020-1960

# Merge data frames by lat and lon
pm_2020_1960 <- merge(pm_1960, pm_2020, by = c("y", "x"))

# Calculate the difference in pm values
pm_2020_1960$pm_difference <- pm_2020_1960$SURF_ug_PM25_rh50.x - pm_2020_1960$SURF_ug_PM25_rh50.y
pm_2020_1960 <- subset(pm_2020_1960, select = -SURF_ug_PM25_rh50.y)
pm_2020_1960 <- subset(pm_2020_1960, select = -SURF_ug_PM25_rh50.x)
names(pm_2020_1960) <- c("y", "x", "pm_difference")


#making df into raster
ext <- ext(min(pm_2020_1960$x), max(pm_2020_1960$x), min(pm_2020_1960$y), max(pm_2020_1960$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_2020_1960, geom = c("x", "y"))
r <- rasterize(points, r, field = "pm_difference", fun = "mean")
pm_2020_1960<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_2020_1960) <- c("longitude", "latitude", "pm_difference")

#making df into sf
pm_2020_1960_sf <- st_as_sf(pm_2020_1960, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
pm_2020_1960_sf_uk<- st_intersection(pm_2020_1960_sf, uk)
max(pm_2020_1960_sf_uk$pm_difference)
#Δ

#plot 2020
pm_2020_1960_plot<- ggplot() +
  geom_sf(data = pm_2020_1960_sf_uk, aes(color=pm_difference))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )+ theme_minimal()+
  labs(color = "Δ PM 2.5 µg/m3 ") +
  ggtitle("Δ PM 2.5 from 1960-2020")
print(pm_2020_1960_plot)

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
combined_pm_plots<- grid.arrange(pm_1960_plot,pm_1970_plot,pm_1980_plot,pm_1990_plot,pm_2000_plot,pm_2010_plot,pm_2020_plot,pm_2020_1960_plot, ncol = 4, nrow = 2)

print(combined_pm_plots)
