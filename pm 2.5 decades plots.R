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
devtools::install_github("karthik/wesanderson")
library(wesanderson)
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

#making df into sf
pm_1960_sf <- st_as_sf(pm_1960, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
pm_1960_sf_uk<- st_intersection(pm_1960_sf, uk)
custom_breaks <- c(1, 10,20,30,40,50)
custom_limits<- c(1, 50)
pal <- wes_palette("Zissou1", 20, type = "continuous")
#plot 1960
pm_1960_plot<- ggplot() +
  geom_sf(data = pm_1960_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1960 PM 2.5")
print(pm_1960_plot)

###########################################################################
##import data - 1965

pm_1965 <- read_emep("1965_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1965<- as.data.frame(pm_1965, xy=TRUE, na.rm=TRUE)
pm_1965$SURF_ug_PM25_rh50 <- as.numeric(pm_1965$SURF_ug_PM25_rh50)

#making df into raster
ext <- ext(min(pm_1965$x), max(pm_1965$x), min(pm_1965$y), max(pm_1965$y))
res <- c(.01, .01)
r <- rast(ext = ext, res = res)
points <- vect(pm_1965, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1965<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1965) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1965_sf <- st_as_sf(pm_1965, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
pm_1965_sf_uk<- st_intersection(pm_1965_sf, uk)

#plot 1965
pm_1965_plot<- ggplot() +
  geom_sf(data = pm_1965_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1965 PM 2.5")+
  theme(legend.position="none")
print(pm_1965_plot)
####################################################################################
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
pm_1970_sf_uk<- st_intersection(pm_1970_sf, uk)

#plot 1970
pm_1970_plot<- ggplot() +
  geom_sf(data = pm_1970_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
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
##import data - 1975

pm_1975 <- read_emep("1975_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1975<- as.data.frame(pm_1975, xy=TRUE, na.rm=TRUE)
pm_1975$SURF_ug_PM25_rh50 <- as.numeric(pm_1975$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1975$x), max(pm_1975$x), min(pm_1975$y), max(pm_1975$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_1975, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1975<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1975) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1975_sf <- st_as_sf(pm_1975, coords = c("longitude", "latitude"), crs = 4326)
pm_1975_sf_uk<- st_intersection(pm_1975_sf, uk)

#plot 1975
pm_1975_plot<- ggplot() +
  geom_sf(data = pm_1975_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1975 PM 2.5")+
  theme(legend.position="none")
print(pm_1975_plot)

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
pm_1980_sf_uk<- st_intersection(pm_1980_sf, uk)

#plot 1980
pm_1980_plot<- ggplot() +
  geom_sf(data = pm_1980_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
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
##import data - 1985

pm_1985 <- read_emep("1985_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1985<- as.data.frame(pm_1985, xy=TRUE, na.rm=TRUE)
pm_1985$SURF_ug_PM25_rh50 <- as.numeric(pm_1985$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1985$x), max(pm_1985$x), min(pm_1985$y), max(pm_1985$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_1985, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1985<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1985) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1985_sf <- st_as_sf(pm_1985, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
pm_1985_sf_uk<- st_intersection(pm_1985_sf, uk)

#plot 1985
pm_1985_plot<- ggplot() +
  geom_sf(data = pm_1985_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1985 PM 2.5")+
  theme(legend.position="none")
print(pm_1985_plot)
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
pm_1990_sf_uk<- st_intersection(pm_1990_sf, uk)

#plot 1990
pm_1990_plot<- ggplot() +
  geom_sf(data = pm_1990_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
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
##import data - 1995

pm_1995 <- read_emep("1995_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1995<- as.data.frame(pm_1995, xy=TRUE, na.rm=TRUE)
pm_1995$SURF_ug_PM25_rh50 <- as.numeric(pm_1995$SURF_ug_PM25_rh50)


#making df into raster
ext <- ext(min(pm_1995$x), max(pm_1995$x), min(pm_1995$y), max(pm_1995$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_1995, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_1995<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_1995) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_1995_sf <- st_as_sf(pm_1995, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
pm_1995_sf_uk<- st_intersection(pm_1995_sf, uk)

#plot 1995
pm_1995_plot<- ggplot() +
  geom_sf(data = pm_1995_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )   +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("1995 PM 2.5")+
  theme(legend.position="none")
print(pm_1995_plot)


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
pm_2000_sf_uk<- st_intersection(pm_2000_sf, uk)

#plot 2000
pm_2000_plot<- ggplot() +
  geom_sf(data = pm_2000_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
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
##import data - 2005

pm_2005 <- read_emep("2005_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2005<- as.data.frame(pm_2005, xy=TRUE, na.rm=TRUE)
pm_2005$SURF_ug_PM25_rh50 <- as.numeric(pm_2005$SURF_ug_PM25_rh50)

#making df into raster
ext <- ext(min(pm_2005$x), max(pm_2005$x), min(pm_2005$y), max(pm_2005$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_2005, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_2005<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_2005) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_2005_sf <- st_as_sf(pm_2005, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
pm_2005_sf_uk<- st_intersection(pm_2005_sf, uk)

#plot 2005
pm_2005_plot<- ggplot() +
  geom_sf(data = pm_2005_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  ) +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("2005 PM 2.5")+
  theme(legend.position="none")
print(pm_2005_plot)

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
    colors = pal,
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
##import data - 2015

pm_2015 <- read_emep("2015_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2015<- as.data.frame(pm_2015, xy=TRUE, na.rm=TRUE)
pm_2015$SURF_ug_PM25_rh50 <- as.numeric(pm_2015$SURF_ug_PM25_rh50)

#making df into raster
ext <- ext(min(pm_2015$x), max(pm_2015$x), min(pm_2015$y), max(pm_2015$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(pm_2015, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_2015<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_2015) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")

#making df into sf
pm_2015_sf <- st_as_sf(pm_2015, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
pm_2015_sf_uk<- st_intersection(pm_2015_sf, uk)

#plot 2015
pm_2015_plot<- ggplot() +
  geom_sf(data = pm_2015_sf_uk, aes(color=SURF_ug_PM25_rh50))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  ) +
  theme_minimal()+
  labs(color = "PM 2.5 µg/m3 ") +
  ggtitle("2015 PM 2.5")+
  theme(legend.position="none")
print(pm_2015_plot)

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
    colors = pal,
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
pm_2020_1960 <- merge(pm_1960, pm_2020, by = c("x", "y"))

# Calculate the difference in pm values
pm_2020_1960$pm_difference <- pm_2020_1960$SURF_ug_PM25_rh50.x - pm_2020_1960$SURF_ug_PM25_rh50.y
pm_2020_1960 <- subset(pm_2020_1960, select = -SURF_ug_PM25_rh50.y)
pm_2020_1960 <- subset(pm_2020_1960, select = -SURF_ug_PM25_rh50.x)
names(pm_2020_1960) <- c("x", "y", "pm_difference")


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
pm_2020_1960_sf_uk<- st_intersection(pm_2020_1960_sf, uk)
max(pm_2020_1960_sf_uk$pm_difference)
#Δ

custom_breaks <- c(1, 20, 40, 60,80, 100, 120)
custom_limits<- c(1, 120)
#plot 2020
pm_2020_1960_plot<- ggplot() +
  geom_sf(data = pm_2020_1960_sf_uk, aes(color=pm_difference))+
  geom_sf(data= uk, fill=NA, color="black", size=5)+
  scale_color_gradientn(
    colors = pal,
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
