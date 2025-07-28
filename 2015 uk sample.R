#3/7/24
#Intro to nc files

#install packages
install.packages("ncdf4")
install.packages("tidync")
install.packages("maps")
install.packages("mapsdata")
install.packages("rworldmap")
install.packages("raster")
install.packages("sf")
install.packages("terra")
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
library(openair)

setwd("C:/Users/vanes/Downloads/ukceh")

#read 2015 sample data
sample2015<- tidync("2015_UK_conc.nc")
print(sample2015)
nc_file<- "2015_UK_conc.nc"
#inspecting data
ncmeta::nc_grids("2015_UK_conc.nc")
ncmeta::nc_vars("2015_UK_conc.nc")


#producing map
        (sample_data<- sample2015 %>% hyper_array())
        length(sample_data)
        names(sample_data)
        dim(sample_data[[1]])
        image(sample_data[[1]])
        image(sample_data[[2]])
        lapply(sample_data, dim)
        (trans<- attr(sample_data, "transforms"))
        image(trans$i$i, trans$j$j, sample_data[[1]])
#long and lat
i<- ncvar_get(nc_file, "i")
j<- ncvar_get(nc_file, "j")
nclat<- ncvar_get(nc_file, "lat")
nclong<- ncvar_get(nc_file, "lon")
nco3<- ncvar_get(nc_file, "SURF_ppb_O3")
nc_open(nc_file)
nco3_df<- expand.grid(j=j, i=i)
dim(lat)
dim(lon)
length(i)
length(j)
lat_matrix <- matrix(lat, nrow = length(unique(j)), ncol = length(unique(i)))
lon_matrix <- matrix(lon, nrow = length(unique(j)), ncol = length(unique(i)))
# Create a data frame with grid indices
grid_indices <- expand.grid(j = 1:nrow(lat_matrix), i = 1:ncol(lat_matrix))

# Match grid indices to latitude and longitude values
grid_indices$lat <- as.vector(lat_matrix[grid_indices$j, grid_indices$i])
grid_indices$lon <- as.vector(lon_matrix[grid_indices$j, grid_indices$i])

# Extract and reshape the data variable
o3_matrix <- matrix(nco3_df, nrow = nrow(lat_matrix), ncol = ncol(lat_matrix))


#more useful map
o3data<- tidync("2015_UK_conc.nc") %>%
  hyper_tibble()

ggplot(o3nc_df, aes(x=lon,y=lat, fill=SURF_ppb_O3)) +
  geom_raster() + coord_fixed()+theme_minimal()+
  labs(title="O3 concentrations over the UK, 2015", x="Longitude", y="Latitude", fill="O3 (ppb)")
print(o3plot)
o3plot+ geom_point(data=top_o3_locations, aes(x=longitude, y=latitude), color="red", size=3)
 


geom_polygon( data= worldmap, aes( x=long, y=lat, group=group),
                color="black", fill="NA" ) +
  coord_fixed(xlim = c(-10,3), 
              ylim = c(50.3, 59))
  
worldmap = map_data('world')

# Make a vector of country names
UK <- map_data("world2Hires", region = "UK")

# Call the vector in `borders()`
  world <- getMap(resolution = "low")
uk_map <- world[world@data$ADMIN %in% uk,]

#openair
install.packages("openair")
install.packages("openairmaps")
library(openair)
library(openairmaps)

#filter sites of o3 and pm2.5
sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3', 'PM2.5','PM25'))
dplyr::glimpse(sites)
write.csv(sites, 'sites.csv')
aurnsites<- read.csv("sites.csv")
print(sites)

networkMap(source= "aurn") 

o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
write.csv(o3_sites, 'o3_sites.csv')

pm_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('PM2.5','PM25'))
write.csv(pm_sites, 'pm_sites.csv')
