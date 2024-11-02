#READ EMEP O3

#####################


install.packages("tidync")
install.packages("stars")
install.packages("stringr")
install.packages("terra")
install.packages("openair")
install.packages("ggplot2")

library(dplyr)
library(tidync)
library(stars)
library(stringr)
library(terra)
library(openair)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(sf)
library(magrittr)
library(gridExtra )
library(lubridate)




read_emep = function(emep_fname, emep_crs, var = 'all', dims = c('i', 'j', 'time'), proxy = T) {
  #reads emep data from a provided file name - value is a star_proxy object!!!
  #crs needs to be given as stars automatically expects Earth to be an ellipsoid
  #if var == 'all' it loads those vars which have dimensions set in dims
  #can subset on time dimension but only for vars with 2 spatial and 1 time dimension
  
  if (is.null(emep_fname)) return(NULL)
  
  emep_tidync = emep_fname %>%
    tidync()
  emep_vars = emep_tidync$variable$name
  
  if (all(var != 'all')) {
    selected_var = base::intersect(emep_vars, var)
    if (length(selected_var) == 0) {
      return(NULL)
    }
  } else {
    selected_dims = emep_tidync$dimension %>%
      filter(name %in% dims) %>%
      pull(id) %>%
      sort() %>%
      str_c('D', .) %>%
      str_c(collapse = ',')
    
    selected_var = emep_tidync$grid %>%
      unnest(col = variables) %>%
      filter(grid == selected_dims) %>%
      pull(variable)
  }
  
  emep_data = read_stars(emep_fname, sub = selected_var, proxy = T, RasterIO = list(nXOff = 1, nYOff = 1)) %>%
    st_set_crs(emep_crs)
  

  if (proxy == F) {
    emep_data = emep_data %>%
      st_as_stars(curvilinear = NULL) #we don't want the output in curvilinear grid
  }
  emep_data
}



######


#working code
reademep_o3 <- read_emep("2015_UK_conc.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

reademep_o3_df<- as.data.frame(reademep_o3, xy=TRUE, na.rm=TRUE)
reademep_o3_df$SURF_ppb_O3 <- as.numeric(reademep_o3_df$SURF_ppb_O3)

#making df into raster
ext <- ext(min(reademep_o3_df$x), max(reademep_o3_df$x), min(reademep_o3_df$y), max(reademep_o3_df$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(reademep_o3_df, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ppb_O3", fun = "mean")
o3_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(o3_df) <- c("longitude", "latitude", "SURF_ppb_O3")


#import aurn
o3_sites<- importMeta(source= "aurn", all=TRUE, year="2015") %>%
  filter(variable %in% c('O3'))

all_sites <- unique(o3_sites$code)

o3sites_values<- importAURN(site = all_sites, year = 2015, pollutant =
                              "o3", hc = FALSE)


#avg by code
o3sites_values<- o3sites_values %>%
  group_by(code)%>% summarize(o3=mean(o3, na.rm=TRUE))
o3sites_values_coords <- merge(o3sites_values, o3_sites, by="code", all.x=TRUE)

#ppb conversion
ozone<- 0.51
o3sites_values_coords<- o3sites_values_coords%>%
  mutate(o3=o3*ozone)

#making df into sf
o3_df <- st_as_sf(o3_df, coords = c("longitude", "latitude"), crs = 4326)

#cropping to uk
uk <- ne_countries(returnclass = "sf", country = "United Kingdom")
o3_df<- st_intersection(o3_df, uk)

#plot
uk<- ne_countries(scale="large", returnclass="sf", country="United Kingdom")

custom_breaks <- c(0, 10, 20, 30, 40)
custom_colors <- rainbow(length(custom_breaks) - 1)
custom_limits <- c(0, 40)


 

ggplot() +
  geom_sf(data = o3_df, aes(color = SURF_ppb_O3)) +
  geom_sf(data = uk, fill = NA, color = "black", size = 0.7) +
  geom_point(data = o3sites_values_coords, aes(x = longitude, y = latitude, fill = o3),color="black", pch=21, size=2) +
  scale_color_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish,
    guide = "colourbar"
  ) +
  scale_fill_gradientn(
    colors = colorRampPalette(rev(custom_colors))(100),
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish,
    guide = "colourbar"
  ) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()+
  ggtitle("UK 2015 O3 ppb")

