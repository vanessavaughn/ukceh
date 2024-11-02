#### read_emep function
library(dplyr)
library(stars)
library(sf)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(scales)
library(tidync)
library(stringr)
library(tidyr)
library(ncdf4)
library(smoothr)
install.packages("smoothr")
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
