#READ EMEP PM


######


#working code
reademep_pm <- read_emep("2015_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)
reademep_pm_df<- as.data.frame(reademep_pm, xy=TRUE, na.rm=TRUE)
reademep_pm_df$SURF_ug_PM25_rh50 <- as.numeric(reademep_pm_df$SURF_ug_PM25_rh50)

#making df into raster
ext <- ext(min(reademep_pm_df$x), max(reademep_pm_df$x), min(reademep_pm_df$y), max(reademep_pm_df$y))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(reademep_pm_df, geom = c("x", "y"))
r <- rasterize(points, r, field = "SURF_ug_PM25_rh50", fun = "mean")
pm_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(pm_df) <- c("longitude", "latitude", "SURF_ug_PM25_rh50")
pm_df<-na.omit(pm_df)

#import aurn
pm_sites<- importMeta(source= "aurn", all=TRUE, year="2015") %>%
  filter(variable %in% c('PM2.5', 'PM25'))
all_sites <- unique(pm_sites$code)
pmsites_values<- importAURN(site = all_sites, year = 2015, pollutant =
                              "pm2.5", hc = FALSE)

#avg by code
pmsites_values<- pmsites_values %>%
  group_by(code)%>% summarize(pm2.5=mean(pm2.5, na.rm=TRUE))
pmsites_values_coords <- merge(pmsites_values, pm_sites, by="code", all.x=TRUE)

#making df into sf
pm_df <- st_as_sf(pm_df, coords = c("longitude", "latitude"), crs = 4326)

pm_df<- st_intersection(pm_df, uk)


custom_breaks <- c(0, 5,10, 15,20)
custom_colors <- rainbow(length(custom_breaks) - 1)
custom_limits <- c(0, 20)

#plot
ggplot() +
  geom_sf(data = pm_df, aes(color = SURF_ug_PM25_rh50)) +
  geom_sf(data = uk, fill = NA, color = "black", size = 0.7) +
  geom_point(data = pmsites_values_coords, aes(x = longitude, y = latitude, fill = pm2.5),color="black", pch=21, size=2) +
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
  ggtitle("UK 2015 pm2.5 µg/m3")


    