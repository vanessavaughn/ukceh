### pm 2.5 plots
###########1960
sample_pm60<- tidync("1960_UK_conc.nc")
#import pm 1960 data
pm_1960 <- read_emep("1960_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1960<- as.data.frame(pm_1960, xy=TRUE, na.rm=TRUE)
names(pm_1960)<- c('longitude', 'latitude', 'pm')
pm_1960$pm <- as.numeric(pm_1960$pm)


ext<- ext(min(pm_1960$longitude), max(pm_1960$longitude), min(pm_1960$latitude), max(pm_1960$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1960, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map60pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map60pm_model<- st_intersection(map60pm_model, uk)
min(map60pm_model$pm)

map60o3_obs<- st_intersection(map60o3_obs, uk)
max(map15o3_obs$o3_obs)

scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map60pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 140))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1960 annual average PM2.5 ug/m3 - Model vs. Observations")

##########1965


#import pm 1965 data
pm_1965 <- read_emep("1965_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1965<- as.data.frame(pm_1965, xy=TRUE, na.rm=TRUE)
names(pm_1965)<- c('longitude', 'latitude', 'pm')
pm_1965$pm <- as.numeric(pm_1965$pm)


ext<- ext(min(pm_1965$longitude), max(pm_1965$longitude), min(pm_1965$latitude), max(pm_1965$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1965, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map65pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map65pm_model<- st_intersection(map65pm_model, uk)
max(map65pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map65pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 140))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1965 annual average PM2.5 ug/m3 - Model vs. Observations")
##########1970


#import pm 1970 data
pm_1970 <- read_emep("1970_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1970<- as.data.frame(pm_1970, xy=TRUE, na.rm=TRUE)
names(pm_1970)<- c('longitude', 'latitude', 'pm')
pm_1970$pm <- as.numeric(pm_1970$pm)


ext<- ext(min(pm_1970$longitude), max(pm_1970$longitude), min(pm_1970$latitude), max(pm_1970$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1970, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map70pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map70pm_model<- st_intersection(map70pm_model, uk)
min(map70pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map65pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1970 annual average PM2.5 ug/m3 - Model vs. Observations")
##########1975


#import pm 1975 data
pm_1975 <- read_emep("1975_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1975<- as.data.frame(pm_1975, xy=TRUE, na.rm=TRUE)
names(pm_1975)<- c('longitude', 'latitude', 'pm')
pm_1975$pm <- as.numeric(pm_1975$pm)




ext<- ext(min(pm_1975$longitude), max(pm_1975$longitude), min(pm_1975$latitude), max(pm_1975$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1975, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map75pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map75pm_model<- st_intersection(map75pm_model, uk)
max(map75pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map75pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1975 annual average PM2.5 ug/m3 - Model vs. Observations")


######1980


#import pm 1980 data
pm_1980 <- read_emep("1980_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1980<- as.data.frame(pm_1980, xy=TRUE, na.rm=TRUE)
names(pm_1980)<- c('longitude', 'latitude', 'pm')
pm_1980$pm <- as.numeric(pm_1980$pm)




ext<- ext(min(pm_1980$longitude), max(pm_1980$longitude), min(pm_1980$latitude), max(pm_1980$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1980, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map80pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map80pm_model<- st_intersection(map80pm_model, uk)
max(map80pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map80pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1980 annual average PM2.5 ug/m3 - Model vs. Observations")


######1985


#import pm 1985 data
pm_1985 <- read_emep("1985_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1985<- as.data.frame(pm_1985, xy=TRUE, na.rm=TRUE)
names(pm_1985)<- c('longitude', 'latitude', 'pm')
pm_1985$pm <- as.numeric(pm_1985$pm)




ext<- ext(min(pm_1985$longitude), max(pm_1985$longitude), min(pm_1985$latitude), max(pm_1985$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1985, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map85pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map85pm_model<- st_intersection(map85pm_model, uk)
max(map85pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map85pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1985 annual average PM2.5 ug/m3 - Model vs. Observations")



######1990


#import pm 1990 data
pm_1990 <- read_emep("1990_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1990<- as.data.frame(pm_1990, xy=TRUE, na.rm=TRUE)
names(pm_1990)<- c('longitude', 'latitude', 'pm')
pm_1990$pm <- as.numeric(pm_1990$pm)




ext<- ext(min(pm_1990$longitude), max(pm_1990$longitude), min(pm_1990$latitude), max(pm_1990$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1990, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map90pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map90pm_model<- st_intersection(map90pm_model, uk)
max(map90pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map90pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1990 annual average PM2.5 ug/m3 - Model vs. Observations")


######1995


#import pm 1995 data
pm_1995 <- read_emep("1995_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_1995<- as.data.frame(pm_1995, xy=TRUE, na.rm=TRUE)
names(pm_1995)<- c('longitude', 'latitude', 'pm')
pm_1995$pm <- as.numeric(pm_1995$pm)


ext<- ext(min(pm_1995$longitude), max(pm_1995$longitude), min(pm_1995$latitude), max(pm_1995$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_1995, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map95pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map95pm_model<- st_intersection(map95pm_model, uk)
max(map95pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map95pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3")+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("1995 annual average PM2.5 ug/m3 - Model vs. Observations")

######2000


#import pm 2000 data
pm_2000 <- read_emep("2000_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2000<- as.data.frame(pm_2000, xy=TRUE, na.rm=TRUE)
names(pm_2000)<- c('longitude', 'latitude', 'pm')
pm_2000$pm <- as.numeric(pm_2000$pm)

#import 2000 pm observations
pm00_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2000, pollutant = "pm2.5", hc=FALSE)

#get observation coords
pm_sites<- importMeta(source= "aurn", all=TRUE)%>%
  filter(variable %in% c('PM2.5'))
pm_sites<- data.frame( code= pm_sites$code, 
                       longitude = pm_sites$longitude,
                       latitude = pm_sites$latitude)
pm00_obs_c <- merge(pm00_obs, pm_sites, by= "code", all.x=TRUE, na.rm=TRUE)

pm00_obs<- data.frame(longitude = pm00_obs_c$longitude,
                      latitude = pm00_obs_c$latitude,
                      time = pm00_obs_c$date,
                      pm = pm00_obs_c$pm,
                      code = pm00_obs_c$code)
pm00_obs<- na.omit(pm00_obs)
pm00_obs$date <- as.POSIXct(pm00_obs$time)
pm00_obs<- pm00_obs %>%
  group_by(longitude, latitude, code) %>% summarize(`pm`= mean(`pm`, na.rm = TRUE))


ext<- ext(min(pm_2000$longitude), max(pm_2000$longitude), min(pm_2000$latitude), max(pm_2000$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_2000, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map00pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)
map00pm_obs <- st_as_sf(pm00_obs, coords = c("longitude", "latitude"), crs = 4326)




#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map00pm_model<- st_intersection(map00pm_model, uk)
map00pm_obs<- st_intersection(map00pm_obs, uk)

max(map95pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map00pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map00pm_obs, aes(fill = `pm`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("2000 annual average PM2.5 ug/m3 - Model vs. Observations")


######2005


#import pm 2005 data
pm_2005 <- read_emep("2005_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2005<- as.data.frame(pm_2005, xy=TRUE, na.rm=TRUE)
names(pm_2005)<- c('longitude', 'latitude', 'pm')
pm_2005$pm <- as.numeric(pm_2005$pm)

#import 2005 pm observations
pm05_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2005, pollutant = "pm2.5", hc=FALSE)

#get observation coords

pm05_obs_c <- merge(pm05_obs, pm_sites, by= "code", all.x=TRUE, na.rm=TRUE)

pm05_obs<- data.frame(longitude = pm05_obs_c$longitude,
                      latitude = pm05_obs_c$latitude,
                      time = pm05_obs_c$date,
                      pm = pm05_obs_c$pm,
                      code = pm05_obs_c$code)
pm05_obs<- na.omit(pm05_obs)
pm05_obs$date <- as.POSIXct(pm05_obs$time)
pm05_obs<- pm05_obs %>%
  group_by(longitude, latitude, code) %>% summarize(`pm`= mean(`pm`, na.rm = TRUE))


ext<- ext(min(pm_2005$longitude), max(pm_2005$longitude), min(pm_2005$latitude), max(pm_2005$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_2005, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map05pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)
map05pm_obs <- st_as_sf(pm05_obs, coords = c("longitude", "latitude"), crs = 4326)




#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map05pm_model<- st_intersection(map05pm_model, uk)
map05pm_obs<- st_intersection(map05pm_obs, uk)

max(map05pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map05pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map05pm_obs, aes(fill = `pm`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("2005 annual average PM2.5 ug/m3 - Model vs. Observations")

######2010


#import pm 2010 data
pm_2010 <- read_emep("2010_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2010<- as.data.frame(pm_2010, xy=TRUE, na.rm=TRUE)
names(pm_2010)<- c('longitude', 'latitude', 'pm')
pm_2010$pm <- as.numeric(pm_2010$pm)

#import 2010 pm observations
pm10_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2010, pollutant = "pm2.5", hc=FALSE)

#get observation coords

pm10_obs_c <- merge(pm10_obs, pm_sites, by= "code", all.x=TRUE, na.rm=TRUE)

pm10_obs<- data.frame(longitude = pm10_obs_c$longitude,
                      latitude = pm10_obs_c$latitude,
                      time = pm10_obs_c$date,
                      pm = pm10_obs_c$pm,
                      code = pm10_obs_c$code)
pm10_obs<- na.omit(pm10_obs)
pm10_obs$date <- as.POSIXct(pm10_obs$time)
pm10_obs<- pm10_obs %>%
  group_by(longitude, latitude, code) %>% summarize(`pm`= mean(`pm`, na.rm = TRUE))


ext<- ext(min(pm_2010$longitude), max(pm_2010$longitude), min(pm_2010$latitude), max(pm_2010$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_2010, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map10pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)
map10pm_obs <- st_as_sf(pm10_obs, coords = c("longitude", "latitude"), crs = 4326)




#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map10pm_model<- st_intersection(map10pm_model, uk)
map10pm_obs<- st_intersection(map10pm_obs, uk)

max(map10pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map10pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map10pm_obs, aes(fill = `pm`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("2010 annual average PM2.5 ug/m3 - Model vs. Observations")


######2015


#import pm 2015 data
pm_2015 <- read_emep("2015_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2015<- as.data.frame(pm_2015, xy=TRUE, na.rm=TRUE)
names(pm_2015)<- c('longitude', 'latitude', 'pm')
pm_2015$pm <- as.numeric(pm_2015$pm)

#import 2015 pm observations
pm15_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2015, pollutant = "pm2.5", hc=FALSE)

#get observation coords

pm15_obs_c <- merge(pm15_obs, pm_sites, by= "code", all.x=TRUE, na.rm=TRUE)

pm15_obs<- data.frame(longitude = pm15_obs_c$longitude,
                      latitude = pm15_obs_c$latitude,
                      time = pm15_obs_c$date,
                      pm = pm15_obs_c$pm,
                      code = pm15_obs_c$code)
pm15_obs<- na.omit(pm15_obs)
pm15_obs$date <- as.POSIXct(pm15_obs$time)
pm15_obs<- pm15_obs %>%
  group_by(longitude, latitude, code) %>% summarize(`pm`= mean(`pm`, na.rm = TRUE))


ext<- ext(min(pm_2015$longitude), max(pm_2015$longitude), min(pm_2015$latitude), max(pm_2015$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_2015, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map15pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)
map15pm_obs <- st_as_sf(pm15_obs, coords = c("longitude", "latitude"), crs = 4326)




#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map15pm_model<- st_intersection(map15pm_model, uk)
map15pm_obs<- st_intersection(map15pm_obs, uk)

max(map15pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map15pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map15pm_obs, aes(fill = `pm`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 50))+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("2015 annual average PM2.5 ug/m3 - Model vs. Observations")


######2020


#import pm 2020 data
pm_2020 <- read_emep("2020_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

pm_2020<- as.data.frame(pm_2020, xy=TRUE, na.rm=TRUE)
names(pm_2020)<- c('longitude', 'latitude', 'pm')
pm_2020$pm <- as.numeric(pm_2020$pm)

#import 2020 pm observations
pm20_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2020, pollutant = "pm2.5", hc=FALSE)

#get observation coords

pm20_obs_c <- merge(pm20_obs, pm_sites, by= "code", all.x=TRUE, na.rm=TRUE)

pm20_obs<- data.frame(longitude = pm20_obs_c$longitude,
                      latitude = pm20_obs_c$latitude,
                      time = pm20_obs_c$date,
                      pm = pm20_obs_c$pm,
                      code = pm20_obs_c$code)
pm20_obs<- na.omit(pm20_obs)
pm20_obs$date <- as.POSIXct(pm20_obs$time)
pm20_obs<- pm20_obs %>%
  group_by(longitude, latitude, code) %>% summarize(`pm`= mean(`pm`, na.rm = TRUE))


ext<- ext(min(pm_2020$longitude), max(pm_2020$longitude), min(pm_2020$latitude), max(pm_2020$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(pm_2020, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "pm", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "pm")


#making df into sf
map20pm_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)
map20pm_obs <- st_as_sf(pm20_obs, coords = c("longitude", "latitude"), crs = 4326)




#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map20pm_model<- st_intersection(map20pm_model, uk)
map20pm_obs<- st_intersection(map20pm_obs, uk)

max(map20pm_model$pm)



scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map20pm_model, aes(color=pm))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map20pm_obs, aes(fill = `pm`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="Annual Average PM 2.5 ug/m3", limits = c(0, 30))+
  scale_color_gradientn(colours = pal,name="Annual Average PM 2.5 ug/m3", limits = c(0, 30),oob=scales::squish)+
  theme_minimal()+
  labs(color = "PM 2.5 ug/m3") +
  ggtitle("2020 annual average PM2.5 ug/m3 - Model vs. Observations")

