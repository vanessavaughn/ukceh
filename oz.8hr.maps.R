###1960
sample_oz60<- tidync("1960_UK_MDA8.nc")


#import ozone 1960 data
oz_1960 <- read_emep("1960_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1960<- as.data.frame(oz_1960, xy=TRUE, na.rm=TRUE)
names(oz_1960)<- c('longitude', 'latitude', 'date', 'oz')
oz_1960$oz <- as.numeric(oz_1960$oz)


colnames(oz_1960)<- c("longitude","latitude", "date","8hr_o3_model")


oz_1960.avg<- oz_1960 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1960.avg$longitude), max(oz_1960$longitude), min(oz_1960.avg$latitude), max(oz_1960$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1960.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map60o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map60o3_model<- st_intersection(map60o3_model, uk)
max(map60o3_model$o3_model)

map60o3_obs<- st_intersection(map60o3_obs, uk)
max(map15o3_obs$o3_obs)

scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map60o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  #geom_sf(data = map75o3_obs, aes(color = `X8hr_o3_obs`), size= 2)+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum")+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1960 annual average 8 hour maximum O3 ppb - Model vs. Observations")


###1965

#import ozone 1965 data
oz_1965 <- read_emep("1965_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1965<- as.data.frame(oz_1965, xy=TRUE, na.rm=TRUE)
names(oz_1965)<- c('longitude', 'latitude', 'date', 'oz')
oz_1965$oz <- as.numeric(oz_1965$oz)


colnames(oz_1965)<- c("longitude","latitude", "date","8hr_o3_model")


oz_1965.avg<- oz_1965 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1965.avg$longitude), max(oz_1965$longitude), min(oz_1965.avg$latitude), max(oz_1965$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1965.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map65o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map65o3_model<- st_intersection(map65o3_model, uk)


scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map65o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum")+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1965 annual average 8 hour maximum O3 ppb - Model vs. Observations")


###1970

#import ozone 1970 data
oz_1970 <- read_emep("1970_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1970<- as.data.frame(oz_1970, xy=TRUE, na.rm=TRUE)
names(oz_1970)<- c('longitude', 'latitude', 'date', 'oz')
oz_1970$oz <- as.numeric(oz_1970$oz)


colnames(oz_1970)<- c("longitude","latitude", "date","8hr_o3_model")


oz_1970.avg<- oz_1970 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1970.avg$longitude), max(oz_1970$longitude), min(oz_1970.avg$latitude), max(oz_1970$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1970.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map70o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)



#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map70o3_model<- st_intersection(map70o3_model, uk)


scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map70o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum")+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1970 annual average 8 hour maximum O3 ppb - Model vs. Observations")


###1975

#import ozone 1975 data
oz_1975 <- read_emep("1975_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1975<- as.data.frame(oz_1975, xy=TRUE, na.rm=TRUE)
names(oz_1975)<- c('longitude', 'latitude', 'date', 'oz')
oz_1975$oz <- as.numeric(oz_1975$oz)


#import 1975 ozone observations
oz75_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=1975, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz75_obs_c <- merge(oz75_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz75_obs<- data.frame(longitude = oz75_obs_c$longitude,
                      latitude = oz75_obs_c$latitude,
                      time = oz75_obs_c$date,
                      oz = oz75_obs_c$o3,
                      code = oz75_obs_c$code)

oz75_obs$date <- as.POSIXct(oz75_obs$time)
# Calculate 8-hour mean
oz75_obs <- oz75_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz75_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz75_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz75_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_1975)<- c("longitude","latitude", "date","8hr_o3_model")

oz75_obs_8hr<- oz75_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz75_obs_8hr.avg<- oz75_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_1975.avg<- oz_1975 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1975.avg$longitude), max(oz_1975$longitude), min(oz_1975.avg$latitude), max(oz_1975$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1975.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map75o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map75o3_obs <- st_as_sf(oz75_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map75o3_model<- st_intersection(map75o3_model, uk)
max(map75o3_model$o3_model)

map75o3_obs<- st_intersection(map75o3_obs, uk)
max(map15o3_obs$o3_obs)

scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map75o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map75o3_obs, color="black", size= 2.5)+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum")+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1975 annual average 8 hour maximum O3 ppb - Model vs. Observations")



###1980

#import ozone 1980 data
oz_1980 <- read_emep("1980_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1980<- as.data.frame(oz_1980, xy=TRUE, na.rm=TRUE)
names(oz_1980)<- c('longitude', 'latitude', 'date', 'oz')
oz_1980$oz <- as.numeric(oz_1980$oz)


#import 1980 ozone observations
oz80_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=1980, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz80_obs_c <- merge(oz80_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz80_obs<- data.frame(longitude = oz80_obs_c$longitude,
                      latitude = oz80_obs_c$latitude,
                      time = oz80_obs_c$date,
                      oz = oz80_obs_c$o3,
                      code = oz80_obs_c$code)

oz80_obs$date <- as.POSIXct(oz80_obs$time)
# Calculate 8-hour mean
oz80_obs <- oz80_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz80_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz80_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz80_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_1980)<- c("longitude","latitude", "date","8hr_o3_model")

oz80_obs_8hr<- oz80_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz80_obs_8hr.avg<- oz80_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_1980.avg<- oz_1980 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1980.avg$longitude), max(oz_1980$longitude), min(oz_1980.avg$latitude), max(oz_1980$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1980.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map80o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map80o3_obs <- st_as_sf(oz80_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map80o3_model<- st_intersection(map80o3_model, uk)


map80o3_obs<- st_intersection(map80o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map75o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map75o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,52))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1975 annual average 8 hour maximum O3 ppb - Model vs. Observations")

###1985

#import ozone 1985 data
oz_1985 <- read_emep("1985_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1985<- as.data.frame(oz_1985, xy=TRUE, na.rm=TRUE)
names(oz_1985)<- c('longitude', 'latitude', 'date', 'oz')
oz_1985$oz <- as.numeric(oz_1985$oz)


#import 1985 ozone observations
oz85_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=1985, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz85_obs_c <- merge(oz85_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz85_obs<- data.frame(longitude = oz85_obs_c$longitude,
                      latitude = oz85_obs_c$latitude,
                      time = oz85_obs_c$date,
                      oz = oz85_obs_c$o3,
                      code = oz85_obs_c$code)

oz85_obs$date <- as.POSIXct(oz85_obs$time)
# Calculate 8-hour mean
oz85_obs <- oz85_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz85_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz85_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz85_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_1985)<- c("longitude","latitude", "date","8hr_o3_model")

oz85_obs_8hr<- oz85_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz85_obs_8hr.avg<- oz85_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_1985.avg<- oz_1985 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1985.avg$longitude), max(oz_1985$longitude), min(oz_1985.avg$latitude), max(oz_1985$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1985.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map85o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map85o3_obs <- st_as_sf(oz85_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map85o3_model<- st_intersection(map85o3_model, uk)


map85o3_obs<- st_intersection(map85o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map85o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map85o3_obs, aes(color = `X8hr_o3_obs`), size= 2)+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum")+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 50))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1985 annual average 8 hour maximum O3 ppb - Model vs. Observations")

###1990

#import ozone 1990 data
oz_1990 <- read_emep("1990_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1990<- as.data.frame(oz_1990, xy=TRUE, na.rm=TRUE)
names(oz_1990)<- c('longitude', 'latitude', 'date', 'oz')
oz_1990$oz <- as.numeric(oz_1990$oz)


#import 1990 ozone observations
oz90_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=1990, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz90_obs_c <- merge(oz90_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz90_obs<- data.frame(longitude = oz90_obs_c$longitude,
                      latitude = oz90_obs_c$latitude,
                      time = oz90_obs_c$date,
                      oz = oz90_obs_c$o3,
                      code = oz90_obs_c$code)

oz90_obs$date <- as.POSIXct(oz90_obs$time)
# Calculate 8-hour mean
oz90_obs <- oz90_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz90_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz90_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz90_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_1990)<- c("longitude","latitude", "date","8hr_o3_model")

oz90_obs_8hr<- oz90_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz90_obs_8hr.avg<- oz90_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_1990.avg<- oz_1990 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1990.avg$longitude), max(oz_1990$longitude), min(oz_1990.avg$latitude), max(oz_1990$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1990.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map90o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map90o3_obs <- st_as_sf(oz90_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map90o3_model<- st_intersection(map90o3_model, uk)


map90o3_obs<- st_intersection(map90o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map90o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map90o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,45))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1990 annual average 8 hour maximum O3 ppb - Model vs. Observations")

###1995

#import ozone 1995 data
oz_1995 <- read_emep("1995_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_1995<- as.data.frame(oz_1995, xy=TRUE, na.rm=TRUE)
names(oz_1995)<- c('longitude', 'latitude', 'date', 'oz')
oz_1995$oz <- as.numeric(oz_1995$oz)


#import 1995 ozone observations
oz95_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=1995, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz95_obs_c <- merge(oz95_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz95_obs<- data.frame(longitude = oz95_obs_c$longitude,
                      latitude = oz95_obs_c$latitude,
                      time = oz95_obs_c$date,
                      oz = oz95_obs_c$o3,
                      code = oz95_obs_c$code)

oz95_obs$date <- as.POSIXct(oz95_obs$time)
# Calculate 8-hour mean
oz95_obs <- oz95_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz95_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz95_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz95_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_1995)<- c("longitude","latitude", "date","8hr_o3_model")

oz95_obs_8hr<- oz95_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz95_obs_8hr.avg<- oz95_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_1995.avg<- oz_1995 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_1995.avg$longitude), max(oz_1995$longitude), min(oz_1995.avg$latitude), max(oz_1995$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_1995.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map95o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map95o3_obs <- st_as_sf(oz95_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map95o3_model<- st_intersection(map95o3_model, uk)


map95o3_obs<- st_intersection(map95o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map95o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map95o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,45))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("1995 annual average 8 hour maximum O3 ppb - Model vs. Observations")


###2000

#import ozone 2000 data
oz_2000 <- read_emep("2000_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_2000<- as.data.frame(oz_2000, xy=TRUE, na.rm=TRUE)
names(oz_2000)<- c('longitude', 'latitude', 'date', 'oz')
oz_2000$oz <- as.numeric(oz_2000$oz)


#import 2000 ozone observations
oz00_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2000, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz00_obs_c <- merge(oz00_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz00_obs<- data.frame(longitude = oz00_obs_c$longitude,
                      latitude = oz00_obs_c$latitude,
                      time = oz00_obs_c$date,
                      oz = oz00_obs_c$o3,
                      code = oz00_obs_c$code)

oz00_obs$date <- as.POSIXct(oz00_obs$time)
# Calculate 8-hour mean
oz00_obs <- oz00_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz00_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz00_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz00_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_2000)<- c("longitude","latitude", "date","8hr_o3_model")

oz00_obs_8hr<- oz00_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz00_obs_8hr.avg<- oz00_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_2000.avg<- oz_2000 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_2000.avg$longitude), max(oz_2000$longitude), min(oz_2000.avg$latitude), max(oz_2000$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_2000.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map00o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map00o3_obs <- st_as_sf(oz00_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map00o3_model<- st_intersection(map00o3_model, uk)


map00o3_obs<- st_intersection(map00o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map00o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map00o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,45))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2000 annual average 8 hour maximum O3 ppb - Model vs. Observations")



###2005

#import ozone 2005 data
oz_2005 <- read_emep("2005_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_2005<- as.data.frame(oz_2005, xy=TRUE, na.rm=TRUE)
names(oz_2005)<- c('longitude', 'latitude', 'date', 'oz')
oz_2005$oz <- as.numeric(oz_2005$oz)


#import 2005 ozone observations
oz05_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2005, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz05_obs_c <- merge(oz05_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz05_obs<- data.frame(longitude = oz05_obs_c$longitude,
                      latitude = oz05_obs_c$latitude,
                      time = oz05_obs_c$date,
                      oz = oz05_obs_c$o3,
                      code = oz05_obs_c$code)

oz05_obs$date <- as.POSIXct(oz05_obs$time)
# Calculate 8-hour mean
oz05_obs <- oz05_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz05_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz05_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz05_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_2005)<- c("longitude","latitude", "date","8hr_o3_model")

oz05_obs_8hr<- oz05_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz05_obs_8hr.avg<- oz05_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_2005.avg<- oz_2005 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_2005.avg$longitude), max(oz_2005$longitude), min(oz_2005.avg$latitude), max(oz_2005$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_2005.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map05o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map05o3_obs <- st_as_sf(oz05_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map05o3_model<- st_intersection(map05o3_model, uk)


map05o3_obs<- st_intersection(map05o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")

ggplot() +
  geom_sf(data = map05o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map05o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,45))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2005 annual average 8 hour maximum O3 ppb - Model vs. Observations")

###2010

#import ozone 2010 data
oz_2010 <- read_emep("2010_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_2010<- as.data.frame(oz_2010, xy=TRUE, na.rm=TRUE)
names(oz_2010)<- c('longitude', 'latitude', 'date', 'oz')
oz_2010$oz <- as.numeric(oz_2010$oz)


#import 2010 ozone observations
oz10_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2010, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz10_obs_c <- merge(oz10_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz10_obs<- data.frame(longitude = oz10_obs_c$longitude,
                      latitude = oz10_obs_c$latitude,
                      time = oz10_obs_c$date,
                      oz = oz10_obs_c$o3,
                      code = oz10_obs_c$code)

oz10_obs$date <- as.POSIXct(oz10_obs$time)
# Calculate 8-hour mean
oz10_obs <- oz10_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz10_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz10_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz10_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_2010)<- c("longitude","latitude", "date","8hr_o3_model")

oz10_obs_8hr<- oz10_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz10_obs_8hr.avg<- oz10_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_2010.avg<- oz_2010 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_2010.avg$longitude), max(oz_2010$longitude), min(oz_2010.avg$latitude), max(oz_2010$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_2010.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map10o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map10o3_obs <- st_as_sf(oz10_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map10o3_model<- st_intersection(map10o3_model, uk)


map10o3_obs<- st_intersection(map10o3_obs, uk)
ggplot() +
  geom_sf(data = map15o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map15o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,45))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2015 annual average 8 hour maximum O3 ppb - Model vs. Observations")


###2020

#import ozone 2005 data
oz_2020 <- read_emep("2020_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_2020<- as.data.frame(oz_2020, xy=TRUE, na.rm=TRUE)
names(oz_2020)<- c('longitude', 'latitude', 'date', 'oz')
oz_2020$oz <- as.numeric(oz_2020$oz)


#import 2020 ozone observations
oz20_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2020, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz20_obs_c <- merge(oz20_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz20_obs<- data.frame(longitude = oz20_obs_c$longitude,
                      latitude = oz20_obs_c$latitude,
                      time = oz20_obs_c$date,
                      oz = oz20_obs_c$o3,
                      code = oz20_obs_c$code)

oz20_obs$date <- as.POSIXct(oz20_obs$time)
# Calculate 8-hour mean
oz20_obs <- oz20_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz20_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz20_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz20_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_2020)<- c("longitude","latitude", "date","8hr_o3_model")

oz20_obs_8hr<- oz20_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz20_obs_8hr.avg<- oz20_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_2020.avg<- oz_2020 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_2020.avg$longitude), max(oz_2020$longitude), min(oz_2020.avg$latitude), max(oz_2020$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_2020.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map20o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map20o3_obs <- st_as_sf(oz20_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map20o3_model<- st_intersection(map20o3_model, uk)


map20o3_obs<- st_intersection(map20o3_obs, uk)

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map20o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map20o3_obs, aes(fill = `X8hr_o3_obs`), size= 2.5, pch=21 , colour="black")+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum", limits = c(10,45))+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", limits = c(10, 45))+
  theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2020 annual average 8 hour maximum O3 ppb - Model vs. Observations")
