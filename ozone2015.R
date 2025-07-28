setwd("C:/Users/vanes/Downloads/ukceh")
library('wesanderson')

###read 2015 ozone data, inspect vars
sample_oz15<- tidync("2015_UK_MDA8.nc")


#import ozone 2015 data
oz_2015 <- read_emep("2015_UK_MDA8.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

oz_2015<- as.data.frame(oz_2015, xy=TRUE, na.rm=TRUE)
names(oz_2015)<- c('longitude', 'latitude', 'date', 'oz')
oz_2015$oz <- as.numeric(oz_2015$oz)


#import 2015 ozone observations
oz15_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                      year=2015, pollutant = "o3", hc=FALSE)

#get observation coords
o3_sites<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('O3'))
o3_sites<- data.frame( code= o3_sites$code, 
                       longitude = o3_sites$longitude,
                       latitude = o3_sites$latitude)
oz15_obs_c <- merge(oz15_obs, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

oz15_obs<- data.frame(longitude = oz15_obs_c$longitude,
                      latitude = oz15_obs_c$latitude,
                      time = oz15_obs_c$date,
                      oz = oz15_obs_c$o3,
                      code = oz15_obs_c$code)

oz15_obs$date <- as.POSIXct(oz15_obs$time)
# Calculate 8-hour mean
oz15_obs <- oz15_obs %>%
  group_by(code) %>%
  arrange(time) %>%
  mutate(
    date_only = as.Date(time),  # Extract the date without time
    oz_8hr_avg = zoo::rollapply(oz, width = 8, FUN = mean, align = "center", fill = NA)
  ) %>%
  ungroup()

# Find the daily maximum 8-hour ozone for each location
df_max_8hr <- oz15_obs %>%
  group_by(code, date_only) %>%
  summarise(max_8hr_o3 = max(oz_8hr_avg, na.rm = TRUE)) %>%
  ungroup()

df_max_8hr <- filter(df_max_8hr, max_8hr_o3 != "-Inf")


oz15_obs_8hr <- merge(df_max_8hr, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)

colnames(oz15_obs_8hr)<- c("code", "date", "8hr_o3_obs","longitude","latitude")
colnames(oz_2015)<- c("longitude","latitude", "date","8hr_o3_model")

oz15_obs_8hr<- oz15_obs_8hr%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

## avg by code

oz15_obs_8hr.avg<- oz15_obs_8hr %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_obs`= mean(`8hr_o3_obs`, na.rm = TRUE))


oz_2015.avg<- oz_2015 %>%
  group_by(longitude, latitude) %>% summarize(`8hr_o3_model`= mean(`8hr_o3_model`, na.rm = TRUE))

ext<- ext(min(oz_2015.avg$longitude), max(oz_2015$longitude), min(oz_2015.avg$latitude), max(oz_2015$latitude))
res<- c(.06, .06)
r<- rast(ext=ext, res=res, crs= "EPSG:4326")
points<- vect(oz_2015.avg, geom=c("longitude","latitude"), crs= "EPSG:4326")
r<- rasterize(points, r, field= "8hr_o3_model", fun="mean")
r_df<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(r_df)<- c("longitude", "latitude", "8hr_o3_model")


#making df into sf
map15o3_model <- st_as_sf(r_df, coords = c("longitude", "latitude"), crs = 4326)

map15o3_obs <- st_as_sf(oz15_obs_8hr.avg, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map15o3_model<- st_intersection(map15o3_model, uk)
max(map15o3_model$o3_model)

map15o3_obs<- st_intersection(map15o3_obs, uk)
max(map15o3_obs$o3_obs)

scico_palette_names()

pal<- wes_palette("Zissou1", 100, type="continuous")
ggplot() +
  geom_sf(data = map15o3_model, aes(color=X8hr_o3_model))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  geom_sf(data = map15o3_obs, aes(color = `X8hr_o3_obs`), size= 2)+
  scale_fill_gradientn(colours= pal, name="O3 ppb average 8 hr maximum")+
  scale_color_gradientn(colours = pal,name="O3 ppb average 8 hr maximum", , limits = c(10, 50))+
   theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2015 annual average 8 hour maximum O3 ppb - Model vs. Observations")


