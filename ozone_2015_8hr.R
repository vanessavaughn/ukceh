setwd("C:/Users/vanes/Downloads/ukceh")


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
# Function to extract the first decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz15_obs_8hr$longitude<- extract_first_decimals(oz15_obs_8hr$longitude)
oz15_obs_8hr$latitude<- extract_first_decimals(oz15_obs_8hr$latitude)

oz_2015$longitude<- extract_first_decimals(oz_2015$longitude)
oz_2015$latitude<- extract_first_decimals(oz_2015$latitude)


lats<- unique(oz15_obs_8hr$latitude)
longs<- unique(oz15_obs_8hr$longitude)


oz_2015_2.0 <- oz_2015 %>% filter(longitude %in% longs)
oz_2015_3.0 <- oz_2015_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_15<- inner_join(oz15_obs_8hr, oz_2015_3.0, by=c('longitude', 'latitude','date'))


merged_o3_15 <- merged_oz_15 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()
  
  
merged_o3_15 <- merge(merged_o3_15, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_15)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_15 <- merged_o3_15 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()

merged_o3_15 <- merged_o3_15 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
r2merged_o3_15 =  dplyr::select(r2merged_o3_15, code, r2)
merged_o3_15<-  merge(merged_o3_15, r2merged_o3_15, by= "code", all.x=TRUE, na.rm=TRUE)


#same units
ozone<- 0.51
merged_o3_15<- merged_o3_15%>%
  mutate(`8hr_o3_obs`=`8hr_o3_obs`*ozone)

plot(merged_o3_15$o3_obs, merged_o3_15$o3_model)
lmodel <- lm( merged_o3_15$o3_model~ merged_o3_15$o3_obs)
summary(lmodel)
abline(a=coef(lmodel)[1], b=coef(lmodel)[2]) 

#r^2=0.4584
ggplot() + geom_point(aes(merged_o3_15$o3_obs, merged_o3_15$o3_model)) + 
  geom_smooth(aes(merged_o3_15$o3_obs, merged_o3_15$o3_model), method="lm", se=F)+
  theme_minimal()+
  
  labs(y="Model 2015 O3 ppb ", x= "Observations 2015 O3 ppb", caption=caption_text)

library(Metrics)
rmse_value<- rmse(merged_o3_15$o3_obs, merged_o3_15$o3_model)
#rmse=7.759431
caption_text<-  paste("R^2 = 0.46, RMSE = ", round(rmse_value, 2))

KC1 <- merged_oz_15[merged_oz_15$code == "KC1", ]
ACTH <- merged_oz_15[merged_oz_15$code == "ACTH", ]
DERY <- merged_oz_15[merged_oz_15$code == "DERY", ]
MAN3 <- merged_oz_15[merged_oz_15$code == "MAN3", ]

ggplot(KC1, aes(x=time))+
  geom_line(aes(y=o3_model, color="Model"))+
  geom_point(aes(y=o3_model, color="Model"))+ 
  geom_line(aes(y=o3_obs, color="Observed"))+
  geom_point(aes(y=o3_obs, color="Observed"))+labs(y="O3 ppb", x="Time", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="North Kensington",caption=caption_text)+
  theme_minimal()
rmse(KC1$o3_obs, KC1$o3_model)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(ACTH, aes(x=time))+
  geom_line(aes(y=o3_model, color="Model"))+
  geom_point(aes(y=o3_model, color="Model"))+ 
  geom_line(aes(y=o3_obs, color="Observed"))+
  geom_point(aes(y=o3_obs, color="Observed"))+labs(y="O3 ppb", x="Time", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="Auchencorth Moss O3 model vs observed",caption=caption_text)+
  theme_minimal()
rmse_value<- rmse(ACTH$o3_obs, ACTH$o3_model)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(DERY, aes(x=time))+
  geom_line(aes(y=o3_model, color="Model"))+
  geom_point(aes(y=o3_model, color="Model"))+ 
  geom_line(aes(y=o3_obs, color="Observed"))+
  geom_point(aes(y=o3_obs, color="Observed"))+labs(y="O3 ppb", x="Time", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="Derry Rosemount O3 model vs observed",caption=caption_text)+
  theme_minimal()
rmse(DERY$o3_obs, DERY$o3_model)
caption_text<-  paste("RMSE =", round(rmse_value, 2))


ggplot(MAN3, aes(x=time))+
  geom_line(aes(y=o3_model, color="Model"))+
  geom_point(aes(y=o3_model, color="Model"))+ 
  geom_line(aes(y=o3_obs, color="Observed"))+
  geom_point(aes(y=o3_obs, color="Observed"))+labs(y="O3 ppb", x="Time", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="Manchester Piccadilly O3 model vs observed",caption=caption_text)+
  theme_minimal()
rmse(MAN3$o3_obs, MAN3$o3_model)
caption_text<-  paste("RMSE =", round(rmse_value, 2))


##map

#making df into raster
ext <- ext(min(merged_o3_15$longitude), max(merged_o3_15$longitude), min(merged_o3_15$latitude), max(merged_o3_15$latitude))
res <- c(.06, .06)
r <- rast(ext = ext, res = res)
points <- vect(merged_o3_15, geom = c("longitude", "latitude"))
r <- rasterize(points, r, field = "o3_model", fun = "mean")
map15o3_model<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(map15o3_model) <- c("longitude", "latitude", "o3_model")

r <- rasterize(points, r, field = "o3_obs", fun = "mean")
map15o3_obs<- as.data.frame(r, xy=TRUE, cells=FALSE)
names(map15o3_obs) <- c("longitude", "latitude", "o3_obs")


#making df into sf
map15o3_model <- st_as_sf(map15o3_model, coords = c("longitude", "latitude"), crs = 4326)

map15o3_obs <- st_as_sf(map15o3_obs, coords = c("longitude", "latitude"), crs = 4326)


#cropping to uk

uk <- ne_countries(returnclass = "sf", country = "United Kingdom", scale="large")
map15o3_model<- st_intersection(map15o3_model, uk)
max(map15o3_model$o3_model)

map15o3_obs<- st_intersection(map15o3_obs, uk)
max(map15o3_obs$o3_obs)

custom_breaks <- c( 10, 20, 30, 40)
custom_colors <- rainbow(length(custom_breaks) - 1)
custom_limits <- c(10, 40)




#plot model
map15o3_model_plot<- ggplot() +
  geom_sf(data = map15o3_model, aes(color=o3_model, size=3))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2015 8hr Ozone - Model")

print(map15o3_model_plot)

#plot observations
map15o3_obs_plot<- ggplot() +
  geom_sf(data = map15o3_obs, aes(color=o3_obs, size=3))+
  geom_sf(data= uk, fill=NA, color="black", size=8)+
  scale_color_gradientn(
    colors = pal,
    limits = custom_limits,
    breaks = custom_breaks,
    values = scales::rescale(custom_breaks, to = c(0, 1)),
    oob = scales::squish
  )  + theme_minimal()+
  labs(color = "O3 ppb") +
  ggtitle("2015 8hr Ozone - Observations")

print(map15o3_obs_plot)


## site averages

site.avg<- merged_o3_15 %>%
  group_by(code) %>%
  summarise( o3_model= mean( o3_model, na.rm=TRUE),
             o3_obs= mean( o3_obs, na.rm=TRUE)) %>% ungroup()


ggplot() + geom_point(aes(site.avg$o3_obs, site.avg$o3_model, col= site.avg$code)) + 
  geom_smooth(aes(site.avg$o3_obs, site.avg$o3_model), method="lm", se=F)+
  xlab("Observations")+
  ylab("Model")+
  xlim(20,40)+ ylim(20,40)+
  theme_minimal()


















