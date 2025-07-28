#openair
install.packages("openair")
install.packages("openairmaps")
library(openair)
library(openairmaps)
library(dplyr)
library(ggplot2)
library(tidync)

#site data for observations
obs<- importMeta(source= "aurn", all=TRUE) %>%
  filter(variable %in% c('PM2.5','PM25'))
dplyr::glimpse(obs)

#import pm 2.5 data for sites observations
site_codes<- unique (obs$code)
site_codes<- paste(sprintf('"%s"', site_codes), collapse=",")
cat(site_codes)
pm_obs<- importAURN(site=c("ABD","ABD9","ACTH","BAR3","BPLE","BEL2","BIRR","AGRN","BIRM","BMLD","BIR1","BIRT","BLAP","BLC2","BLWD","BDMP","BORN","BRT3","BRS8","BR11","BOTR","BURY","CA1","CANT","CARD","CARM","CARL","MACK","CHAT","CHP","CHS6","CHLG","CHS7","CHBO","CHBR","COAL","COBR","COV3","COPP","DESB","DERY","DERR","DYAG","EB","ED3","GLA3","GHSR","GLA4","GLKP","GLAZ","GRAN","GKA8","HG1","HSAW","HAR","HM","HONI","HORE","HUL2","IMGM","INV2","LB","LEAM","LEAR","LEED","LED6","LEIC","LECU","LVP","BEX","CLL2","LON6","HRL","HR3","HIL","HP1","MY1","KC1","TED","TED2","HORS","LN","LH","MAN3","MAHG","MALA","MID","MKCC","PEMB","NEWC","NPT3","NTN4","NO12","NOTT","NOTK","OX8","PEGE","PLYM","PT4","PMTH","PRES","REA1","RRKL","ROCH","ROED","ECCL","SASH","SALT","SDY","SHBR","SHE2","SHDG","SHE","SHUN","SIB","SOUT","SEND","OSY","HOPE","SOTR","EAGL","STOK","STOR","SUN2","SWA1","SWHO","TALL","TDHD","THUR","TOFT","WAR","WEYB","WFEN","WIG5","TRAN","WTHG","WOEA","WREX","YW","YK10","YK11"), 
                    year=1998:2020, pollutant = c('pm2.5', 'pm25'), hc=FALSE)
pm_obs1<- pm_obs%>%
  mutate(date=as.Date(date, format="%d/%m/%y")) 

pm_obs1$Year<-format(pm_obs1$date,format="%y")

pm_obs2<- pm_obs1 %>%
  group_by(Year) %>% summarize(avgpm=mean(pm2.5, na.rm=TRUE))
extract model data now from AURN site coords
extract_first_decimal<- function(x) {
  sprintf("%.1f", x)
}
unique_lat<- unique(obs$latitude)
unique_long<- unique(obs$longitude)
lat_str<- sapply(unique_lat, extract_first_decimal)
long_str<- sapply(unique_long, extract_first_decimal)
aurn_coords<- paste(lat_str, long_str)

#import model data
#2000
pm_2000 <- read_emep("2000_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)
pm_2000<- as.data.frame(pm_2000, xy=TRUE, na.rm=TRUE)
pm_2000$SURF_ug_PM25_rh50 <- as.numeric(pm_2000$SURF_ug_PM25_rh50)
#2005
pm_2005 <- read_emep("2005_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)
pm_2005<- as.data.frame(pm_2005, xy=TRUE, na.rm=TRUE)
pm_2005$SURF_ug_PM25_rh50 <- as.numeric(pm_2005$SURF_ug_PM25_rh50)
#2010
pm_2010 <- read_emep("2010_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)
pm_2010<- as.data.frame(pm_2010, xy=TRUE, na.rm=TRUE)
pm_2010$SURF_ug_PM25_rh50 <- as.numeric(pm_2010$SURF_ug_PM25_rh50)
#2015
pm_2015 <- read_emep("2015_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)
pm_2015<- as.data.frame(pm_2015, xy=TRUE, na.rm=TRUE)
pm_2015$SURF_ug_PM25_rh50 <- as.numeric(pm_2015$SURF_ug_PM25_rh50)
#2020
pm_2020 <- read_emep("2020_UK_conc.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)
pm_2020<- as.data.frame(pm_2020, xy=TRUE, na.rm=TRUE)
pm_2020$SURF_ug_PM25_rh50 <- as.numeric(pm_2020$SURF_ug_PM25_rh50)

#filter for AURN sites
#2000
pm_2000<- pm_2000 %>%
  mutate(first_lat=extract_first_decimal(y),
         first_long=extract_first_decimal(x),
         coord=paste(first_lat, first_long))
pm_2000<- pm_2000 %>%
  filter(coord %in% aurn_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord)
avg_2005<- mean(pm_2005$SURF_ug_PM25_rh50, na.rm=TRUE)
#2005
pm_2005<- pm_2005 %>%
  mutate(first_lat=extract_first_decimal(y),
         first_long=extract_first_decimal(x),
         coord=paste(first_lat, first_long))
pm_2005<- pm_2005 %>%
  filter(coord %in% aurn_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord)
avg_2005<- mean(pm_2005$SURF_ug_PM25_rh50, na.rm=TRUE)
#2010
pm_2010<- pm_2010 %>%
  mutate(first_lat=extract_first_decimal(y),
         first_long=extract_first_decimal(x),
         coord=paste(first_lat, first_long))
pm_2010<- pm_2010 %>%
  filter(coord %in% aurn_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord)
avg_2010<- mean(pm_2010$SURF_ug_PM25_rh50, na.rm=TRUE)
#2015
pm_2015<- pm_2015 %>%
  mutate(first_lat=extract_first_decimal(y),
         first_long=extract_first_decimal(x),
         coord=paste(first_lat, first_long))
pm_2015<- pm_2015 %>%
  filter(coord %in% aurn_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord)
avg_2015<- mean(pm_2015$SURF_ug_PM25_rh50, na.rm=TRUE)
#2020
pm_2020<- pm_2020 %>%
  mutate(first_lat=extract_first_decimal(y),
         first_long=extract_first_decimal(x),
         coord=paste(first_lat, first_long))
pm_2020<- pm_2020 %>%
  filter(coord %in% aurn_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord)
avg_2020<- mean(pm_2020$SURF_ug_PM25_rh50, na.rm=TRUE)

#make df
modelvsobs<- data.frame(Year= c(2000, 2005, 2010, 2015, 2020),
                        Model = c(avg_2000, avg_2005, avg_2010, avg_2015, avg_2020),
                        Observations = c(15.477312, 13.312443, 13.657411, 10.215029, 7.892933))
#plot
ggplot(modelvsobs, aes(x=Year))+
  geom_line(aes(y=Model, color="Model"))+
  geom_point(aes(y=Model, color="Model"))+
  geom_line(aes(y=Observations, color="Observations"))+
  geom_point(aes(y=Observations, color="Observations"))+
  labs(y="PM 2.5 ppb")+
  theme_minimal()

###model validation
install.packages('Metrics')
library(Metrics)
rmse(modelvsobs$Observations, modelvsobs$Model)
