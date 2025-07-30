###pm2.5modelval.scatter



#######################################
#2000

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}
#yY



pm00_obs$longitude<- extract_first_decimals(pm00_obs$longitude)
pm00_obs$latitude<- extract_first_decimals(pm00_obs$latitude)

pm_2000$longitude<- extract_first_decimals(pm_2000$longitude)
pm_2000$latitude<- extract_first_decimals(pm_2000$latitude)


lats<- unique(pm00_obs$latitude)
longs<- unique(pm00_obs$longitude)


pm_2000_2.0 <- pm_2000 %>% filter(longitude %in% longs)
pm_2000_3.0 <- pm_2000_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_pm_00<- inner_join(pm00_obs, pm_2000_3.0, by=c('longitude', 'latitude'))
r2merged_pm_00 <- merged_pm_00 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(pm.x ~ pm.y))$r.squared) %>% ungroup()


r2merged_pm_00 <- r2merged_pm_00 %>%
  group_by(code) %>%
  summarise( pm_model= mean( pm.x, na.rm=TRUE),
             pm_obs= mean( pm.y, na.rm=TRUE),
             r2 = mean( r2, na.rm=TRUE)) %>% ungroup()


write.csv(yr2015, "pm2015.csv")


ggplot(r2merged_pm_00, aes(x= pm_obs, y= pm_model))+
  geom_point( colour="black", pch= 16, size=2.5)+theme_minimal()+
  xlab("Observations (PM2.5 ug/m3)")+
  ylab("Model (PM2.5 ug/m3)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(0,30))+ ylim(c(0,30))



#######################################
#2005

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}


pm05_obs$longitude<- extract_first_decimals(pm05_obs$longitude)
pm05_obs$latitude<- extract_first_decimals(pm05_obs$latitude)

pm_2005$longitude<- extract_first_decimals(pm_2005$longitude)
pm_2005$latitude<- extract_first_decimals(pm_2005$latitude)


lats<- unique(pm05_obs$latitude)
longs<- unique(pm05_obs$longitude)


pm_2005_2.0 <- pm_2005 %>% filter(longitude %in% longs)
pm_2005_3.0 <- pm_2005_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_pm_05<- inner_join(pm05_obs, pm_2005_3.0, by=c('longitude', 'latitude'))
merged_pm_05 <- merged_pm_05 %>%   group_by(code) %>%
  summarise( pm_model= mean( pm.x, na.rm=TRUE),
             pm_obs= mean( pm.y, na.rm=TRUE))
  


ggplot(merged_pm_05, aes(x= pm_obs, y= pm_model))+
  geom_point( colour="black", pch= 16, size=2.5)+theme_minimal()+
  xlab("Observations (PM2.5 ug/m3)")+
  ylab("Model (PM2.5 ug/m3)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(0,30))+ ylim(c(0,30))

#######################################
#2010

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}


pm10_obs$longitude<- extract_first_decimals(pm10_obs$longitude)
pm10_obs$latitude<- extract_first_decimals(pm10_obs$latitude)

pm_2010$longitude<- extract_first_decimals(pm_2010$longitude)
pm_2010$latitude<- extract_first_decimals(pm_2010$latitude)


lats<- unique(pm10_obs$latitude)
longs<- unique(pm10_obs$longitude)


pm_2010_2.0 <- pm_2010 %>% filter(longitude %in% longs)
pm_2010_3.0 <- pm_2010_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_pm_10<- inner_join(pm10_obs, pm_2010_3.0, by=c('longitude', 'latitude'))
merged_pm_10 <- merged_pm_10 %>%   group_by(code) %>%
  summarise( pm_model= mean( pm.x, na.rm=TRUE),
             pm_obs= mean( pm.y, na.rm=TRUE))



ggplot(merged_pm_10, aes(x= pm_obs, y= pm_model))+
  geom_point( colour="black", pch= 16, size=2.5)+theme_minimal()+
  xlab("Observations (PM2.5 ug/m3)")+
  ylab("Model (PM2.5 ug/m3)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(0,30))+ ylim(c(0,30))

#######################################
#2015

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}


pm15_obs$longitude<- extract_first_decimals(pm15_obs$longitude)
pm15_obs$latitude<- extract_first_decimals(pm15_obs$latitude)

pm_2015$longitude<- extract_first_decimals(pm_2015$longitude)
pm_2015$latitude<- extract_first_decimals(pm_2015$latitude)


lats<- unique(pm15_obs$latitude)
longs<- unique(pm15_obs$longitude)


pm_2015_2.0 <- pm_2015 %>% filter(longitude %in% longs)
pm_2015_3.0 <- pm_2015_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_pm_15<- inner_join(pm15_obs, pm_2015_3.0, by=c('longitude', 'latitude'))
merged_pm_15 <- merged_pm_15 %>%   group_by(code) %>%
  summarise( pm_model= mean( pm.x, na.rm=TRUE),
             pm_obs= mean( pm.y, na.rm=TRUE))



ggplot(merged_pm_15, aes(x= pm_obs, y= pm_model))+
  geom_point( colour="black", pch= 16, size=2.5)+theme_minimal()+
  xlab("Observations (PM2.5 ug/m3)")+
  ylab("Model (PM2.5 ug/m3)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(0,30))+ ylim(c(0,30))

#######################################
#2020

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}


pm20_obs$longitude<- extract_first_decimals(pm20_obs$longitude)
pm20_obs$latitude<- extract_first_decimals(pm20_obs$latitude)

pm_2020$longitude<- extract_first_decimals(pm_2020$longitude)
pm_2020$latitude<- extract_first_decimals(pm_2020$latitude)


lats<- unique(pm20_obs$latitude)
longs<- unique(pm20_obs$longitude)


pm_2020_2.0 <- pm_2020 %>% filter(longitude %in% longs)
pm_2020_3.0 <- pm_2020_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_pm_20<- inner_join(pm20_obs, pm_2020_3.0, by=c('longitude', 'latitude'))
merged_pm_20 <- merged_pm_20 %>%   group_by(code) %>%
  summarise( pm_model= mean( pm.x, na.rm=TRUE),
             pm_obs= mean( pm.y, na.rm=TRUE))



ggplot(merged_pm_20, aes(x= pm_obs, y= pm_model))+
  geom_point( colour="black", pch= 16, size=2.5)+theme_minimal()+
  xlab("Observations (PM2.5 ug/m3)")+
  ylab("Model (PM2.5 ug/m3)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(0,30))+ ylim(c(0,30))
