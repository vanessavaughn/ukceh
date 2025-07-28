
#######################################
#1985

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz85_obs_8hr$longitude<- extract_first_decimals(oz85_obs_8hr$longitude)
oz85_obs_8hr$latitude<- extract_first_decimals(oz85_obs_8hr$latitude)

oz_1985$longitude<- extract_first_decimals(oz_1985$longitude)
oz_1985$latitude<- extract_first_decimals(oz_1985$latitude)


lats<- unique(oz85_obs_8hr$latitude)
longs<- unique(oz85_obs_8hr$longitude)


oz_1985_2.0 <- oz_1985 %>% filter(longitude %in% longs)
oz_1985_3.0 <- oz_1985_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_85<- inner_join(oz85_obs_8hr, oz_1985_3.0, by=c('longitude', 'latitude','date'))


merged_o3_85 <- merged_oz_85 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_85 <- merge(merged_o3_85, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_85)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_85 <- merged_o3_85 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_85 =  dplyr::select(r2merged_o3_85, code, r2)
merged_o3_85<-  merge(merged_o3_85, r2merged_o3_85, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_85 <- merged_o3_85 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_85, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))#######################################
#######################################
#1990

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz90_obs_8hr$longitude<- extract_first_decimals(oz90_obs_8hr$longitude)
oz90_obs_8hr$latitude<- extract_first_decimals(oz90_obs_8hr$latitude)

oz_1990$longitude<- extract_first_decimals(oz_1990$longitude)
oz_1990$latitude<- extract_first_decimals(oz_1990$latitude)


lats<- unique(oz90_obs_8hr$latitude)
longs<- unique(oz90_obs_8hr$longitude)


oz_1990_2.0 <- oz_1990 %>% filter(longitude %in% longs)
oz_1990_3.0 <- oz_1990_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_90<- inner_join(oz90_obs_8hr, oz_1990_3.0, by=c('longitude', 'latitude','date'))


merged_o3_90 <- merged_oz_90 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_90 <- merge(merged_o3_90, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_90)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_90 <- merged_o3_90 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_90 =  dplyr::select(r2merged_o3_90, code, r2)
merged_o3_90<-  merge(merged_o3_90, r2merged_o3_90, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_90 <- merged_o3_90 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_90, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))#######################################
#1995

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz95_obs_8hr$longitude<- extract_first_decimals(oz95_obs_8hr$longitude)
oz95_obs_8hr$latitude<- extract_first_decimals(oz95_obs_8hr$latitude)

oz_1995$longitude<- extract_first_decimals(oz_1995$longitude)
oz_1995$latitude<- extract_first_decimals(oz_1995$latitude)


lats<- unique(oz95_obs_8hr$latitude)
longs<- unique(oz95_obs_8hr$longitude)


oz_1995_2.0 <- oz_1995 %>% filter(longitude %in% longs)
oz_1995_3.0 <- oz_1995_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_95<- inner_join(oz95_obs_8hr, oz_1995_3.0, by=c('longitude', 'latitude','date'))


merged_o3_95 <- merged_oz_95 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_95 <- merge(merged_o3_95, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_95)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_95 <- merged_o3_95 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_95 =  dplyr::select(r2merged_o3_95, code, r2)
merged_o3_95<-  merge(merged_o3_95, r2merged_o3_95, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_95 <- merged_o3_95 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_95, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))
#######################################
#2000

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz00_obs_8hr$longitude<- extract_first_decimals(oz00_obs_8hr$longitude)
oz00_obs_8hr$latitude<- extract_first_decimals(oz00_obs_8hr$latitude)

oz_2000$longitude<- extract_first_decimals(oz_2000$longitude)
oz_2000$latitude<- extract_first_decimals(oz_2000$latitude)


lats<- unique(oz00_obs_8hr$latitude)
longs<- unique(oz00_obs_8hr$longitude)


oz_2000_2.0 <- oz_2000 %>% filter(longitude %in% longs)
oz_2000_3.0 <- oz_2000_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_00<- inner_join(oz00_obs_8hr, oz_2000_3.0, by=c('longitude', 'latitude','date'))


merged_o3_00 <- merged_oz_00 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_00 <- merge(merged_o3_00, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_00)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_00 <- merged_o3_00 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_00 =  dplyr::select(r2merged_o3_00, code, r2)
merged_o3_00<-  merge(merged_o3_00, r2merged_o3_00, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_00 <- merged_o3_00 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_00, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))
#######################################
#2005

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz05_obs_8hr$longitude<- extract_first_decimals(oz05_obs_8hr$longitude)
oz05_obs_8hr$latitude<- extract_first_decimals(oz05_obs_8hr$latitude)

oz_2005$longitude<- extract_first_decimals(oz_2005$longitude)
oz_2005$latitude<- extract_first_decimals(oz_2005$latitude)


lats<- unique(oz05_obs_8hr$latitude)
longs<- unique(oz05_obs_8hr$longitude)


oz_2005_2.0 <- oz_2005 %>% filter(longitude %in% longs)
oz_2005_3.0 <- oz_2005_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_05<- inner_join(oz05_obs_8hr, oz_2005_3.0, by=c('longitude', 'latitude','date'))


merged_o3_05 <- merged_oz_05 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_05 <- merge(merged_o3_05, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_05)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_05 <- merged_o3_05 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_05 =  dplyr::select(r2merged_o3_05, code, r2)
merged_o3_05<-  merge(merged_o3_05, r2merged_o3_05, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_05 <- merged_o3_05 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_05, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))

#######################################
#2010

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz10_obs_8hr$longitude<- extract_first_decimals(oz10_obs_8hr$longitude)
oz10_obs_8hr$latitude<- extract_first_decimals(oz10_obs_8hr$latitude)

oz_2010$longitude<- extract_first_decimals(oz_2010$longitude)
oz_2010$latitude<- extract_first_decimals(oz_2010$latitude)


lats<- unique(oz10_obs_8hr$latitude)
longs<- unique(oz10_obs_8hr$longitude)


oz_2010_2.0 <- oz_2010 %>% filter(longitude %in% longs)
oz_2010_3.0 <- oz_2010_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_10<- inner_join(oz10_obs_8hr, oz_2010_3.0, by=c('longitude', 'latitude','date'))


merged_o3_10 <- merged_oz_10 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_10 <- merge(merged_o3_10, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_10)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_10 <- merged_o3_10 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_10 =  dplyr::select(r2merged_o3_10, code, r2)
merged_o3_10<-  merge(merged_o3_10, r2merged_o3_10, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_10 <- merged_o3_10 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_10, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))

#######################################
#2015

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

ggplot(merged_o3_15, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))

#######################################
#2020

extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

oz20_obs_8hr$longitude<- extract_first_decimals(oz20_obs_8hr$longitude)
oz20_obs_8hr$latitude<- extract_first_decimals(oz20_obs_8hr$latitude)

oz_2020$longitude<- extract_first_decimals(oz_2020$longitude)
oz_2020$latitude<- extract_first_decimals(oz_2020$latitude)


lats<- unique(oz20_obs_8hr$latitude)
longs<- unique(oz20_obs_8hr$longitude)


oz_2020_2.0 <- oz_2020 %>% filter(longitude %in% longs)
oz_2020_3.0 <- oz_2020_2.0 %>% filter(latitude %in% lats)


#merge obs and model

merged_oz_20<- inner_join(oz20_obs_8hr, oz_2020_3.0, by=c('longitude', 'latitude','date'))


merged_o3_20 <- merged_oz_20 %>%
  group_by(code, date) %>%
  summarise( `8hr_o3_model`= mean( `8hr_o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `8hr_o3_obs`, na.rm=TRUE)) %>% ungroup()


merged_o3_20 <- merge(merged_o3_20, o3_sites, by= "code", all.x=TRUE, na.rm=TRUE)
colnames(merged_o3_20)<- c("code","date","o3_model","o3_obs", "longitude","latitude")
r2merged_o3_20 <- merged_o3_20 %>% group_by(code) %>% 
  mutate(r2 = summary(lm(o3_model ~ o3_obs))$r.squared) %>% ungroup()


r2merged_o3_20 =  dplyr::select(r2merged_o3_20, code, r2)
merged_o3_20<-  merge(merged_o3_20, r2merged_o3_20, by= "code", all.x=TRUE, na.rm=TRUE)
merged_o3_20 <- merged_o3_20 %>%
  group_by(code) %>%
  summarise( `8hr_o3_model`= mean( `o3_model`, na.rm=TRUE),
             `8hr_o3_obs`= mean( `o3_obs`, na.rm=TRUE),
             r2= mean(r2, na.rm=TRUE)) %>% ungroup()
ggplot(merged_o3_20, aes(x= `8hr_o3_obs`, y=`8hr_o3_model`))+
  geom_point(aes(fill=r2), colour="black", pch= 21, size=2.5)+theme_minimal()+
  scico::scale_fill_scico(palette = "vik", midpoint = 0.5)+
  xlab("Observations (O3 ppb)")+
  ylab("Model (O3 ppb)")+
  geom_abline(slope = 1, color="grey") + geom_abline(slope = 2, color="grey") + geom_abline(slope = 0.5, color="grey")+
  xlim(c(10,40))+ ylim(c(10,40))

