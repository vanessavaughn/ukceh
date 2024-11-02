### monthly observed v model
### 19/7/24
### sites: HORS (london), LN (n.ireland), MAN3 (manchester)



#libraries
install.packages("lubridate")
install.packages("Metrics")
library(lubridate)
library(dplyr)
library(ggplot2)
library(stars)
library(tidync)
library(openair)
library(Metrics)


#working code
monthlyemep_o3 <- read_emep("2015_UK_conc_month.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

monthlyemep_o3_df<- as.data.frame(monthlyemep_o3, xy=TRUE, na.rm=TRUE)

monthlyemep_o3_df<- monthlyemep_o3_df %>%
  mutate(date = as.Date(time, format = "%d/%m/%y"))
#add column for months
monthlyemep_o3_df <- monthlyemep_o3_df %>%
  mutate(month = month(date))

# Function to extract the first decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

# kc latitude
kclatitude <-  51.521050
kclatitude_str <- extract_first_decimals(kclatitude)

# Filter the data frame using dplyr
o3_kc <- monthlyemep_o3_df %>%
  mutate(first_lat = extract_first_decimals(y)) %>%
  filter(first_lat == kclatitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# kc longitude
kclongitude <- -0.213492
kclongitude_str <- extract_first_decimals(kclongitude)

# Filter the data frame using dplyr
o3_kc <- o3_kc %>%
  mutate(first_long = extract_first_decimals(x)) %>%
  filter(first_long == kclongitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
o3_kc<- o3_kc %>%
  group_by(month)%>% summarize(avgo3=mean(SURF_ppb_O3, na.rm=TRUE))
o3_kc$avgo3<- as.numeric(o3_kc$avgo3)



au_o3_kc <- importAURN(site = "KC1", year = 2015, pollutant =
                              "o3", hc = FALSE)
au_o3_kc<- au_o3_kc[,c("o3","date")]

au_o3_kc<- au_o3_kc %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
au_o3_kc <- au_o3_kc %>%
  mutate(month = month(date))
au_o3_kc<- au_o3_kc %>%
  group_by(month)%>% summarize(avgo3=mean(o3, na.rm=TRUE))

ozone<- 0.51
au_o3_kc<- au_o3_kc%>%
  mutate(avgo3=avgo3*ozone)


#combine dfs
combined_kc_o3 <- merge(o3_kc,au_o3_kc,  by = "month",all.x=TRUE, suffixes = c("model", "observed" ))
cor(combined_kc_o3$avgo3model, combined_kc_o3$avgo3observed)
rmse_value<- rmse(combined_kc_o3$avgo3model, combined_kc_o3$avgo3observed)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(combined_kc_o3, aes(x=month))+
  geom_line(aes(y=avgo3model, color="Model"))+
  geom_point(aes(y=avgo3model, color="Model"))+ 
  geom_line(aes(y=avgo3observed, color="Observed"))+
  geom_point(aes(y=avgo3observed, color="Observed"))+labs(y="o3 ppb", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="North Kensington O3 model vs observed",caption=caption_text)+
  theme_minimal()



###################################################################################################
####### kc pm2.5

monthlyemep_pm <- read_emep("2015_UK_conc_month.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

monthlyemep_pm_df<- as.data.frame(monthlyemep_pm, xy=TRUE, na.rm=TRUE)

monthlyemep_pm_df<- monthlyemep_pm_df %>%
  mutate(date = as.Date(time, format = "%d/%m/%y"))
#add column for months
monthlyemep_pm_df <- monthlyemep_pm_df %>%
  mutate(month = month(date))

# Function to extract the first decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

# kc latitude
kclatitude <-  51.521050
kclatitude_str <- extract_first_decimals(kclatitude)

# Filter the data frame using dplyr
pm_kc <- monthlyemep_pm_df %>%
  mutate(first_lat = extract_first_decimals(y)) %>%
  filter(first_lat == kclatitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# kc longitude
kclongitude <- -0.213492
kclongitude_str <- extract_first_decimals(kclongitude)

# Filter the data frame using dplyr
pm_kc <- pm_kc %>%
  mutate(first_long = extract_first_decimals(x)) %>%
  filter(first_long == kclongitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
pm_kc<- pm_kc %>%
  group_by(month)%>% summarize(avgpm=mean(SURF_ug_PM25_rh50, na.rm=TRUE))
pm_kc$avgpm<- as.numeric(pm_kc$avgpm)



au_pm_kc <- importAURN(site = "KC1", year = 2015, pollutant =
                         "pm2.5", hc = FALSE)
au_pm_kc<- au_pm_kc[,c("pm2.5","date")]

au_pm_kc<- au_pm_kc %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
au_pm_kc <- au_pm_kc %>%
  mutate(month = month(date))
au_pm_kc<- au_pm_kc %>%
  group_by(month)%>% summarize(avgpm=mean(pm2.5, na.rm=TRUE))


#combine dfs
combined_kc_pm <- merge(pm_kc,au_pm_kc,  by = "month",all.x=TRUE, suffixes = c("model", "observed" ))
cor(combined_kc_pm$avgpmmodel, combined_kc_pm$avgpmobserved)
rmse_value<- rmse(combined_kc_pm$avgpmmodel, combined_kc_pm$avgpmobserved)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(combined_kc_pm, aes(x=month))+
  geom_line(aes(y=avgpmmodel, color="Model"))+
  geom_point(aes(y=avgpmmodel, color="Model"))+ 
  geom_line(aes(y=avgpmobserved, color="Observed"))+
  geom_point(aes(y=avgpmobserved, color="Observed"))+labs(y="pm 2.5 µg/m3 ", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="North Kensington PM 2.5 model vs observed",caption=caption_text)+
  theme_minimal()

#################################################################
#man3 o3

# man latitude
manlatitude <-  51.521050
manlatitude_str <- extract_first_decimals(manlatitude)

# Filter the data frame using dplyr
o3_man <- monthlyemep_o3_df %>%
  mutate(first_lat = extract_first_decimals(y)) %>%
  filter(first_lat == manlatitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# kc longitude
kclongitude <- -0.213492
kclongitude_str <- extract_first_decimals(kclongitude)

# Filter the data frame using dplyr
o3_kc <- o3_kc %>%
  mutate(first_long = extract_first_decimals(x)) %>%
  filter(first_long == kclongitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
o3_kc<- o3_kc %>%
  group_by(month)%>% summarize(avgo3=mean(SURF_ppb_O3, na.rm=TRUE))
o3_kc$avgo3<- as.numeric(o3_kc$avgo3)



au_o3_kc <- importAURN(site = "KC1", year = 2015, pollutant =
                         "o3", hc = FALSE)
au_o3_kc<- au_o3_kc[,c("o3","date")]

au_o3_kc<- au_o3_kc %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
au_o3_kc <- au_o3_kc %>%
  mutate(month = month(date))
au_o3_kc<- au_o3_kc %>%
  group_by(month)%>% summarize(avgo3=mean(o3, na.rm=TRUE))

ozone<- 0.51
au_o3_kc<- au_o3_kc%>%
  mutate(avgo3=avgo3*ozone)


#combine dfs
combined_kc_o3 <- merge(o3_kc,au_o3_kc,  by = "month",all.x=TRUE, suffixes = c("model", "observed" ))
cor(combined_kc_o3$avgo3model, combined_kc_o3$avgo3observed)
rmse_value<- rmse(combined_kc_o3$avgo3model, combined_kc_o3$avgo3observed)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(combined_kc_o3, aes(x=month))+
  geom_line(aes(y=avgo3model, color="Model"))+
  geom_point(aes(y=avgo3model, color="Model"))+ 
  geom_line(aes(y=avgo3observed, color="Observed"))+
  geom_point(aes(y=avgo3observed, color="Observed"))+labs(y="o3 ppb", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="North Kensington O3 model vs observed",caption=caption_text)+
  theme_minimal()