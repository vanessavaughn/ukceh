## monthly o3 auch


######


#working code
monthlyemep_o3 <- read_emep("2015_UK_conc_month.nc", "EPSG:4326", var = 'SURF_ppb_O3', dims = c('i', 'j', 'time'), proxy = TRUE)

monthlyemep_o3_df<- as.data.frame(monthlyemep_o3, xy=TRUE, na.rm=TRUE)

monthlyemep_o3_df<- monthlyemep_o3_df %>%
  mutate(date = as.Date(time, format = "%d/%m/%y"))
#add column for months
monthlyemep_o3_df <- monthlyemep_o3_df %>%
  mutate(month = month(date))

# Function to extract the first four decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

# auchencorth latitude
latitude <- 55.79216
latitude_str <- extract_first_decimals(latitude)

# Filter the data frame using dplyr
o3_auch <- monthlyemep_o3_df %>%
  mutate(first_lat = extract_first_decimals(y)) %>%
  filter(first_lat == latitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# auch longitude
longitude <- -3.242756
longitude_str <- extract_first_decimals(longitude)

# Filter the data frame using dplyr
o3_auch <- o3_auch %>%
  mutate(first_long = extract_first_decimals(x)) %>%
  filter(first_long == longitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
o3_auch<- o3_auch %>%
  group_by(month)%>% summarize(avgo3=mean(SURF_ppb_O3, na.rm=TRUE))
o3_auch$avgo3<- as.numeric(o3_auch$avgo3)



o3_sites_2015 <- importAURN(site = "ACTH", year = 2015, pollutant =
                              "o3", hc = FALSE)
o3sites2015_df<- o3_sites_2015[,c("o3","date")]

o3sites2015_df<- o3sites2015_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
o3sites2015_df <- o3sites2015_df %>%
  mutate(month = month(date))
o3sites2015_df<- o3sites2015_df %>%
  group_by(month)%>% summarize(avgo3=mean(o3, na.rm=TRUE))

ozone<- 0.51
o3sites2015_df<- o3sites2015_df%>%
  mutate(avgo3=avgo3*ozone)


#combine dfs
combined_df_o3 <- merge(o3sites2015_df,o3_auch,  by = "month",all.x=TRUE, suffixes = c("observed", "model"))
cor(combined_df_o3$avgo3model, combined_df_o3$avgo3observed)


rmse_value<- rmse(combined_df_o3$avgo3model, combined_df_o3$avgo3observed)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(combined_df_o3, aes(x=month))+
  geom_line(aes(y=avgo3model, color="Model"))+
  geom_point(aes(y=avgo3model, color="Model"))+ 
  geom_line(aes(y=avgo3observed, color="Observed"))+
  geom_point(aes(y=avgo3observed, color="Observed"))+labs(y="o3 ppb", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="Auchencorth Moss O3 model vs observed",caption=caption_text)+
  theme_minimal()


############
#now for pm


#working code
monthlyemep_pm <- read_emep("2015_UK_conc_month.nc", "EPSG:4326", var = 'SURF_ug_PM25_rh50', dims = c('i', 'j', 'time'), proxy = TRUE)

monthlyemep_pm_df<- as.data.frame(monthlyemep_pm, xy=TRUE, na.rm=TRUE)

monthlyemep_pm_df<- monthlyemep_pm_df %>%
  mutate(date = as.Date(time, format = "%d/%m/%y"))
#add column for months
monthlyemep_pm_df <- monthlyemep_pm_df %>%
  mutate(month = month(date))

# Function to extract the first four decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

# auchencorth latitude
latitude <- 55.79216
latitude_str <- extract_first_decimals(latitude)

# Filter the data frame using dplyr
pm_auch <- monthlyemep_pm_df %>%
  mutate(first_lat = extract_first_decimals(y)) %>%
  filter(first_lat == latitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# auch longitude
longitude <- -3.242756
longitude_str <- extract_first_decimals(longitude)

# Filter the data frame using dplyr
pm_auch <- pm_auch %>%
  mutate(first_long = extract_first_decimals(x)) %>%
  filter(first_long == longitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
pm_auch<- pm_auch %>%
  group_by(month)%>% summarize(avgpm=mean(SURF_ug_PM25_rh50, na.rm=TRUE))
pm_auch$avgpm<- as.numeric(pm_auch$avgpm)


pm_sites_2015 <- importAURN(site = "ACTH", year = 2015, pollutant =
                              "pm2.5", hc = FALSE)
pmsites2015_df<- pm_sites_2015[,c("pm2.5","date")]

pmsites2015_df<- pmsites2015_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
pmsites2015_df <- pmsites2015_df %>%
  mutate(month = month(date))
pmsites2015_df<- pmsites2015_df %>%
  group_by(month)%>% summarize(avgpm2.5=mean(pm2.5, na.rm=TRUE))




#combine dfs
combined_df_pm <- merge(pmsites2015_df,pm_auch,  by = "month",all.x=TRUE, suffixes = c("observed", "model"))




rmse_value<- rmse(combined_df_pm$avgpm, combined_df_pm$avgpm2.5)
caption_text<-  paste("RMSE =", round(rmse_value, 2))

ggplot(combined_df_pm, aes(x=month))+
  geom_line(aes(y=avgpm, color="Model"))+
  geom_point(aes(y=avgpm, color="Model"))+ 
  geom_line(aes(y=avgpm2.5, color="Observed"))+
  geom_point(aes(y=avgpm2.5, color="Observed"))+labs(y="pm 2.5 µg/m3 ", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  labs(title="Auchencorth Moss PM 2.5 model vs observed",caption=caption_text)+
  theme_minimal()
