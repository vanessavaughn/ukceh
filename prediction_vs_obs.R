#9/7/24
#model predictions vs observations


###read 2015 monthly sample data
monthly2015<- tidync("2015_UK_conc_month.nc")
print(monthly2015)
nc_file<- nc_open("2015_UK_conc_month.nc")
class(nc_file)

#inspecting data
ncmeta::nc_grids("2015_UK_conc_month.nc")
ncmeta::nc_vars("2015_UK_conc_month.nc")

pmnc_df<- r_df

## filter for hotspots
#convert to sf objects to match up coords
pm_sites_2015 <- importAURN(site = "ACTH", year = 2015, pollutant =
                              "pm2.5", hc = FALSE)
pmsites2015_df<- pm_sites_2015[,c("pm2.5","date")]

pmsites2015_df<- pmsites2015_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
pmsites2015_df <- pmsites2015_df %>%
  mutate(month = month(date))


# Check the number of unique values in the "time" column
unique_times <- length(unique(pmnc_df$time))
ranks <- rank(unique_times, ties.method = "min")
pmnc_df$time <- as.numeric(pmnc_df$time)

pmnc_df <- pmnc_df %>%
  mutate(rank = rank(time, ties.method = "min"))
pm3nc_df<- pmnc_df%>%
  mutate(month = ceiling(rank / max(rank) * 12)) %>%
  dplyr::select(-rank)
  
  
  # Function to extract the first four decimal places as a string
  extract_first_decimals <- function(x) {
    sprintf("%.1f", x)
  }

# auchencorth latitude
latitude <- 55.79216
latitude_str <- extract_first_decimals(latitude)

# Filter the data frame using dplyr
pm_auch <- r_df %>%
  mutate(first_lat = extract_first_decimals(latitude)) %>%
  filter(first_lat == latitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# auch longitude
longitude <- -3.242756
longitude_str <- extract_first_decimals(longitude)

# Filter the data frame using dplyr
pm_auch <- pm_auch %>%
  mutate(first_long = extract_first_decimals(longitude)) %>%
  filter(first_long == longitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
pmnc_df_auch<- pmnc_df_auch %>%
  group_by(month)%>% summarize(avgpm=mean(SURF_ug_PM25_rh50, na.rm=TRUE))

pmsites2015_df<- pmsites2015_df %>%
  group_by(month)%>% summarize(avgpm=mean(pm2.5, na.rm=TRUE))

#combine dfs
combined_df_pm <- merge(pmsites2015_df,pmnc_df_auch,  by = "month",all.x=TRUE, suffixes = c("observed", "model"))

#convert all values to ppb
ozone<- 0.51
combined_df_o3<- combined_df_o3%>%
  mutate(avgo3observed=avgo3observed*ozone)

#correlation
cor(combined_df_o3$avgo3model, combined_df_o3$avgo3observed)
cor(combined_df_pm$avgo3, combined_df_pm$avgpm)

# t-test
t.test(combined_df_o3$avgo3model, combined_df_o3$avgo3observed, paired = TRUE)
#p-value>0.05, no statistically significant difference

t.test(combined_df_pm$avgo3, combined_df_pm$avgpm, paired = TRUE)
# Create the plot
ggplot(combined_df_o3, aes(x=month))+
  geom_line(aes(y=avgo3model, color="Model"))+
  geom_point(aes(y=avgo3model, color="Model"))+ 
  geom_line(aes(y=avgo3observed, color="Observed"))+
  geom_point(aes(y=avgo3observed, color="Observed"))+labs(y="o3 ppb", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  theme_minimal()

ggplot(combined_df_pm, aes(x=month))+
  geom_line(aes(y=avgpmmodel, color="Model"))+
  geom_point(aes(y=avgpmmodel, color="Model"))+ 
  geom_line(aes(y=avgpmobserved, color="Observed"))+
  geom_point(aes(y=avgpmobserved, color="Observed"))+labs(y="pm2.5 ug/m^-3", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  theme_minimal()

