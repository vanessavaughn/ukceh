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

###making dataframe from nc
#get longitude from file
lon <- ncvar_get(nc_file, "lon")
nlon<- dim(lon)  #check length or dimension
head(lon)        #print first few columns
lon_matrix <- matrix(lon, nrow = 369, ncol = 447)
lon_vector<- as.vector(lon_matrix)
#get latitude from file
lat<- ncvar_get(nc_file, "lat")
nlat<- dim(lat)
head(lat)
lat_matrix <- matrix(lat, nrow = 369, ncol = 447)
lat_vector<- as.vector(lat_matrix)
#get time from file
tt<- ncvar_get(nc_file, "time")
units<- ncatt_get(nc_file, "time", "units")
ntt<-dim(tt)
print(tt)
print(c(nlon, nlat, ntt))
time_vector<- as.vector(tt)
o3 <- "SURF_ppb_O3" 
#Pull the o3 data
o3.array1 <- ncvar_get(nc_file, o3)
#Get other information for the data
o31<- ncatt_get(nc_file, o3, "long_name")
o3units1<- ncatt_get(nc_file, o3, "units")
fillvalue<- ncatt_get(nc_file, o3, "_FillValue")
#Check dimension of o3 data
dim(o3.array1)
#(ncdf4) automatically converts Fillvalues to NA
nc_close(nc_file)
#Convert o3 into vector form
o3.vec.long1<- as.vector(o3.array1)
#Check length of vector 
length(o3.vec.long1)
#Convert o3_final into data.frame format
o3nc_df<- data.frame(lon=lon_vector, lat=lat_vector, SURF_ppb_O3= o3.vec.long1, time=time_vector)
names(o3nc_df) <- c("longitude", "latitude", "SURF_ppb_O3", "time")


## filter for hotspots
#convert to sf objects to match up coords
o3_sites_2015 <- importAURN(site = "ACTH", year = 2015, pollutant =
                          "o3", hc = FALSE)
o3sites2015_df<- o3_sites_2015[,c("o3","date")]

o3sites2015_df<- o3sites2015_df %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
o3sites2015_df <- o3sites2015_df %>%
  mutate(month = month(date))

# Check the number of unique values in the "time" column
unique_times <- length(unique(o3nc_df_auch$time))
ranks <- rank(unique_times, ties.method = "min")
o3nc_df_auch$time <- as.numeric(o3nc_df_auch$time)

o3nc_df_auch <- o3nc_df_auch %>%
  mutate(rank = rank(time, ties.method = "min"))
o3nc_df_auch <- o3nc_df_auch %>%
  mutate(month = ceiling(rank / max(rank) * 12)) %>%
  dplyr::select(-rank)%>%


# Function to extract the first four decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.1f", x)
}

# auchencorth latitude
latitude <- 55.79216
latitude_str <- extract_first_decimals(latitude)

# Filter the data frame using dplyr
o3nc_df_auch <- o3nc_df %>%
  mutate(first_lat = extract_first_decimals(latitude)) %>%
  filter(first_lat == latitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# auch longitude
longitude <- -3.242756
longitude_str <- extract_first_decimals(longitude)

# Filter the data frame using dplyr
o3nc_df_auch <- o3nc_df_auch %>%
  mutate(first_long = extract_first_decimals(longitude)) %>%
  filter(first_long == longitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

#average monthly values
o3nc_df_auch<- o3nc_df_auch %>%
  group_by(month)%>% summarize(avgo3=mean(SURF_ppb_O3, na.rm=TRUE))

o3sites2015_df<- o3sites2015_df %>%
  group_by(month)%>% summarize(avgo3=mean(o3, na.rm=TRUE))

#combine dfs
combined_df_o3 <- merge(o3sites2015_df,o3nc_df_auch,  by = "month",all.x=TRUE, suffixes = c("observed", "model"))

#convert all values to ppb
ozone<- 0.51
combined_df_o3<- combined_df_o3%>%
  mutate(avgo3observed=avgo3observed*ozone)

#correlation
cor(combined_df_o3$avgo3model, combined_df_o3$avgo3observed)

# t-test
t.test(combined_df_o3$avgo3model, combined_df_o3$avgo3observed, paired = TRUE)
#p-value>0.05, no statistically significant difference


# Create the plot
ggplot(combined_df_o3, aes(x=month))+
  geom_line(aes(y=avgo3model, color="Model"))+
  geom_point(aes(y=avgo3model, color="Model"))+ 
  geom_line(aes(y=avgo3observed, color="Observed"))+
  geom_point(aes(y=avgo3observed, color="Observed"))+labs(y="o3 ppb", x="month", color="legend")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  theme_minimal()


  