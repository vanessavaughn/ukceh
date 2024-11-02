#8/7/24
#2015 uk sample monthly data

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

###Get openair site data
#filter site ACTH, year 2015
auch_data <- importAURN(site = "ACTH", year = 2015, pollutant =
                        "all", hc = FALSE)
###monthly averages of o3 and pm2.5
#reformat dates
auch_data <- auch_data %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
auch_data <- auch_data %>%
  mutate(month = month(date))
#average monthly values
monthly_avg_o3<- auch_data %>%
  group_by(month)%>% summarize(avgo3=mean(o3, na.rm=TRUE))
monthly_avg_pm2.5<- auch_data %>%
  group_by(month)%>% summarize(avgpm2.5=mean(pm2.5, na.rm=TRUE))
#join together to one df
monthly_avg_auch<- left_join(monthly_avg_o3, monthly_avg_pm2.5) 

### create monthly line chart
o3_monthly_chart<- ggplot(monthly_avg_auch, aes(month)) + 
  geom_line(aes(y = avgo3)) + 
  geom_point(aes(y = avgo3)) + 
  ylab("O3 (µgm-3)")+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  theme_minimal()

pm_monthly_chart<- ggplot(monthly_avg_auch, aes(month)) + 
  geom_line(aes(y = avgpm2.5)) + 
  geom_point(aes(y = avgpm2.5)) + 
  ylab("PM 2.5 (µgm-3)")+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(1,12, 1))+
  theme_minimal()
