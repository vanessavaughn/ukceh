#all sites vs observations, 1960-2020

# Function to extract the first four decimal places as a string
extract_first_decimals <- function(x) {
  sprintf("%.2f", x)
}

#extract site latitude and long
latitude_sites <- extract_first_decimals(AURN_Sites$latitude)
longitude_sites <- extract_first_decimals(AURN_Sites$latitude)

# Filter the data frame using dplyr
o3_auch <- monthlyemep_o3_df %>%
  mutate(first_lat = extract_first_decimals(y)) %>%
  filter(first_lat == latitude_str)  %>%
  dplyr::select(-first_lat) # Remove the temporary column if not needed

# Filter the data frame using dplyr
o3_auch <- o3_auch %>%
  mutate(first_long = extract_first_decimals(x)) %>%
  filter(first_long == longitude_str) %>%
  dplyr::select(-first_long) # Remove the temporary column if not needed

all_sites<- unique(AURN_Sites$code)
all_sites<-  paste(sprintf('"%s"', all_sites), collapse = ", ")
cat(all_sites)

#import site data
sites_19602020 <- importAURN(site = c("ABD", "ABD9", "AH", "ACTH", "BAR3", "BPLE", "BEL2", "BIRR", "AGRN", "BIRM", "BIR2", "BMLD", "BIR1", "BIRT", "BLAP", "BLAC", "BLC2", "BOLT", "BDMP", "BOT", "BORN", "BRAD", "BRT3", "BRIS", "BRS8", "BOTR", "BURY", "BUSH", "CA1", "CANT", "CARD", "CAR", "CARM", "CARL", "CLL", "MACK", "CHAT", "CHP", "CHS6", "CHLG", "CHS7", "CHBO", "CHIL", "CHBR", "COAL", "COV2", "COV3", "COPP", "CWMB", "CWMC", "DESB", "DERY", "DERR", "DYAG", "EK", "EB", "ED", "ED3", "ESK", "EX", "FW", "GLA3", "GHSR", "GLA4", "GLKP", "GLAZ", "GRAN", "GDF", "GKA8", "HG1", "HSAW", "HAR", "HM", "HONI", "HULL", "HUL2", "IMGM", "INV2", "LB", "LEAM", "LEAR", "LEED", "LED6", "LEIC", "LECU", "LEOM", "LERW", "LIVR", "LVP", "BEX", "CLL2", "BREN", "BRI", "CAN", "CRD", "LON6", "HK4", "HG2", "HG4", "HRL", "HARR", "HR3", "HIL", "HP1", "ISL", "LW1", "MY1", "KC1", "SK1", "SUT3", "TED", "TED2", "WA2", "HORS", "LN", "LH", "MH", "MAN3", "MAHG", "MAN4", "MKTH", "MID", "MKCC", "MOLD", "PEMB", "NEWC", "NPT3", "NTON", "NTN3", "NTN4", "NOR2", "NO12", "NOTT", "NOTK", "OX8", "PEEB", "PLYM", "PT", "PT4", "PMTH", "PRES", "READ", "REA1", "REDC", "ROCH", "ROTH", "ECCL", "SASH", "SALT", "OLDB", "WBRO", "SDY", "SHBR", "SHE2", "SHDG", "SHE", "SIB", "SOM", "SOUT", "SEND", "OSY", "HOPE", "STE", "SOTR", "EAGL", "STOK", "STOR", "SV", "SUN2", "SWAN", "SWA1", "SWHO", "TALL", "TDHD", "THUR", "TOFT", "WAL4", "WAR", "WBKP", "WEYB", "WC", "WFEN", "WIG5", "WIG3", "TRAN", "WOLV", "WTHG", "WRAY", "WREX", "YW", "YK10", "YK11") , year = 1960:2020 , pollutant =
                             c( "pm2.5", "o3"), hc = FALSE)

sites_19602020<- sites_19602020 %>%
  mutate(date = as.Date(date, format = "%d/%m/%y"))
#add column for months
sites_19602020 <- sites_19602020 %>%
  mutate(year = year(date))

#group by month
o3sites_19602020 <- sites_19602020 %>%
  group_by(year)%>% summarize(avgo3=mean(o3, na.rm=TRUE))

o3sites_19602020 <- o3sites_19602020 %>%
  mutate(decade = floor(year / 10) * 10)

# Group by decade and calculate the average O3
o3sites_19602020 <- o3sites_19602020 %>%
  group_by(decade) %>%
  summarize(avg_o3 = mean(avgo3, na.rm = TRUE))

#now for pm

#group by month
pmsites_19602020 <- sites_19602020 %>%
  group_by(year)%>% summarize(avgpm=mean(pm2.5, na.rm=TRUE))

pmsites_19602020 <- pmsites_19602020 %>%
  mutate(decade = floor(year / 10) * 10)

# Group by decade and calculate the average O3
pmsites_19602020 <- pmsites_19602020 %>%
  group_by(decade) %>%
  summarize(avg_pm = mean(avgpm, na.rm = TRUE))

#combine dfs
o3_pm_sites <- merge(pmsites_19602020,o3sites_19602020,  by = "decade",all.x=TRUE, suffixes = c("pm2.5", "o3"))


#extract model data


# Function to extract the first decimal place as a string
extract_first_decimal <- function(x) {
  sprintf("%.1f", x)
}

# Extract unique latitude and longitude values from AURN_Sites
unique_latitudes <- unique(AURN_Sites$latitude)
unique_longitudes <- unique(AURN_Sites$longitude)

# Extract the first decimal place for both latitude and longitude
latitude_str <- sapply(unique_latitudes, extract_first_decimal)
longitude_str <- sapply(unique_longitudes, extract_first_decimal)

# Create a combined coordinate string for easy matching
AURN_coords <- paste(latitude_str, longitude_str)

# Add columns to o3_1960 with the first decimal of lat and long
o3_1960 <- o3_1960 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_1960_filtered <- o3_1960 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_1960 <- mean(o3_1960_filtered$SURF_ppb_O3, na.rm = TRUE)

#1970
o3_1970 <- o3_1970 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_1970_filtered <- o3_1970 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_1970 <- mean(o3_1970_filtered$SURF_ppb_O3, na.rm = TRUE)

#1980
o3_1980 <- o3_1980 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_1980_filtered <- o3_1980 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_1980 <- mean(o3_1980_filtered$SURF_ppb_O3, na.rm = TRUE)

#1990
o3_1990 <- o3_1990 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_1990_filtered <- o3_1990 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_1990 <- mean(o3_1990_filtered$SURF_ppb_O3, na.rm = TRUE)

#2000
o3_2000 <- o3_2000 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_2000_filtered <- o3_2000 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_2000 <- mean(o3_2000_filtered$SURF_ppb_O3, na.rm = TRUE)

#2010
o3_2010 <- o3_2010 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_2010_filtered <- o3_2010 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_2010 <- mean(o3_2010_filtered$SURF_ppb_O3, na.rm = TRUE)

#2020
o3_2020 <- o3_2020 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
o3_2020_filtered <- o3_2020 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_o3_2020 <- mean(o3_2020_filtered$SURF_ppb_O3, na.rm = TRUE)


###################################PM###########################################################################


pm_1960 <- pm_1960 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_1960_filtered <- pm_1960 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_1960 <- mean(pm_1960_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

#1970
pm_1970 <- pm_1970 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_1970_filtered <- pm_1970 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_1970 <- mean(pm_1970_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

#1980
pm_1980 <- pm_1980 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_1980_filtered <- pm_1980 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_1980 <- mean(pm_1980_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

#1990
pm_1990 <- pm_1990 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_1990_filtered <- pm_1990 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_1990 <- mean(pm_1990_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

#2000
pm_2000 <- pm_2000 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_2000_filtered <- pm_2000 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_2000 <- mean(pm_2000_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

#2010
pm_2010 <- pm_2010 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_2010_filtered <- pm_2010 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_2010 <- mean(pm_2010_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

#2020
pm_2020 <- pm_2020 %>%
  mutate(
    first_lat = extract_first_decimal(latitude),
    first_long = extract_first_decimal(longitude),
    coord = paste(first_lat, first_long)
  )

# Filter based on combined coordinates
pm_2020_filtered <- pm_2020 %>%
  filter(coord %in% AURN_coords) %>%
  dplyr::select(-first_lat, -first_long, -coord) # Remove temporary columns

avg_pm_2020 <- mean(pm_2020_filtered$SURF_ug_PM25_rh50, na.rm = TRUE)

modeldf<- data.frame(pm2.5= c(avg_pm_1960, avg_pm_1970, avg_pm_1980, avg_pm_1990, avg_pm_2000, avg_pm_2010, avg_pm_2020),
                        o3 = c(avg_o3_1960, avg_o3_1970, avg_o3_1980, avg_o3_1990, avg_o3_2000, avg_o3_2010, avg_o3_2020),
                        decade = c(1960, 1970, 1980, 1990, 2000, 2010, 2020))

combined_df <- merge(modeldf,o3_pm_sites,  by = "decade",all.x=TRUE)
combined_df <- combined_df %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

#making the plot

ggplot(combined_df, aes(x= decade))+
  geom_line(aes(y=avg_pm, color="Observed"))+
  geom_point(aes(y=avg_pm, color="Observed"))+ 
  geom_line(aes(y=pm2.5, color="Model"))+
  geom_point(aes(y=pm2.5, color="Model"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  labs(y="pm 2.5 µg/m3 ", x="Year", color="legend")+
  labs(title=" 1960-2020 PM 2.5 model vs observed")+
  theme_minimal()

combined_df <- combined_df %>%
  mutate(avg_o3 = avg_o3 * ozone)

ggplot(combined_df, aes(x= decade))+
  geom_line(aes(y=avg_o3, color="Observed"))+
  geom_point(aes(y=avg_o3, color="Observed"))+ 
  geom_line(aes(y=o3, color="Model"))+
  geom_point(aes(y=o3, color="Model"))+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  labs(y="O3 ppb ", x="Year", color="legend")+
  labs(title=" 1960-2020 O3 model vs observed")+
  theme_minimal()





