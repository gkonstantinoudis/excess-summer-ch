#:::::::::::::::::::::::::::::
# Project: excess deaths summer 2022
# description: Clean temperature ERA5
#:::::::::::::::::::::::::::::


# read the files
files2read <- list.files("Output/")[list.files("Output/") %>% startsWith(.,"temperature20")]
temperature <- lapply(paste0("Output/", files2read), nc_open) 
extr.tmp <- lapply(temperature, function(X) ncvar_get(X, varid="t2m"))

# extract space 
lon <- lapply(temperature, function(X) ncvar_get(X,"longitude")) 
lon <- lon[[1]]
lat <- lapply(temperature, function(X) ncvar_get(X,"latitude")) 
lat <- lat[[1]]
# and time
hour <- lapply(temperature, function(X) ncvar_get(X,"time")) 
hour <- do.call(c, hour)
# the format is hours since 1900-01-01:
hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
# Set time zone (UTC)
attr(hour_tr, "tzone") <- "UTC"

# set the correct timezone for Switzerland (same as in Italy)
hour_tr <- format(hour_tr, format='%Y-%m-%d', tz = "Europe/Rome")

# and from these dates we need to remove September, which comes due to the time difference
extr.tmp <- abind(extr.tmp, along = 3)
extr.tmp[,,month(hour_tr) %in% 6:8] -> extr.tmp
hour_tr[month(hour_tr) %in% 6:8] -> hour_tr



# define the start/end points of each date
dat <- as.data.frame(table(hour_tr))

start <- numeric(nrow(dat))
stop <- numeric(nrow(dat))

start[1] <- 1
stop[1] <- dat$Freq[1]

for(i in 2:nrow(dat)){
  start[i] <- stop[i-1] + 1
  stop[i] <- start[i] + dat$Freq[i] - 1
}

dat$start <- start
dat$stop <- stop


# function to retrieve daily mean
DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.tmp[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y-273.15))
  tmp <- as.data.frame(tmp)
  
  colnames(tmp) <- lat
  rownames(tmp) <- lon
  
  mat2store <- expand.grid(lon, lat)
  colnames(mat2store) <- c("lon", "lat")
  mat2store <- cbind(mat2store, as.vector(as.matrix(tmp)))  
  
  mat2store <- as.data.frame(mat2store)
  colnames(mat2store)[3] <- "temperature"
  
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  
  return(mat2store)
}

# run the DailyMean function across the data
GetTemperature <- 
  pbapply(dat, 1, function(X){
    
    return(DailyMean(start = X[3], stop = X[4], date = X[1]))
    
  } 
  ) # ~5 minutes


##
##


GetTemperature <- do.call(rbind, GetTemperature)

# create and id by latitude and longitude
GetTemperature %>% 
  dplyr::group_by(lon, lat) %>% 
  dplyr::mutate(ID = cur_group_id()) -> GetTemperature


# Now we need the shp.
# Now I need to overlay it on the shp and take the mean by municipality and week

shp = sf::read_sf("data/shp.shp")

shp$IDSpace <- 1:nrow(shp)
shp$IDSpace <- as.character(shp$IDSpace)

# Work on data.table to speed up the filter() computation
# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(shp))
DT_sf <- st_coordinates(DT_sf)
DT_sf <- as.data.frame(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)
GetTemperature_tmp <- as.data.table(GetTemperature)

tmp_sf <- st_as_sf(GetTemperature_tmp, coords = c("X", "Y"), crs = st_crs(shp))
tmp_sf$X <- GetTemperature_tmp$X
tmp_sf$Y <- GetTemperature_tmp$Y

tmp_stjoin <- st_join(tmp_sf, shp)
tmp_stjoin <- as.data.frame(tmp_stjoin)
tmp_stjoin$geometry <- NULL
head(tmp_stjoin)
# the nas are the ones outside the region

tmp_stjoin <- tmp_stjoin[!is.na(tmp_stjoin$NAME),] # name is just a the name of the region on the shp
summary(tmp_stjoin)

tmp_stjoin %>% select(date, NAME, temperature) %>%
  dplyr::group_by(date, NAME) %>% 
  dplyr::summarize(mean.temp = mean(temperature, na.rm = TRUE)) -> tmp_stjoin

tail(tmp_stjoin)
# check if the rows add up
# 26 cantons and 

# wrong: this is why we are missing a canton, namely Basel-Stadt. This is because is a tiny canton
# and there were no grid cells from the ERA5 falling there. I will impute with the temperature values 
# from the closest canton, i.e. Basel-Landschaft

rbind(
  tmp_stjoin, 
  tmp_stjoin %>% filter(NAME == "Basel-Landschaft") %>% mutate(NAME="Basel-Stadt")
) -> tmp_stjoin


saveRDS(tmp_stjoin, file = file.path(path_output, "temperature"))


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################






