#:::::::::::::::::::::::::::::
# Project: excess deaths summer 2022
# description: Population weighted temperature
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

GetTemperature <- do.call(rbind, GetTemperature)

##
##

# Download population figures: https://www.pxweb.bfs.admin.ch/pxweb/en/ and store it as municipal_population.csv in the data folder
Sys.setlocale("LC_CTYPE", "german")
pop_munic1 = read.csv("data/municipal_population.csv", encoding="de_DE.UTF-8") 
pop_munic1$Population.type <- pop_munic1$Citizenship..category. <- pop_munic1$Sex <- NULL
colnames(pop_munic1) <- c("year", "mun_name", "pop")
pop_munic1$mun_code <- substr(pop_munic1$mun_name, start = 7, stop = 10) %>% as.numeric()
pop_munic1$mun_name <- gsub('[[:digit:]]+ ', '', gsub("\\.*", "", pop_munic1$mun_name)) 
pop_munic <- pop_munic1
rm(pop_munic1)
pop_munic$year <- pop_munic$year + 1 # as it corresponds to the 31 Dec

# note in the footnotes that the boundaries correspond to the specification as of 01.07.2021, so we need to download the corresponding shp from 
# https://www.swisstopo.admin.ch/en/geodata/landscape/boundaries3d.html#download. Export in a folder called munic

mun <- read_sf("data/munic/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")
# plot(mun$geometry)
# mun$BFS_NUMMER %>% table()
sum(!mun$BFS_NUMMER %in% pop_munic$mun_code)
sum(!pop_munic$mun_code %in% mun$BFS_NUMMER)

# what are these 54 missing?
chk <- mun[!mun$BFS_NUMMER %in% pop_munic$mun_code, "NAME"] # looks that they are lakes
plot(chk$geometry) # so they are lakes and lichtenstein

mun <- mun[mun$BFS_NUMMER %in% pop_munic$mun_code,]
unique(pop_munic$mun_code) %>% length()
unique(mun$BFS_NUMMER) %>% length() # some municipalities are duplicated which is fine, I will remove the duplicates randomly
mun <- mun[!duplicated(mun$BFS_NUMMER),]
shp <- mun

# ok so now I will merge the mun with the mean population over the study area:
mun <- mun %>% dplyr::select(BFS_NUMMER)
summary(mun) # ok no NAs.

# Now I need to link this with the temperature file. 
# First I will change the projection string
cbind(mun, mun %>% st_centroid() %>% st_coordinates()) -> mun_coords
coords <- st_as_sf(data.frame(X = mun_coords$X, Y = mun_coords$Y), coords = c("X","Y"))
coords <- st_set_crs(coords, st_crs(mun_coords)) 
coords <- st_transform(coords, crs = 4326)
mun_coords <- cbind(mun_coords, st_coordinates(coords))
mun_coords$geometry <- NULL

pop_munic <- left_join(pop_munic, mun_coords, by = c("mun_code" = "BFS_NUMMER"))

# Now I need the temperature file
GetTemperature$year <- year(GetTemperature$date)

# check
# plot(mun_coords$X.1, mun_coords$Y.1)
# sam <- sample(1:nrow(GetTemperature), size = 1000)
# points(GetTemperature$lon[sam], GetTemperature$lat[sam], col = "red")
# # ok looks fine


# unique temperature locations
geo_tmp = GetTemperature %>% 
  ungroup() %>% 
  dplyr::select(X=lon, Y=lat) %>% 
  unique() %>% 
  sf::st_as_sf(coords=c("X","Y"), remove = FALSE, crs = 4326, agr = "identity")

tmp_coords <- st_coordinates(geo_tmp)[FNN::get.knnx(st_coordinates(geo_tmp), pop_munic[c("X.1", "Y.1")], k = 1)$nn.index,]
tmp_coords <- tmp_coords %>% as.data.frame() 
colnames(tmp_coords) <- c("lon", "lat")

pop_munic <- cbind(pop_munic, tmp_coords)
GetTemperature_tmp <- left_join(GetTemperature, pop_munic, by = c("lat" = "lat", "lon" = "lon", "year" = "year"), relationship = "many-to-many")
GetTemperature_tmp <- GetTemperature_tmp[complete.cases(GetTemperature_tmp$mun_name),]

# t_0 <- Sys.time()
# date.loop <- unique(GetTemperature$date)
# list.loop <- list()
# for(i in 1:length(date.loop)){
#   GetTemperature_tmp <- GetTemperature %>% dplyr::filter(date %in% date.loop[i])
#   pop_munic_tmp <- pop_munic %>% dplyr::filter(year %in% year(date.loop[i]))
#   list.loop[[i]] <- left_join(pop_munic_tmp, GetTemperature_tmp)
# }
# t_1 <- Sys.time()
# t_1 - t_0

date.loop <- unique(GetTemperature$date)
getWeights <- function(X){
  GetTemperature_tmp <- GetTemperature %>% dplyr::filter(date %in% date.loop[X])
  subyear <- lubridate::year(date.loop[X]) %>% as.numeric()
  pop_munic_tmp <- pop_munic %>% dplyr::filter(year %in% subyear)
  left_join(pop_munic_tmp, GetTemperature_tmp) %>% return()
}


# parallel
t_0 <- Sys.time()

no_cores = 20
cl <- makeCluster(no_cores) 

clusterExport(cl, c("date.loop", "GetTemperature", "pop_munic"))

clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
})

cat("Launch model on parallel")

result <- parLapply(cl, 1:length(date.loop), getWeights)  
stopCluster(cl) 
t_1 <- Sys.time()
t_1 - t_0 # ~ 5 minutes

result <- do.call(rbind, result)

# I need to add the cantons on the result
canton <- read_sf("data/munic/swissboundaries3d_2021-07_2056_5728.shp/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp")
link1 <- data.frame(number = canton$KANTONSNUM, name = canton$NAME)
link1 <- link1[!duplicated(link1$number),]
link2 <- data.frame(cnumber = shp$KANTONSNUM, BFS_NUMBER = shp$BFS_NUMMER)
link3 <- left_join(link2, link1, by = c("cnumber" = "number"))

#define ID space (as it was done in Akis' file)
mun.ma <- read_sf("data/shp.shp", options = "ENCODING=WINDOWS-1252")

linkCH <- data.frame(
  NAME = unique(mun.ma$NAME), 
  NUTS3 = c("CH011", "CH012", "CH013", "CH021", "CH022", 
            "CH023", "CH024", "CH025", "CH031", "CH032", 
            "CH033", "CH040", "CH051", "CH052", "CH053", 
            "CH054", "CH055", "CH056", "CH057", "CH061", 
            "CH062", "CH063", "CH064", "CH065", "CH066", 
            "CH070"),
  CN = c("VD", "VS", "GE", "BE", "FR", "SO", "NE", "JU", "BS", "BL", 
         "AG", "ZH", "GL", "SH", "AR", "AI", "SG", "GR", "TG", "LU", 
         "UR", "SZ", "OW", "NW", "ZG", "TI"), 
  Name_german = c("Waadt", "Wallis", "Genf", "Bern", "Freiburg", "Solothurn", 
                  "Neuenburg", "Jura", "Basel-Stadt", "Basel-Landschaft", "Aargau", 
                  "Zurich", "Glarus", "Schaffhausen", "Appenzell A.Rh", "Appenzell I.Rh", 
                  "Sankt Gallen", "Graubunden", "Thurgau", "Luzern", "Uri", "Schwyz", 
                  "Obwalden", "Nidwalden", "Zug", "Tessin")
)

saveRDS(linkCH, file = "output/linkCH")

linkCH <- left_join(linkCH, link3, by = c("NAME" = "name"))

# and bring to the result:
result.tmp <- left_join(result, linkCH, by = c("mun_code" = "BFS_NUMBER"))

summary(result.tmp)
sum(is.na(result.tmp$cn)) # looks correct.

# Now I need to take the daily mean by canton weighted for population.

result.tmp %>% 
  dplyr::group_by(date, NUTS3) %>% 
  dplyr::summarise(temperature = sum(temperature*pop), sum.pop = sum(pop)) %>% 
  dplyr::mutate(weighted_temperature = temperature/sum.pop) -> result.tmp
  
result.tmp$temperature <- result.tmp$sum.pop <- NULL

linkCH <- readRDS("output/linkCH")
result.tmp <- left_join(result.tmp, linkCH, relationship = "many-to-many")
result.tmp <- result.tmp[,c("CN", "date", "NUTS3", "NAME", "weighted_temperature")]
colnames(result.tmp) <- c("canton", "date", "ID_PE", "NAME", "temperature")

saveRDS(result.tmp, file="output/temperature_pop_weighted.RDS")

# check
if(check == TRUE){
  tmp <- readRDS("output/temperature")
  head(tmp)
  table(tmp$NAME)
  tmp2check <- left_join(tmp, result.tmp, by = c("date" = "date", "NAME" = "NAME"))
  sum(is.na(tmp2check$temperature))
  
  plot(tmp2check$mean.temp, tmp2check$temperature)
  # the population weighted version is a bit higher as the alps are sparsely populated with higher temperatures.
  
}




#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################




