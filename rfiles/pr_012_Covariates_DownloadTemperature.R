#:::::::::::::::::::::::::::::
# Project: excess deaths summer 2022
# description: Download temperature ERA5
#:::::::::::::::::::::::::::::

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# agree with the terms here: https://cds.climate.copernicus.eu/cdsapp/#!/terms/licence-to-use-copernicus-products,
# log in and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.

cds.user <- "" # Insert your CDS user here
cds.key <- "" #"Insert_your_CDS_API_KEY_here"

# Set up the API and UID
wf_set_key(user = cds.user, key = cds.key, service = "cds")

if(is.null(cds.user) | is.null(cds.key)) {
  print("You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, and once you are ok and logged in, click on your name on the top right next to logout and retrieve the information about the API key.")
}


DonwloadTemperature <- function(X){
  
  request <- list(
    dataset_short_name = "reanalysis-era5-land",
    product_type   = "reanalysis",
    format = "netcdf",
    variable = "2m_temperature",
    date = X, # this is to match the ISO weeks
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
             "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
             "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
    # area is specified as N, W, S, E
    area = c(48, 5, 45, 11),
    target = paste0("temperature", sub(pattern = "/", replacement = "_", x=X), ".nc")
  )
  
  if(!file.exists(paste0("Output/temperature", sub(pattern = "/", replacement = "_", x=X), ".nc"))) {
    file <- wf_request(user = cds.user,
                       request = request,
                       transfer = TRUE,
                       path = "Output",
                       time_out = 3600*24,
                       verbose = TRUE)
  }
  
}


year <- 2011:2022
define_dates <- lapply(year, function(X) paste(X, c("-05-30", "-07-01", "-08-01", "-08-31"), sep = "")) 

lapply(define_dates, function(X) data.frame(start = X[-length(X)], end = X[-1])) %>% do.call(rbind,.) -> define_dates

define_dates$start <- as.Date(define_dates$start)
define_dates$end <- as.Date(define_dates$end)

define_dates$start <- define_dates$start + 1
# keep last day of May to account for the time difference

toloop <- paste(define_dates$start, define_dates$end, sep = "/")

# run on parallel
funpar <- function(k) DonwloadTemperature(X = toloop[k])

t_0 <- Sys.time()

# Set up parallel environment
ncores <- 20
# ncores <- detectCores() - 1
k <- 1:length(toloop)
cl_inla <- makeCluster(ncores, methods=FALSE)

# extract packages on parallel environment 
clusterEvalQ(cl_inla, {
  library(ecmwfr)
})

# extract R objects on parallel environment
clusterExport(cl_inla, c("toloop", "DonwloadTemperature", "cds.user", "cds.key"))

# run the the function in parallel
outpar <- parLapply(cl = cl_inla, k, funpar)

# close parallel environment
stopCluster(cl_inla)
t_1 <- Sys.time()
t_1 - t_0 


rm(list = ls())
gc()



#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################



