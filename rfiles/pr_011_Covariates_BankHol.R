#:::::::::::::::::::::::::::::
# Project: excess deaths summer 2022
# description: Get public holidays in England
#:::::::::::::::::::::::::::::

# select country
country <- "CH"

years <- 2010:2022
pathURL <- paste0("https://date.nager.at/api/v2/publicholidays/", years, "/", country)

gatBankHol <- function(X){
  bankHolidays <- fromJSON(X)
  
  bankHolidays$counties[sapply(bankHolidays$counties, is.null)] <- country
  
  bankHolidays %>% dplyr::select(date, counties) -> bankHolidays
  
  N <- nrow(bankHolidays)
  max.cantons <- max(sapply(bankHolidays$counties, length))
  
  mat <- as.data.frame(matrix(NA, ncol = max.cantons, nrow = N))
  
  for(i in 1:N){
    
    m <- lengths(bankHolidays$counties[i])
    mat[i, 1:m] <- bankHolidays$counties[[i]]
    
  }
  
  mat$date <- bankHolidays$date
  
  # long format
  data_long <- gather(mat, name.col, county, V1:V3, factor_key=TRUE)
  data_long$name.col <- NULL
  data_long <- data_long[complete.cases(data_long$county),]
  data_long$hol <- 1
  
  return(data_long)
}

lapply(pathURL, gatBankHol) -> hol
hol <- do.call(rbind, hol)
hol <- hol %>% select(date, county, hol) 
if(country == "GB"){
  hol %>% filter(county %in% c("GB", "GB-ENG")) -> hol
}

saveRDS(hol, file = file.path(path_output, paste0("hol", country, ".rds")))


rm(list = ls())
gc()


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################










