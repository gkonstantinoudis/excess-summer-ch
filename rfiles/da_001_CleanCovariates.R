#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: clean data
#:::::::::::::::::::::::::::::


da_001_CleanCovariates <- function() {
  
  ## public holidays in Switzerland by canton
  source("rfiles/pr_011_Covariates_BankHol.R")
  ## download temperature
  source("rfiles/pr_012_Covariates_DownloadTemperature.R")
  ## daily mean temperature by canton
  source("rfiles/pr_013_Covariates_CleanTemperature.R")
  ## population weighted temperature
  source("rfiles/pr_014_Covariates_WeightedTemperature.R")
  
}