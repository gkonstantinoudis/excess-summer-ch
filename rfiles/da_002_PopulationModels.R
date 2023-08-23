#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: clean data
#:::::::::::::::::::::::::::::


da_002_PopulationModels <- function() {
  
  ## population in Switzerland by canton, year, age and sex
  source("rfiles/pr_021_Population_Models.R", local = TRUE)
  ## download temperature
  source("rfiles/pr_022_Population_Predictions.R")
  ## linear interpolation
  source("rfiles/pr_023_Population_Interpolation.R")

}

