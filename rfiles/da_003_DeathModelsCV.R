#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: clean data
#:::::::::::::::::::::::::::::


da_003_DeathModelsCV <- function() {
  
  ## clean outcome death and bring everything together
  source("rfiles/pr_031_Outcome_Deaths.R", local=TRUE)
  ## Check best performing model
  source("rfiles/pr_041_CrossValidation.R", local=TRUE)

}



