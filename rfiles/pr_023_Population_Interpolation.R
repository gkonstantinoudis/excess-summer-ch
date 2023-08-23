#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Population per week
#:::::::::::::::::::::::::::::

country <- "CH"
pop <- readRDS(file.path(path_output, paste0("pop2011_2022", country, ".rds")))

# the population file should have the following format:
# NUTS318CD   ageg  sex year population
# 1         1 less40 male 2015      64769
# 2         1 less40 male 2016      62578
# 3         1 less40 male 2017      68788
# 4         1 less40 male 2018      62038
# 5         1 less40 male 2019      67761
# 6         1 less40 male 2020      60105


pop %>% dplyr::filter(year <= 2022) %>% 
  dplyr::select(NUTS3_code, age.groups, sex, year, population) %>% 
  dplyr::rename(NUTS318CD = NUTS3_code, ageg = age.groups) %>% 
  dplyr::mutate(sex = ifelse(sex == 1, "male", "female"), 
         ageg = recode(ageg, `0-39` = "less40", `80+` = "80plus")) -> pop

pop$year <- pop$year - 1

start_date = "2010-12-31"
end_date = "2022-12-31"

pop$daily_date <- as.Date(paste0(pop$year, "-", substr(start_date, start = 6, stop = 10)))

expand.grid(age = c("less40", "40-59", "60-69", "70-79", "80plus"), 
            sex = c("male", "female"), 
            region = unique(pop$NUTS318CD), 
            daily_date = seq(as.Date(start_date), as.Date(end_date), by="days")) -> pop_daily

pop_daily %>% dplyr::mutate(year = as.numeric(substr(daily_date, start = 1, stop = 4))) -> pop_daily
pop_daily$day2pred <- as.numeric(pop_daily$daily_date) - as.numeric(as.Date(start_date)) + 1





# Add the predictions

pred2022 <- readRDS(file.path(path_output,paste0("pois.samples.population", country, ".rds")))
pred2022$year <- pred2022$year - 1

# clean the pred2021_23 to be compatible with the pop

pred2022 %>% 
  filter(year == "2022") %>% 
  dplyr::rename(NUTS318CD = NUTS3_code, ageg = age.groups) %>% 
  dplyr::mutate(sex = ifelse(sex == 1, "male", "female"), 
         ageg = recode(ageg, `0-39` = "less40", `80+` = "80plus"), 
         daily_date = as.Date(paste0(year, "-", substr(start_date, start = 6, stop = 10)))) -> pred2022


# create a list of pop data frames and loop over it

pop_store <- pop
pop_daily_store <- pop_daily
CHECK <- FALSE
listpop <- NULL


t_0 <- Sys.time()
for(i in 1:200){
  
  if(i %% 10 == 0){
    print(i)
  }
  
  pop <- pop_store
  pop_daily <- pop_daily_store
  
  pop$year <- as.character(pop$year)
  pop_daily$year <- as.character(pop_daily$year)
  
  pop_loop <- pred2022[,c("year", "NUTS318CD", "sex", "ageg", "daily_date")]
  pop_loop %>% 
    dplyr::select(year, NUTS318CD, sex, ageg, daily_date) %>% 
    dplyr::mutate(year = as.character(year)) -> pop_loop
  
  pop_loop$population <- pred2022 %>% select(paste0("V", i)) %>% pull(paste0("V", i))

  
  pop <- rbind(pop, pop_loop[,colnames(pop)])
  
  pop_daily$age <- as.character(pop_daily$age)
  pop$ageg <- as.character(pop$ageg)
  
  pop_daily <- left_join(pop_daily, pop, 
                         by = c("age" = "ageg", 
                                "sex" = "sex", 
                                "year" = "year",
                                "region" = "NUTS318CD", 
                                "daily_date" = "daily_date"))


  
  # impute the missing. This is particularly important for England as the populations as available for the mid-year
  # and creates artefacts if the data cleaning is done based on the year only. I am conducting a forward imputation
  # assuming the value of the population to be the same till the new value comes in the data
  
  pop_daily %>% 
    group_by(age, sex, region) %>% 
    arrange(daily_date) %>% 
    fill(population, .direction = "downup") -> pop_daily
  

  
  
  # need to add the population of next year
  pop$year <- as.numeric(pop$year)
  pop$year <- pop$year - 1
  pop %>% dplyr::rename(pop.next.year = population) -> pop
  pop$daily_date <- pop$daily_date %m-% years(1)
  
  pop_daily$year <- as.numeric(pop_daily$year)
  pop_daily <- left_join(pop_daily, pop, 
                         by = c("year" = "year", 
                                "age" = "ageg", 
                                "sex" = "sex", 
                                "region" = "NUTS318CD", 
                                "daily_date" = "daily_date")
                         )
  
  # SET REFERENCE DATES BEFORE IMPUTING
  pop_daily$refdate <- pop_daily$daily_date
  pop_daily$refdate[is.na(pop_daily$pop.next.year)] <- NA
  
  # and impute again
  pop_daily %>% 
    group_by(age, sex, region) %>% 
    arrange(daily_date) %>% 
    fill(pop.next.year, refdate, .direction = "downup") -> pop_daily
  
  pop_daily$refdate2 <- pop_daily$refdate %m+% years(1)
  
  # INTERPOLATE
  pop_daily %>% mutate(lambda = (pop.next.year - population)/as.numeric((refdate2 - refdate))) %>% 
    mutate(beta0 = population - lambda*as.numeric(refdate - as.Date(start_date) + 1)) %>% 
    mutate(popfin = beta0 + lambda*day2pred) -> pop_daily
  
  if(CHECK == TRUE){
    
    pop_daily %>% filter(age %in% "80plus", 
                         sex %in% "female", 
                         region  %in% "CH040") %>%  
      ggplot() + geom_line(aes(x=daily_date, y = popfin)) + 
      geom_line(aes(x=daily_date, y = population), col = "red", linetype = "dashed")
    
  }

  pop_daily <- pop_daily[complete.cases(pop_daily$popfin),]
  pop_daily$refdate <- pop_daily$pop.next.year <- pop_daily$refdate2 <-
  pop_daily$lambda <- pop_daily$beta0 <- pop_daily$days2plot <- 
  pop_daily$population <- pop_daily$day2pred <- NULL
  
  pop_daily %>% dplyr::rename(NUTS318CD = region, 
                       population = popfin) -> pop_daily

  
  # I would subset for summer months during 2010-2022
  pop_daily %>% 
    filter(daily_date >= "2010-06-01") %>% 
    mutate(month = month(daily_date)) %>% 
    filter(month %in% c(6,7,8)) %>% 
    select(-month, -year)-> pop_daily
  
  pop_daily %>% setNames(c(colnames(pop_daily)[-length(colnames(pop_daily))], paste0("popV", i))) -> pop_daily
  
  if(is.null(listpop)){
    listpop <- pop_daily
  }else{
    listpop <- left_join(listpop, pop_daily)
  }
  rm(pop_daily)
  gc()
}

t_1 <- Sys.time()
t_1 - t_0 # ~20 minutes



saveRDS(listpop, file = file.path(path_output,"popfin_post_df.rds"))
rm(listpop)
gc()



#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################




