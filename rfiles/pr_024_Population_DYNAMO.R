#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: Dynamic population estimation
#:::::::::::::::::::::::::::::

country <- "CH"

setwd("C:/Users/gkonstan/Desktop/summer 2022/")

# load clean population file
pop <- readRDS(file.path(path_output, paste0("pop2011_2022", country, ".rds")))

# load full population file to get the changes by age group
pop_CHANGE <- read.csv("data/popCH2010_2021.csv", sep = ";", header = TRUE, fileEncoding = "ISO-8859-3")

# get the population predictions
pred23 <- readRDS(file.path(path_output,"pois.samples.populationCH.rds"))
pred23 <- pred23 %>% filter(year == 2023)
# load the deaths
deaths <- readRDS(file.path(path_output, paste0("findata", country, ".rds")))
deaths$year <- year(deaths$time)

pop$sex <- recode(pop$sex, '1'='male', '2'='female')

# ##
# ## Add the population of the next year
pop_next <- pop
pop_next$canton_id <- pop_next$Urban <- pop_next$semi <- pop_next$rural <- 
  pop_next$lang_reg <- pop_next$NAME <- pop_next$NUTS3_code <- NULL
pop_next$pop_next_year <- pop_next$population
pop_next$year <- pop_next$year - 1
pop_next$population <- NULL
pop_dat <- left_join(pop, pop_next)

##
## Add deaths current year
# 
# For simplicity we assume that the number on the 31st of December corresponds to the 1st of January for the population. 
# For the deaths there is a similar assumption; the aggregation is done by year and not by year minus 31st of December
deaths_yearly <- deaths %>% dplyr::group_by(year, NUTS3_code, age, sex) %>% dplyr::summarize(deaths.yearly = sum(deaths))

pop_dat$sex <- recode(pop_dat$sex, '1'='male', '2'='female')
pop_dat <- left_join(pop_dat, deaths_yearly, by = c("age.groups" = "age", "sex" = "sex", "NUTS3_code" = "NUTS3_code", "year" = "year"))

# its fine
# sum(is.na(pop_dat$deaths.yearly))
# pop_dat$year[is.na(pop_dat$deaths.yearly)]
# pop_dat$year[is.na(pop_dat$population)]

##
##


# Now we have NAs for the ones predicted for 2023. We need to merge with the predictions
pred23$sex <- recode(pred23$sex, '1'='male', '2'='female')
pred23$lang_reg <- NULL
pop_dat <- left_join(pop_dat, pred23 %>% dplyr::select(year, NUTS3, sex, age.groups, starts_with("V")), 
                     by = c("year" = "year", "NUTS3" = "NUTS3", "sex" = "sex", "age.groups" = "age.groups"))



##
## 

start_date <- "2011-01-01"
end_date <- "2023-01-01"
expand.grid(age = c("0-39", "40-59", "60-69", "70-79", "80+"), 
            sex = c("male", "female"), 
            region = unique(pop_dat$NUTS3), 
            daily_date = seq(as.Date(start_date), as.Date(end_date), by="days")) -> pop_daily
pop_daily$year <- year(pop_daily$daily_date)
pop_daily <- left_join(pop_daily, pop_dat, by = c("year" = "year", "age" = "age.groups", "region" = "NUTS3", "sex" = "sex"))

# the only thing that should vary daily is the number of deaths, so we need to put it here
pop_daily$deaths <- NULL
pop_daily <- left_join(pop_daily, deaths %>% select(time, NUTS318CD, sex, age, deaths), 
                       by = c("daily_date" = "time", "age" = "age", "region" = "NUTS318CD", "sex" = "sex"))


sum(is.na(pop_daily$deaths))
pop_daily$daily_date[is.na(pop_daily$deaths)] # it is fine as we dont really had the data after summer 2022.


##
##

# function to get the dynamic population, X is the sample

calc_dynapo <- function(X){
  
  # this is by assuming constant population
  pop_daily$pop <- pop_daily$population
  pop_daily$pop.2023 <- pop_daily %>% dplyr::pull(paste0("V", X))
  pop_daily$pop.2023[is.na(pop_daily$pop.2023)] <- 0
  
  pop_daily$pop[is.na(pop_daily$pop)] <- 0
  pop_daily$pop <- pop_daily$pop + pop_daily$pop.2023

  pop_next <- pop_daily %>% select(age, sex, canton_id, year, pop)
  dupl <- !duplicated(paste0(pop_next$age, pop_next$sex, pop_next$canton_id, pop_next$year))
  pop_next <- pop_next[dupl,]
  pop_next <- pop_next %>% dplyr::rename(pop.next = pop)
  pop_next$year <- pop_next$year - 1
  pop_daily <- left_join(pop_daily, pop_next, by = c("age" = "age", "sex" = "sex", "year" = "year", "canton_id" = "canton_id"))
  
  # net effect:
  pop_daily$net.effect <- 
    pop_daily$pop.next - 
    pop_daily$pop + 
    pop_daily$deaths.yearly
  
  pop_daily$net.effect.daily <- pop_daily$net.effect/365.25
  pop_daily$add.portion <- 
    - pop_daily$deaths +
    pop_daily$net.effect.daily
  
  # defining the cumsum like this one automatically assumes that the pop refers to 31st Dec which are the correct assumptions. 
  pop_daily %>% 
    dplyr::arrange(daily_date) %>% 
    dplyr::group_by(age, sex, year, canton_id) %>% 
    dplyr::mutate(Z1 = cumsum(add.portion)) -> pop_daily
  
  pop_daily %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(popdyn=pop+Z1) %>% 
    dplyr::select(age, sex, canton_id, daily_date, popdyn, pop) -> pop_daily 
  
  return(pop_daily[complete.cases(pop_daily$popdyn),])
  
} # ~ 30 minutes if you loop


datloop <- calc_dynapo(X = 1)
datloop %>% dplyr::rename_with(.cols = "popdyn", .fn = ~paste0(., 1)) -> datloop


t_0 <- Sys.time()
for(i in 2:200){
  ld <- calc_dynapo(X = i)
  ld <- ld %>% dplyr::rename_with(.cols = "popdyn", .fn = ~paste0(., i))
  ld$pop <- NULL
  datloop <- left_join(datloop, ld, by = c("age" = "age", "sex" = "sex", "canton_id" = "canton_id", "daily_date" = "daily_date"))
}
t_1 <- Sys.time()
t_1 - t_0 # ~15 minutes

saveRDS(datloop, file = file.path(path_output, "datdynpop.rds"))


##
##



# a couple of checks

# datloop <- readRDS(file.path(path_output, "datdynpop.rds"))
# 
# ageg <- "70-79"
# sexg <- "male"
# can <- "ZH"
# chk <- datloop %>% filter(canton_id %in% can, age %in% ageg, sex %in% sexg)
# 
# 
# plot(chk$pop, type = "l", ylim = c(40000, 58000))
# for(i in 1:200){
#   lines(chk[,paste0("popdyn", i)], type = "l", col = "red")
# }

# Some more checks


# # linear interpolation 
# pop.list <- readRDS(file.path(path_output, "popfin_post_df.rds"))
# # DYNAMO
# pop.list.dyn <- readRDS(file.path(path_output, "datdynpop.rds"))
# # predictions
# pred <- readRDS(file.path(path_output,"pois.samples.populationCH.rds"))
# pred$sex <- recode(pred$sex, '1'='male', '2'='female')
# start_date <- "2011-01-01"
# end_date <- "2023-01-01"
# expand.grid(age = unique(pred$age.groups), 
#             sex = unique(pred$sex), 
#             region = unique(pred$NUTS3), 
#             daily_date = seq(as.Date(start_date), as.Date(end_date), by="days")) -> pop_daily
# pop_daily$year <- year(pop_daily$daily_date)
# pop_daily <- left_join(pop_daily, pred, by = c("year" = "year", "age" = "age.groups", "region" = "NUTS3", "sex" = "sex"))
# # deaths
# findata <- readRDS(file.path(path_output, "findataCH.rds"))
# 
# sexg <- "female"
# ageg <- "80+"
# 
# 
# pop.list %>% 
#   dplyr::filter(sex %in% sexg, age %in% "80plus") %>% 
#   dplyr::group_by(daily_date) %>% 
#   dplyr::summarise(tot = sum(popV110)) -> tmplp
# 
# pop.list.dyn %>% 
#   dplyr::filter(sex %in% sexg, age %in% ageg) %>% 
#   dplyr::group_by(daily_date) %>% 
#   dplyr::summarise(totdyn = sum(popdyn110)) -> tmpdyn
# 
# pop_daily %>% 
#   dplyr::filter(sex %in% sexg, age %in% ageg) %>% 
#   dplyr::group_by(daily_date) %>% 
#   dplyr::summarise(const = sum(V110)) -> tmpconst
# 
# ggplot() + 
#   geom_line(data = tmplp, aes(x=daily_date, y=tot)) + 
#   geom_line(data = tmpdyn, aes(x=daily_date, y=totdyn), col = "blue") + 
#   geom_line(data = tmpconst, aes(x=daily_date, y=const), col = "red")



#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################


