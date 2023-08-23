#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: deaths
#:::::::::::::::::::::::::::::

# download deaths from: https://www.bfs.admin.ch/bfs/en/home/statistics/population/births-deaths/deaths.assetdetail.22324726.html
# and store it as ch_deaths_YEAR.csv in the data folder

country <- "CH"

# Clean deaths

deaths_2010_2022 <- read.csv("data/Datenlieferung_Mortality_20230619.csv", sep = ",", header = TRUE)
deaths_2010_2022$time <- as.Date(deaths_2010_2022$time, format = "%d-%B-%y")
deaths_2010_2022$canton[deaths_2010_2022$canton %in% "Graub\xfcnden"] <- "Graubunden"

exp.grd <- expand.grid(
  time = seq(from = as.Date("2010-01-01"), to = as.Date("2022-12-31"), by = 1), 
  canton = unique(deaths_2010_2022$canton), 
  age = unique(deaths_2010_2022$age), 
  sex = unique(deaths_2010_2022$sex)
)
deaths_2010_2022 <- left_join(exp.grd, deaths_2010_2022, by = c("time" = "time", "canton" = "canton", "age" = "age", "sex" = "sex"))


linkCH <- readRDS("output/linkCH")
table(linkCH$Name_german)
linkCH$Name_german[linkCH$Name_german %in% "Appenzell I.Rh"] <- "Appenzell I.Rh."
linkCH$Name_german[linkCH$Name_german %in% "Appenzell A.Rh"] <- "Appenzell A.Rh."
table(deaths_2010_2022$canton)

deaths_2010_2022 <- left_join(deaths_2010_2022, linkCH, by = c("canton" = "Name_german"))
deaths_2010_2022 %>% mutate(deaths = ifelse(is.na(deaths), 0, deaths)) -> deaths_2010_2022
deaths_2010_2022$canton %>% table() %>% length()
deaths_2010_2022$NUTS3 %>% table() %>% length()
# deaths_2010_2022 %>% dplyr::group_by(canton) %>% dplyr::summarize(sum(deaths)) %>% View()
##
##
## Bring everything together
hol <- readRDS(file.path("output","holCH.rds"))
hol$date <- as.Date(hol$date)
shp <- sf::read_sf("data/shp.shp")



holch <- hol %>% filter(county %in% "CH") %>% filter(!duplicated(date))
holcn <- hol %>% filter(!(county %in% "CH")) %>% mutate(ID = paste0(county, date)) %>% filter(!duplicated(ID))
holcn$CN <- substr(holcn$county, start = 4, stop = 5)

findata <- left_join(deaths_2010_2022, holch[, c("date", "hol")], by = c("time" = "date"))
findata <- left_join(findata, holcn[, c("date", "hol", "CN")], by = c("time" = "date", "CN" = "CN"))
findata$hol.x[is.na(findata$hol.x)] <- 0
findata$hol.y[is.na(findata$hol.y)] <- 0
findata$hol <- findata$hol.x + findata$hol.y
findata$hol[findata$hol>=1] <- 1

findata$hol.x <- findata$hol.y <- NULL

# and now the temperature
# temperature <- readRDS(file.path("output","temperature"))
temperature_weighted <- readRDS(file.path("output","temperature_pop_weighted.RDS"))
# define the heatwave

# threshold1 <- 25
# threshold2 <- quantile(temperature$mean.temp, probs = 0.99)
# 
# temperature %>%
#   # need arrange by name and date for lag
#   dplyr::arrange(NAME, date) %>%
#   # make sure we dont go over cantons
#   dplyr::group_by(NAME) %>%
#   dplyr::mutate(
#     heatwave_l2 = dplyr::case_when(mean.temp >= threshold1 ~ 1, 
#                                    TRUE ~ 0),
#     heatwave_l3 = dplyr::case_when(mean.temp >= threshold1 &
#                                      dplyr::lag(mean.temp, 1) >= threshold1 & 
#                                      dplyr::lag(mean.temp, 2) >= threshold1 ~ 1, 
#                                      TRUE ~ 0), 
#     heatwave_percentile_l2 = dplyr::case_when(mean.temp >= threshold2 ~ 1, 
#                                               TRUE ~ 0),
#     heatwave_percentile_l3 = dplyr::case_when(mean.temp >= threshold2 &
#                                      dplyr::lag(mean.temp, 1) >= threshold2 & 
#                                      dplyr::lag(mean.temp, 2) >= threshold2 ~ 1, 
#                                    TRUE ~ 0)
#     ) -> temperature
# 
# # to make periods of 1s
# temperature$heatwave_l3_1 <- temperature$heatwave_l3_2 <- 0
# temperature$heatwave_l3_1[which(temperature$heatwave_l3 == 1) - 1] <- 1
# temperature$heatwave_l3_2[which(temperature$heatwave_l3 == 1) - 2] <- 1
# temperature$heatwave_l3 <- temperature$heatwave_l3 + temperature$heatwave_l3_1 + temperature$heatwave_l3_2
# temperature$heatwave_l3_1 <- temperature$heatwave_l3_2 <- NULL
# temperature$heatwave_l3[temperature$heatwave_l3 > 1] <- 1
#   
# # similar for the percentile
# temperature$heatwave_percentile_l3_1 <- temperature$heatwave_percentile_l3_2 <- 0
# temperature$heatwave_percentile_l3_1[which(temperature$heatwave_percentile_l3 == 1) - 1] <- 1
# temperature$heatwave_percentile_l3_2[which(temperature$heatwave_percentile_l3 == 1) - 2] <- 1
# temperature$heatwave_percentile_l3 <- temperature$heatwave_percentile_l3 + temperature$heatwave_percentile_l3_1 + temperature$heatwave_percentile_l3_2
# temperature$heatwave_percentile_l3_1 <- temperature$heatwave_percentile_2 <- NULL
# temperature$heatwave_percentile_l3[temperature$heatwave_percentile_l3 > 1] <- 1

linkCH$NAME[linkCH$NAME %in% "Gen\xe8ve"] <- "Genève"
linkCH$NAME[linkCH$NAME %in% "Z\xfcrich"] <- "Zürich"
linkCH$NAME[linkCH$NAME %in% "Graub\xfcnden"] <- "Graubünden"
linkCH$NAME[linkCH$NAME %in% "Neuch\xe2tel"] <- "Neuchâtel"

temperature <- left_join(temperature_weighted, linkCH, by = c("canton" = "CN", "NAME" = "NAME"))

# checks
findata$canton %>% table()
findata$canton %>% table() %>% length()
temperature$NUTS3 %>% table() %>% length()
findata$NUTS3 %>% table()

findata <- left_join(findata, temperature_weighted, by = c("time" = "date", "NUTS3" = "ID_PE"))

## add also covid-19 deaths
covid <- readRDS(file.path("data","daily_labd_2022-10-18.rds"))
covid %>% filter(pttoddat <= "2022-08-31") -> covid

# restrict to summer
findata %>% 
  dplyr::mutate(month = month(time), year = year(time)) %>% 
  dplyr::filter(month %in% 6:8, year > 2010) %>% 
  dplyr::select(!c(month, year)) -> findata

# and pop weighted temperature
findata <- left_join(findata, 
                     temperature_weighted %>% dplyr::select(date, ID_PE, temperature_weighted = temperature), 
                     by = c("time" = "date", "NUTS3" = "ID_PE"))


summary(findata)
head(findata)
findata$canton.y <- findata$NAME.y <- NULL
findata$temperature <- NULL
findata %>% dplyr::rename(canton = canton.x, NAME = NAME.x) -> findata
# get the posterior of the previous paper
# tmp <- readRDS(file.path("data","combined_samples_trun_temperature_OV_0.001_finmodel"))
# 
# # keep only age and phase
# 
# cbind(
#   # tmp$age_group[,6:10],
#   tmp$phase[,8:14]
# ) -> post.prev.paper

# 
# hist(post.prev.paper$`beta[6]:age_group_0-39:age_group_0-39`)
# hist(post.prev.paper$`beta[7]:age_group_40-59:age_group_40-59`)
# hist(post.prev.paper$`beta[8]:age_group_60-69:age_group_60-69`)
# hist(post.prev.paper$`beta[9]:age_group_70-79:age_group_70-79`)
# hist(post.prev.paper$`beta[10]:age_group_80+:age_group_80+`)
# 
# 
# mean(post.prev.paper$`beta[8]:phase_1`>1)
# mean(post.prev.paper$`beta[9]:phase_2`>1)
# mean(post.prev.paper$`beta[10]:phase_3`>1)
# mean(post.prev.paper$`beta[11]:phase_4`>1)
# mean(post.prev.paper$`beta[12]:phase_5`>1)
# mean(post.prev.paper$`beta[13]:phase_6`>1)
# mean(post.prev.paper$`beta[14]:phase_7`>1)
# 
# 
# # we calibrated the phases for which we have a Pr>0.95 of an effect and where relatively stable. These are 1, 3 and 6
# hist(post.prev.paper$`beta[8]:phase_1`)
# hist(post.prev.paper$`beta[10]:phase_3`)
# hist(post.prev.paper$`beta[13]:phase_6`)
# 
# hist(1/post.prev.paper$`beta[8]:phase_1`)
# hist(1/post.prev.paper$`beta[10]:phase_3`)
# hist(1/post.prev.paper$`beta[13]:phase_6`)
# 
# post.prev.paper <- t(post.prev.paper)
# dim(post.prev.paper)
# post.prev.paper %>% as.data.frame() %>% mutate(phase = 1:7) -> post.prev.paper
# 
# covid$phase <- 1
# covid$phase[covid$pttoddat >= as.Date("2020-06-28") & covid$pttoddat <= as.Date("2020-09-27")] <- 2
# covid$phase[covid$pttoddat >= as.Date("2020-09-28") & covid$pttoddat <= as.Date("2021-02-14")] <- 3
# covid$phase[covid$pttoddat >= as.Date("2021-02-15") & covid$pttoddat <= as.Date("2021-06-20")] <- 4
# covid$phase[covid$pttoddat >= as.Date("2021-06-21") & covid$pttoddat <= as.Date("2021-10-10")] <- 5
# covid$phase[covid$pttoddat >= as.Date("2021-10-11") & covid$pttoddat <= as.Date("2021-12-19")] <- 6
# covid$phase[covid$pttoddat >= as.Date("2021-12-20")] <- 7
# 
# 
# covid <- left_join(covid, post.prev.paper, by = c("phase" = "phase"))
# covid %>% ungroup() %>% dplyr::select(starts_with("V"))
# 
# 
# sweep(covid %>% ungroup() %>% dplyr::select(starts_with("V")), 
#       1, 
#       covid$n, 
#       FUN = "*") -> sweep_dat
# 
# sweep_dat[which(covid$phase %in% c(2, 4, 5, 7)),] <- covid$n[which(covid$phase %in% c(2, 4, 5, 7))]
# 
# 
# cbind(covid %>% select(canton, age_group, sex, pttoddat, n, phase), sweep_dat) -> covid

findata <- left_join(findata, covid, by = c("time" = "pttoddat", 
                                            "sex" = "sex", 
                                            "age" = "age_group", 
                                            "CN" = "canton"))
findata %>% head()
# covid %>% group_by(canton) %>% dplyr::summarize(sum(n, na.rm = TRUE)) %>% t()
# findata %>% group_by(CN) %>% dplyr::summarize(sum(n, na.rm = TRUE)) %>% t()
# findata[is.na(findata$n), startsWith(colnames(findata), "V")] <- 0

findata %>% mutate(covid_deaths = ifelse(is.na(n), 0, n)) %>% select(!n) -> findata

saveRDS(findata, file = file.path(path_output, paste0("findata", country, ".rds")))



rm(list = ls())
gc()


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################



