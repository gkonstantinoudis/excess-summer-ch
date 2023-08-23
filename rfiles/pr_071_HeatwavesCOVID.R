FitHeatwaveCOVID <- function(ageg, sexg, y2021 = FALSE, ncores = 10, thr, temp = TRUE, definition = "exposure2heatwave"){
  
  # thet <- c(3.7853151, 2.8500302, -0.9965181)
  threshold <- thr
  thet <- NULL
  # read the data
  
  # read the data
  shp = sf::read_sf("data/shp.shp")
  
  shp$nameid <- factor(shp$NAME)
  shp %>% arrange(nameid) -> shp
  
  W.nb <- spdep::poly2nb(shp)
  spdep::nb2INLA("data/W.adj", W.nb) 
  
  data <- readRDS(file.path(path_output, "datmod.rds"))
  
  pop.list <- readRDS(file.path(path_output, "datdynpop.rds"))
  setnames(pop.list, old = paste0("popdyn", 1:200), new = paste0("popV", 1:200))
  pop.list$pop <- NULL
  
  data <- left_join(data, pop.list, 
                    by = c("age" = "age", 
                           "sex" = "sex", 
                           "time" = "daily_date", 
                           "CN" = "canton_id"))
  
  # I need to add holidays and dow here
  data$dow <- lubridate::wday(data$time)
    
  base <- c("1", 
            "offset(log(pop))",  
            "factor(hol)",
            "factor(dow)",
            "factor(hotperiods_l1_lag0_3)",
            "f(id_space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
            "f(id_time, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)")
  
  
  if(y2021==TRUE){
    data %>% 
    mutate(month = month(time) %>% as.numeric(), year = year(time) %>% as.numeric()) %>% 
    filter(month %in% 6:8, year %in% 2021:2022) -> data
    
    base <- 
      c(base, "factor(id_year)")

    }else{
      data %>% 
        mutate(month = month(time) %>% as.numeric(), year = year(time) %>% as.numeric()) %>% 
        filter(month %in% 6:8, year %in% 2022) -> data 
      
      base <- base
    }
  
  if(temp == TRUE){
    base <- c(base, "temperature_weighted")
  }else{
    base <- base
  }
  
  mod <- paste("covid_deaths", paste(base, collapse=" + "), sep=" ~ ")
  
  if(definition == "heatwaveperiod"){
  data %>%
    dplyr::arrange(canton, time) %>%
    dplyr::group_by(canton, sex, age) %>%
    dplyr::mutate(
      hotperiods_l1 = dplyr::case_when(temperature_weighted >= threshold &
                                         dplyr::lag(temperature_weighted, 1) >= threshold & 
                                         dplyr::lag(temperature_weighted, 2) >= threshold ~ 1, 
                                       TRUE ~ 0), # this gives 1 to the third consecutive temperature larger than the threshold, so we need to flag as
      # 1 the two previous too
      hotperiods_l1 = dplyr::case_when(hotperiods_l1 == 1 |
                                         dplyr::lead(hotperiods_l1, 1) == 1 |
                                         dplyr::lead(hotperiods_l1, 2) == 1 ~ 1,  
                                       TRUE ~ 0), 
      hotperiods_l1_lag0_3 = dplyr::case_when(hotperiods_l1 == 1 |
                                                dplyr::lag(hotperiods_l1, 1) == 1 |
                                                dplyr::lag(hotperiods_l1, 2) == 1 |
                                                dplyr::lag(hotperiods_l1, 3) == 1 ~ 1,  
                                              TRUE ~ 0) # and these are the heat periods with 0 days lag to account for delayed effects
    ) -> res_tab
  
  }
  
  if(definition == "exposure2heatwave"){
  data %>%
    dplyr::arrange(canton, time) %>%
    dplyr::group_by(canton, sex, age) %>%
    dplyr::mutate(
      hotperiods_l1 = dplyr::case_when(temperature_weighted >= threshold &
                                         dplyr::lag(temperature_weighted, 1) >= threshold & 
                                         dplyr::lag(temperature_weighted, 2) >= threshold ~ 1, 
                                       TRUE ~ 0), # this gives 1 to the third consecutive temperature larger than the threshold, so we need to flag as
      hotperiods_l1_lag0_3 = dplyr::case_when(hotperiods_l1 == 1 |
                                                dplyr::lag(hotperiods_l1, 1) == 1 |
                                                dplyr::lag(hotperiods_l1, 2) == 1 |
                                                dplyr::lag(hotperiods_l1, 3) == 1 ~ 1,  
                                              TRUE ~ 0) # and these are the heat periods with 0 days lag to account for delayed effects
    ) -> res_tab
  }
  
  res_tab$id_time <- yday(res_tab$time) %>% as.factor() %>% as.numeric()
  res_tab$id_year <- res_tab$year %>% as.factor() %>% as.numeric()

  data <- res_tab
  rm(res_tab);gc() 
  
  
  data$id_space <- data$canton %>% as.factor() %>% as.numeric()

  # INLA SET UP
  # priors
  hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  
  # Under Poisson uses default set up
  control.family=inla.set.control.family.default()
  data_subset <- data %>% filter(age %in% ageg, sex %in% sexg)
  rm(data);gc()
  agegnam <- gsub("\\+", "", as.character(ageg))
  agegnam <- gsub("-", "_", as.character(ageg))
  nsamples.inla = 200
  
  inla.funpar <- function(X){
    
    inla.setOption(inla.timeout=600)
    
    tryCatch({
    data_subset %>% mutate(pop = get(str_c('popV', X))) -> data_sub
    
    in.mod = inla(as.formula(mod),
                  data=data_sub,
                  family="Poisson",  
                  verbose = FALSE, 
                  control.family=control.family,
                  control.compute=list(config = TRUE), 
                  control.mode=list(theta = NULL, restart=TRUE),
                  num.threads = 5, 
                  control.predictor = list(link = 1))

    in.mod = inla.rerun(in.mod)
    exp(inla.rmarginal(in.mod$marginals.fixed$`factor(hotperiods_l1_lag0_3)1`, n = nsamples.inla)) %>% return()}, error = function(e){
      if (grepl("reached elapsed time limit|reached CPU time limit", e$message)){
        -999
      }else{
        # error not related to timeout
        -999
      }
    }
    )
  }
  
  # inla.funpar.try <- purrr::possibly(inla.funpar, otherwise = NA)

  t_0 <- Sys.time()
  # Set up parallel environment
  ncores <- ncores 
  k <- 1:200
  cl_inla <- makeCluster(ncores, methods=FALSE)

  # extract packages on parallel environment 
  clusterEvalQ(cl_inla, {
    library(INLA)
    library(dplyr)
    library(stringr)
  })
  
  # extract R objects on parallel environment
  clusterExport(cl_inla, c("data_subset", "mod", "hyper.iid", "thet", "nsamples.inla",
                           "inla.funpar", "control.family", "k", "hyper.bym"), envir = environment())
  
  # run the the function in parallel
  outpar <- parLapply(cl = cl_inla, k, inla.funpar)
  
  # close parallel environment
  stopCluster(cl_inla)
  t_1 <- Sys.time()
  t_1 - t_0 # 20min
  
  pois.samples <- do.call(c, outpar)
  
  retlist <- list(
    pois.samples = pois.samples,
    n_covid = sum(data_subset$covid_deaths), 
    n_covid_heatwaves = data_subset %>% filter(hotperiods_l1_lag0_3 == 1) %>% pull(covid_deaths) %>% sum()
  )
    
  return(retlist)
}

