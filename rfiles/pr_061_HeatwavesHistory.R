InternalVal <- function(ageg, sexg, thr, covid_cov = TRUE, int = "rw", temp = TRUE, definition){ 
  
  # int can be rw or linear
  # thet <- c(4.411354, -1.467619, -2.239993, 6.356343, 7.705752)
  thet <- NULL
  
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
  
  # definition of exposure
  threshold <- thr
  
  if(definition == "exposure2heatwave"){
    data %>%
      dplyr::arrange(canton, time) %>%
      dplyr::group_by(canton, sex, age, year) %>%
      dplyr::mutate(
        hotperiods_l1 = dplyr::case_when(temperature_weighted >= threshold &
                                           dplyr::lag(temperature_weighted, 1) >= threshold & 
                                           dplyr::lag(temperature_weighted, 2) >= threshold ~ 1, 
                                         TRUE ~ 0), # this gives 1 to the third consecutive temperature larger than the threshold
        hotperiods_l1_lag0_3 = dplyr::case_when(hotperiods_l1 == 1 |
                                                  dplyr::lag(hotperiods_l1, 1) == 1 |
                                                  dplyr::lag(hotperiods_l1, 2) == 1 |
                                                  dplyr::lag(hotperiods_l1, 3) == 1 ~ 1,  
                                                TRUE ~ 0) # and this is heatperiods with 3 days lag to account for delayed effects
      )  -> data
  }
  
  if(definition == "heatwaveperiod"){
    data %>%
      dplyr::arrange(canton, time) %>%
      dplyr::group_by(canton, sex, age, year) %>%
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
                                                TRUE ~ 0) # and this is heatperiods with 3 days lag to account for delayed effects
      )  -> data
  }
  
  
  # ids for the random effects
  data$id_time <- yday(data$time) %>% as.factor() %>% as.numeric()
  data$id_space <- data$canton %>% as.factor() %>% as.numeric()
  data$id_year <- data$year %>% as.factor() %>% as.numeric()
  data$hotperiods_l1_lag0_3 <- as.factor(data$hotperiods_l1_lag0_3)
  data$dow <- lubridate::wday(data$time)
  data$id.tmp <- inla.group(data$temperature_weighted, n = 100, method = "cut", idx.only = TRUE) 
  
  # priors
  hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  control.family=inla.set.control.family.default()
  
  # subsetting
  data_subset <- data %>% filter(age %in% ageg, sex %in% sexg)
  agegnam <- gsub("\\+", "", as.character(ageg))
  agegnam <- gsub("-", "_", as.character(ageg))
  data_subset$x.year <- data_subset$year - min(data_subset$year) + 1
  
  base <- c("1", 
            "offset(log(pop))",  
            "factor(hol)",
            "factor(dow)",
            "factor(hotperiods_l1_lag0_3)",
            "f(id_space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
            "f(id_time, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)")
  
  
   if(temp == TRUE){
     base <- c(base, "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)")
   }else{
     base <- base
   }
  # main model
  if(covid_cov == FALSE){
    data_subset$deaths_outcome <- abs(data_subset$deaths - data_subset$covid_deaths) # Im using abs here because the deaths are provisional
    base <- base
    namcov <- ""
  }else{
    data_subset$deaths_outcome <- data_subset$deaths
    if(sum(data_subset$covid_deaths) < 10){ # for the age groups for which we have low covid deaths, 
      # we wont use the covid covariate
      base <- base
    }else{
      base <- c(base, "factor(covid_deaths_factor)")
    }
    namcov <- "_covidCov"
  }
  
  # the model
  
  if(int == "linear"){
    base = c(base, "id_year*factor(hotperiods_l1_lag0_3)")
  }
  
  if(int == "rw"){
    base = c(base, "f(id_year, factor(hotperiods_l1_lag0_3), model='rw2', hyper=hyper.iid, constr = TRUE)")
  }

  mod <- paste("deaths_outcome", paste(base, collapse=" + "), sep=" ~ ")
  
  inla.funpar <- function(X){
    
    inla.setOption(inla.timeout=600)
    
    tryCatch({
    
    data_subset %>% mutate(pop = get(str_c('popV', X))) -> data_sub
    
    in.mod = inla(as.formula(mod),
                  data=data_sub,
                  family="Poisson",  
                  verbose = TRUE, 
                  control.family=control.family,
                  control.compute=list(config = TRUE), 
                  control.mode=list(theta = thet, restart=TRUE),
                  num.threads = 5, 
                  control.predictor = list(link = 1))
    in.mod = inla.rerun(in.mod)
    
    if(int == "rw"){
      inla.rmarginal(in.mod$marginals.fixed$`factor(hotperiods_l1_lag0_3)1`, n = 1000) -> marg.beta
      lapply(in.mod$marginals.random$id_year, inla.rmarginal, n = 1000) -> marg.re
      lapply(marg.re, function(X) X+marg.beta) -> marg.sum
    }
    
    if(int == "linear"){
      inla.rmarginal(in.mod$marginals.fixed$`factor(hotperiods_l1_lag0_3)1`, n = 1000) + 
        inla.rmarginal(in.mod$marginals.fixed$`factor(hotperiods_l1_lag0_3)1:id_year`*12, n = 1000) -> marg.sum
    }
    
    marg.sum %>% return()
    }, error = function(e){
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
  ncores <- 10 
  k <- 1:200
  cl_inla <- makeCluster(ncores, methods=FALSE)
  
  # extract packages on parallel environment 
  clusterEvalQ(cl_inla, {
    library(INLA)
    library(dplyr)
    library(stringr)
  })
  
  # extract R objects on parallel environment
  clusterExport(cl_inla, c("data_subset", "mod", "hyper.iid", "thet", 
                           "inla.funpar", "control.family", "k", "hyper.bym"), envir = environment())
  
  # run the the function in parallel
  outpar <- parLapply(cl = cl_inla, k, inla.funpar)
  
  # close parallel environment
  stopCluster(cl_inla)
  t_1 <- Sys.time()
  t_1 - t_0 # 20min

  return(outpar)
}


