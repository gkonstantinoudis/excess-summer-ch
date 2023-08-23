#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: Cross validation for the mortality data
#:::::::::::::::::::::::::::::

# leave the past 1 year out for validating the model
# INLA:::inla.binary.install()

crosvaldeaths <- function(ageg, sexg, path2store, alpha, dynamo = TRUE, covid_cov = TRUE, temperature = TRUE){
  
  # read the data
  shp = sf::read_sf("data/shp.shp")
  # shp = shp[!duplicated(shp$KANTONSNUM),]
  
  shp$nameid <- factor(shp$NAME)
  shp %>% arrange(nameid) -> shp
  
  W.nb <- spdep::poly2nb(shp)
  spdep::nb2INLA("data/W.adj", W.nb) 
  
  # a bit more wrangling of the finaldb to get it on the format needed for the model
  
  if(!file.exists(file.path(path_output, "datmod.rds"))){
    
    country <- "CH"
    finaldb=readRDS(file.path(path_output, paste0("findata", country, ".rds")))
    # Select data for the period 2011-2019 
    data = finaldb 
    data %>% mutate(time = as.Date(time), 
                    year = year(time), 
                    month=month(time)) -> data
    
    # Create indexes
    data$id.space <- as.numeric(as.factor(data$canton))
    data %>% mutate(year = year(time) %>% as.numeric()) -> data
    data$date.id <- data$date.id2 <- data$time %>% as.factor() %>% as.numeric()
    data %>% mutate(month = month(time) %>% as.factor() %>% as.numeric()) -> data
    
    data$id.tmp <- inla.group(data$temperature_weighted, n = 100, method = "cut", idx.only = TRUE)
    data$id.week <- data$id.week2 <- week(data$time) %>% as.factor() %>% as.numeric()
    data$day <- yday(data$time) %>% as.factor() %>% as.numeric()
    data %>% mutate(covid_deaths_factor = ifelse(covid_deaths == 0, 0, 1)) -> data
    data$date.id3 <- inla.group(data$date.id, n = 200)
    data$yearid <- data$year - 2010
    data$spacetime <- paste(data$date.id, data$canton) %>% as.factor() %>% as.numeric()
    
    # change point
    data$date.id <- scale(data$date.id)
    thr <- data$date.id[data$time == "2020-06-01"]
    data$date.id.lt <- (data$date.id - thr)*(data$date.id<thr)
    
    # natural splines
    sp <- ns(data$date.id, df = 5)
    sp <- sp %>% as.data.frame()
    colnames(sp) <- paste0("X", 1:5)
    data <- cbind(data, sp)
    
    # temporal interactions
    data$month <- month(data$time)
    data$month2 <- str_pad(data$month, 2, pad = "0")
    data$ymonth <- paste(data$year, data$month2, sep = "-") %>% as.factor() %>% as.numeric()
    saveRDS(data, file.path(path_output, "datmod.rds"))
  }else{
    data <- readRDS(file.path(path_output, "datmod.rds"))
  }

  ##
  ##
  
  # INLA SET UP
  # priors
  hyper.bym <- list(theta1 = list('PCprior', c(0.1, 0.01)), theta2 = list('PCprior', c(0.5, 0.5)))
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  hyper.iid2 <- list(theta = list(prior="pc.prec", param=c(0.1, 0.01)))
  hyper.ar <- list(prec = list(prior="pc.prec", param=c(0.01, 0.01)), 
                   pacf1 = list(prior="pc.prec", param=c(0.9, 0.9)), 
                   pacf2 = list(prior="pc.prec", param=c(0.9, 0.9)))
  
  # Under Poisson uses default set up
  control.family=inla.set.control.family.default()
  
  
  if(dynamo == FALSE){
    # read population samples
    if(!file.exists(file.path(path_output, "datpoplinear.rds"))){
      pop.list <- readRDS(file.path(path_output, paste0("popfin_post_df", ".rds")))
      pop.list$age[pop.list$age %in% "less40"] <- "0-39"
      pop.list$age[pop.list$age %in% "80plus"] <- "80+"
      
      saveRDS(pop.list, file.path(path_output, "datpoplinear.rds"))
    }else{
      pop.list <- readRDS(file.path(path_output, "datpoplinear.rds"))
      
      # Merge population with the data
      data <- left_join(data, pop.list, 
                        by = c("age" = "age", 
                               "sex" = "sex", 
                               "time" = "daily_date", 
                               "NUTS3_code" = "NUTS318CD"))
    }
  }else{
    pop.list <- readRDS(file.path(path_output, "datdynpop.rds"))
    
    # Merge population with the data
    data <- left_join(data, pop.list, 
                      by = c("age" = "age", 
                             "sex" = "sex", 
                             "time" = "daily_date", 
                             "CN" = "canton_id"))

    setnames(data, colnames(data)[startsWith(colnames(data), "popdyn")], paste0("popV", 1:200))
  }

  data <- 
    data %>% 
    mutate(year = year(time)) %>% 
    filter(year < 2022) %>% 
    filter(age %in% ageg, sex %in% sexg)
  data$date.id.sc <- scale(data$date.id)
  
  year.out <- 2021
  truth <- data$deaths[data$year %in% year.out]
  data$deaths[data$year %in% year.out] <- NA
  ind <- which(is.na(data$deaths))

  
  # Define formulas
  if(covid_cov == TRUE){
    data$deaths_outcome <- data$deaths
    if(sum(data$covid_deaths) < 10){ # for the age groups for which we have low covid deaths, 
      # we wont use the covid covariate
      base <- c("1", 
                "offset(log(popV1))",  
                "factor(hol)", 
                "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)", 
                "f(id.space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
                "f(id.week, model='ar1', constr = TRUE, cyclic = TRUE)")
    }else{
      base <- c("1", 
                "offset(log(popV1))",  
                "factor(hol)", 
                "factor(covid_deaths_factor)",
                "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)", 
                "f(id.space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
                "f(id.week, model='ar1', constr = TRUE, cyclic = TRUE)")
    }
    
  }else{
    data$deaths_outcome <- abs(data$deaths - data$covid_deaths) # Im using abs here because the deaths are provisional, 
    # thus if we have a covid death and not all cause death might mean that this death is not yet reported.
    base <- c("1", 
              "offset(log(popV1))",  
              "factor(hol)", 
              "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)", 
              "f(id.space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
              "f(id.week, model='ar1', constr = TRUE, cyclic = TRUE)")
  }

  base_theta <- c(6.362587, -1.192528, -2.616886, 9.956576, -1.228586)
  
  if(temperature == TRUE){
    base <- base
    temperature_nam <- ""
  }else{
    base <- base[!grepl("id.tmp", base, fixed = TRUE)]
    base_theta <- c(-1.192528, -2.616886, 9.956576, -1.228586)
    temperature_nam <- "_notemperature"
  }
  
  list(
    mod1 = c(base),
    mod2 = c(base, "date.id"),
    mod3 = c(base, "date.id", "date.id.lt"),
    mod4 = c(base, paste0("X", 1:5)),
    mod5 = c(base, "f(yearid, model='iid', hyper = hyper.iid, constr = TRUE)"),
    mod6 = c(base, "f(date.id3, model='ar', order = 2, hyper = hyper.ar, constr = TRUE)"),
    mod7 = c(base, 
             "date.id", 
             "f(date.id3, model='ar', order = 2, hyper = hyper.ar, constr = TRUE)"),
    mod8 = c(base, 
             "date.id",
             "f(yearid, model='iid', hyper = hyper.iid, constr = TRUE)")
    # ,
    # mod9 = c(base, 
    #          "f(date.id3, model='ar', order = 2, hyper = hyper.ar, constr = TRUE)", 
    #          "f(yearid, model='iid', hyper = hyper.iid2, constr = TRUE)"),
    # mod10 = c(base, 
    #           "date.id", 
    #           "f(date.id3, model='ar', order = 2, hyper = hyper.ar, constr = TRUE)", 
    #           "f(yearid, model='iid', hyper = hyper.iid2, constr = TRUE)")
  ) -> listmod
  
  lapply(listmod, function(X) paste("deaths_outcome", paste(X, collapse=" + "), sep=" ~ ")) -> listmod
  n_models <- length(listmod)
  
  # give initial values for theta
  
  theta_list <- 
    list(
      mod1 = base_theta,
      mod2 = base_theta,
      mod3 = base_theta,
      mod4 = base_theta,
      mod5 = c(base_theta, 5.690160),
      mod6 = c(base_theta, 12.2984438974, -4.0918340206, -0.0001528106), 
      mod7 = c(base_theta, 12.2984438974, -4.0918340206, -0.0001528106),
      mod8 = c(base_theta, 8.035468)
      # ,
      # mod9 = c(base_theta, 12.67808585, -3.96668253, -0.02891939, 5.68744258),
      # mod10 = c(base_theta, 12.477304161, -4.182575784, -0.001022373, 8.238356465)
    )
  
  theta_list <- c(theta_list, theta_list)
  
  
  t_0 <- Sys.time()
  
  lapply(1:n_models, function(X){
    in.mod = inla(as.formula(listmod[[X]]),
                  data=data,
                  family="Poisson",  
                  verbose = TRUE, 
                  control.family=control.family,
                  control.compute=list(config = TRUE), 
                  control.mode=list(restart=TRUE, theta = theta_list[[X]]),
                  num.threads = 4, 
                  control.predictor = list(link = 1)) 
  
    # in.mod <- inla.rerun(in.mod)
    return(in.mod)
  }) -> res_list # ~ 15min
  t_1 <- Sys.time()
  t_1 - t_0
  
  n_sam <- 200
  
  post.samples <- lapply(res_list, function(Y) inla.posterior.sample(n = n_sam, result = Y)) # ~2 minutes

  lp <- lapply(post.samples, function(Y) lapply(Y, function(X) exp(X$latent[ind])))
  N <- length(ind)
  
  
  set.seed(11)
  lapply(1:n_models, function(Y){
    return(
      list(
        inla_res = res_list[[Y]], 
        post = list(
          linear_pred = lp[[Y]],
          predictions = lapply(lp[[Y]], function(X) rpois(n = N, lambda = X)),
          true_values =  truth, 
          datainfo = data[ind,] %>% select(age, time, canton, sex, NUTS3, NAME, covid_deaths)
        )
      )
    )
  }) -> list.res
  
  
  # Extract the results
  # Assess bias, mse and coverage
  
  metrics <- function(K){
    Y <- list.res[[K]]$post
    
    do.call(c, Y$predictions) -> pred_combined
    pred_combined <- data.frame(truth = rep(Y$true_values, times = n_sam), 
                                predictions = pred_combined)
    
    pred.samples <- do.call(cbind, Y$prediction)
    true_values <- Y$true_values
    tmp <- data[ind,]
    tmp %>% select(!starts_with("V")) %>% # just to make sure there is nothing starting with V not to mess up the pred values
      cbind(., as.data.frame(pred.samples)) %>% 
      as.data.frame() %>% 
      mutate(true_values = true_values) -> tmp
    
    lapply(list(year = "year", space = "NUTS3", time = "time", spacetime = c("time", "NUTS3")), function(Y){
      
      tmp %>% ungroup() %>% select(starts_with("V"), Y, true_values) %>% group_by_at(Y) %>% dplyr::summarise_all(sum) -> 
        ag_deaths
      
      pois.quant <- as.data.frame(t(apply(ag_deaths[,!(colnames(ag_deaths) %in% c(Y, "true_values"))], 1, function(Z) quantile(Z, probs = c(0.025, 0.975)))))
      list(
        bias = sweep(ag_deaths %>% ungroup() %>% dplyr::select(!Y), 1, ag_deaths$true_values, "-") %>% select(!true_values) %>% as.data.frame(), 
        mse = sweep(ag_deaths %>% ungroup() %>% dplyr::select(!Y), 1, ag_deaths$true_values, "-")^2 %>% select(!true_values) %>% as.data.frame(), 
        coverage = mean((pois.quant$`2.5%` <= ag_deaths$true_values) & (pois.quant$`97.5%` > ag_deaths$true_values)), 
        pois.quant = pois.quant, 
        true_values = ag_deaths$true_values
      ) %>% return()
    }) -> ret_list
    names(ret_list) <- c("year", "space", "time", "spacetime")
    ret_list %>% return()
  }
  
  res <- lapply(1:(n_models), metrics)
  
  # bias specific weights:
  data.frame(mod = rep(1:n_models, each = n_sam), 
             res = lapply(res, function(x) x$year$bias %>% as.numeric() %>% return()) %>% unlist()) %>% 
    group_by(mod) %>% 
    dplyr::summarize(mean.bias = mean(res)) -> ww.bias

  
  weights <- (1/abs(ww.bias$mean.bias))/sum(1/abs(ww.bias$mean.bias))
  
  ##
  ## BMA based on different alphas
  
  for(i in 1:length(alpha)){
    if(alpha[i] == 1){
      weights.alpha <- rep(1/n_models, times = n_models)
    }else{
      weights.alpha <- weights
      weights.alpha[weights.alpha<alpha[i]] <- 0
      # rescale
      weights.alpha <- weights.alpha/sum(weights.alpha)
    }

    
    set.seed(11)
    ens <- sample(1:n_models, replace = TRUE, prob = weights.alpha, size = 200)
    sapply(ens, function(X) list.res[[X]]$post$predictions[sample(1:200, 1)]) -> ens.samples
    U <- n_models + i
    list.res[[U]] <- list()
    list.res[[U]]$post$predictions <- ens.samples
    list.res[[U]]$post$true_values <- list.res[[n_models]]$post$true_values 
    list.res[[U]]$post$datainfo <- list.res[[n_models]]$post$datainfo
    
    res[[U]] <- metrics(U)
  }

  # bias
  data.frame(mod = rep(1:(n_models+length(alpha)), each = n_sam), 
             res = lapply(res, function(x) x$year$bias %>% as.numeric() %>% return()) %>% unlist()) -> p1_dat 
 
  # and a table with the summary statistics of the total. 
  
  # coverage
  
  covres <- list(
    sapply(res, function(X) X$year$coverage),
    sapply(res, function(X) X$time$coverage),
    sapply(res, function(X) X$space$coverage)
  )
  
  cbind(
    # models
    paste0("Model ", 1:(n_models+length(alpha))),
    
    # mse
    lapply(res, function(X) X$spacetime$mse %>% apply(., 2, mean) %>% quantile(., probs = c(0.5, 0.025, 0.975))) %>% 
      do.call(rbind, .) %>% round(digits = 2) %>% format(nsmall = 2) %>% 
      apply(., 1, function(x) paste0(x[1], " (", x[2], ", ", x[3], ")")),
    
    # bias
    lapply(res, function(X) X$spacetime$bias %>% apply(., 2, mean) %>% quantile(., probs = c(0.5, 0.025, 0.975))) %>% 
      do.call(rbind, .) %>% round(digits = 2) %>% format(nsmall = 2) %>% 
      apply(., 1, function(x) paste0(x[1], " (", x[2], ", ", x[3], ")")),
    
    # coverage
    sapply(res, function(X) X$spacetime$coverage) %>% round(digits = 2) %>% format(nsmall = 2)
  ) %>% as.data.frame() %>% dplyr::rename(Model = V1, MSE = V2, Bias = V3, Coverage = V4) -> tab
  
  
  listret <- 
    list(
      p1_dat = p1_dat, 
      covres = covres, 
      tab = tab,
      weights = weights
    )

  gc()
  
  saveRDS(listret, file.path(path2store, paste0("CVData", ageg, sexg, temperature_nam)))

}

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################



