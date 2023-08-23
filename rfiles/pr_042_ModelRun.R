#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: INLA prediction model
#:::::::::::::::::::::::::::::


FitFinalModel <- function(ageg, sexg, w.weights, dynamo = FALSE, covid_cov = FALSE, popsamples = TRUE, temperature =  TRUE){
  
  # read the data
  shp = sf::read_sf("data/shp.shp")
  
  shp$nameid <- factor(shp$NAME)
  shp %>% arrange(nameid) -> shp
  
  W.nb <- spdep::poly2nb(shp)
  spdep::nb2INLA("data/W.adj", W.nb) 
  
  data <- readRDS(file.path(path_output, "datmod.rds"))

  # give initial values for theta
  base_theta <- c(6.362587, -1.192528, -2.616886, 9.956576, -1.228586)

  ##
  ##
  
  # INLA SET UP
  # priors
  hyper.bym <- list(theta1 = list('PCprior', c(1, 0.01)), theta2 = list('PCprior', c(0.5, 0.1)))
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  hyper.iid2 <- list(theta = list(prior="pc.prec", param=c(0.1, 0.01)))
  hyper.ar <- list(prec = list(prior="pc.prec", param=c(0.01, 0.01)), 
                   pacf1 = list(prior="pc.prec", param=c(0.9, 0.9)), 
                   pacf2 = list(prior="pc.prec", param=c(0.9, 0.9)))
  # Under Poisson uses default set up
  control.family=inla.set.control.family.default()
  
  if(dynamo == TRUE){
    pop.list <- readRDS(file.path(path_output, "datdynpop.rds"))
    setnames(pop.list, old = paste0("popdyn", 1:200), new = paste0("popV", 1:200))
    pop.list$pop <- NULL
    
    data <- left_join(data, pop.list, 
                      by = c("age" = "age", 
                             "sex" = "sex", 
                             "time" = "daily_date", 
                             "CN" = "canton_id"))
    dyn <- "_dynamo"
  }else{
    pop.list <- readRDS(file.path(path_output, "popfin_post_df.rds"))
    
    data <- left_join(data, pop.list, 
                      by = c("age" = "age", 
                             "sex" = "sex", 
                             "time" = "daily_date", 
                             "NUTS318CD" = "NUTS318CD"))
    dyn <- ""
  }

  
  data_subset <- data %>% filter(age %in% ageg, sex %in% sexg)
  
  if(covid_cov == FALSE){
    data_subset$deaths_outcome <- abs(data_subset$deaths - data_subset$covid_deaths) # Im using abs here because the deaths are provisional
    base <- c("1", 
              "offset(log(pop))",  
              "factor(hol)", 
              "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)", 
              "f(id.space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
              "f(id.week, model='ar1', constr = TRUE, cyclic = TRUE)")
    namcov <- ""
  }else{
    data_subset$deaths_outcome <- data_subset$deaths
    if(sum(data_subset$covid_deaths) < 10){ # for the age groups for which we have low covid deaths, 
      # we wont use the covid covariate
      base <- c("1", 
                "offset(log(pop))",  
                "factor(hol)", 
                "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)", 
                "f(id.space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
                "f(id.week, model='ar1', constr = TRUE, cyclic = TRUE)")
    }else{
      base <- c("1", 
                "offset(log(pop))",  
                "factor(hol)", 
                "factor(covid_deaths_factor)",
                "f(id.tmp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE)", 
                "f(id.space, model='bym2', graph='data/W.adj', scale.model = TRUE, constr = TRUE, hyper = hyper.bym)", 
                "f(id.week, model='ar1', constr = TRUE, cyclic = TRUE)")
    }
    
    namcov <- "_covidCov"
  }
  
  if(temperature == TRUE){
    base <- base
    temperature_nam <- ""
  }else{
    base <- base[!grepl("id.tmp", base, fixed = TRUE)]
    base_theta <- c(-1.192528, -2.616886, 9.956576, -1.228586)
    temperature_nam <- "_notemperature"
  }
  
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
  
  # and the models
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
  
  
  # set the year to predict
  year.out <- 2022
  data_subset$obs_deaths <- data_subset$deaths
  data_subset$deaths[data_subset$year %in% year.out] <- NA
  # ind <- which(is.na(data_subset$deaths))

  agegnam <- gsub("\\+", "", as.character(ageg))
  agegnam <- gsub("-", "_", as.character(ageg))
  
    
  inla.funpar <- function(X){
    
    pops = TRUE
    n_sam = 50
    
    inla.setOption(inla.timeout=600) # what im doing here is if inla get stuck return an error on the pop sample with timeout being 600 secs
    
    tryCatch({
      
      if(pops == FALSE){
        data_subset %>% dplyr::mutate(
          pop = data_subset %>% dplyr::select(starts_with("popV")) %>% apply(., 1, mean)
        ) -> data_sub
        X <- "00"
      }
      
      if(pops){
        data_subset %>% mutate(pop = get(str_c('popV', X))) -> data_sub
      }
    
    
    lapply(1:n_models, function(K){
      
      in.mod = inla(as.formula(listmod[[K]]),
                    data=data_sub,
                    family="Poisson",  
                    verbose = FALSE, 
                    control.family=control.family,
                    control.compute=list(config = TRUE), 
                    control.mode=list(restart=TRUE, theta = theta_list[[K]]),
                    num.threads = 1, 
                    control.predictor = list(link = 1)) %>% return()
      
      # inla.rerun(in.mod) %>% return()
    }) -> res_list # ~ 15min
    
    n_sam <- n_sam
    
    post.samples <- lapply(res_list, function(Y) inla.posterior.sample(n = n_sam, result = Y)) # ~2 minutes
    lp <- lapply(post.samples, function(Y) lapply(Y, function(J){
      Z <- J$latent
      trans.Z <- exp(Z)
      trans.Z[rownames(Z) %>% startsWith(.,"Predictor")] %>% return()
      }
      )
      )
    
    set.seed(11)
    predictions = lapply(1:n_models,
                         function(L){
                           lapply(lp[[L]], function(P) rpois(n = length(P), lambda = P)) %>% return()
                         }
    )
    
    ens <- sample(1:n_models, replace = TRUE, prob = w.weights, size = n_sam)
    sapply(ens, function(M) predictions[[M]][[sample(1:n_sam, 1)]]) -> ens.samples
  
    pois.samples <- ens.samples %>% as.data.frame()
    setnames(pois.samples, new = paste0("V", 1:n_sam, "_", "popV", X))
    pois.samples$pop = data_sub$pop
    setnames(pois.samples, old = "pop", new = str_c('popV', X))
    
    pois.samples %>% return()}, error = function(e){
      if (grepl("reached elapsed time limit|reached CPU time limit", e$message)){
        -999
      }else{
        # error not related to timeout
        -999
      }
    }
    )
  }
  
  if(popsamples == TRUE){
    t_0 <- Sys.time()
    message("launching the parallel environment")
    # Set up parallel environment
    ncores <- 15 
    k <- 1:200
    cl_inla <- makeCluster(ncores, methods=FALSE)
    
    # extract packages on parallel environment 
    clusterEvalQ(cl_inla, {
      library(INLA)
      library(dplyr)
      library(spdep)
      library(stringr)
      library(data.table)
      library(splines)
    })
    
    # extract R objects on parallel environment
    clusterExport(cl_inla, c("data_subset", "n_models", "hyper.bym", "hyper.ar", "hyper.iid", "theta_list", 
                             "listmod", "inla.funpar", "W.nb", "control.family", "k", "w.weights"), envir = environment())
    
    # run the the function in parallel
    message("running the outpar")
    outpar <- parLapply(cl = cl_inla, k, inla.funpar)
    
    # close parallel environment
    stopCluster(cl_inla)
    t_1 <- Sys.time()
    t_1 - t_0 # 2h
    
    message("outpar completed")
    pois.samples <- do.call(cbind, outpar)
    
    psam_nam <- ""
  }
  
  if(popsamples == FALSE){
    t_0 <- Sys.time()
    pois.samples <- inla.funpar(X = 1, pops = FALSE, n_sam = 1000)
    psam_nam <- "_nosamples"
    t_1 <- Sys.time()
    t_1 - t_0 # ~10 minutes
  }

  data_subset %>% select(age, time, canton, sex, obs_deaths, 
                         covid_deaths, NUTS3, CN, NAME, 
                         temperature_weighted
                         ) %>% 
    cbind(., pois.samples) -> pois.samples
  
  message("store results")
  saveRDS(pois.samples, file = file.path(path_output, 
                                         paste0("PoissonSamplesMerged_", agegnam, sexg, dyn, namcov, psam_nam, temperature_nam,".rds")))
  rm(pois.samples); gc()
  
}


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################

