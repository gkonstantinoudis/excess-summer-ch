#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: Fit the best performing population model
#:::::::::::::::::::::::::::::

country <- "CH"

pop <- readRDS(file.path(path_output, paste0("pop2011_2022", country, ".rds")))

# Now fit the model

# Set INLA
# very vague prior
hyper.iid <- list(theta = list(prior="pc.prec", param=c(10, 0.1)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()

# very vague prior
hyper.iid <- list(theta = list(prior="pc.prec", param=c(10, 0.1)))

# Under Poisson uses default set up
control.family=inla.set.control.family.default()


base <- c("1", 
          "factor(lang_reg)", 
          "f(id.space, model='iid', hyper=hyper.iid, constr = TRUE)", 
          "f(id.spacetime, model='iid', hyper=hyper.iid, constr = TRUE)")

list(
  
  mod1 = c(base, 
           "year.c1"
  ),
  
  mod2 = c(base, 
           "f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE)"
  ),
  
  mod3 = c(base, 
           "year.c1",
           "f(id.time, model='rw1', hyper=hyper.iid, constr = TRUE)"
  ),
  
  mod4 = c(base, 
           "year.c1",
           "f(id.space1, year.c2, model='iid', hyper=hyper.iid, constr = TRUE)",
           "f(id.time, model='iid', hyper=hyper.iid, constr = TRUE)"
  )
) -> form_list


lapply(form_list, function(X) paste("population", paste(X, collapse=" + "), sep=" ~ ")) -> form_list



data <- pop


# Create indexes
data$id.space = data$id.space1 = as.numeric(as.factor(data$NUTS3))
data$year.c1 <- data$year.c2 <- data$id.time <- data$year - min(data$year) + 1
data$id.spacetime <- as.numeric(as.factor(paste0(data$year, data$NUTS3)))



t_0 <- Sys.time()
thet <- NULL
list_store <- list()



torun <- expand.grid(sex=unique(pop$sex), age.groups=unique(pop$age.groups))

l2store <- list()

t_0 <- Sys.time()
for(i in 1:nrow(torun)){
  
  print(i)
  
  datCV <- data %>% filter(sex %in% torun$sex[i], age.groups %in% torun$age.groups[i]) 
  
  lapply(1:length(form_list), function(K){
    
    in.mod <- 
      inla(form_list[[K]] %>% as.formula(),
           data=datCV,
           family="poisson",  
           verbose = FALSE, 
           control.family=control.family,
           control.compute=list(config = TRUE), 
           control.mode=list(restart=TRUE, theta = thet),
           num.threads = round(parallel::detectCores()*.8), 
           control.predictor = list(link = 1)
      )
    
    in.mod <- inla.rerun(in.mod)
    post.samples <- inla.posterior.sample(n = 200, result = in.mod)
    
    set.seed(11)
    lp <- lapply(post.samples, function(Y){
      Z <- Y$latent
      trans.Z <- exp(Z)
      trans.Z <- trans.Z[rownames(Z) %>% startsWith(.,"Predictor")] 
      rpois(n = length(trans.Z), lambda= trans.Z) %>% return()
    }
    ) -> lp
    
    do.call(cbind, lp) -> lp.df
    lp.df <- as.data.frame(lp.df)
    colnames(lp.df) <- paste0("V", 1:200)
    lp.df <- cbind(datCV, lp.df)
    lp.df$mod <- paste0("mod", K)
    lp.df %>% return()
  }) -> l2store[[i]]
  
}

t_1 <- Sys.time()
t_1 - t_0 # ~ 7 min



# and the bma samples
lapply(l2store, function(X){
  n_sam <- 200
  set.seed(11)
  ens <- sample(1:4, replace = TRUE, size = n_sam)
  sapply(ens, function(M) X[[M]][,paste0("V", sample(1:n_sam, 1))]) -> ens.samples
  ens.samples <- as.data.frame(ens.samples)
  colnames(ens.samples) <- paste0("V", 1:200)
  ens.samples <- cbind(X[[1]][,1:19], ens.samples)
  ens.samples$mod <- paste0("mod", 5)
  return(ens.samples)
}) -> bmasamples



bmasamples <- do.call(rbind, bmasamples)

saveRDS(bmasamples, file = file.path(path_output,paste0("pois.samples.population", country, ".rds")))


##
## Some checks
colnames(chk)
chk <- bmasamples
age <- "60-69"
sexg <- 2
can <- "Zürich"
chk <- chk %>% dplyr::filter(age.groups %in% age, sex %in% sexg, NUTS3 %in% can)

y <- chk %>% filter(age.groups %in% age, sex %in% sexg, NUTS3 %in% can) %>% dplyr::select(starts_with("V"))

dat2 <- data.frame(x=rep(2022, 200), y=unlist(as.vector(y)))

ggplot() + geom_point(data = chk, aes(x=year, y=population)) + geom_point(data=dat2, aes(x=x, y=y))




rm(list = ls())
gc()




#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################


