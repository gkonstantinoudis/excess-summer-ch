#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: Model for population and cross validation
#:::::::::::::::::::::::::::::

# Switzerland
# download the file from https://www.pxweb.bfs.admin.ch/pxweb/en/
# and select:
# Topic: Population 
# Variable: age class, canton, sex and year
# Year: 2010:2021
# Then click on: Permanent and non-permanent resident population by canton, citizenship (selection), country of birth, sex and age class, 2010-2021
# Select all the years, all the cantons, male and females and the age classes. 
# The name of the file should be : Permanent and non permanent resident population by canton, residence permit, place of birth, sex and age
# download the shapefile from https://www.swisstopo.admin.ch/en/geodata/landscape/boundaries3d.html#dokumente
# select swissboundaries3d_2022-05_2056_5728.shp.zip store it in the data folder and unzip it.


country <- "CH"


# Store it as data/popCH2010_2021.csv
if(!file.exists(file.path(path_output, paste0("pop2011_2022", country, ".rds")))){
  


  ##
  ## Clean data
  ##
  
  # The generic format after cleaning the data
  
  # NUTS3      year age.groups   sex population
  # <chr>     <dbl> <fct>      <dbl>      <int>
  # E06000001  2001 0-39           1      23644
  # E06000002  2001 0-39           1      39751
  # E06000003  2001 0-39           1      34468
  # E06000004  2001 0-39           1      49575
  # E06000005  2001 0-39           1      24584
  # E06000006  2001 0-39           1      31817
  # E06000007  2001 0-39           1      51576
  # E06000008  2001 0-39           1      41395
  # E06000009  2001 0-39           1      34374
  # E06000010  2001 0-39           1      72563

  pop <- read.csv("data/popCH2010_2021.csv", sep = ";", header = TRUE, fileEncoding = "ISO-8859-3")
  
  colnames(pop)[2] <- "canton"
  pop$Population.type <- NULL
  
  pop %>% dplyr::rowwise() %>% dplyr::mutate(X0_39 = sum(c_across(X0.years:X39.years), na.rm = T), 
                                             X40_59 = sum(c_across(X40.years:X59.years), na.rm = T), 
                                             X60_69 = sum(c_across(X60.years:X69.years), na.rm = T), 
                                             X70_79 = sum(c_across(X70.years:X79.years), na.rm = T), 
                                             X80plus = sum(c_across(X80.years:X100.years.or.older), na.rm = T)) %>% 
    dplyr::select(Year, canton, Sex, X0_39, X40_59, X60_69, X70_79, X80plus) -> pop
  
  
  pop$canton <- gsub("- ", "", pop$canton) 
  pop$Sex[pop$Sex %in% "Male"] <- 1
  pop$Sex[pop$Sex %in% "Female"] <- 2
  pop_long <- gather(pop, age, population, X0_39:X80plus, factor_key=TRUE)
  
  pop_long %>% dplyr::rename(NUTS3 = canton, year = Year, age.groups = age, sex = Sex, population = population) %>% 
    dplyr::select(NUTS3, year, age.groups, sex, population) -> pop_long
  pop_long$age.groups <- gsub("X", "", pop_long$age.groups)
  pop_long$age.groups <- gsub("_", "-", pop_long$age.groups)
  pop_long$age.groups[pop_long$age.groups %in% "80plus"] <- "80+"
  
  ##
  ## The population file in CH represents the 31st December of each year. For simplicity I will assume it represents the 1st of January
  pop_long$year <- pop_long$year + 1
  
  expand.grid(year = 2023, 
              NUTS3 = unique(pop_long$NUTS3),
              sex = c(1, 2), 
              age.groups = c("0-39", "40-59", "60-69", "70-79", "80+")) -> pred
  
  pred <- as.data.frame(pred)
  pred$population <- NA
  
  pred <- pred[,c("NUTS3", "year", "age.groups", "sex", "population")]
  pop_long <- pop_long[,c("NUTS3", "year", "age.groups", "sex", "population")]
  pop_long <- rbind(pop_long, pred)
  
  
  
  ##
  # We can also get language region and some sort of urbanicity. To download these factors go here:
  # https://www.agvchapp.bfs.admin.ch/de/typologies/query
  # select: Sprachgebiete 2016 and Urbanisierungsgrad 2011 (DEGURBA eurostat)
  # and store it in the data folder 
  
  
  ##
  
  Raumgliederungen <- read_excel("data/Raumgliederungen.xlsx")
  Raumgliederungen <- Raumgliederungen[-c(1:2),]
  Raumgliederungen %>% dplyr::select(`...4`, `Sprachgebiete 2016`, `Urbanisierungsgrad 2011 (DEGURBA eurostat)`) %>% 
    dplyr::rename(canton_id = `...4`, lang_reg = `Sprachgebiete 2016`, urbanicity = `Urbanisierungsgrad 2011 (DEGURBA eurostat)`) ->
    Raumgliederungen
  
  
  table(Raumgliederungen$canton_id, Raumgliederungen$lang_reg) -> lg
  table(Raumgliederungen$canton_id, Raumgliederungen$urbanicity) -> urbn
  
  # for the language region i will select the max municipalities
  apply(lg, 1, which.max) %>% as.data.frame() -> lg
  lg$canton_id <- rownames(lg)
  rownames(lg) <- NULL
  colnames(lg)[1] <- "lang_reg"
  
  # for the urbanicity i will use proportions
  urbn %>% as.data.frame() %>% 
    dplyr::rename(canton_id = Var1, 
                  urban = Var2) %>% 
    dplyr::group_by(canton_id) %>% 
    dplyr::mutate(prop = Freq / sum(Freq)) %>% 
    dplyr::select(canton_id, urban, prop) %>% 
    tidyr::spread(., urban, prop) %>% 
    dplyr::rename(Urban = `1`, semi = `2`, rural = `3`) -> urbn
  
  
  # and add these on the dataframe
  # before this i need to establish the link between the datasets
  
  ##
  ## HERE BE CAREFUL WITH THE UMLAUT AND STRESS ON A AND E IN GENEVA AND NEUCHATEL
  
  linkCH <- data.frame(
    NAME = c("Graubünden", "Bern", "Valais", "Vaud", "Ticino", "St. Gallen", "Zürich", "Fribourg", "Luzern", 
             "Aargau", "Uri", "Thurgau", "Schwyz", "Jura", "Neuchâtel", "Solothurn", "Glarus", "Basel-Landschaft", 
             "Obwalden", "Nidwalden", "Genève", "Schaffhausen", "Appenzell Ausserrhoden", "Zug", "Appenzell Innerrhoden", 
             "Basel-Stadt"),
    NUTS318CD = c("Graubünden / Grigioni / Grischun", "Bern / Berne", "Valais / Wallis", "Vaud", "Ticino", "St. Gallen", 
                  "Zürich", "Fribourg / Freiburg", "Luzern", 
                  "Aargau", "Uri", "Thurgau", "Schwyz", "Jura", "Neuchâtel", "Solothurn", "Glarus", "Basel-Landschaft", 
                  "Obwalden", "Nidwalden", "Genève", "Schaffhausen", "Appenzell Ausserrhoden", "Zug", "Appenzell Innerrhoden", 
                  "Basel-Stadt"),
    NUTS3_code = c("CH056", "CH021", "CH012", "CH011", "CH070", 
                   "CH055", "CH040", "CH022", "CH061", "CH033", 
                   "CH062", "CH057", "CH063", "CH025", "CH024", 
                   "CH023", "CH051", "CH032", "CH064", "CH065", 
                   "CH013", "CH052", "CH053", "CH066", "CH054", 
                   "CH031"), 
    CN = c("GR", "BE", "VS", "VD", "TI", "SG", "ZH", "FR", "LU", "AG", "UR", "TG", "SZ", "JU", "NE", "SO", "GL", "BL", "OW", 
           "NW", "GE", "SH", "AR", "ZG", "AI", "BS"), 
    
    Name_german = c("Graub?nden", "Bern", "Wallis", "Waadt", "Tessin", "Sankt Gallen", "Zurich", "Freiburg", "Luzern", 
                    "Aargau", "Uri", "Thurgau", "Schwyz", "Jura", "Neuenburg", "Solothurn", "Glarus", "Basel-Landschaft", 
                    "Obwalden", "Nidwalden", "Genf", "Schaffhausen", "Appenzell A.Rh.", "Zug", "Appenzell I.Rh.", 
                    "Basel-Stadt")
  )
  saveRDS(linkCH, file = file.path(path_output, paste0("linkCH.rds")))
  
  left_join(urbn, lg) %>% 
    left_join(., linkCH, by = c("canton_id" = "CN")) %>% 
    left_join(pop_long, ., by = c("NUTS3" = "NUTS318CD")) -> pop_long
  
  saveRDS(pop_long, file = file.path(path_output, paste0("pop2011_2022", country, ".rds")))
  
  }else{
    
    ##
    ## Fit the model
    ##
    
    if(!file.exists(file.path(path_output, "res_cv_population.rds"))){
      
      pop <- readRDS(file.path(path_output, paste0("pop2011_2022", country, ".rds")))
      
      
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
      
      # and leave the past 1 year cross validation
      years <- 2022
      data <- data[complete.cases(data),]
      torun <- expand.grid(sex=unique(pop$sex), age.groups=unique(pop$age.groups))
      
      l2store <- list()
      
      t_0 <- Sys.time()
      for(i in 1:nrow(torun)){
        
        print(i)
        
        datCV <- data %>% filter(sex %in% torun$sex[i], age.groups %in% torun$age.groups[i]) 
        datCV$truth <- datCV$population
        datCV$population[datCV$year %in% years] <- NA
        
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
        ens.samples <- cbind(X[[1]][,1:20], ens.samples)
        ens.samples$mod <- paste0("mod", 5)
        return(ens.samples)
      }) -> bmasamples
      
      
      for(i in 1:10){
        l2store[[i]][[5]] <- bmasamples[[i]]
      }
      saveRDS(l2store, file = file.path(path_output, "res_cv_population.rds"))
    }
  }




rm(list = ls())
gc()


#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################



