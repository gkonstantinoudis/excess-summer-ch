#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: Prepare data for figures
#:::::::::::::::::::::::::::::

DatFigures <- function(X){
  
  # Time
  X %>% group_by(time) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig11
  
  cbind(
    time = res_fig11 %>% pull(time), 
    res_fig11 %>% 
      ungroup() %>% 
      dplyr::select(starts_with("V")) %>% 
      apply(., 1, quantile, probs = c(0.025, seq(from = 0.10, to = 0.90, by = 0.10), 0.975)) %>% 
      t() %>% 
      as.data.frame(), 
    obs_deaths = res_fig11 %>% pull(obs_deaths)
  ) -> res_fig11
  
  # sub Figure 1
  X %>% group_by(sex, time) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig12
  
  pblapply(c("male", "female"), 
           function(X){
             sexg = X
             cbind(
               time = res_fig12 %>% filter(sex %in% sexg) %>% pull(time), 
               res_fig12 %>% 
                 filter(sex %in% sexg) %>% 
                 ungroup() %>% 
                 dplyr::select(starts_with("V")) %>% 
                 apply(., 1, quantile, probs = c(0.025, seq(from = 0.10, to = 0.90, by = 0.10), 0.975)) %>% 
                 t() %>% 
                 as.data.frame(), 
               obs_deaths = res_fig12 %>% filter(sex %in% sexg) %>% pull(obs_deaths)
             ) %>% 
               mutate(sex = sexg) %>% 
               return()
           }) -> res_fig12
  
  do.call(rbind, res_fig12) -> res_fig12
  res_fig12 %>% mutate(sex = recode(sex, male = "Males", female = "Females")) %>% 
    mutate(sex = factor(sex, levels = c("Males", "Females"))) -> res_fig12
  
  # sub Figure 1
  X %>% group_by(sex, NAME) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig13
  

  pblapply(c("male", "female"), 
           function(X){
             sexg = X
             dat <- res_fig13 %>% filter(sex %in% sexg)
             
             cbind(
               space = dat %>% pull(NAME), 
               sweep(
                 dat %>% 
                   ungroup() %>% 
                   dplyr::select(starts_with("V")) %>% 
                   sweep(., 
                         1, 
                         dat$obs_deaths) %>% 
                   ChangeSign(), 
                 1, 
                 dat$obs_deaths, 
                 FUN = "/"
               ) %>% 
                 apply(., 1, quantile, probs = c(0.5, 0.025, 0.975)) %>% 
                 t() %>% 
                 as.data.frame()
             ) %>% 
               mutate(sex = sexg) %>% 
               return()
           }) -> res_fig13
  
  
  do.call(rbind, res_fig13) -> res_fig13
  res_fig13 %>% mutate(sex = recode(sex, male = "Males", female = "Females")) %>% 
    mutate(sex = factor(sex, levels = c("Males", "Females"))) -> res_fig13
  
  # Figure 13 for the oldest age group
  # sub Figure 1
  X %>% filter(age %in% "80+") %>% 
    group_by(sex, NAME) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig131
  
  
  pblapply(c("male", "female"), 
           function(X){
             sexg = X
             dat <- res_fig131 %>% filter(sex %in% sexg)
             
             cbind(
               space = dat %>% pull(NAME), 
               sweep(
                 dat %>% 
                   ungroup() %>% 
                   dplyr::select(starts_with("V")) %>% 
                   sweep(., 
                         1, 
                         dat$obs_deaths) %>% 
                   ChangeSign(), 
                 1, 
                 dat$obs_deaths, 
                 FUN = "/"
               ) %>% 
                 apply(., 1, quantile, probs = c(0.5, 0.025, 0.975)) %>% 
                 t() %>% 
                 as.data.frame()
             ) %>% 
               mutate(sex = sexg) %>% 
               return()
           }) -> res_fig131
  
  
  do.call(rbind, res_fig131) -> res_fig131
  res_fig131 %>% mutate(sex = recode(sex, male = "Males", female = "Females")) %>% 
    mutate(sex = factor(sex, levels = c("Males", "Females"))) -> res_fig131
  
  
  
  # Space
  X %>% group_by(age, sex, time) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig2
  
  torun <- expand.grid(sex = c("male", "female"), age = c("0-39", "40-59","60-69", "70-79", "80+"))
  
  pbapply(torun, 
          1, 
          function(X){
            sexg = X[1]
            ageg = X[2]
            
            cbind(
              time = res_fig2 %>% filter(age %in% ageg, sex %in% sexg) %>% pull(time), 
              res_fig2 %>% 
                filter(age %in% ageg, sex %in% sexg) %>% 
                ungroup() %>% 
                dplyr::select(starts_with("V")) %>% 
                apply(., 1, quantile, probs = c(0.025, seq(from = 0.10, to = 0.90, by = 0.10), 0.975)) %>% 
                t() %>% 
                as.data.frame(), 
              obs_deaths = res_fig2 %>% filter(age %in% ageg, sex %in% sexg) %>% pull(obs_deaths)
            ) %>% 
              mutate(age = ageg, sex = sexg) %>% 
              return()
          }) -> res_fig2
  
  do.call(rbind, res_fig2) -> res_fig2
  res_fig2 %>% mutate(sex = recode(sex, male = "Males", female = "Females")) %>% 
    mutate(sex = factor(sex, levels = c("Males", "Females"))) -> res_fig2
  
  
  ####
  # Space
  shp = sf::read_sf("data/shp.shp")
  shp$NAME[shp$NAME %in% "Gen\xe8ve"] <- "Genève"
  shp$NAME[shp$NAME %in% "Neuch\xe2tel"] <- "Neuchâtel"
  shp$NAME[shp$NAME %in% "Z\xfcrich"] <- "Zürich"
  shp$NAME[shp$NAME %in% "Graub\xfcnden"] <- "Graubünden"
  
  X %>% group_by(NAME) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig14
  

  data.frame(
    space = res_fig14 %>% pull(NAME), 
    medianrr = (sweep(
      res_fig14 %>% 
        ungroup() %>% 
        dplyr::select(starts_with("V")) %>% 
        sweep(., 
              1, 
              res_fig14$obs_deaths) %>% 
        ChangeSign(), 
      1, 
      res_fig14$obs_deaths, 
      FUN = "/"
    ) %>% 
      apply(., 1, quantile, probs = 0.5))*100 , 
    exceedance = sweep(
      res_fig14 %>% 
        ungroup() %>% 
        dplyr::select(starts_with("V")) %>% 
        sweep(., 
              1, 
              res_fig14$obs_deaths) %>% 
        ChangeSign(), 
      1, 
      res_fig14$obs_deaths, 
      FUN = "/"
    ) %>% 
      apply(., 1, function(X) mean(X>=0))
  )  %>% 
    mutate(excat0_8 = case_when(exceedance > 0.80 ~ "(0.80, 1]",
                             exceedance > 0.20 & exceedance <= 0.80 ~ "(0.20, 0.80]",
                             TRUE ~ "[0, 0.2]")) %>% 
    mutate(excat0_8 = factor(excat0_8, levels = c("[0, 0.1]", "(0.10, 0.90]", "(0.90, 1]"))) %>% 
    mutate(excat0_9 = case_when(exceedance > 0.90 ~ "(0.90, 1]",
                                exceedance > 0.10 & exceedance <= 0.90 ~ "(0.10, 0.90]",
                                TRUE ~ "[0, 0.1]")) %>% 
    mutate(excat0_9 = factor(excat0_9, levels = c("[0, 0.1]", "(0.10, 0.90]", "(0.90, 1]"))) %>% 
    mutate(excat0_95 = case_when(exceedance > 0.95 ~ "(0.95, 1]",
                                exceedance > 0.05 & exceedance <= 0.95 ~ "(0.05, 0.95]",
                                TRUE ~ "[0, 0.05]")) %>% 
    mutate(excat0_95 = factor(excat0_95, levels = c("[0, 0.05]", "(0.05, 0.95]", "(0.95, 1]"))) %>% 
    left_join(shp, ., by = c("NAME" = "space")) -> res_fig14
  
  
  X %>% group_by(age, sex, NAME) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig3
  
  
  pbapply(torun, 
          1, 
          function(X){
            sexg = X[1]
            ageg = X[2]
            
            data.frame(
              space = res_fig3 %>% filter(age %in% ageg, sex %in% sexg) %>% pull(NAME), 
              median = res_fig3 %>% 
                filter(age %in% ageg, sex %in% sexg) %>% 
                ungroup() %>% 
                dplyr::select(starts_with("V")) %>% 
                apply(., 1, quantile, probs = 0.50) %>% 
                as.numeric(), 
              exceedance = sweep(
                res_fig3 %>% 
                  filter(age %in% ageg, sex %in% sexg) %>% 
                  ungroup() %>% 
                  dplyr::select(starts_with("V"))
                , 
                1, 
                res_fig3 %>% 
                  filter(age %in% ageg, sex %in% sexg) %>% 
                  pull(obs_deaths), 
                FUN = "-"
              ) %>% 
                ChangeSign() %>% 
                apply(., 1, function(X) mean(X>=0))
            ) -> df
            
            df %>% 
              mutate(excat0_8 = case_when(exceedance > 0.80 ~ "(0.80, 1]",
                                          exceedance > 0.20 & exceedance <= 0.80 ~ "(0.20, 0.80]",
                                          TRUE ~ "[0, 0.2]")) %>% 
              mutate(excat0_8 = factor(excat0_8, levels = c("[0, 0.2]", "(0.20, 0.80]", "(0.80, 1]"))) %>% 
              mutate(excat0_9 = case_when(exceedance > 0.90 ~ "(0.90, 1]",
                                          exceedance > 0.10 & exceedance <= 0.90 ~ "(0.10, 0.90]",
                                          TRUE ~ "[0, 0.1]")) %>% 
              mutate(excat0_9 = factor(excat0_9, levels = c("[0, 0.1]", "(0.10, 0.90]", "(0.90, 1]"))) %>% 
              mutate(excat0_95 = case_when(exceedance > 0.95 ~ "(0.95, 1]",
                                           exceedance > 0.05 & exceedance <= 0.95 ~ "(0.05, 0.95]",
                                           TRUE ~ "[0, 0.05]")) %>% 
              mutate(excat0_95 = factor(excat0_95, levels = c("[0, 0.05]", "(0.05, 0.95]", "(0.95, 1]"))) %>% 
              left_join(shp, ., by = c("NAME" = "space")) %>% 
              mutate(age = ageg, sex = sexg) %>% return()
            
          }) -> res_fig3
  
  do.call(rbind, res_fig3) -> res_fig3
  res_fig3 %>% mutate(sex = recode(sex, male = "Males", female = "Females")) %>% 
    mutate(sex = factor(sex, levels = c("Males", "Females"))) -> res_fig3
  
  # and by sex
  X %>% group_by(sex, NAME) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig31
  
  
  pblapply(c("male", "female"), 
          function(X){
            sexg = X
            
            data.frame(
              space = res_fig31 %>% filter(sex %in% sexg) %>% pull(NAME), 
              median = res_fig31 %>% 
                filter(sex %in% sexg) %>% 
                ungroup() %>% 
                dplyr::select(starts_with("V")) %>% 
                apply(., 1, quantile, probs = 0.50) %>% 
                as.numeric(), 
              exceedance = sweep(
                res_fig31 %>% 
                  filter(sex %in% sexg) %>% 
                  ungroup() %>% 
                  dplyr::select(starts_with("V"))
                , 
                1, 
                res_fig31 %>% 
                  filter(sex %in% sexg) %>% 
                  pull(obs_deaths), 
                FUN = "-"
              ) %>% 
                ChangeSign() %>% 
                apply(., 1, function(X) mean(X>=0))
            ) -> df
            
            df %>% 
              mutate(excat0_8 = case_when(exceedance > 0.80 ~ "(0.80, 1]",
                                          exceedance > 0.20 & exceedance <= 0.80 ~ "(0.20, 0.80]",
                                          TRUE ~ "[0, 0.2]")) %>% 
              mutate(excat0_8 = factor(excat0_8, levels = c("[0, 0.2]", "(0.20, 0.80]", "(0.80, 1]"))) %>% 
              mutate(excat0_9 = case_when(exceedance > 0.90 ~ "(0.90, 1]",
                                          exceedance > 0.10 & exceedance <= 0.90 ~ "(0.10, 0.90]",
                                          TRUE ~ "[0, 0.1]")) %>% 
              mutate(excat0_9 = factor(excat0_9, levels = c("[0, 0.1]", "(0.10, 0.90]", "(0.90, 1]"))) %>% 
              mutate(excat0_95 = case_when(exceedance > 0.95 ~ "(0.95, 1]",
                                           exceedance > 0.05 & exceedance <= 0.95 ~ "(0.05, 0.95]",
                                           TRUE ~ "[0, 0.05]")) %>% 
              mutate(excat0_95 = factor(excat0_95, levels = c("[0, 0.05]", "(0.05, 0.95]", "(0.95, 1]"))) %>% 
              left_join(shp, ., by = c("NAME" = "space")) %>% 
              mutate(sex = sexg) %>% return()
            
          }) -> res_fig31
  
  do.call(rbind, res_fig31) -> res_fig31
  res_fig31 %>% mutate(sex = recode(sex, male = "Males", female = "Females")) %>% 
    mutate(sex = factor(sex, levels = c("Males", "Females"))) -> res_fig31
  
  
  
  # Results for tile plot

  X %>% group_by(NAME, time) %>% 
    dplyr::select(starts_with("V"), obs_deaths) %>% 
    summarize_all(sum) -> res_fig4
  
  
  data.frame(
    space = res_fig4 %>% pull(NAME), 
    time = res_fig4 %>%pull(time),
    median = res_fig4 %>%
      ungroup() %>% 
      dplyr::select(starts_with("V")) %>% 
      apply(., 1, quantile, probs = 0.50) %>% 
      as.numeric(), 
    exceedance = sweep(
      res_fig4 %>% 
        ungroup() %>% 
        dplyr::select(starts_with("V"))
      , 
      1, 
      res_fig4 %>% 
        pull(obs_deaths), 
      FUN = "-"
    ) %>% 
      ChangeSign() %>% 
      apply(., 1, function(X) mean(X>=0))) -> res_fig4
  
  res_fig4 %>%
    mutate(excat = ifelse(exceedance < 0.95, "[0, 0.95)", "[0.95, 1]")) %>% 
    mutate(excat = factor(excat, levels = c("[0, 0.95)", "[0.95, 1]"))) -> res_fig4
  
  
  
  
  ret_list <- list(
    res_fig11 = res_fig11,
    res_fig12 = res_fig12, 
    res_fig13 = res_fig13, 
    res_fig131 = res_fig131,
    res_fig14 = res_fig14,
    res_fig2 = res_fig2, 
    res_fig3 = res_fig3,
    res_fig31 = res_fig31,
    res_fig4 = res_fig4
  )
  
  ret_list %>% return()
}







#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################




