#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: tile plot heatwaves
#:::::::::::::::::::::::::::::

res_tab <- readRDS(file.path(path_output, "res_tab_dynamo_covid"))
temperature_weighted <- readRDS(file.path(path_output,"temperature_pop_weighted.RDS")) 
temperature_weighted %>% dplyr::mutate(year = year(date)) %>% filter(year < 2022) -> temperature

tile_heatwaves <- function(Y){
  
  threshold <- quantile(temperature$temperature, probs = Y)
  
  res_tab %>% 
    dplyr::filter(age %in% "80+") %>% 
    dplyr::select(age, time, canton, obs_deaths, covid_deaths, starts_with("V")) %>% 
    dplyr::group_by(age, time, canton) %>% 
    dplyr::summarise_all(sum) -> res_tab_elderly 
  
  # add temperature
  res_tab <- left_join(res_tab, temperature_weighted %>% dplyr::select(NAME, date, temperature), 
                       by = c("time" = "date", "NAME" = "NAME"))
  res_tab$temperature_weighted <- res_tab$temperature
  
  res_tab_elderly %>% 
    left_join(., res_tab %>% 
                dplyr::filter(age %in% "80+", sex %in% "male") %>% 
                dplyr::select(time, canton, temperature_weighted), 
              by = c("time" = "time", "canton" = "canton")) -> res_tab_elderly
  
  # calculate heatwaves
  res_tab_elderly %>% 
    dplyr::mutate(
      hotperiods_l1 = temperature_weighted >= threshold,
      hotperiods_l2 = dplyr::case_when(temperature_weighted >= threshold &
                                         dplyr::lag(temperature_weighted, 1) >= threshold ~ 1, 
                                       TRUE ~ 0), 
      hotperiods_l3 = dplyr::case_when(temperature_weighted >= threshold &
                                         dplyr::lag(temperature_weighted, 1) >= threshold & 
                                         dplyr::lag(temperature_weighted, 2) >= threshold ~ 1, 
                                       TRUE ~ 0),
      hotperiods_l4 = dplyr::case_when(temperature_weighted >= threshold &
                                         dplyr::lag(temperature_weighted, 1) >= threshold & 
                                         dplyr::lag(temperature_weighted, 2) >= threshold &
                                         dplyr::lag(temperature_weighted, 3) >= threshold ~ 1, 
                                       TRUE ~ 0),
      hotperiods_l5 = dplyr::case_when(temperature_weighted >= threshold &
                                         dplyr::lag(temperature_weighted, 1) >= threshold & 
                                         dplyr::lag(temperature_weighted, 2) >= threshold &
                                         dplyr::lag(temperature_weighted, 3) >= threshold &
                                         dplyr::lag(temperature_weighted, 4) >= threshold ~ 1, 
                                       TRUE ~ 0),
      ##
      
      # hotperiods_l2 = dplyr::case_when(hotperiods_l2 == 1 |
      #                                    dplyr::lead(hotperiods_l2, 1) == 1 ~ 1,  
      #                                  TRUE ~ 0), 
      # hotperiods_l3 = dplyr::case_when(hotperiods_l3 == 1 |
      #                                    dplyr::lead(hotperiods_l3, 1) == 1 |
      #                                    dplyr::lead(hotperiods_l3, 2) == 1 ~ 1,  
      #                                  TRUE ~ 0), 
      # hotperiods_l4 = dplyr::case_when(hotperiods_l4 == 1 |
      #                                    dplyr::lead(hotperiods_l4, 1) == 1 |
      #                                    dplyr::lead(hotperiods_l4, 2) == 1 |
      #                                    dplyr::lead(hotperiods_l4, 3) == 1 ~ 1,  
      #                                  TRUE ~ 0),
      # hotperiods_l5 = dplyr::case_when(hotperiods_l5 == 1 |
      #                                    dplyr::lead(hotperiods_l5, 1) == 1 |
      #                                    dplyr::lead(hotperiods_l5, 2) == 1 |
      #                                    dplyr::lead(hotperiods_l5, 3) == 1 |
      #                                    dplyr::lead(hotperiods_l5, 2) == 1 ~ 1,  
      #                                  TRUE ~ 0), 
      
      ## and the lags 0-3
      hotperiods_l1_lag0_3 = dplyr::case_when(hotperiods_l1 == 1 |
                                                dplyr::lag(hotperiods_l1, 1) == 1 |
                                                dplyr::lag(hotperiods_l1, 2) == 1 |
                                                dplyr::lag(hotperiods_l1, 3) == 1 ~ 1,  
                                              TRUE ~ 0) , 
      hotperiods_l2_lag0_3 = dplyr::case_when(hotperiods_l2 == 1 |
                                                dplyr::lag(hotperiods_l2, 1) == 1 |
                                                dplyr::lag(hotperiods_l2, 2) == 1 |
                                                dplyr::lag(hotperiods_l2, 3) == 1 ~ 1,  
                                              TRUE ~ 0) , 
      hotperiods_l3_lag0_3 = dplyr::case_when(hotperiods_l3 == 1 |
                                                dplyr::lag(hotperiods_l3, 1) == 1 |
                                                dplyr::lag(hotperiods_l3, 2) == 1 |
                                                dplyr::lag(hotperiods_l3, 3) == 1 ~ 1,  
                                              TRUE ~ 0) , 
      hotperiods_l4_lag0_3 = dplyr::case_when(hotperiods_l4 == 1 |
                                                dplyr::lag(hotperiods_l4, 1) == 1 |
                                                dplyr::lag(hotperiods_l4, 2) == 1 |
                                                dplyr::lag(hotperiods_l4, 3) == 1 ~ 1,  
                                              TRUE ~ 0) , 
      hotperiods_l5_lag0_3 = dplyr::case_when(hotperiods_l5 == 1 |
                                                dplyr::lag(hotperiods_l5, 1) == 1 |
                                                dplyr::lag(hotperiods_l5, 2) == 1 |
                                                dplyr::lag(hotperiods_l5, 3) == 1 ~ 1,  
                                              TRUE ~ 0) 
    ) -> res_tab_elderly
  
  
  # number of events
  c(
    sum(res_tab_elderly$hotperiods_l1),
    sum(res_tab_elderly$hotperiods_l2)/2,
    sum(res_tab_elderly$hotperiods_l2)/3,
    sum(res_tab_elderly$hotperiods_l2)/4,
    sum(res_tab_elderly$hotperiods_l2)/5
  ) -> tot_events
  
  
  tot_events <- round(tot_events)
  
  # number of deaths
  res_tab_elderly$hotperiods_l1_lag0_3 <- res_tab_elderly$hotperiods_l1_lag0_3 %>% as.numeric() %>% as.factor()
  res_tab_elderly$hotperiods_l2_lag0_3 <- res_tab_elderly$hotperiods_l2_lag0_3 %>% as.numeric() %>% as.factor()
  res_tab_elderly$hotperiods_l3_lag0_3 <- res_tab_elderly$hotperiods_l3_lag0_3 %>% as.numeric() %>% as.factor()
  res_tab_elderly$hotperiods_l4_lag0_3 <- res_tab_elderly$hotperiods_l4_lag0_3 %>% as.numeric() %>% as.factor()
  res_tab_elderly$hotperiods_l5_lag0_3 <- res_tab_elderly$hotperiods_l5_lag0_3 %>% as.numeric() %>% as.factor()
  
  res_num_deaths <- list()
  for(i in 1:5){
    res_tab_elderly %>% 
      dplyr::filter(!!rlang::parse_expr(paste0("hotperiods_l", i, "_lag0_3")) == 1) -> tmp
    
    obs <- sum(tmp$obs_deaths)
    exp <- (tmp %>%
              dplyr::ungroup(age, time) %>% 
              select(dplyr::starts_with("V")) %>% 
              apply(., 2, sum))
    excess <- obs - exp
    rel_excess <- (obs - exp)/exp
    
    res_num_deaths[[i]] <- 
      list(
        obs = obs, 
        exp = exp, 
        excess = excess, 
        rel_excess = rel_excess
      )
  }
  
  
  
  num_deaths <- sapply(lapply(res_num_deaths, function(X) X$excess), median)
  rel_ex <- sapply(lapply(res_num_deaths, function(X) X$rel_excess), median)
  
  # exceedance
  exceed_deaths <- sapply(lapply(res_num_deaths, function(X) X$excess), function(X) mean(X>0))
  exceed_rel_ex <- sapply(lapply(res_num_deaths, function(X) X$rel_excess), function(X) mean(X>0))
  
  list(
    threshold = threshold,
    tot_events = tot_events,
    num_deaths = num_deaths, 
    rel_ex = rel_ex,
    exceed_deaths = exceed_deaths, 
    exceed_rel_ex = exceed_rel_ex,
    res_all = res_num_deaths
  ) %>% return()
}

# I will define 5 thresholds
thr_grid <- seq(from = 0.05, to = 0.99, by = 0.05)
res <- lapply(thr_grid, tile_heatwaves)


saveRDS(res, file = file.path(path_output,"dat_tile"))
