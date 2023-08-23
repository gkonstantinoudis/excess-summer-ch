#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: Table 1
#:::::::::::::::::::::::::::::


# Table 1

GenerateTab1 <- function(Y){
  
  # main bulk of table 1
  GetSubTabs(Y = Y, c("age", "sex")) -> tab11
  
  GetSubTabs(Y = Y, c("age")) -> tab12
  tab12 %>% mutate(sex = "Total") -> tab12
  tab12 %>% select_at(colnames(tab11)) -> tab12
  
  GetSubTabs(Y = Y, c("sex")) -> tab13
  tab13 %>% mutate(age = "Total") -> tab13
  tab13 %>% select_at(colnames(tab11)) -> tab13
  
  # totals
  obs_deaths_total <- sum(Y$obs_deaths)
  
  Y %>% 
    ungroup() %>% 
    dplyr::select(starts_with("V")) %>% 
    summarize_all(sum) %>% 
    as.numeric() %>% 
    quantile(., probs = c(0.5, 0.025, 0.975)) %>% 
    round() %>% 
    cri() -> expected_total
  
  Y %>% 
    ungroup() %>% 
    dplyr::select(starts_with("V")) %>% 
    summarize_all(sum) %>% 
    sweep(., 1, obs_deaths_total, FUN = "-") %>% 
    ChangeSign() %>% 
    as.numeric() %>% 
    quantile(., probs = c(0.5, 0.025, 0.975)) %>% 
    round() %>% 
    cri() -> excess_total
  
  (Y %>% 
      ungroup() %>% 
      dplyr::select(starts_with("V")) %>% 
      summarize_all(sum) %>% 
      sweep(., 1, obs_deaths_total, FUN = "-") %>% 
      ChangeSign() / 
      Y %>% 
      ungroup() %>% 
      dplyr::select(starts_with("V")) %>% 
      summarize_all(sum)) %>% 
    as.numeric() %>% 
    quantile(., probs = c(0.5, 0.025, 0.975)) %>% 
    round(digits = 2) %>% 
    format(nsmall = 2) %>% 
    cri() -> relative.excess_total
  
  covid_deaths_total <- sum(Y$covid_deaths)
  
  rbind(
    tab11,
    tab12, 
    tab13,
    c("Total", "Total", obs_deaths_total, expected_total, excess_total, relative.excess_total, covid_deaths_total)
  ) %>% return()
}











#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################








