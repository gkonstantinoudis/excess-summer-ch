#:::::::::::::::::::::::::::::
# Project: death_ascertainment
# description: initialize R session
#:::::::::::::::::::::::::::::

# load libraries -----

library(jsonlite)
library(tidyverse)
library(readxl)
library(sf)
library(tidyr)
library(ggplot2)
library(INLA)
library(patchwork)
library(lubridate)
library(readODS)
library(xtable)
library(zoo)
library(readxl)
library(ecmwfr)
library(ncdf4)
library(pbapply)
library(plyr)
library(data.table)
library(cowplot)
library(doParallel)
library(data.table)
library(spData)
library(spdep)
library(viridis)
library(purrr)
library(splines)
library(stringr)
library(gridExtra)
library(abind)

# set paths ----
path_script = "rfiles"
path_output = "output"

# small custom functions ----

cri <- function(X) paste0(X[1], " (", X[2], ", ", X[3], ")")

ChangeSign <- function(X) X*(-1)

GetSubTabs <- function(Y, group_var){
  
  observed <- Y %>% 
    group_by_at(group_var) %>% 
    dplyr::select(obs_deaths) %>% 
    summarize_all(sum)
  
  fitted.sums <- Y %>% 
    group_by_at(group_var) %>% 
    dplyr::select(starts_with("V")) %>% 
    summarize_all(sum) %>% 
    ungroup()
  
  expected <- 
    fitted.sums %>% 
    dplyr::select(starts_with("V")) %>% 
    apply(., 1, quantile, probs = c(0.50, 0.025, 0.975)) %>% 
    t() %>% 
    round() %>% 
    as.data.frame() %>% 
    apply(., 1, cri) %>%
    as.data.frame() %>% 
    setNames(., "Expected")
  
  excess <- sweep(
    fitted.sums %>% 
      dplyr::select(starts_with("V")), 
    1, 
    observed$obs_deaths, 
    FUN = "-"
  ) %>% 
    ChangeSign() %>% 
    apply(., 1, quantile, probs = c(0.50, 0.025, 0.975)) %>% 
    round() %>% 
    t() %>% 
    as.data.frame() %>% 
    apply(., 1, cri) %>%
    as.data.frame() %>% 
    setNames(., "Excess")
  
  
  relative.excess <- sweep(
    fitted.sums %>% 
      dplyr::select(starts_with("V")), 
    1, 
    observed$obs_deaths, 
    FUN = "-"
  ) %>% 
    ChangeSign() %>% 
    sweep(
      ., 
      1, 
      fitted.sums %>% 
        dplyr::select(starts_with("V")) %>% 
        apply(., 1, quantile, probs = 0.50), 
      FUN = "/"
    ) %>% 
    apply(., 1, quantile, probs = c(0.50, 0.025, 0.975)) %>% 
    t() %>% 
    as.data.frame() %>% 
    round(digits = 2) %>% 
    format(nsmall = 2) %>% 
    apply(., 1, cri) %>%
    as.data.frame() %>% 
    setNames(., "RelativeExcess")
  
  data.frame(observed, 
             expected, 
             excess, 
             relative.excess, 
             covid = Y %>% 
               group_by_at(group_var) %>% 
               dplyr::summarize(covid_deaths = sum(covid_deaths)) %>% 
               pull(covid_deaths)) %>% return()
  
}
##
##
##?



##
## DO NOT RUN HERE

# source functions

fili = dir(path_script,pattern="da_[0123456789]")
lapply(X = fili, FUN = function(x) {source(file.path(path_script, x), echo=FALSE)} )


# aesthetics ----
theme_set(theme_bw())
col_labd = "firebrick"
col_excess1 = "chartreuse4"
col_excess2 = "dodgerblue"
col_expected = "skyblue"
