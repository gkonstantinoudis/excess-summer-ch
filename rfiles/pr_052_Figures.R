#:::::::::::::::::::::::::::::
# Project: summer_2022
# description: Figures 1-3
#:::::::::::::::::::::::::::::

  
PlotTimeSeries <- function(dat, fcol = cividis(15)[11], lcol = "black", agg = "agesex"){
  
  ggplot(data = dat) +

    geom_ribbon(aes(x = time,
                    ymin=`2.5%`, 
                    ymax=`97.5%`), 
                linetype=0, alpha=0.2,
                fill = fcol) +
    
    geom_ribbon(aes(x = time,
                    ymin=`10%`, 
                    ymax=`90%`), 
                linetype=0, alpha=0.2,
                fill = fcol) +
    
    geom_ribbon(aes(x = time,
                    ymin=`20%`, 
                    ymax=`80%`), 
                linetype=0, alpha=0.3,
                fill = fcol) +
    
    geom_ribbon(aes(x = time,
                    ymin=`30%`, 
                    ymax=`70%`), 
                linetype=0, alpha=0.3,
                fill = fcol) +
    
    geom_ribbon(aes(x = time,
                    ymin=`40%`, 
                    ymax=`60%`), 
                linetype=0, alpha=0.4,
                fill = fcol) + 
    
    geom_line(color = lcol, linewidth = 0.4, aes(x= time, y = obs_deaths))  + 
    geom_point(color = lcol, size = 0.55, aes(x= time, y = obs_deaths)) + theme_bw() + 
    xlab("") + ylab("") + 
    theme(plot.margin = unit(c(0,0,0,0), "cm"), 
          text = element_text(size=8)) -> p1
  

  if(agg == "total"){
    p1 -> p1
  }
  if(agg == "sex"){
    p1 + 
      geom_rug(aes(x=time, linewidth = covid_deaths), inherit.aes = F, alpha = 0.3) + 
      facet_grid(cols = vars(sex)) + 
      theme(legend.position = "none") -> p1
  }
  if(agg == "agesex"){
    p1 + 
      geom_rug(aes(x=time, linewidth = covid_deaths), inherit.aes = F, alpha = 0.3) + 
      facet_grid(cols = vars(sex), rows = vars(age)) + 
      theme(legend.position = "none") -> p1
  }
  p1 %>% return()
}



ForestPlot <- function(X){
  
  X %>% filter(sex %in% "Males") %>% pull(space) -> lev
  X %>% mutate(space = factor(space, levels = 
                                lev[X %>% filter(sex %in% "Males") %>% pull(`50%`) %>% order(decreasing = FALSE)])) -> X
    
  ggplot() + 
  geom_point(data = X, aes(x = `50%`, y = space), size = 0.7) + 
  geom_errorbar(data = X, aes(y = space, xmin = `2.5%`, xmax = `97.5%`), width = .5, linewidth = 0.4) + 
  facet_grid(cols = vars(sex), rows = vars(age)) + 
  geom_vline(xintercept = 0, col = "red", linetype = "dashed", linewidth = 0.4) + 
  ylab("") + xlab("") + 
    theme(plot.margin = unit(c(0,0,0,0), "cm"), 
          text = element_text(size=8)) %>% return()
    
}


MapFig <- function(X, agg = "agesex", metric = "excat0_9"){
  
  # NOTE thus function works with to dataframes. One for each of the if case.
  
  if(agg == "agesex"){
    ggplot() + geom_sf(data = X, aes_string(fill = metric)) + 
      scale_fill_viridis_d(option = "mako", 
                           begin = 0.2, 
                           end = 1, 
                           alpha = 0.70, 
                           name = "") + 
      xlab("") + ylab("") + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"), 
            text = element_text(size=8)) + 
      facet_grid(rows = vars(age), cols = vars(sex)) -> p
    
  }
  

  if(agg == "total"){
    ggplot() + geom_sf(data = X, aes(fill = medianrr)) + 
      scale_fill_viridis_c(option = "mako", 
                           begin = 0.2, 
                           end = 1, 
                           alpha = 0.70, 
                           name = "") + 
      xlab("") + ylab("") + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"), 
            text = element_text(size=8)) -> p1
    
    ggplot() + geom_sf(data = X, aes_string(fill = metric)) + 
      scale_fill_viridis_d(option = "mako", 
                           begin = 0.2, 
                           end = 1, 
                           alpha = 0.70, 
                           name = "") + 
      xlab("") + ylab("") + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"), 
            text = element_text(size=8))-> p2 
    
    list(p1, p2) -> p 
  }

  return(p)
}


# Figure 3

TilePlot <- function(X){
  colfunc <- c("white", "grey66")
  N <- X$space %>% unique() %>% length() 
  x.names <- X$space %>% as.factor() %>% levels()
  X$y <- X$space %>% as.factor() %>% as.numeric()
  
  ggplot() +  
    ylim(c(0, c(N+2))) + 
    scale_fill_manual(values=colfunc) + 
    theme_bw() + geom_raster(data = X, aes(x = time, y = y, fill = excat))  + 
    scale_y_continuous(breaks = 1:(N),
                       labels = x.names, 
                       expand = expansion(mult = c(0, 0))) + 
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor.x = element_blank(), 
      panel.grid.minor.y = element_line(size=.1, color="black" ),
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE, 
      legend.spacing.y = unit(0, "mm"), 
      legend.spacing.x = unit(0, "mm"), 
      legend.title = element_blank(),
      legend.position = "none"
      
    ) + ylab("") + xlab("") + guides(fill=guide_legend(nrow=1, byrow=TRUE)) %>% return()
  
}

# Figure 4.1

Heatwave_ROC <- function(X){
  ggplot() + 
    geom_point(data = X, aes(x = quant, y = excess, col = Duration), size = 1) +
    geom_line(data = X, aes(x = quant, y = excess, col = Duration)) + 
    scale_color_viridis_d(option = "A", end = 0.8) + 
    geom_abline(slope = -465, intercept = 465, col = "blue", lwd = 1, lty = 2) + 
    ylim(0, 470) + 
    ylab("Excess deaths") + 
    xlab("") + theme(legend.position="none") -> p51
  
  ggplot(data = X %>% mutate(excess = ifelse(excess<=0, NA, excess))) + 
    geom_point(aes(x = quant, y = excess/total_events, col = Duration), size = 1) +
    geom_line(aes(x = quant, y = excess/total_events, col = Duration)) + 
    scale_color_viridis_d(option = "A", end = 0.8) + 
    ylab("Excess deaths per heatwave") + 
    xlab("Quantile") + theme(legend.position="none") -> p52
  
  ggplot(data = X %>% mutate(rel_ex = ifelse(rel_ex<=0, NA, rel_ex))) + 
    geom_point(aes(x = quant, y = rel_ex, col = Duration), size = 1) +
    geom_line(aes(x = quant, y = rel_ex, col = Duration)) + 
    scale_color_viridis_d(option = "A", end = 0.8) + 
    ylab("Relative excess") + 
    xlab("") -> p53
  
  p51|p52|p53 %>% return()
}

# Figure 4.2
Heatwave_tile <- function(X){
  
  ggplot() +
    geom_tile(data = X, aes(x = quant, y = Duration, fill = total_events)) + 
    ylab("Duration") + xlab("") + 
    scale_fill_viridis_c(name = "", 
                         breaks = c(500, 1000, 1500, 2000), 
                         labels = c("0.5", "1", "1.5", "2"), 
                         option = "A") +
    ggtitle("A. Number of events per 1000") +
    theme(legend.position="bottom", 
          legend.key.size = unit(0.5, "cm"), 
          legend.margin=margin(0,0,0,0), 
          legend.box.margin=margin(-20, -10, 3, -10), 
          text = element_text(size = 7.5), 
          plot.margin = unit(c(0,0,0,0), "cm"))|
    
    ggplot() +
    geom_tile(data = X, aes(x = quant, y = Duration, fill = excess)) + 
    ylab("") + xlab("Temperature quantile") + 
    scale_fill_viridis_c(name = "", option = "A") + 
    ggtitle("B. Excess deaths") +
    theme(legend.position="bottom", 
          legend.key.size = unit(0.5, "cm"), 
          legend.margin=margin(0,0,0,0), 
          legend.box.margin=margin(-20, -10, 3, -10), 
          text = element_text(size = 7.5), 
          plot.margin = unit(c(0,0,0.3,0), "cm"), 
          axis.title.x = element_text(vjust = -15))+ 
    geom_segment(data = seg1, aes(x = quant - 0.025, y=duration-0.5, xend = quant - 0.025, yend=duration+0.5), 
                 lwd = 1.5, col = "white")|
  # + 
  #   geom_segment(data = seg2, aes(x = quant - 0.025, y=duration-0.5, xend = quant - 0.025, yend=duration+0.5), 
  #                lwd = 1.5, col = "gray60")
  
    
    ggplot(data = X) +
    geom_tile(aes(x = quant, y = Duration, fill = rel_ex)) + 
    ylab("") + xlab("") + 
    scale_fill_viridis_c(name = "", option = "A") + 
    ggtitle("C. Relative excess") +
    theme(legend.position="bottom", 
          legend.key.size = unit(0.5, "cm"), 
          legend.margin=margin(0,0,0,0), 
          legend.box.margin=margin(-20, -10, 3, -10), 
          text = element_text(size = 7.5), 
          plot.margin = unit(c(0,0,0,0), "cm"))+ 
    geom_segment(data = seg1, aes(x = quant - 0.025, y=duration-0.5, xend = quant - 0.025, yend=duration+0.5), 
                 lwd = 1.5, col = "white") -> p54
  # + 
  #   geom_segment(data = seg2, aes(x = quant - 0.025, y=duration-0.5, xend = quant - 0.025, yend=duration+0.5), 
  #                lwd = 1.5, col = "gray60") -> p54
  
  p54 %>% return()
  
}


getborders <- function(ov.excess = ov.excess, timesrr = timesrr, br = br){
  dat_tile$ExceedancePr <- 
    lapply(res, function(X){
      sapply(X$res_all, function(Y){
        mean(Y$rel_excess > timesrr*ov.excess)
      })
    }) %>% unlist()
  
  dat_tile$ExceedancePr <- dat_tile$ExceedancePr
  dat_tile$ExceedancePr_cat <- 
    cut(dat_tile$ExceedancePr, breaks = br, 
        labels = c("low", "mid", "high"),
        include.lowest = TRUE)
  
  dat_tile %>% 
    dplyr::filter(ExceedancePr_cat == "high") %>% 
    dplyr::group_by(duration) %>% 
    dplyr::mutate(lab = ifelse(quant == min(quant), 1, 0)) %>% 
    dplyr::filter(lab == 1) %>% return()
}
  
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################



