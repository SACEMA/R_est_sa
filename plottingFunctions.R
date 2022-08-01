plot_ts_onset <- function(ts_onset,
                          qq = use_quant,
                          x_min = min_plot_date,
                          x_max = max_plot_date,
                          y_lab = "Lab-confirmed COVID-19 deaths",
                          y_max = 22000){
  tmp <- (ts_onset 
          %>% group_by(onset_date) 
          %>% summarize(med = median(cnt)
                        , med_adj = median(adj_cnt)
                        , upper = quantile(cnt, 1-(100-qq)/200)
                        , lower = quantile(cnt, (100-qq)/200)
                        , upper_adj = quantile(adj_cnt, 1-(100-qq)/200)
                        , lower_adj = quantile(adj_cnt, (100-qq)/200))
          %>% filter(onset_date >= x_min, onset_date <= x_max)
  )
  
  labheight <- max(tmp$upper_adj + 50)
  blockheight <- labheight + 100
  segheight <- blockheight
  
  lvl5 <- data.frame(xmin = as.Date('2020-03-27')-0.5, xmax = as.Date('2020-05-01'), ymin = -20, ymax = blockheight)
  lvl4 <- data.frame(xmin = as.Date('2020-05-01'), xmax = as.Date('2020-06-01'), ymin = -20, ymax = blockheight)
  lvl3 <- data.frame(xmin = as.Date('2020-06-01'), xmax = as.Date('2020-08-17'), ymin = -20, ymax = blockheight)
  lvl2 <- data.frame(xmin = as.Date('2020-08-17'), xmax = as.Date('2020-09-21'), ymin = -20, ymax = blockheight)
  lvl1 <- data.frame(xmin = as.Date('2020-09-21'), xmax = as.Date('2020-12-27'), ymin = -20, ymax = blockheight)
  lvl3p <-data.frame(xmin = as.Date('2020-12-28'), xmax = x_max, ymin = -20, ymax = blockheight)
  # print(max(tmp$upper_adj + 10))
  (ggplot(tmp)
    + aes(x = onset_date)
    + geom_rect(data = lvl5, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#99241B')
    + geom_rect(data = lvl4, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#E03127')
    + geom_rect(data = lvl3, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
    + geom_rect(data = lvl2, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f7e51e')
    + geom_rect(data = lvl1, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f9ff8f')
    + geom_rect(data = lvl3p, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
    + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = 'white')
    + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3)
    + geom_ribbon(aes(ymin = lower_adj, ymax = upper_adj), alpha = 0.3, fill = 'white')
    + geom_ribbon(aes(ymin = lower_adj, ymax = upper_adj), alpha = 0.3, fill = 'blue')
    + geom_segment(data = lvl5, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl4, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl2, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl1, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3p, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl5$xmax) + as.numeric(lvl5$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'Level 5'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl4$xmax) + as.numeric(lvl4$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'Level 4'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3$xmax) + as.numeric(lvl3$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'Level 3'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2$xmax) + as.numeric(lvl2$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'Level 2'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1$xmax) + as.numeric(lvl1$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'Level 1'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3p$xmax) + as.numeric(lvl3p$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'Adjusted Level 3'), color = '#333333', fontface = 'bold')
    # + geom_line(aes(y = med), color = '#797A7D')
    + geom_line(aes(y = med_adj), color = 'blue')
    + theme_minimal(base_size = 19)
    + xlab('Date of onset')
    + ylab(y_lab)
    + coord_cartesian(ylim = c(0, min(y_max, max(tmp$upper_adj + tmp$upper_adj/12))))
    + scale_x_date(date_breaks = "months",
                   date_minor_breaks = "months",
                   date_labels = "%b-%y",
                   limits = c(x_min,x_max)
    )
    + xlim(x_min, x_max)
  )
}

plot_rt <- function(rt, qq = use_quant, x_min = min_plot_date, x_max = max_plot_date){
  tmp <- rt %>% group_by(onset_date) %>% summarize(median = median(med)
                                                   , upper = median(upr)
                                                   , lower = median(lwr))
  lvl5 <- data.frame(xmin = as.Date('2020-03-27')-0.5, xmax = as.Date('2020-05-01'), ymin = -1, ymax = 4)
  lvl4 <- data.frame(xmin = as.Date('2020-05-01'), xmax = as.Date('2020-06-01'), ymin = -1, ymax = 4)
  lvl3 <- data.frame(xmin = as.Date('2020-06-01'), xmax = as.Date('2020-08-17'), ymin = -1, ymax = 4)
  lvl2 <- data.frame(xmin = as.Date('2020-08-17'), xmax = as.Date('2020-09-21'), ymin = -1, ymax = 4)
  lvl1 <- data.frame(xmin = as.Date('2020-09-21'), xmax = as.Date('2020-12-27'), ymin = -1, ymax = 4)
  lvl3p <- data.frame(xmin = as.Date('2020-12-28'), xmax = x_max, ymin = -1, ymax = 4)
  (ggplot(tmp)
    + aes(x = onset_date)
    + geom_rect(data = lvl5, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#99241B')
    + geom_rect(data = lvl4, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#E03127')
    + geom_rect(data = lvl3, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
    + geom_rect(data = lvl2, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f7e51e')
    + geom_rect(data = lvl1, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f9ff8f')
    + geom_rect(data = lvl3p, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
    + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = 'white')
    + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = 'blue')
    + geom_segment(data = lvl5, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl4, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl2, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl1, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3p, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_line(aes(y = median), color = 'blue')
    + theme_minimal(base_size = 19)
    + theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl5$xmax) + as.numeric(lvl5$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*3), aes(x = x, y = y, label = 'Level 5'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl4$xmax) + as.numeric(lvl4$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*3), aes(x = x, y = y, label = 'Level 4'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3$xmax) + as.numeric(lvl3$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*3), aes(x = x, y = y, label = 'Level 3'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2$xmax) + as.numeric(lvl2$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*3), aes(x = x, y = y, label = 'Level 2'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1$xmax) + as.numeric(lvl1$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*3), aes(x = x, y = y, label = 'Level 1'), color = '#333333', fontface = 'bold')
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3p$xmax) + as.numeric(lvl3p$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*3), aes(x = x, y = y, label = 'Adjusted Level 3'), color = '#333333', fontface = 'bold')
    + xlab('Date of onset')
    + ylab(expression(R[t]))
    + ylab('Reproduction number')
    + coord_cartesian(ylim = c(0,3))
    + scale_x_date(date_breaks = "months",
                   date_minor_breaks = "months",
                   date_labels = "%b",
                   limits = c(x_min,x_max),
    )
    + xlim(x_min, x_max)
  )
}



plot_rt_overlay <- function(rt,
                            x_min = min_plot_date,
                            x_max = max_plot_date,
                            show_Level_colors = FALSE,
                            pal = brewer.pal(3, "Dark2"),
                            size_adj = 1.2,
                            ommit_legend = F,
                            ommit_deaths = F,
                            legend_label = "Data type",
                            plot_title = NULL,
                            sep_ltypes = FALSE,
                            indicate_holidays = FALSE,
                            show_unrest = FALSE,
                            hline_one = TRUE,
                            R_max = 4.5,
                            level_label_size = 5){

  if("Cases (EpiNow2)" %in% rt$dtype){
    x_lab = "Date of infection"
  }else{
    x_lab = "Date of onset"
  }
  lvl5 <- data.frame(xmin = as.Date('2020-03-27') - 0.5, xmax = as.Date('2020-05-01'), ymin = -1, ymax = R_max)
  lvl4 <- data.frame(xmin = as.Date('2020-05-01'), xmax = as.Date('2020-06-01'), ymin = -1, ymax = R_max)
  lvl3 <- data.frame(xmin = as.Date('2020-06-01'), xmax = as.Date('2020-08-17'), ymin = -1, ymax = R_max)
  lvl2 <- data.frame(xmin = as.Date('2020-08-17'), xmax = as.Date('2020-09-21'), ymin = -1, ymax = R_max)
  lvl1 <- data.frame(xmin = as.Date('2020-09-21'), xmax = as.Date('2020-12-27'), ymin = -1, ymax = R_max)
  lvl3p <- data.frame(xmin = as.Date('2020-12-28'), xmax = as.Date('2021-02-28'), ymin = -1, ymax = R_max)
  lvl1ii <- data.frame(xmin = as.Date('2021-03-01'), xmax = as.Date('2021-05-30'), ymin = -1, ymax = R_max)
  lvl2ii <-  data.frame(xmin = as.Date('2021-05-31'), xmax = as.Date('2021-06-14'), ymin = -1, ymax = R_max)
  # lvl3iii <-  data.frame(xmin = as.Date('2021-06-15'), xmax = as.Date('2021-06-27'), ymin = -1, ymax = R_max) # level 3 in a few places
  lvl4ii <-  data.frame(xmin = as.Date('2021-06-15'), xmax = as.Date('2021-07-25'), ymin = -1, ymax = R_max)
  lvl3iv <- data.frame(xmin = as.Date('2021-07-26'), xmax = as.Date('2021-09-12'), ymin = -1, ymax = R_max)
  lvl2iii <- data.frame(xmin = as.Date('2021-09-13'), xmax = as.Date('2021-09-30'), ymin = -1, ymax = R_max)
  lvl1iii <- data.frame(xmin = as.Date('2021-10-01'), xmax = as.Date('2022-04-05'), ymin = -1, ymax = R_max)
  nolockdown <- data.frame(xmin = as.Date('2022-04-05'), xmax = x_max, ymin = -1, ymax = R_max)
  if(ommit_deaths == TRUE){
    rt[rt$dtype == 'Deaths', c("rt_med", "rt_low", "rt_up")] <- NA
  }
  outplot <- (ggplot(rt)
              + aes(x = onset_date))
  if(show_Level_colors){ # not updated currently
    outplot <- (outplot 
              + geom_rect(data = lvl5, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#99241B')
              + geom_rect(data = lvl4, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#E03127')
              + geom_rect(data = lvl3, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
              + geom_rect(data = lvl2, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f7e51e')
              + geom_rect(data = lvl1, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f9ff8f')
              + geom_rect(data = lvl3p, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E') 
              + geom_rect(data = lvl1ii, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f9ff8f') 
              )
  }
  if(show_unrest){
    si_mean_cumulative <- readRDS('./data/si_mean_cumulative_1-25.RDS')[1:18]
    # data.table::frollmean(1-si_mean_cumulative, n= 7) # to check when sliding window has average (within window) SI less than 5% lying inside disruption window 
    unrest_july21 <- data.frame(xmin = as.Date('2021-07-10'), xmax = as.Date('2021-07-19'), ymin = -1, ymax = 8)
    pud <- data.frame(date = as.Date('2021-07-19') + 0:17, residual_si <- (1 - si_mean_cumulative))
    outplot <- (outplot 
                +  geom_rect(data = unrest_july21, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = 'red')
                +  geom_tile(data = pud, inherit.aes = FALSE, aes(x = date, y = 0, width = 1, height = 22, alpha = (0.1*residual_si)), fill = 'grey', show.legend = FALSE)
    )
    
  }
    # + geom_ribbon(aes(ymin = rt_low, ymax = rt_up), alpha = 0.3, fill = 'white')
  outplot <- (outplot
    + geom_hline(yintercept =  1, lty = 2, alpha = 0.5)
    + geom_ribbon(aes(ymin = rt_low, ymax = rt_up, color = dtype, fill = dtype), alpha = 0.5)
    + geom_segment(data = lvl5, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl4, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl2, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl1, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3p, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl1ii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl2ii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    # + geom_segment(data = lvl3iii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl4ii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl3iv, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl2iii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = lvl1iii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_segment(data = nolockdown, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
    + geom_line(aes(y = rt_med, color = dtype, linetype = dtype), size = size_adj)
    + theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl5$xmax) + as.numeric(lvl5$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L5'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl4$xmax) + as.numeric(lvl4$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L4'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3$xmax) + as.numeric(lvl3$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2$xmax) + as.numeric(lvl2$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L2'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1$xmax) + as.numeric(lvl1$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L1'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3p$xmax) + as.numeric(lvl3p$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1ii$xmax) + as.numeric(lvl1ii$xmin))/2 , origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L1'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2ii$xmax) + as.numeric(lvl2ii$xmin))/2 , origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L2'), color = '#333333', fontface = 'bold', size = level_label_size)
    # + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3iii$xmax) + as.numeric(lvl3iii$xmin))/2 , origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl4ii$xmax) + as.numeric(lvl4ii$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L4'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3iv$xmax) + as.numeric(lvl3iv$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2iii$xmax) + as.numeric(lvl2iii$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L2'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1iii$xmax) + as.numeric(lvl1iii$xmin))/2, origin = as.Date('1970-01-01')), y = 0.95*R_max), aes(x = x, y = y, label = 'L1'), color = '#333333', fontface = 'bold', size = level_label_size)
    + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(nolockdown$xmax) + as.numeric(nolockdown$xmin))/2 + 3, origin = as.Date('1970-01-01')), y = 0.90*R_max), aes(x = x, y = y, label = 'NSD no\nlonger\nin place'), color = '#333333', fontface = 'bold', size = level_label_size)
    + xlab(x_lab)
    + ylab(expression(R[t]))
    + ylab('Reproduction number')
    + coord_cartesian(ylim = c(0,R_max))
    + labs(title = plot_title,
           fill = legend_label,
           color = legend_label,
           linetype = legend_label)
    + xlim(x_min, x_max) 
    + scale_x_date(date_breaks = "months",
                   date_minor_breaks = "months",
                   date_labels = "%b-%y",
                   limits = c(x_min,x_max),
    )
    + theme_minimal(base_size = 19)      
    + scale_color_manual(values = pal)
    + scale_fill_manual(values = pal)
    + scale_linetype_manual(values =  (if(sep_ltypes) c(1,2,3,4,5) else c(1,1,1,1,1)))
  )
  if(ommit_legend==TRUE){
    outplot <- (outplot
                + theme(legend.position = 'none')
    )}
  if(indicate_holidays == TRUE){ # very incomplete
    # define holidays
    easter2020 <- data.frame(xmin = as.Date('2020-04-10'), ymin = -1, ymax = 4)
    easter2021 <- data.frame(xmin = as.Date('2021-04-02'), ymin = -1, ymax = 4)
    freedom_day2021<- data.frame(xmin = as.Date('2021-04-27'), ymin = -1, ymax = 4)
    
    outplot <- (outplot
                + geom_segment(data = easter2020, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = 'red', size = 1.2)
                + geom_segment(data = easter2021, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = 'red', size = 1.2)
                + geom_segment(data = freedom_day2021, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = 'red', size = 1.2)
                )
    
  }
  # if(hline_one == TRUE){
  #   outplot <- (outplot
  #               + geom_hline(yintercept =  1)
  #   )
  # }
  return(outplot)
}

# tmp <- readRDS('./.RDS')

plot_ts_overlay <- function(ts_onset,
                            x_min = min_plot_date,
                            x_max = max_plot_date,
                            y_lab = "Lab-confirmed COVID-19 cases/admissions",#/deaths",
                            y_max = 22000,
                            show_Level_colors = FALSE,
                            lab_axis_adjust = 1,
                            pal = brewer.pal(3, "Dark2"),
                            size_adj = 1.2,
                            ommit_legend = TRUE,
                            dtype_lab = "Cases",
                            legend_label = "Data type",
                            sep_ltypes = F,
                            show_unrest = F,
                            level_label_size = 5){
  
  if("Cases (EpiNow2)" %in% ts_onset$dtype){
    x_lab = "Date of infection"
  }else{
      x_lab = "Date of onset"
    }
  # print(sum(ts_onset$dtype == dtype_lab))
  dtype_lab = c('Cases', 'Cases (EpiNow2)')
  tmp <- ts_onset %>% rename(upper_adj = ts_up,
                             lower_adj = ts_low) %>%
    mutate(ts_med = (dtype %in% dtype_lab)*ts_med/lab_axis_adjust 
           + (!(dtype %in% dtype_lab))*ts_med,
           upper_adj = (dtype %in% dtype_lab)*upper_adj/lab_axis_adjust 
           + (!(dtype %in% dtype_lab))*upper_adj,
           lower_adj = (dtype %in% dtype_lab)*lower_adj/lab_axis_adjust 
           + (!(dtype %in% dtype_lab))*lower_adj)
  
  
  
  labheight <-min(y_max - y_max/30, max(tmp$upper_adj + tmp$upper_adj/10))
  blockheight <- labheight + 500
  segheight <- blockheight
  
  lvl5 <- data.frame(xmin = as.Date('2020-03-27') - 0.5, xmax = as.Date('2020-05-01'), ymin = -20, ymax = blockheight)
  lvl4 <- data.frame(xmin = as.Date('2020-05-01'), xmax = as.Date('2020-06-01'), ymin = -20, ymax = blockheight)
  lvl3 <- data.frame(xmin = as.Date('2020-06-01'), xmax = as.Date('2020-08-17'), ymin = -20, ymax = blockheight)
  lvl2 <- data.frame(xmin = as.Date('2020-08-17'), xmax = as.Date('2020-09-21'), ymin = -20, ymax = blockheight)
  lvl1 <- data.frame(xmin = as.Date('2020-09-21'), xmax = as.Date('2020-12-27'), ymin = -20, ymax = blockheight)
  lvl3p <-data.frame(xmin = as.Date('2020-12-28'), xmax = as.Date('2021-02-28'), ymin = -20, ymax = blockheight)
  lvl1ii <- data.frame(xmin = as.Date('2021-03-01'), xmax = as.Date('2021-05-30'), ymin = -20, ymax = blockheight)
  lvl2ii <- data.frame(xmin = as.Date('2021-05-31'), xmax = as.Date('2021-06-14'), ymin = -20, ymax = blockheight)
  # lvl3iii <- data.frame(xmin = as.Date('2021-06-15'), xmax = as.Date('2021-06-27'), ymin = -20, ymax = blockheight)
  lvl4ii <- data.frame(xmin = as.Date('2021-06-15'), xmax = as.Date('2021-07-25'), ymin = -20, ymax = blockheight)
  lvl3iv <- data.frame(xmin = as.Date('2021-07-26'), xmax = as.Date('2021-09-12'), ymin = -20, ymax = blockheight)
  lvl2iii <- data.frame(xmin = as.Date('2021-09-13'), xmax = as.Date('2021-09-30'), ymin = -20, ymax = blockheight)
  lvl1iii <- data.frame(xmin = as.Date('2021-10-01'), xmax = as.Date('2022-04-05'), ymin = -20, ymax = blockheight)
  nolockdown <- data.frame(xmin = as.Date('2022-04-05'), xmax = x_max, ymin = -20, ymax = blockheight)
  # print(max(tmp$upper_adj + 10))
  outplot <- (ggplot(tmp)
    + aes(x = onset_date))
  if(show_unrest){
    # si_mean_cumulative <- readRDS('./si_mean_cumulative_1-25.RDS')[1:18]
    # data.table::frollmean(1-si_mean_cumulative, n= 7) # to check when sliding window has average (within window) SI less than 5% lying inside disruption window 
    unrest_july21 <- data.frame(xmin = as.Date('2021-07-10'), xmax = as.Date('2021-07-19'), ymin = -1, ymax = y_max*2)
    # pud <- data.frame(date = as.Date('2021-07-21') + 0:17, residual_si <- (1 - si_mean_cumulative))
    outplot <- (outplot 
                +  geom_rect(data = unrest_july21, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = 'red')
                # +  geom_tile(data = pud, inherit.aes = FALSE, aes(x = date, y = 0, width = 1, height = 8, alpha = (0.1*residual_si)), fill = 'grey', show.legend = FALSE)
    )
  }
  if(show_Level_colors){ outplot <- (outplot # not updated currently
                                     + geom_rect(data = lvl5, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#99241B')
                                     + geom_rect(data = lvl4, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#E03127')
                                     + geom_rect(data = lvl3, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
                                     + geom_rect(data = lvl2, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f7e51e')
                                     + geom_rect(data = lvl1, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f9ff8f')
                                     + geom_rect(data = lvl3p, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#F7941E')
                                     + geom_rect(data = lvl1ii, inherit.aes = FALSE, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = '#f9ff8f')
  )}
    # + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = 'white')
    # + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3)
    # + geom_ribbon(aes(ymin = lower_adj, ymax = upper_adj), alpha = 0.3, fill = 'white')
  outplot <- (outplot
              + geom_ribbon(aes(ymin = lower_adj, ymax = upper_adj, fill = dtype), alpha = 0.5)
              + geom_segment(data = lvl5, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl4, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl3, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl2, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl1, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl3p, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl1ii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl2ii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              # + geom_segment(data = lvl3iii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl4ii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl3iv, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl2iii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = lvl1iii, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_segment(data = nolockdown, inherit.aes = FALSE, aes(x = xmin, xend = xmin, y = ymin, yend = ymax), color = '#333333')
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl5$xmax) + as.numeric(lvl5$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L5'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl4$xmax) + as.numeric(lvl4$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L4'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3$xmax) + as.numeric(lvl3$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2$xmax) + as.numeric(lvl2$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L2'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1$xmax) + as.numeric(lvl1$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L1'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3p$xmax) + as.numeric(lvl3p$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1ii$xmax) + as.numeric(lvl1ii$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L1'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2ii$xmax) + as.numeric(lvl2ii$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L2'), color = '#333333', fontface = 'bold', size = level_label_size)
              # + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3iii$xmax) + as.numeric(lvl3iii$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L3'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl4ii$xmax) + as.numeric(lvl4ii$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L4'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl3iv$xmax) + as.numeric(lvl3iv$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = ' L3'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl2iii$xmax) + as.numeric(lvl2iii$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L2'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(lvl1iii$xmax) + as.numeric(lvl1iii$xmin))/2, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/14))), aes(x = x, y = y, label = 'L1'), color = '#333333', fontface = 'bold', size = level_label_size)
              + geom_text(inherit.aes = FALSE, data = data.frame(x = as.Date((as.numeric(nolockdown$xmax) + as.numeric(nolockdown$xmin))/2 + 3, origin = as.Date('1970-01-01')), y = min(labheight, max(tmp$upper_adj + tmp$upper_adj/20))), aes(x = x, y = y, label = 'NSD no\nlonger\nin place'), color = '#333333', fontface = 'bold', size = level_label_size)
              # + geom_line(aes(y = med), color = '#797A7D')
              + geom_line(aes(y = ts_med, color = dtype, linetype = dtype), size = size_adj)
              + xlab(x_lab)
              + ylab(y_lab)
              + coord_cartesian(ylim = c(0, min(y_max, max(tmp$upper_adj + tmp$upper_adj/12))))
              + labs(color = legend_label, fill = legend_label,
                     linetype = legend_label)
              + xlim(x_min, x_max) 
              + scale_x_date(date_breaks = "months",
                             date_minor_breaks = "months",
                             date_labels = "%b-%y",
                             limits = c(x_min,x_max)
                             )
              + theme_minimal(base_size = 19)
              + scale_color_manual(values = pal)
              + scale_fill_manual(values = pal)
              + scale_linetype_manual(values =  (if(sep_ltypes) c(1,2,3,4,5) else c(1,1,1,1,1)))
              # + scale_color_manual(values = pal, breaks = c("lab", "admissions", "deaths"))
              # + scale_fill_manual(values = pal, breaks = c("lab", "admissions", "deaths"))
            )
  if(lab_axis_adjust != 1){
    outplot <- (outplot
                + scale_y_continuous(
                  # Features of the first axis
                  name = "Lab-confirmed admissions/deaths",
                  # Add a second axis and specify its features
                  sec.axis = sec_axis( trans=~.*lab_axis_adjust, name="Laboratory-confirmed cases")
                )
                )
  }
  if(ommit_legend){
    outplot <- outplot + theme(legend.position = 'none')}
  return(outplot)
}