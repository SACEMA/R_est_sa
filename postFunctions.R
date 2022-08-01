#functions for organising results, for outputs to tables, plots, etc.


get_dd_combined <- function(pubpriv_sector,
                            prov,
                            min_o_date,
                            max_o_date,
                            max_lab_onset = max_o_date,
                            max_admit_onset = max_o_date,
                            max_deaths_onset = max_o_date,
                            window_size = R_est_window,
                            alt_dc = FALSE,
                            alt_lab = FALSE,
                            en2 = NULL,
                            shift_epiestim = FALSE){
  if(alt_dc == FALSE){
    filename_admit_ts <- sprintf("./data/%s_admit_ts_%s.RDS", pubpriv_sector, prov)
    filename_admit_rt <- sprintf("./results/%s_admit_rt_%s_%s.RDS", pubpriv_sector,window_size, prov)
    filename_deaths_ts <- sprintf("./data/%s_deaths_ts_%s.RDS", pubpriv_sector, prov)
    filename_deaths_rt <- sprintf("./results/%s_deaths_rt_%s_%s.RDS", pubpriv_sector,window_size, prov)
  }else{
    filename_admit_ts <- sprintf("./data/%s_admit_ts_%s_alt.RDS", pubpriv_sector, prov)
    filename_admit_rt <- sprintf("./results/%s_admit_rt_%s_%s_alt.RDS", pubpriv_sector,window_size, prov)
    filename_deaths_ts <- sprintf("./data/%s_deaths_ts_%s_alt.RDS", pubpriv_sector, prov)
    filename_deaths_rt <- sprintf("./results/%s_deaths_rt_%s_%s_alt.RDS", pubpriv_sector,window_size, prov)
  }
  if(alt_lab == FALSE){
  filename_lab_ts <-  sprintf("./data/%s_lab_ts_%s.RDS", pubpriv_sector, prov)
  filename_lab_rt <-  sprintf("./results/%s_lab_rt_%s_%s.RDS", pubpriv_sector, window_size, prov)  
  }else{
    filename_lab_ts <- sprintf("./data/%s_lab_ts_%s_alt.RDS", pubpriv_sector, prov)
    filename_lab_rt <- sprintf("./results/%s_lab_rt_%s_%s_alt.RDS", pubpriv_sector, window_size, prov)   
  }
  
  
  ts_lab <- readRDS(filename_lab_ts) %>%
    summarise_ts(x_min = min_o_date, x_max=max_lab_onset)
  rt_lab <- readRDS(filename_lab_rt) %>%
	  filter(onset_date <= max_lab_onset, onset_date >= min_o_date) %>%
    group_by(onset_date) %>%
    summarize(rt_med = median(med)
              , rt_up = median(upr)
              , rt_low = median(lwr))  
  dd_c_lab <- merge(ts_lab,rt_lab, all = TRUE, by = "onset_date") %>%
    mutate(dtype = as.factor("Cases"))
  
  ts_admit <- readRDS(filename_admit_ts) %>%
    summarise_ts(x_min = min_o_date, x_max=max_admit_onset)
  rt_admit <- readRDS(filename_admit_rt) %>% 
	  filter(onset_date <= max_admit_onset, onset_date >= min_o_date) %>%
    group_by(onset_date) %>%
    summarize(rt_med = median(med)
              , rt_up = median(upr)
              , rt_low = median(lwr))
  dd_c_admit <- merge(ts_admit,rt_admit, all = TRUE, by = "onset_date") %>%
    mutate(dtype = as.factor("Admissions"))
  
  ts_deaths <- readRDS(filename_deaths_ts) %>%
    summarise_ts(x_min = min_o_date, x_max = max_deaths_onset)
  rt_deaths <- readRDS(filename_deaths_rt) %>%
	  filter(onset_date <= max_deaths_onset, onset_date >= min_o_date) %>%
    group_by(onset_date) %>%
    summarize(rt_med = median(med)
              , rt_up = median(upr)
              , rt_low = median(lwr))  
  dd_c_deaths <- merge(ts_deaths ,rt_deaths, all = TRUE, by = "onset_date") %>%
    mutate(dtype = as.factor("Deaths"))
  
  dd_alltypes <- rbind(dd_c_deaths, dd_c_admit, dd_c_lab )

  return(dd_alltypes)
}


prep_per_for_tbl <- function(filename,
                             label = "Cases",
                             precision_rt = 2,
                             min_per_date = as.Date('2020-03-05')){
  dd_tmp <- readRDS(filename) %>%
    group_by(onset_date) %>%
    summarize(med_med = median(med), med_lwr = median(lwr), med_upr = median(upr)) %>%
    filter(onset_date >= min_per_date) %>%
    mutate(lab_tmp = sprintf("%1.2f (%1.2f,%1.2f)",
                             round(med_med, precision_rt),
                             round(med_lwr, precision_rt),
                             round(med_upr, precision_rt))) %>%
    select(lab_tmp)
  names(dd_tmp)[1] = label
  return( dd_tmp %>% t() )
}

summarise_ts <- function(ts, x_min, x_max, qq = 95) {
  tmp <- (ts 
          %>% group_by(onset_date) 
          %>% summarize(med = median(cnt)
                        , med_adj = median(adj_cnt)
                        , upper = quantile(cnt, 1-(100-qq)/200)
                        , lower = quantile(cnt, (100-qq)/200)
                        , upper_adj = quantile(adj_cnt, 1-(100-qq)/200)
                        , lower_adj = quantile(adj_cnt, (100-qq)/200))
          %>% filter(onset_date >= x_min, onset_date <= x_max)
          %>% rename(ts_med = med_adj,
                     ts_low = lower_adj,
                     ts_up = upper_adj)
          %>% select(onset_date, ts_med, ts_low, ts_up)
  )
  return(tmp)
}



max_est_period <- function(dd_c_period, data_endpoint, rounding = 2){
  sprintf("%s (%s, %s)",
          round(dd_c_period %>% filter(dtype==data_endpoint) %>% select(rt_med) %>% max(na.rm = T),rounding),
          round(dd_c_period %>% filter(dtype==data_endpoint) %>% select(rt_low) %>% max(na.rm = T),rounding),
          round(dd_c_period %>% filter(dtype==data_endpoint) %>% select(rt_up) %>% max(na.rm = T),rounding))
}

