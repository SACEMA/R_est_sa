rt_est <- function(dat
                   , si_uncertain = use_uncertain_SI
                   , si_mean_gam = si_mean_gamma
                   , si_sd_gam = si_sd_gamma
                   , si_mean_sd_gam = si_mean_std_gamma
                   , si_sd_sd_gam = si_std_std_gamma
                   , use_periods = FALSE
                   , max_date = max_est_date
                   , browse = FALSE
                   , n1 = 500
                   , n2 = 50
                   , window_size = 7){
  
  T = nrow(dat)
  t_start = seq(2,T - (window_size + 1))
  t_end = t_start + (window_size - 1)
  if(si_uncertain){
    config <- make_config(list(mean_si = si_mean_gam,
                               std_si = si_sd_gam,
                               std_mean_si = si_mean_sd_gam,
                               min_mean_si = si_mean_gam - 2 * si_mean_sd_gam,
                               max_mean_si = si_mean_gam + 2 * si_mean_sd_gam,
                               std_std_si = si_sd_sd_gam,
                               min_std_si = si_sd_gam - 2 * si_sd_sd_gam,
                               max_std_si = si_sd_gam + 2 * si_sd_sd_gam,
                               n1 = n1,
                               n2 = n2,
                               t_start = t_start,
                               t_end = t_end))
  }else{
    config <- make_config(list(mean_si = si_mean_gam,
                               std_si = si_sd_gam,
                               t_start = t_start,
                               t_end = t_end))
  }

  if('rep' %in% names(dat)){
    n_rep <- unique(dat$rep)
    if(length(n_rep) > 1){
      error('Multiple replicates included in data frame.')
    }
  }
  dd_estimate <- (dat 
                  %>% rename(I = adj_cnt
                             , dates = onset_date)
                  %>% select(dates, I))
  if(use_periods){
      config$t_start <- as.vector(c(2
                                    # , which(dat$onset_date == as.Date('2020-03-19'))
                                    , which(dat$onset_date == as.Date('2020-03-27'))
                                    , which(dat$onset_date == as.Date('2020-05-01'))
                                    , which(dat$onset_date == as.Date('2020-06-01'))
                                    , which(dat$onset_date == as.Date('2020-08-18'))
                                    , which(dat$onset_date == as.Date('2020-09-21'))
                                    , which(dat$onset_date == as.Date('2020-12-28'))
                                    , which(dat$onset_date == as.Date('2021-03-01'))
                                    , which(dat$onset_date == as.Date('2021-05-31'))
                                    , which(dat$onset_date == as.Date('2021-06-16'))
                                    # , which(dat$onset_date == as.Date('2021-06-28'))
                                    , which(dat$onset_date == as.Date('2021-07-26'))
                                    , which(dat$onset_date == as.Date('2021-09-13'))
                                    , which(dat$onset_date == as.Date('2021-10-01'))
                                    , which(dat$onset_date == as.Date('2022-04-05'))
      ))
      config$t_end <- as.vector(c( #which(dat$onset_date == as.Date('2020-03-18'))
                                  which(dat$onset_date == as.Date('2020-03-26'))
                                  , which(dat$onset_date == as.Date('2020-04-30'))
                                  , which(dat$onset_date == as.Date('2020-05-31'))
                                  , which(dat$onset_date == as.Date('2020-08-17'))
                                  , which(dat$onset_date == as.Date('2020-09-20'))
                                  , which(dat$onset_date == as.Date('2020-12-27'))
                                  , which(dat$onset_date == as.Date('2021-02-28'))
                                  , which(dat$onset_date == as.Date('2021-05-30'))
                                  , which(dat$onset_date == as.Date('2021-06-15'))
                                  # , which(dat$onset_date == as.Date('2021-06-27'))
                                  , which(dat$onset_date == as.Date('2021-07-25'))
                                  , which(dat$onset_date == as.Date('2021-09-12'))
                                  , which(dat$onset_date == as.Date('2021-09-30'))
                                  , which(dat$onset_date == as.Date('2022-04-04'))
                                  , which(dat$onset_date == max(dat$onset_date))
      ))
   }



  if(browse) browser()
  
  if(si_uncertain){
    res_tmp <- estimate_R(dd_estimate, config = config, method = "uncertain_si")
  }else{
    res_tmp <- estimate_R(dd_estimate, config = config, method = "parametric_si")
  }
  rt <- res_tmp$R
  saveRDS(res_tmp, file = "example_output_epiestim.RDS")
  if(use_periods){
    rt$onset_date <- as.Date(c('2020-03-26', '2020-04-30', '2020-05-31', '2020-08-16', '2020-09-20', '2020-12-27', '2021-02-28' , '2021-05-30', '2021-06-15', '2021-07-25', '2021-09-12', '2021-09-30', '2022-04-04', as.character(max_date)))
  }else{
  rt$onset_date <- dat$onset_date[(window_size + 3):nrow(dat)]# jb: I think this is based on the window size used for esimate_R. Currently one week (7 days), so first prediction is on the 8th day.
  }
  rt <- (rt 
         %>% rename(med = `Median(R)`, lwr = `Quantile.0.025(R)`
                    , upr = `Quantile.0.975(R)`) 
         %>% select(onset_date, med, lwr, upr)
         %>% filter(onset_date <= max_date)
  )
  if('rep' %in% names(dat)){
    rt$rep <- n_rep
  }
  # print(Sys.time())
  return(rt)
}

wrap_rt_est <- function(all_ts_onset, ...){
  # print(sprintf("Starting wrap_rt_est at: %s", Sys.time()))
  out_tmp <- foreach(ii = unique(all_ts_onset$rep),
                     .packages = c('tidyverse','EpiEstim'),
                     .export = c('rt_est', 'use_uncertain_SI',
                                 'si_mean_gamma', 'si_sd_gamma',
                                 'max_est_date', 'si_mean_gamma',
                                 'si_sd_gamma','si_mean_std_gamma',
                                 'si_std_std_gamma')) %dopar% {
      	  tmp <- rt_est(all_ts_onset %>% filter(rep == ii), ...)
  }
  # print(sprintf("Combining wrap_rt_est(imates) at: %s", Sys.time()))
  out <- NULL
  for(ii in unique(all_ts_onset$rep)){
    out <- bind_rows(out, out_tmp[[as.numeric(ii)]])
  }
  # could we replace this with:
  # do.call(rbind, out_tmp[[seq_lenmax(as.numeric(unique(all_ts_onset$rep)))]]) ?
  return(out)
}

rt_specific_date <- function(rt, date){
  tmp <- rt %>% group_by(onset_date) %>% summarize(median = median(med)
                                            , upper = median(upr)
                                            , lower = median(lwr)) %>%
    filter(onset_date == date)
  return(tmp)
}
