library(tidyverse)
library(readxl)
library(haven)
library(EpiEstim) # Install from github: mrc-ide/EpiEstim
library(bbmle)
library(doParallel)

source('rtFunctions.R')
source('plottingFunctions.R')

select <- dplyr::select

.args <- if(interactive()){
  c("7",      #days
    "lab", # data type; placeholder for later integration with est_R_hosp
    "FALSE",# use uncertain SI? this will take a lot longer.
    "5",    # how many cores to use?
    "FALSE" # run period analyses? (estimate R per lockdown level)
    ) 
}else{
  commandArgs(trailingOnly =TRUE) 
}

R_est_window <- as.numeric(.args[[1]])
dtype <- .args[[2]]
use_uncertain_SI <- .args[[3]]
use_cores <- as.numeric(.args[[4]])
run_period_analyses  = .args[[5]] == "TRUE"

pubpriv_sector <- "ALL" # not available in public release, sorry. variable kept for minor convenience

n1_config = 25
n2_config = 25

max_onset_date <- as.Date('2022-04-28') # Update with new data
min_onset_date <- as.Date('2020-03-05') # earliest date of interest (to#as.Date('2020-03-07') # Keep

mcos <- c(as.Date('2022-04-28'), as.Date('2022-04-28'), as.Date('2022-04-28'))
names(mcos) <- c("PUBLIC", "PRIVATE", "ALL")

max_common_onset_date <-  mcos[[pubpriv_sector]]#as.Date('2022-02-18')

max_onset2report <- 21

trim_days_cases <- 3

si_mean_gamma <- 6.63
si_sd_gamma <- 3.28
si_mean_std_gamma <- 0.51
si_std_std_gamma <-  0.27
c(si_mean_gamma,si_sd_gamma,si_mean_std_gamma,si_std_std_gamma)

use_quant <- 95 # Quantile to use for plotting data
max_est_date <- max_onset_date - trim_days_cases # as.Date('2020-07-01')

set.seed(20200727) # Note that if running in parallel doesn't gaurantee reproducibility
registerDoParallel(use_cores) # Register cores

provs = c('za', 'gt', 'wc', 'ec', 'kzn',  'nw', 'mp',  'fs','lm', 'nc')
prov_names = c('ZA', 'GAUTENG', 'WESTERN CAPE', 'EASTERN CAPE',
               'KWAZULU-NATAL', 'NORTH WEST',
               'MPUMALANGA', 'FREE STATE', 'LIMPOPO' , 'NORTHERN CAPE')

names(provs) = prov_names

for(province_name in prov_names){
  prov_code = provs[province_name]
  ts_onset_tmp <- readRDS(sprintf('./data/All_lab_ts_%s.RDS', prov_code))
  rt_est_tmp <- wrap_rt_est(ts_onset_tmp, n1 = n1_config, n2 = n2_config, window_size = R_est_window, si_uncertain=use_uncertain_SI)
  saveRDS(rt_est_tmp, file = sprintf("./results/All_lab_rt_%s_%s.RDS", R_est_window, prov_code))
  
  if(run_period_analyses == TRUE){
    rt_per_tmp <- wrap_rt_est(ts_onset_tmp, use_periods = T, browse = FALSE, n1 = n1_config, n2 = n2_config, si_uncertain=use_uncertain_SI)
    saveRDS(rt_per_tmp, file = sprintf("./results/Period_analyses/All_lab_per_%s_%s", R_est_window, prov_code))
  }
}

# compute "alt" (probably better) ts and associated R estimates at national level by adding together (individually adjusted for right censoring) provincial time series

alt_ts_za <- data.frame(onset_date = as.Date(character()), rep = numeric(), adj_cnt = numeric(), cnt = numeric(), prov = character())

count_combo_provs = 0

for(province_code in c("ec", "wc", "nc", "nw", "lm", "fs", "kzn", "gt", "mp")){ #("EASTERN CAPE", "WESTERN CAPE", "NORTHERN CAPE", "NORTH WEST", "LIMPOPO", "FREE STATE", "KWAZULU-NATAL", "GAUTENG", "MPUMALANGA"){
  tmp <- readRDS(sprintf("./data/%s_lab_ts_%s.RDS", str_to_title(pubpriv_sector), province_code)) %>% mutate(prov = province_code)
  alt_ts_za <- rbind(alt_ts_za,tmp)
  count_combo_provs = count_combo_provs + 1
}

alt_ts_za <- (alt_ts_za
              %>% group_by(onset_date, rep)
              %>% summarise(adj_cnt = sum(adj_cnt), cnt = sum(cnt))
              %>% filter(onset_date <= max_common_onset_date)
)

za_rt_alt <- wrap_rt_est(alt_ts_za, n1 = n1_config, n2 = n2_config, window_size = R_est_window)

saveRDS(alt_ts_za, file = sprintf('./data/%s_lab_ts_za_alt.RDS', str_to_title(pubpriv_sector)))
saveRDS(za_rt_alt, file = sprintf('./results/%s_lab_rt_%s_za_alt.RDS', str_to_title(pubpriv_sector), R_est_window))
sprintf('./results/%s_lab_rt_%s_za_alt.RDS', str_to_title(pubpriv_sector), R_est_window)

if(count_combo_provs != 9){print("!!!!!!!!! combo ts incomplete")}

