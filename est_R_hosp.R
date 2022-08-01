library(tidyverse)
library(readxl)
library(haven)
library(EpiEstim) # Install from github: mrc-ide/EpiEstim
library(bbmle)
library(doParallel)

source('rtFunctions.R')
source('plottingFunctions.R')

.args <- if(interactive()){ # if running from inside R (rather than command line) edit these four parameters here
	c("7",#days
	"deaths", # admit for all admissions / deaths for only hospital-associated deaths
	"FALSE",  # use uncertain SI? this will take a lot longer.
	"5",       # how many cores do you want to allocate to this task? 
	"FALSE" #  run period analyses? (estimate R per lockdown level)
	)
}else{
	commandArgs(trailingOnly =TRUE) 
	}

R_est_window <- as.numeric(.args[[1]])
analyse_only_deaths <- .args[[2]]
use_uncertain_SI <- .args[[3]]
use_cores <- as.numeric(.args[[4]])
run_period_analyses = (.args[[5]] == "TRUE")

pubpriv_sector <- "ALL" # not available in public release, sorry. variable kept for minor convenience

max_onset_date <- as.Date('2022-04-30') # Update with new data
min_onset_date <- as.Date('2020-03-05') # Keep

trim_days_admissions <- 7
trim_days_deaths <- 7

max_est_date <- max_onset_date - trim_days_admissions

# set max common (between-province) onset dates per sector, for constructing national time series
mcos <-  c(as.Date('2022-04-28'), # public
           as.Date('2022-04-28'), #private 
           as.Date('2022-04-28')) -  #all
  rep(trim_days_admissions, 3)
mcos_d <- c(as.Date('2022-04-20'), #public
            as.Date('2022-04-20'), #private
            as.Date('2022-04-20')) - #all
  rep(trim_days_deaths, 3)
  
names(mcos) <- c("Public", "Private", "ALL")
names(mcos_d) <- c("Public", "Private", "ALL")


max_combo_onset_date_admit <-  mcos[pubpriv_sector]# as.Date('2022-02-18') - trim_days_admissions
max_combo_onset_date_deaths <- mcos_d[pubpriv_sector] #as.Date('2022-02-09') - trim_days_deaths

n1_config = 25
n2_config = 25

#if(analyse_only_deaths== "admit"){
#  plot_ylab <- "Lab-confirmed COVID-19 deaths"
file_suffix = sprintf("%s_%s", str_to_title(pubpriv_sector), analyse_only_deaths)
#}else{
#  plot_ylab <- "Lab-confirmed COVID-19 hospitalisations"
#}

file_suffix = sprintf("%s_%s", str_to_title(pubpriv_sector), analyse_only_deaths)

max_onset2admit <- 21 
max_onset2death <- 40

si_mean_gamma <- 6.63
si_sd_gamma <- 3.28
si_mean_std_gamma <- 0.51
si_std_std_gamma <-  0.27


set.seed(20200727) # Note that if running in parallel doesn't gaurantee reproducibility
registerDoParallel(use_cores) # Register cores

provs = c('za', 'gt', 'wc', 'ec', 'kzn',  'nw', 'mp',  'fs','lm', 'nc')
prov_names = c('ZA', 'Gauteng', 'Western Cape', 'Eastern Cape',
               'KwaZulu-Natal', 'North West',
               'Mpumalanga', 'Free State', 'Limpopo' , 'Northern Cape')

names(provs) = prov_names

for(province_name in prov_names){
  prov_code <- provs[province_name]
	ts_onset_tmp <- readRDS(sprintf("./data/All_%s_ts_%s.RDS", analyse_only_deaths, prov_code )) #adjust_rcens_imputed_ts() %>% filter(onset_date <= max(onset_date) - ifelse(analyse_only_deaths, trim_days_deaths, trim_days_admissions))
  rt_est_tmp <- wrap_rt_est(ts_onset_tmp, n1 = n1_config, n2 = n2_config, window_size = R_est_window, si_uncertain = use_uncertain_SI)
  saveRDS(rt_est_tmp, file = sprintf("./results/%s_rt_%s_%s.RDS", file_suffix, R_est_window, prov_code))
  readRDS(file = sprintf("./data/%s_ts_%s.RDS", file_suffix, prov_code))
}


alt_ts_za <- data.frame(onset_date = as.Date(character()), rep = numeric(), adj_cnt = numeric(), cnt = numeric(), prov = character())
#names(alt_ts_za) <- c("onset_date", "rep", "adj_cnt", "prov")
count_combo_provs = 0
for (province_code in provs){
	if(province_code == "za") {next}
	else{
		tmp <-readRDS(sprintf("./data/%s_ts_%s.RDS", file_suffix, province_code)) %>% mutate(prov = province_code)
		alt_ts_za <- rbind(alt_ts_za, tmp)
		count_combo_provs = count_combo_provs + 1
	}
}
if(count_combo_provs != 9){print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! combo ts incomplete")}

alt_ts_za <- alt_ts_za %>%
  group_by(onset_date, rep) %>% 
  summarise(adj_cnt = sum(adj_cnt), cnt = sum(cnt)) %>%
  filter(if(analyse_only_deaths == "deaths") onset_date <= max_combo_onset_date_deaths else 
                onset_date <= max_combo_onset_date_admit)
             

za_rt_alt <- wrap_rt_est(alt_ts_za, n1 = n1_config, n2 = n2_config, window_size = R_est_window)

saveRDS(alt_ts_za, file = sprintf('./data/%s_ts_za_alt.RDS', file_suffix))
saveRDS(za_rt_alt, file = sprintf('./results/%s_rt_%s_za_alt.RDS',  file_suffix, R_est_window))

# PERIOD ANALYSIS
if(run_period_analyses == TRUE){
  for(province_code in provs){
  	print(province_code)
  	rt_per_tmp <- wrap_rt_est(readRDS(sprintf("./data/%s_ts_%s.RDS", file_suffix, ifelse(province_code == "za", "za_alt", province_code))), use_periods = T, browse = FALSE, n1 = n1_config, n2 = n2_config, window_size = 7)
         	saveRDS(rt_per_tmp, file = sprintf("./results/Period_analyses/%s_per_%s.RDS", file_suffix, province_code)) 
  }
}
