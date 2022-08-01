# combine_results from analyses based on multiple data sources
# and plot them together

library(tidyverse)
library(cowplot)
library(RColorBrewer)
source("plottingFunctions.R")
source("postFunctions.R")


.args <- if(interactive()){
  c("7",      #r estimation window width, days
    "FALSE" # set to "include_period_table" to ... include the table of R estimates per lockdown level
    ) 
}else{
  commandArgs(trailingOnly =TRUE) 
}

R_est_window = .args[[1]]
make_period_table = .args[[2]] == TRUE
pubpriv_sector = "All"  # hardcoded as not available in public release (sorry! wish we could share everything.)


max_o_date <- as.Date("2022-04-30")
min_o_date <- as.Date("2020-03-05")

min_plot_date <- min_o_date 
max_plot_date <- max_o_date + 5

plot_pal <- RColorBrewer::brewer.pal(4, "Dark2")
plot_res <- 220 #dpi for saved figures

dd_c_za <- get_dd_combined(pubpriv_sector = pubpriv_sector,
                           prov = 'za',
                           min_o_date = min_o_date,
                           max_o_date = max_o_date,
                           window_size = R_est_window,
                           alt_dc = TRUE, #alt_dc is to account for differences in up-to-date-ness of province-level reporting
                           alt_lab = TRUE) #%>% filter(dtype != "Deaths")

rt_plot_overlay_za <- plot_rt_overlay(dd_c_za,
                                      x_min = min_o_date,
                                      x_max = max_plot_date,
                                      pal =  plot_pal[1:3],
                                      show_unrest = T)
ts_plot_overlay_za <- plot_ts_overlay(dd_c_za,
                                      x_min = min_o_date,
                                      x_max = max_plot_date,
                                      lab_axis_adjust = 10,
                                      ommit_legend = T,
                                      pal = plot_pal[1:3],
                                      show_unrest =T)
combo_plot_za <- plot_grid(rt_plot_overlay_za,
                           ts_plot_overlay_za,
                           ncol = 1,
                           align = 'v',
                           axis = 'lr')
combo_plot_za

ggsave( sprintf("./plots/%s_%s_za_overlay.jpeg" ,pubpriv_sector, R_est_window), 
        plot = combo_plot_za, 
        scale = 1,
        width = 24.2,
        height = 14.5,
        dpi = plot_res,
        units = "in",
        device = "jpeg")


dd_lastday <-  c("", "Cases", "Cases", "Admissions")
dd_lastday <- rbind(dd_lastday, c("", "25 April 2022", "21 April 2022", "21 April 2022"))#, "1 July 2021"))
precision_rt = 2 # for table of last-day estimates.


provs <- c("za","ec","fs","gt", "kzn", "lm","mp",  "nc", "nw",   "wc")
province_names <- c("National","Eastern Cape","Free State",
                    "Gauteng", "KwaZulu-Natal", "Limpopo",
                    "Mpumalanga", "Northern Cape", "North West",
                    "Western Cape")
names(province_names) = provs
prov_plot_list = list()
prov_ts_plot_list = list()
list_index = 1

for(province_code in provs){
  min_p_date <- min_plot_date # why oh why
  dd_c_tmp <- get_dd_combined(pubpriv_sector = pubpriv_sector,
                              prov = province_code,
                              min_o_date = min_o_date,
                              max_o_date = max_o_date,
                              window_size = R_est_window,
                              alt_dc = ifelse(province_code =='za', TRUE, FALSE)
                              )  #%>%
  # filter(dtype != "Deaths") #%>%
  
  dd_c_ylim1 <- dd_c_tmp %>% filter(onset_date > min_p_date) %>% filter(dtype == "Cases")
  dd_c_ylim2 <- dd_c_tmp %>% filter(onset_date > min_p_date) %>% filter(dtype == "Admissions")
  y_max =max(max(dd_c_ylim2$ts_up), max((dd_c_ylim1$ts_up)/9.5))
  
  
  rt_plot_overlay_tmp <- plot_rt_overlay(dd_c_tmp, # %>% filter(dtype != "Deaths"),
                                         x_min = min_p_date,
                                         x_max = max_plot_date,
                                         pal = plot_pal[1:3],
                                         ommit_deaths = (province_code %in% c('mp','nc')),
                                         show_unrest = province_code %in% c('za','kzn','gt'),
                                         R_max = 6.5
  )
  ts_plot_overlay_tmp <- plot_ts_overlay(dd_c_tmp,
                                         x_min = min_p_date,
                                         x_max = max_plot_date,
                                         y_max = y_max,
                                         lab_axis_adjust = 10, 
                                         ommit_legend = TRUE,
                                         pal = plot_pal[1:3],
                                         y_lab = "Lab-confirmed COVID-19 cases",
                                         show_unrest = province_code %in% c('za','kzn','gt'))
  
  
  combo_plot_tmp <-  plot_grid(rt_plot_overlay_tmp,
                               ts_plot_overlay_tmp,
                               ncol = 1,
                               align = 'v',
                               axis = 'lr')
  if(interactive()){plot(combo_plot_tmp)}
  
  dd_cases_match <- dd_c_tmp %>% filter(dtype == "Cases", !is.na(rt_med)) %>% filter(onset_date == as.Date('2022-04-21')) %>% tail(1)
  dd_cases_last <- dd_c_tmp %>% filter(dtype == "Cases", !is.na(rt_med))%>% filter(onset_date == as.Date('2022-04-25')) %>% tail(1)
  dd_admissions <- dd_c_tmp %>% filter(dtype == "Admissions", !is.na(rt_med)) %>% filter(onset_date == as.Date('2022-04-21')) %>% tail(1)
  last_est_cases <- sprintf("%1.2f (%1.2f,%1.2f)",
                            round(dd_cases_last[1,'rt_med'], precision_rt),
                            round(dd_cases_last[1,'rt_low'], precision_rt),
                            round(dd_cases_last[1,'rt_up'], precision_rt)
                            
  )
  match_est_cases <- sprintf("%1.2f (%1.2f,%1.2f)",
                             round(dd_cases_match[1,'rt_med'], precision_rt),
                             round(dd_cases_match[1,'rt_low'], precision_rt),
                             round(dd_cases_match[1,'rt_up'], precision_rt)#,
  )
  last_est_admissions <- sprintf("%1.2f (%1.2f,%1.2f)",
                                 round(dd_admissions[1,'rt_med'], precision_rt),
                                 round(dd_admissions[1,'rt_low'], precision_rt),
                                 round(dd_admissions[1,'rt_up'], precision_rt))
  dd_lastday <- rbind(dd_lastday, 
                      c(province_names[province_code],
                        last_est_cases, 
                        match_est_cases,
                        last_est_admissions)) 
  
  ggsave( sprintf("./plots/%s_%s_%s_overlay.jpeg",
                  pubpriv_sector,
                  R_est_window,
                  ifelse(province_code == 'za', 'za_short',province_code)), 
          plot = combo_plot_tmp, 
          scale = 1,
          width = 24.2,
          height = 14.5,
          dpi = plot_res,
          units = "in",
          device = "jpeg")
  if(province_code!="za"){
    # if (province_code == "wc"){
    rt_plot_overlay_tmp <- rt_plot_overlay_tmp +
      theme(plot.margin = unit(c(2, 1, 1, 1), "cm"))
    # }
    prov_plot_list[[list_index]] = rt_plot_overlay_tmp
    prov_ts_plot_list[[list_index]] = ts_plot_overlay_tmp
    list_index = list_index + 1
  }
  
}

combo_rt_plot_panel = plot_grid(plotlist = prov_plot_list,
                                ncol = 1,
                                align = 'vh',
                                axis = 'lb',
                                labels = province_names[provs[2:10]],
                                label_size = 30,
                                vjust = 2.2
)


ggsave( sprintf("./plots/%s_combo_rt_panel_%s.jpeg",
                pubpriv_sector,
                R_est_window),
        plot = combo_rt_plot_panel, 
        scale = 1,
        width = 38.2,
        height = 45,
        dpi = plot_res,
        units = "in",
        device = "jpeg",
        limitsize = FALSE)

combo_ts_plot_panel = plot_grid(plotlist = prov_ts_plot_list,
                                ncol = 1,
                                align = 'vh',
                                axis = 'lb',
                                labels = province_names[provs[2:10]],
                                label_size = 30,
                                vjust = 1
)


ggsave( sprintf("./plots/%s_combo_ts_panel_%s.jpeg",
                pubpriv_sector,
                R_est_window),
        plot = combo_ts_plot_panel, 
        scale = 1,
        width = 34.2,
        height = 50,
        dpi = plot_res,
        units = "in",
        device = "jpeg",
        limitsize = FALSE)


write.table(dd_lastday,
            file = sprintf("./results/tables/tbl_POINT_%s.txt", str_to_title(pubpriv_sector)),
            sep = ";",
            row.names = F,
            col.names = F,
            quote = F)


##make tables showing results of period analyses
# read in data for each analysis, and keep just the important bits!
# min_per_date <- as.Date('2020-09-20')


if(make_period_table == TRUE){
  precision_rt <- 2
  
  for(province_code in provs){
    lab_per <- prep_per_for_tbl(file = sprintf("./results/Period_analyses/%s_lab_per_%s",
                                               str_to_title(pubpriv_sector), province_code),
                                label = "Cases")
    admit_per <- prep_per_for_tbl(file = sprintf("./results/Period_analyses/%s_admit_per_%s.RDS",
                                                 str_to_title(pubpriv_sector), province_code),
                                  label = "Admissions")
    deaths_per <- prep_per_for_tbl(file = sprintf("./results/Period_analyses/%s_deaths_per_%s.RDS",
                                                  str_to_title(pubpriv_sector), province_code),
                                   label = "Deaths")
    prov_tbl_tmp <- rbind(lab_per, admit_per, deaths_per)
    prov_tbl_tmp <- cbind(cbind(c("Cases","Admissions","Deaths")),
                          prov_tbl_tmp)
    colnames(prov_tbl_tmp) <- c(" ","Pre-lockdown", "L5","L4","L3","L2", "L1", "L3 (12/20)", "L1 (02/21)", "L2 (05/21)", "L4 (06/21)", "L3 (07/21)", "L2 (09/21)", "L1 (10/21)", "Post-NSOD" )#, "Level 1")
    write.table(prov_tbl_tmp,
                file = sprintf("./results/tables/tbl_%s_%s.txt", str_to_title(pubpriv_sector), province_code),
                sep = ";",
                row.names = F,
                quote = F)
  }
}