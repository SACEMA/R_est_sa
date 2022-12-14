UNCERTAINSI := FALSE
PERIODANALYSES := FALSE
NCORES := 5

default: plots_7

plots_%: Rt_hosp_% Rt_lab_% Rt_mort_%
	Rscript plot_results.R $* ${PERIODANALYSES}

Rt_hosp_%: 
	Rscript est_R_hosp.R $* admit ${UNCERTAINSI} ${NCORES} ${PERIODANALYSES}

Rt_mort_%: 
	Rscript est_R_hosp.R $* deaths ${UNCERTAINSI} ${NCORES} ${PERIODANALYSES}

Rt_lab_%:
	Rscript est_R_lab.R $* lab ${UNCERTAINSI} ${NCORES} ${PERIODANALYSES}

all_plots: plots_7 plots_14 plots_21

setup:
	mkdir -p plots;
	mkdir -p data;
	mkdir -p results; 
	mkdir -p results/tables;
	mkdir -p results/Period_analyses