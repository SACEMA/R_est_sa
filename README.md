This code reproduces the combined public-private sector figures from the manuscript https://www.medrxiv.org/content/10.1101/2022.07.22.22277932v1.full.

The easiest way to use this code is via the GNU make build tool; the code can also be run directly from R.

# Running this code:

### Download data

- Time series data for running this code can be downloaded from: https://zenodo.org/record/6948468.

- Filenames include a province code. Each data file includes 25 imputated timeseries (see manuscript for description of imputation procedure).

- Data files should be put into a folder named 'data'.

- the R code provided runs out the box using the .RDS data files; .csv files are also provided

### Using make:
- run 'make setup' to set up directory structure
- run 'make' to generate R estimates and plots with 7-day sliding windows
- run 'make plots_<window_size>' (e.g. 'plots_14') for generate R estimates and plots with your desired length of sliding window.
- run 'make all_plots' to generate R estimates and plots with 7, 14, and 21-day sliding windows.
- set variable "UNCERTAINSI" to TRUE if you want to reproduce figures in the manuscript (using a less-certain serial interval), or set to FALSE for faster runtime.
- set variable "PERIODANALYSES" to TRUE to generate R estimates per lockdown level, or FALSE to skip this.
- other variables (e.g. plot resolution or level of sampling for uncertain serial interval 

### Using R without make:
- Set up repository structure:
	- data/
	- results/tables
	- results/Period_analyses
	- plots/

- Variables should be edited inside the "if(interactive()){c(<variable_1>, <variable_2>, etc.)}" portion of the ".args <-" assignment block. See comments and/or code just below the assignment block for which variable is which.

- To produce R estimates:
	- run "est_R_hosp.R" with the second argument set to "admit" (for estimates based on hospital admissions)
	- run "est_R_hosp.R" with the second argument set to "deaths" (for estimates based on hospital-associated deaths)
	- run "est_R_lab.R" (for estimates based  on rT-PCR-confirmed cases)
- To produce plots:
	- first generate R estimates with the desired window size
	- run "plot_results.R" with the first variable set to the desired window size
	