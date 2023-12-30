## Did TV ads funded by fossil fuel industry defeat the Washington carbon tax?

This repository contains the data and code for the article:

> [Karceski, SM, Nives Dolšak, Aseem Prakash, and Travis N. Ridout (2020) Did TV ads funded by fossil fuel industry defeat the Washington carbon tax?. *Climatic Change*, 158:301–307 https://doi.org/10.1007/s10584-019-02626-z](https://link.springer.com/article/10.1007/s10584-019-02626-z) 

### Structure 

The repository includes the data and scripts necessary to replicate the analysis in the paper. The repository is organized as follows: 

- `data` contains all the data necessary to replicate the analysis presented in the paper. 
 - `interp_results_acs_2018.csv` contains the areal weighted interpolated American Community Survey data 
- `syntax` contains scripts used in cleaning and manipulating the data, and those that can be used to replicate the analysis in the paper. 
  - `acs_prep.R` contains code to download and clean the ACS 5-year data for the analysis. 
  - `analysis_figures.R` contains the script 
  - `data_prep.R` contains a script that will load the data used in the `analysis_figures.R` script (and replicate the analysis and figures from the paper). 
  - `energy_data_prep.R` contains 
  - `yale_pccc_prep.R` contains code to load and merge the public opinion data from YPCCC for 2016 and 2018. 
- `tables_figures` contains the final versions of each table and figure included in the paper and in the appendix. 


