# cl_spat_nonstat
This repository contains code to conduct the analysis and visualisation for the paper "Exploring spatial non-stationarity of child labour and its related factors: A multiscale geographically weighted regression study of India" published in Applied Geography.

## Code
The code files in this repository are as follows:
- `read_india_census_files.R`: functions to read India Census table Excel files and wrangle into a useful format
- `data_preprocessing.R`: code to create a single dataset used for modelling
- `linreg_moransi.R`: code to create initial linear regression model, test spatial autocorrelation of residuals and visualise local spatial autocorrelation
- `gwr_mgwr_models.py`: code to run the GWR and MGWR models in the paper, saving outputs as CSVs in an `ouput` folder
- `gwr_mgwr_results_vis.R`: code to produce visualisations based on results from `gwr_mgwr_models.py`
- `akaike_weights.R`: code to calculate Akaike weights of different bandwidths of MGWR models and resulting 95% confidence intervals

## Data
Data used in this project was from the India Census 2011 and downloaded from https://censusindia.gov.in/nada/index.php/catalog.

The folders containing all state files for each table are:
- `/data/district_age_ed_inst_ec_activity` - "C-12: Population age 5-19 attending educational institution by economic activity status and sex (total)"
- `/data/district_age_education_level` - "C-08: Educational level by age and sex for population age 7 and above (total)"
- `/data/district_age_sex_hh_head` - "HH-06: Households by marital status, sex and age of the head of household (total)"
- `/data/district_norm_hh_size` - "HH-01: Normal households by household size (total)"
- `/data/district_sc_pop` - "A-10 Appendix: District wise scheduled caste population (Appendix)"
- `/data/district_st_pop` - "A-11 Appendix: District wise scheduled tribe population (Appendix)"
- `/data/district_religion_sex` - "C-15: Religious community by age group and sex"
- `/data/district_year_births` - "F-09: Number of women and currently married women by present age, number of births last year by sex and birth order (total)"

Two other reference tables were downloaded and saved in the /data folder:
- `data/A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx` - "A-01: Number of villages, towns, households, population and area (India, states/UTs, districts and Sub-districts) - 2011"
- `data/PC11_TV_DIR.xlsx` - "Census 2011 - Location Code Directory"
