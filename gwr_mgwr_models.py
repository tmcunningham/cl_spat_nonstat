import geopandas as gp
import numpy as np
import pandas as pd
import itertools
import random
import pickle
import copy
from mgwr.gwr import GWR
from mgwr.gwr import MGWR
from mgwr.sel_bw import Sel_BW

random.seed(1234)

# Read projected district boundaries
district_data = gp.read_file("outputs/scaled_data_proj.gpkg")

# Prepare data for modelling
indep_vars = ["u14_perc_att_ed", "o18_perc_lit", "hh_mean_size","sc_perc", 
              "st_perc", "muslim_perc", "perc_urban_pop", "wealth_index",
              "hh_perc_f_head", "u14_perc_male", "total_fertility_rate"]
var_list = indep_vars.copy()
var_list.insert(0, "intercept")
X = np.array(district_data[indep_vars].values)
y = np.array(np.log(district_data['u14_perc_working'])).reshape(-1, 1)

# Get coords of centroids
u = district_data.centroid.map(lambda p: p.x)
v = district_data.centroid.map(lambda p: p.y)
centroid_coords = list(zip(u, v))

# GWR model using interval selector of width 1
gwr_bw_interval1_selector = Sel_BW(centroid_coords, y, X, multi=False, fixed = False, kernel = "bisquare")
gwr_bw_interval1 = gwr_bw_interval1_selector.search(criterion = "AICc", search_method = "interval",
                                                    interval = 1,
                                                    bw_min = 35, bw_max=635,
                                                    max_iter=2000)
interval1_gwr_model = GWR(centroid_coords, y, X, bw = gwr_bw_interval1,  fixed = False, kernel = "bisquare", hat_matrix = True)
interval1_gwr_results = interval1_gwr_model.fit()

# Create dataframe of GWR coefficients
interval1_gwr_coef_df = pd.DataFrame(interval1_gwr_results.params)
coef_names = map(lambda x: '{}_coef.gwr'.format(x), var_list)
interval1_gwr_coef_df.columns = coef_names

# Create DF of filtered t-values
interval1_gwr_tval_filt_df = pd.DataFrame(interval1_gwr_results.filter_tvals())
tval_filt_names = map(lambda x: '{}_filttval.gwr'.format(x), var_list)
interval1_gwr_tval_filt_df.columns = tval_filt_names

# Create DF of all t-values
interval1_gwr_tval_df = pd.DataFrame(interval1_gwr_results.tvalues)
tval_names = map(lambda x: '{}_tval.gwr'.format(x), var_list)
interval1_gwr_tval_df.columns = tval_names

# Write GWR DFs to CSVs
interval1_gwr_coef_df.to_csv("outputs/interval1_gwr_coefs.csv")
interval1_gwr_tval_filt_df.to_csv("outputs/interval1_gwr_tval_filt.csv")
interval1_gwr_tval_df.to_csv("outputs/interval1_gwr_tvals.csv")

# MGWR model with interval 1 selector
bw_selector = Sel_BW(centroid_coords, y, X,multi=True, fixed = False, kernel = "bisquare")
bw_selector.search(criterion = "AICc", search_method = "interval",
                   interval = 1,
                   multi_bw_min=[35], multi_bw_max=[635],
                   init_multi = gwr_model.bw, 
                   max_iter_multi = 2000)
interval1_mgwr_model = MGWR(centroid_coords, y, X, selector = bw_interval1_selector,  fixed = False, kernel = "bisquare", hat_matrix = True)
interval1_mgwr_results = interval1_mgwr_model.fit()

# Create dataframe of MGWR coefficients
interval1_mgwr_coef_df = pd.DataFrame(interval1_mgwr_results.params)
coef_names = map(lambda x: '{}_coef.mgwr'.format(x), var_list)
interval1_mgwr_coef_df.columns = coef_names

# Create DF of filtered t-values
interval1_mgwr_tval_filt_df = pd.DataFrame(interval1_mgwr_results.filter_tvals())
tval_filt_names = map(lambda x: '{}_filttval.mgwr'.format(x), var_list)
interval1_mgwr_tval_filt_df.columns = tval_filt_names

# Create DF of all t-values
interval1_mgwr_tval_df = pd.DataFrame(interval1_mgwr_results.tvalues)
tval_names = map(lambda x: '{}_tval.mgwr'.format(x), var_list)
interval1_mgwr_tval_df.columns = tval_names

# Create DF of 95% confidence intervals for MGWR BWs (based on Akaike weights)
interval1_mgwr_95ci = interval1_mgwr_results.get_bws_intervals(bw_interval1_selector)
interval1_mgwr_bw_df = pd.DataFrame({"var":var_list,
                                     "bws.mgwr": interval1_mgwr_model.bws,
                                     "ci95_bws.mgwr": interval1_mgwr_95ci,
                                     "adj_alpha": 0.05/interval1_mgwr_results.ENP_j,
                                     "var_crit_tval": interval1_mgwr_results.critical_tval()})

# Write MGWR dataframes to CSVs
interval1_mgwr_coef_df.to_csv("outputs/interval1_mgwr_coefs.csv")
interval1_mgwr_tval_filt_df.to_csv("outputs/interval1_mgwr_tval_filt.csv")
interval1_mgwr_tval_df.to_csv("outputs/interval1_mgwr_tvals.csv")
interval1_mgwr_bw_df.to_csv("outputs/interval1_mgwr_bws.csv", index = False)

# Get AICc values for bandwidth selection history used to calculate Akaike weights (for analysis in R)
# See https://github.com/pysal/mgwr/pull/81/commits/0519ad38a2c83d129ca8d0b1247b4b66ab451f81
aiccs = list()
for j in range(12):
    aiccs.append([float(i) for i in list(zip(*bw_interval1_selector.sel_hist[-12+j]))[1]])
    aiccs_df = pd.DataFrame(aiccs).transpose()
aiccs_df.columns = var_list
aiccs_df["bw"] = list(zip(*bw_interval1_selector.sel_hist[-1]))[0]
aiccs_df.to_csv("outputs/bw_selector1_aicc.csv")

# Create CSV of overall diagnostics of GWR and MGWR models
interval1_overall_diags = pd.DataFrame({
    "model": ["Global", "GWR", "MGWR"],
    "R2": [0, interval1_gwr_results.R2,interval1_mgwr_results.R2],
    "adj_R2":[0,interval1_gwr_results.adj_R2,interval1_mgwr_results.adj_R2],
    "AIC": [0,interval1_gwr_results.aic, interval1_mgwr_results.aic],
    "AICc": [0, interval1_gwr_results.aicc, interval1_mgwr_results.aicc]
})
interval1_overall_diags.to_csv("outputs/interval1_gwr_mgwr_overall_diagnostics.csv", index = False)

# Local condition numbers for each model 
# GWR has more local col. metrics available so indexes are different
interval1_mgwr_cn = interval1_mgwr_results.local_collinearity()[0]
interval1_gwr_cn = interval1_gwr_results.local_collinearity()[2]
interval1_cn_df = pd.concat([pd.DataFrame(interval1_mgwr_cn, columns = ["mgwr_cn"]),pd.DataFrame(interval1_gwr_cn, columns = ["gwr_cn"])], axis =1)
interval1_cn_df.to_csv("outputs/interval1_loc_cond_nums.csv")
