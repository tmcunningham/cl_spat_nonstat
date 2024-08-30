library(dplyr)
library(sf)
library(tidyr)
library(stringr)
library(ggplot2)
library(classInt)

# Variables to use in model
indep_vars <- c("u14_perc_att_ed", "o18_perc_lit", "hh_mean_size","sc_perc", 
                "st_perc", "muslim_perc", "perc_urban_pop", "wealth_index",
                "hh_perc_f_head", "u14_perc_male", "total_fertility_rate")

data_sf <- st_read("outputs/scaled_complete_data_proj.gpkg")
covars <- c(indep_vars, "intercept")


# Sf for district where data unavailable (for mapping)
na_sf <- st_read("outputs/mainland_data_proj.gpkg") %>% 
  filter(is.na(tot_pop))


# GWR results -------------------------------------------------------------


interval1_gwr_coefs <- read.csv("outputs/interval1_gwr_coefs.csv") %>% 
  rename(index = X)
interval1_gwr_tvals_filt <- read.csv("outputs/interval1_gwr_tval_filt.csv") %>% 
  select(-X)
interval1_gwr_tvals <- read.csv("outputs/interval1_gwr_tvals.csv") %>% 
  select(-X)


# Combine params into 1 df
interval1_gwr_results_df <- bind_cols(data_sf,
                                      interval1_gwr_coefs,
                                      interval1_gwr_tvals, 
                                      interval1_gwr_tvals_filt) %>% 
  bind_rows(na_sf)


# Columns to pivot
interval1_gwr_cols <- c(paste(covars, "coef.gwr", sep = "_"),
                        paste(covars, "tval.gwr", sep = "_"),
                        paste(covars, "filttval.gwr", sep = "_"))


interval1_gwr_param_sf <- interval1_gwr_results_df %>% 
  pivot_longer(all_of(interval1_gwr_cols), names_to = "param", 
               values_to = "param_value") %>% 
  mutate(var = str_split_i(param, "_(?=[^_]*$)", 1),
         param = str_split_i(param, "_(?=[^_]*$)", 2)) %>% 
  pivot_wider(id_cols = c(state_code, censuscode, var, geom),
              names_from = param, values_from = param_value) %>%
  st_sf() %>% 
  mutate(filt_coef.gwr = ifelse(filttval.gwr != 0, coef.gwr, NA))

# Summary stats of coefs
interval1_gwr_coef_summary <- interval1_gwr_param_sf %>% 
  data.frame() %>% 
  group_by(var) %>% 
  summarise(`Median` = median(coef.gwr, na.rm = T),
            `SD` = sd(coef.gwr, na.rm = T),
            `Min` = min(coef.gwr, na.rm = T),
            Q1 = unname(quantile(coef.gwr,0.25, na.rm = T)),
            `Median` = median(coef.gwr, na.rm = T),
            Q3 = unname(quantile(coef.gwr,0.75, na.rm = T)),
            `Max` = max(coef.gwr, na.rm = T),
            n_sig_distr = sum(filttval.gwr != 0, na.rm = T)
  )

write.csv(interval1_gwr_coef_summary, "outputs/interval1_gwr_sum_stats.csv",
          row.names = F)


# MGWR results ------------------------------------------------------------

interval1_mgwr_coefs <- read.csv("outputs/interval1_mgwr_coefs.csv") %>% 
  rename(index = X)
interval1_mgwr_tvals_filt <- read.csv("outputs/interval1_mgwr_tval_filt.csv") %>% 
  select(-X)
interval1_mgwr_tvals <- read.csv("outputs/interval1_mgwr_tvals.csv") %>% 
  select(-X)

# Combine params into 1 df
interval1_mgwr_results_df <- bind_cols(data_sf,
                                       interval1_mgwr_coefs,
                                       interval1_mgwr_tvals, 
                                       interval1_mgwr_tvals_filt) %>% 
  bind_rows(na_sf)

# Columns to pivot
cols <- c(paste(covars, "coef.mgwr", sep = "_"),
          paste(covars, "tval.mgwr", sep = "_"),
          paste(covars, "filttval.mgwr", sep = "_"))


interval1_mgwr_param_sf <- interval1_mgwr_results_df %>% 
  pivot_longer(all_of(cols), names_to = "param", 
               values_to = "param_value") %>% 
  mutate(var = str_split_i(param, "_(?=[^_]*$)", 1),
         param = str_split_i(param, "_(?=[^_]*$)", 2)) %>% 
  pivot_wider(id_cols = c(state_code, censuscode, var, geom),
              names_from = param, values_from = param_value) %>%
  st_sf() %>% 
  mutate(filt_coef.mgwr = ifelse(filttval.mgwr != 0, coef.mgwr, NA))

interval1_mgwr_bws <- read.csv("outputs/interval1_mgwr_bws.csv")


# Summary stats of coefs
interval1_mgwr_coef_summary <- interval1_mgwr_param_sf %>% 
  data.frame() %>% 
  group_by(var) %>% 
  summarise(`Median` = median(coef.mgwr, na.rm = T),
            `SD` = sd(coef.mgwr, na.rm = T),
            `Min` = min(coef.mgwr, na.rm = T),
            Q1 = unname(quantile(coef.mgwr,0.25, na.rm = T)),
            `Median` = median(coef.mgwr, na.rm = T),
            Q3 = unname(quantile(coef.mgwr,0.75, na.rm = T)),
            `Max` = max(coef.mgwr, na.rm = T),
            n_sig_distr = sum(filttval.mgwr != 0, na.rm = T)
  ) %>% 
  left_join(by = "var", interval1_mgwr_bws) %>% 
  select(-var_crit_tval)

write.csv(interval1_mgwr_coef_summary, 
          "outputs/interval1_mgwr_sum_stats.csv",
          row.names = F)

interval1_gwr_mgwr_coef_summary <- interval1_gwr_coef_summary %>% 
  select(var, Median, SD, Q1, Q3, n_sig_distr) %>% 
  left_join(interval1_mgwr_coef_summary %>% 
              select(var, Median, SD, Q1, Q3, n_sig_distr, bws.mgwr,
                     ci95_bws.mgwr, adj_alpha),
            by = "var")

write.csv(interval1_gwr_mgwr_coef_summary, 
          "outputs/interval1_gwr_mgwr_sum_stats.csv",
          row.names = F)

# Continuous coefficient maps ---------------------------------------------


# map_labels <- c("% U15 in education",
#                 "% 18+ literate",
#                 "Mean HH size",
#                 "% SC population",
#                 "% ST population",
#                 "% Muslim population",
#                 "% urban population",
#                 "Wealth index",
#                 "% female head HH",
#                 "% U15 male",
#                 "TFR",
#                 "Intercept")
# 
# names(map_labels) <- covars
#
# # GWR coefficient maps
# interval1_gwr_coef_maps <- ggplot(interval1_gwr_param_sf %>% filter(var != "intercept"), 
#                                    aes(fill = coef.gwr,
#                                        colour = ifelse(filttval.gwr == 0, "not_sig", "sig"))) +
#   geom_sf(linewidth = 0.15) +
#   facet_wrap(~factor(var, levels = indep_vars, ordered = T, 
#                      labels = map_labels[-length(map_labels)]),
#              ncol = 3)+
#   scale_fill_gradient2(low = "#40004b", mid = "white", 
#                        high = "#00441b",
#                        limits = c(-0.765,0.765),
#                        breaks = c(-0.75, -0.5,-0.25,0,0.25,0.5,0.75)) +
#   scale_colour_manual(values = c("not_sig" = "grey70", "sig" = "grey8"),
#                       guide = "none") +
#   labs(fill = "MGWR coef.\nestimate")+
#   theme(panel.grid.major = element_blank(),    
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         legend.position = "bottom",
#         legend.key.width = unit(3, "cm"))
# 
# 
# ggplot(interval1_gwr_param_sf %>% filter(var != "intercept"), 
#                                   aes(fill = coef.gwr,
#                                       colour = ifelse(filttval.gwr == 0, "not_sig", "sig"),
#                                       alpha = ifelse(filttval.gwr == 0, "not_sig", "sig"))) +
#   geom_sf(linewidth = 0.15) +
#   facet_wrap(~factor(var, levels = indep_vars, ordered = T, 
#                      labels = map_labels[-length(map_labels)]),
#              ncol = 3)+
#   scale_fill_gradient2(low = "#40004b", mid = "white", 
#                        high = "#00441b",
#                        limits = c(-0.765,0.765),
#                        breaks = c(-0.75, -0.5,-0.25,0,0.25,0.5,0.75)) +
#   scale_colour_manual(values = c("not_sig" = "grey8", "sig" = "grey8"),
#                       guide = "none") +
#   labs(fill = "MGWR coef.\nestimate")+
#   theme(panel.grid.major = element_blank(),    
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         legend.position = "bottom",
#         legend.key.width = unit(3, "cm"))
# 
# 
# 
# ggsave("plots/interval1_gwr_coef_maps.png", 
#        interval1_gwr_coef_maps,
#        height = 11,
#        width = 8,
#        dpi = 1000)
# 
# # MGWR coefficient maps
# interval1_mgwr_coef_maps <- ggplot(interval1_mgwr_param_sf %>% filter(var != "intercept"), 
#                          aes(fill = coef.mgwr,
#                              colour = ifelse(filttval.mgwr == 0, "not_sig", "sig"))) +
#   geom_sf(linewidth = 0.15) +
#   facet_wrap(~factor(var, levels = indep_vars, ordered = T, 
#                      labels = map_labels[-length(map_labels)]),
#              ncol = 3)+
#   scale_fill_gradient2(low = "#40004b", mid = "white", 
#                        high = "#00441b",
#                        limits = c(-0.765,0.765),
#                        breaks = c(-0.75, -0.5,-0.25,0,0.25,0.5,0.75)) +
#   scale_colour_manual(values = c("not_sig" = "grey70", "sig" = "grey8"),
#                       guide = "none") +
#   labs(fill = "MGWR coef.\nestimate")+
#   theme(panel.grid.major = element_blank(),    
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         legend.position = "bottom",
#         legend.key.width = unit(3, "cm"))
# 
# ggsave("plots/interval1_mgwr_coef_maps.png", 
#        interval1_mgwr_coef_maps,
#        height = 11,
#        width = 8,
#        dpi = 1000)




# Discrete maps of coefficients -------------------------------------------

# # Same breaks for GWR and MGWR maps (different min and max)
# coef_breaks <- c(-0.8, -0.4, -0.2, 0, 0.2, 0.4, 0.8)
# 
# coef_levels <- c(1:6,
#                  #"Not sig.", 
#                  "No data")
# 
# coef_pal <- c(RColorBrewer::brewer.pal(length(coef_breaks)-1, "PRGn"),
#               #"#A9A9A9", 
#               "grey20")
# 
# # Bin GWR coefficients
# interval1_gwr_param_sf_no_int <- interval1_gwr_param_sf %>%  
#   filter(var != "intercept") 
# 
# min_interval1_gwr_coef <- round(min(as.data.frame(interval1_gwr_param_sf_no_int)$coef.gwr, 
#                                     na.rm = T), 2)
# max_interval1_gwr_coef <- round(max(as.data.frame(interval1_gwr_param_sf_no_int)$coef.gwr, 
#                                     na.rm = T), 2)
# 
# interval1_gwr_coef_bin_labs <- c(paste(min_interval1_gwr_coef, "to -0.40"),
#                                  paste(min_interval1_gwr_coef, "to -0.20"),
#                                  "-0.20 to 0",
#                                  "0 to 0.20",
#                                  "0.20 to 0.40",
#                                  paste("0.40 to", max_interval1_gwr_coef),
#                                  #"Not sig. (p > 0.05)",
#                                  "No data")
# 
# interval1_gwr_param_sf_no_int <- interval1_gwr_param_sf_no_int %>% 
#   mutate(interval1_gwr_coef_bin = cut(coef.gwr, 
#                                        breaks = coef_breaks), 
#          interval1_gwr_coef_bin_filt = factor(ifelse(is.na(filttval.gwr),
#                                                       "No data",
#                                                       # ifelse(filttval.gwr == 0, 
#                                                       #        "Not sig.",
#                                                              interval1_gwr_coef_bin
#                                                      #)
#                                                      ),
#                                                levels = coef_levels,
#                                                labels = interval1_gwr_coef_bin_labs,
#                                                ordered = T))
# 
# names(coef_pal) <- interval1_gwr_coef_bin_labs
# 
# # Spearate sf into sig and non-sig layers for mapping
# interval1_gwr_param_sf_sig <- interval1_gwr_param_sf_no_int %>% 
#   filter(!(filttval.gwr == 0 | is.na(filttval.gwr)))
# 
# interval1_gwr_param_sf_nonsig <- interval1_gwr_param_sf_no_int %>% 
#   filter((filttval.gwr == 0 | is.na(filttval.gwr)))
# 
# # Map binned GWR coefs
# interval1_gwr_coef_maps <- ggplot() +
#   geom_sf(data = interval1_gwr_param_sf_nonsig, 
#           aes(fill = interval1_gwr_coef_bin_filt),
#           linewidth = 0.05,
#           color = "grey80") +
#   geom_sf(data = interval1_gwr_param_sf_sig, 
#           aes(fill = interval1_gwr_coef_bin_filt),
#           linewidth = 0.15,
#           color = "grey3") +
#   facet_wrap(~factor(var, levels = indep_vars, ordered = T, 
#                      labels = map_labels[-length(map_labels)]),
#              ncol = 3) +
#   scale_fill_manual(values = coef_pal,
#                     breaks = c(interval1_gwr_coef_bin_labs[1:6],
#                                "Not sig. (p > 0.05)",
#                                "No data")) +
#   labs(fill = "GWR coefficient\nestimate")+
#   theme(panel.grid.major = element_blank(),    
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         legend.position = c(.85,.13),
#         legend.key.width = unit(1, "cm"),
#         legend.background = element_rect(colour = 1),
#         strip.background =element_rect(fill="grey20"),
#         strip.text = element_text(colour = 'white'))
# 
# ggsave("plots/binned_interval1_gwr_coef_maps_with_nonsig.png",
#        interval1_gwr_coef_maps,
#        width = 210, 
#        height = 297, 
#        units = "mm",
#        dpi = 1000)
# 
# 
# # Bin MGWR coefficients
# interval1_mgwr_param_sf_no_int <- interval1_mgwr_param_sf %>%  
#   filter(var != "intercept") 
# 
# min_interval1_mgwr_coef <- round(min(as.data.frame(interval1_mgwr_param_sf_no_int)$coef.mgwr, 
#                                      na.rm = T), 2)
# 
# max_interval1_mgwr_coef <- round(max(as.data.frame(interval1_mgwr_param_sf_no_int)$coef.mgwr,
#                                      na.rm = T), 2)
# 
# interval1_mgwr_coef_bin_labs <- c(paste(min_interval1_mgwr_coef, "to -0.40"),
#                                   "-0.40 to -0.20",
#                                   "-0.20 to 0",
#                                   "0 to 0.20",
#                                   "0.20 to 0.40",
#                                   paste("0.40 to", max_interval1_mgwr_coef),
#                                   #"Not sig. (p > 0.05)",
#                                   "No data")
# 
# interval1_mgwr_param_sf_no_int <- interval1_mgwr_param_sf_no_int %>% 
#   mutate(interval1_mgwr_coef_bin = cut(coef.mgwr, 
#                                        breaks = coef_breaks), 
#          interval1_mgwr_coef_bin_filt = factor(ifelse(is.na(filttval.mgwr),
#                                                       "No data",
#                                                       # ifelse(filttval.mgwr == 0, 
#                                                       #        "Not sig.",
#                                                       interval1_mgwr_coef_bin
#                                                       #)
#          ),
#          levels = coef_levels,
#          labels = interval1_mgwr_coef_bin_labs,
#          ordered = T))
# 
# names(coef_pal) <- interval1_mgwr_coef_bin_labs
# 
# # Map binned MGWR coefficients
# # Spearate sf into sig and non-sig layers for mapping
# interval1_mgwr_param_sf_sig <- interval1_mgwr_param_sf_no_int %>% 
#   filter(!(filttval.mgwr == 0 | is.na(filttval.mgwr)))
# 
# interval1_mgwr_param_sf_nonsig <- interval1_mgwr_param_sf_no_int %>% 
#   filter((filttval.mgwr == 0 | is.na(filttval.mgwr)))
# 
# 
# interval1_mgwr_coef_maps <- ggplot() +
#   geom_sf(data = interval1_mgwr_param_sf_nonsig, 
#           aes(fill = interval1_mgwr_coef_bin_filt),
#           linewidth = 0.05,
#           color = "grey80") +
#   geom_sf(data = interval1_mgwr_param_sf_sig, 
#           aes(fill = interval1_mgwr_coef_bin_filt),
#           linewidth = 0.15,
#           color = "grey3") +
#   facet_wrap(~factor(var, levels = indep_vars, ordered = T, 
#                      labels = map_labels[-length(map_labels)]),
#              ncol = 3) +
#   scale_fill_manual(values = coef_pal,
#                     breaks = c(interval1_mgwr_coef_bin_labs[1:6],
#                                "Not sig. (p > 0.05)",
#                                "No data")) +
#   labs(fill = "MGWR coefficient\nestimate")+
#   theme(panel.grid.major = element_blank(),    
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         legend.position = c(.85,.13),
#         legend.key.width = unit(1, "cm"),
#         legend.background = element_rect(colour = 1),
#         strip.background =element_rect(fill="grey20"),
#         strip.text = element_text(colour = 'white'))
# 
# ggsave("plots/binned_interval1_mgwr_coef_maps_with_nonsig.png",
#        interval1_mgwr_coef_maps,
#        width = 210, 
#        height = 297, 
#        units = "mm",
#        dpi = 1000)



# Individual coefficient breaks maps --------------------------------------


# Individual plotting function
ind_coef_plot <- function(param_sf_list, var, model = "gwr", 
                          num_breaks = 5, method = "fisher",
                          breaks_dp = 2) {
  
  coef <- paste0("coef.", model)
  filttval <- paste0("filttval.", model)
  
  # Get DF for this var
  df <- param_sf_list[[var]]
  
  # Creat break points
  class_int <- classIntervals(as.data.frame(df)[[coef]],num_breaks, 
                              method, dataPrecision = 2)
  var_brks <- class_int$brks
  
  # Round min and max breaks to ensure all vals included
  var_brks[1] <- floor(var_brks[1]*10^breaks_dp)/10^breaks_dp
  var_brks[num_breaks + 1] <- ceiling(var_brks[num_breaks + 1]*10^breaks_dp)/10^breaks_dp
  var_brks <- unique(round(var_brks, breaks_dp))
  
  # Create palette big enough for num of breaks either side of 0
  coef_pal <- c(RColorBrewer::brewer.pal(2*(length(var_brks)-1), "RdYlBu"))
  
  # Make break closest to zero equal to 0 (if not all breaks positive or negative)
  # Avoid setting max or min to 0
  if(!(all(var_brks > 0)) & !(all(var_brks < 0))) {
    var_brks[which.min(abs(var_brks[-c(1,num_breaks+1)])) + 1] <- 0
  }
  
  
  # Palette for num of breaks
  var_pal <- c(coef_pal[(sum(var_brks[-1]>0) + 1):(sum(var_brks[-1]>0) + length(var_brks[-1]))],
               "grey20")
  
  # Labels for breaks
  brk_labs <- c(sapply(1:(length(var_brks)-1), 
                       function(i) paste(var_brks[i], "to", var_brks[i+1])),
                "No data")
  
  # Levels for breaks
  brk_levels <- c(1:(length(var_brks)-1),
                  "No data")
  
  df <- df %>% 
    mutate(interval1_gwr_coef_bin = cut(get(coef), 
                                        breaks = var_brks,
                                        inlcude.lowest = TRUE), 
           interval1_gwr_coef_bin_filt = factor(ifelse(is.na(get(filttval)),
                                                       "No data",
                                                       interval1_gwr_coef_bin),
           levels = brk_levels,
           labels = brk_labs,
           ordered = T))
  
  # Separate sf into sig and non-sig layers for mapping
  df_sig <- df %>% 
    filter(!(get(filttval) == 0 | is.na(get(filttval))))
  
  df_nonsig <- df %>% 
    filter((get(filttval) == 0 | is.na(get(filttval))))
  
  legend_spacing <- ((14.6 - length(var_brks))*.6)/2
  
  # Map binned GWR coefs
  plot <- ggplot() +
    geom_sf(data = df_nonsig, 
            aes(fill = interval1_gwr_coef_bin_filt, 
                color = "Not sig (p > 0.05)"),
            linewidth = 0.03
            ) +
    geom_sf(data = df_sig, 
            aes(fill = interval1_gwr_coef_bin_filt,
                color = "p-value < 0.05"),
            linewidth = 0.2
            ) +
    # facet_wrap(~factor(var, levels = covars, ordered = T,
    #                    labels = map_labels),
    #            ncol = 1) +
    scale_fill_manual(values = var_pal,
                      breaks = c(brk_labs,
                                 "No data")) +
    scale_color_manual(values = c("p-value < 0.05" = "grey3", 
                                  "Not sig (p > 0.05)" = "grey95"),
                       limits = c("p-value < 0.05")) +
    labs(fill = paste(toupper(model), "coefficient"),
         colour = "") +
    guides(color = guide_legend(override.aes = list(fill = NA,
                                                    linewidth = 0.2),
                                position = "inside",
                                order = 1),
           fill = guide_legend(override.aes = list(color = NA),
                               position = "inside",
                               order = 2)) +
    theme(panel.grid.major = element_blank(),    
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_rect(fill = "white"),
          legend.position.inside = c(.78,.48),
          legend.text = element_text(size = 7),
          legend.key = element_rect(color = "transparent"),
          legend.key.width = unit(.6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.title = element_text(size = 9),
          legend.background = element_rect(fill = "transparent"),
          legend.spacing.y = unit(legend_spacing, "cm"),
          strip.background = element_rect(fill="grey20"),
          strip.text = element_text(colour = 'white'),
          plot.margin=unit(c(3,1,3,1), "points"))
}


# Plot each plot one-by-one so each has its own legend
list_gwr_param_sf <- split(interval1_gwr_param_sf, 
                           interval1_gwr_param_sf$var)

# Create plot for each var
gglist_gwr <- lapply(covars,
                     function(x) ind_coef_plot(list_gwr_param_sf, x))

gwr_grid <- gridExtra::grid.arrange(grobs = gglist_gwr, 
                                    nrow = 4, ncol = 3)


ggsave("plots/sep_legend_gwr_coef_maps_with_int.png", gwr_grid,
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 1000)

# Plot each plot one-by-one so each has its own legend
list_mgwr_param_sf <- split(interval1_mgwr_param_sf, 
                            interval1_mgwr_param_sf$var)

# Create plot for each var
gglist_mgwr <- lapply(covars,
                      function(x) ind_coef_plot(list_mgwr_param_sf, x,
                                                model = "mgwr"))

mgwr_grid <- gridExtra::grid.arrange(grobs = gglist_mgwr, 
                                     nrow = 4, ncol = 3)

ggsave("plots/sep_legend_mgwr_coef_maps_with_int.png",
       mgwr_grid,
       width = 210, 
       height = 297, 
       units = "mm",
       dpi = 1000)

# CDT conference plots ----------------------------------------------------



# Intercept maps ----------------------------------------------------------

# interval1_intercepts <- interval1_mgwr_param_sf %>% 
#   filter(var == "intercept") %>%
#   mutate(intercept_filt = ifelse(filttval.mgwr == 0, NA, coef.mgwr)) %>%
#   select(intercept_filt) %>% 
#   bind_rows(interval1_gwr_param_sf %>% 
#               filter(var == "intercept") %>%
#               mutate(intercept_filt = ifelse(filttval.gwr == 0, NA, coef.gwr)) %>% 
#               select(intercept_filt),
#             .id = "model") %>% 
#   mutate(model = ifelse(model == 1, "MGWR", "GWR"))
# 
# min_interval1_intercept <- round(min(interval1_intercepts$intercept_filt, 
#                                      na.rm = T), 2)
# max_interval1_intercept <- round(max(interval1_intercepts$intercept_filt, 
#                                      na.rm = T), 2)
# interval1_int_breaks <- c(-3.6, -3.44, -3.32, -3.20, -3.08, -2.96)
# interval1_int_levels <- c(1:(length(interval1_int_breaks)-1), "Not sig.", "No data")
# interval1_int_bin_labs <- c(paste(min_interval1_intercept, "to -3.44"),
#                             "-3.44 to -3.32",
#                             "-3.32 to -3.20",
#                             "-3.20 to -3.08",
#                             paste("-3.08 to", max_interval1_intercept),
#                             "Not sig. (p > 0.05)",
#                             "No data")
# 
# interval1_intercepts_binned <- interval1_intercepts %>% 
#   mutate(int_bin = cut(intercept_filt,
#                        breaks = interval1_int_breaks), 
#          int_bin_filt = factor(ifelse(is.na(intercept_filt),
#                                       "No data",
#                                       ifelse(intercept_filt == 0, 
#                                              "Not sig.",
#                                              int_bin)),
#                                levels = interval1_int_levels,
#                                labels = interval1_int_bin_labs,
#                                ordered = T))
# 
# interval1_int_pal <- c(RColorBrewer::brewer.pal(length(interval1_int_breaks)-1, 
#                                                 "YlOrRd"),
#                        "#A9A9A9", "grey20")
# 
# names(interval1_int_pal) <- interval1_int_bin_labs
# 
# interval1_int_maps <- ggplot(interval1_intercepts_binned, 
#                              aes(fill = int_bin_filt)) +
#   geom_sf() +
#   facet_wrap(~model) +
#   labs(fill = "Intercept\nestimate") +
#   scale_fill_manual(values = interval1_int_pal,
#                     breaks = interval1_int_bin_labs) +
#   theme(panel.grid.major = element_blank(),    
#         panel.grid.minor = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         legend.position = c(.9,.2),
#         legend.key.width = unit(1, "cm"),
#         legend.background = element_rect(colour = 1),
#         strip.background =element_rect(fill="grey20"),
#         strip.text = element_text(colour = 'white'))
# 
# 
# ggsave("plots/interval1_int_maps.png",
#        interval1_int_maps,
#        width = 210, 
#        height = 140, 
#        units = "mm",
#        dpi = 1000)
# 
# interval1_intercepts %>% 
#   as.data.frame() %>% 
#   filter(!is.na(intercept_filt)) %>% 
#   ggplot(aes(x = intercept_filt)) +
#   geom_density() +
#   facet_wrap(~model)
# 
# 
# mgwr_int <- interval1_intercepts %>% 
#   as.data.frame() %>% 
#   filter(model == "MGWR")
# 
# max(mgwr_int$intercept_filt, na.rm = T) - min(mgwr_int$intercept_filt, na.rm = T)
# 
# interval1_mgwr_results_df %>% 
#   as.data.frame() %>% 
#   mutate(t = st_perc_filttval.mgwr !=0 & st_perc_coef.mgwr > .2) %>% 
#   group_by(state_name) %>% 
#   summarise(tot = n(), perc = mean(t), num = tot *perc) %>% 
#   filter(perc > 0)
# 
# interval1_mgwr_results_df %>% 
#   as.data.frame() %>% 
#   mutate(t = perc_urban_pop_filttval.mgwr !=0  & perc_urban_pop_coef.mgwr < 0) %>% 
#   group_by(state_name) %>% 
#   summarise(tot = n(), perc = mean(t), num = tot *perc) %>% 
#   filter(perc > 0)
# 
# plot(as.data.frame(interval1_mgwr_results_df)$st_perc,
#      as.data.frame(interval1_mgwr_results_df)$st_perc_coef.mgwr)
#   
# 
# interval1_mgwr_results_df %>% 
#   as.data.frame() %>% 
#   group_by(state_name) %>% 
#   summarise(s = mean(perc_urban_pop_coef.mgwr < -.4)) %>% 
#   filter(s> 0)
# 
# 
# data_sf %>% 
#   group_by(state_name) %>% 
#   summarise(sc_pop = sum(sc_pop, na.rm = T),
#             st_pop = sum(st_pop, na.rm = T),
#             tot_pop = sum(tot_pop)) %>% 
#   mutate(sc_st_pop = sc_pop + st_pop,
#          sc_perc = sc_pop/tot_pop,
#          st_perc = st_pop/tot_pop,
#          sc_st_perc = sc_st_pop/tot_pop) %>% 
#   ggplot(aes(fill = st_perc)) +
#   geom_sf()


# Local collinearity ------------------------------------------------------

# Read local condition numbers
cns_df <- read.csv("outputs/interval1_loc_cond_nums.csv") %>% 
  select(-X)

cns_sf <- bind_cols(data_sf,
                    cns_df) %>% 
  pivot_longer(cols = c(mgwr_cn, gwr_cn),
               names_to = "model",
               values_to = "cn") %>% 
  mutate(model = ifelse(model == "mgwr_cn", "MGWR", "GWR"))

# Plot local condition numbers 
interval1_cn_maps <- ggplot(cns_sf, aes(fill = cn)) +
  geom_sf() +
  facet_wrap(~model) +
  scale_fill_fermenter(breaks = seq(0,30,5),
                       palette = "YlOrRd",
                       direction = 1,
                       limits = c(0,30)) +
  labs(fill = "Local condition\nnumber") +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.9,.2),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(colour = 1),
        strip.background =element_rect(fill="grey20"),
        strip.text = element_text(colour = 'white'))

ggsave("plots/interval1_cn_maps.png",
       interval1_cn_maps,
       width = 210, 
       height = 140, 
       units = "mm",
       dpi = 1000)
