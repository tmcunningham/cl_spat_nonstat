library(dplyr)
library(sf)
library(ggplot2)
library(spdep)
library(classInt)
library(scales)

# Read data and set indep vars --------------------------------------------

# Read in projected MAINLAND shapefile with data
mainland_data_proj_sf <- st_read("outputs/mainland_data_proj.gpkg")

# Variables to use in model
indep_vars <- c("u14_perc_att_ed", "o18_perc_lit", "hh_mean_size","sc_perc", 
                "st_perc", "muslim_perc", "perc_urban_pop", "wealth_index",
                "hh_perc_f_head", "u14_perc_male", "total_fertility_rate")

# Filter out NAs (data not available for contested part of Kashmir) and centre/scale indep vars
scaled_complete_data_sf <- mainland_data_proj_sf %>% 
  filter(!is.na(tot_pop)) %>%
  mutate(across(all_of(indep_vars), ~c(scale(.)))) 

# Save scaled complete and projected data
st_write(scaled_complete_data_sf,
         "outputs/scaled_complete_data_proj.gpkg",
         append = F)


# Correlation matrix
cor_mat <- cov2cor(cov(mainland_data_proj_sf %>% 
                         as.data.frame() %>% 
                         select(u14_perc_working, 
                                all_of(indep_vars)) %>% 
                         mutate(log_u14_perc_working = log(u14_perc_working)), 
                       use = "complete.obs"))


# Simulations to look at distribution -------------------------------------

# Actual CL count
cl_true <- scaled_complete_data_sf$u14_count_working

log_cl_true <- log(cl_true)

# Simulate CL counts from log normal dist.
cl_log_sim <- exp(rnorm(n = length(cl_true), mean = mean(log_cl_true), 
                        sd = sd(log_cl_true)))

# Simulate CL counts from Poisson dist.
cl_pois_sim <- rpois(n = length(cl_true),lambda = sqrt(var(cl_true)))

# Simulate CL counts from neg. bin. dist.
cl_nb_sim <- MASS::rnegbin(n = length(cl_true),mu = mean(cl_true), 
                           theta = mean(cl_true)^2/(var(cl_true) - mean(cl_true)))

# Plot dists. of simulated and true data
ggplot() +
  geom_density(aes(x = cl_true, colour = "True data")) +
  geom_density(aes(x = cl_nb_sim, colour = "Neg. bin. simulated")) +
  geom_density(aes(x = cl_log_sim, colour = "Log-normal simulated")) +
  #geom_density(aes(x = cl_pois_sim, colour = "pois_sim")) +
  labs(y = "Density", x = "Child labour district count", colour = "")

ggsave("plots/sim_data_plot.png",
       height = 10, width = 20, units = "cm")


# Modelling ---------------------------------------------------------------

# Formula for Poisson and NB models
fmla <- paste("u14_count_working ~",
              paste(indep_vars, collapse = " + "),
              "+ offset(log(u14_pop))")

# Model formula using a log transformation
log_fmla <- paste("log(u14_perc_working) ~",
                  paste(indep_vars, collapse = " + "))

# # Poisson regression model with offset
# pglm <- glm(fmla,
#             data = scaled_complete_data,
#             family = poisson)
# 
# # Summary of Poisson model
# summary(pglm)
# 
# plot(predict(pglm), log((scaled_complete_data_sf$u14_count_working - fitted(pglm))^2))
# 
# abline(0,1)
# 
# # Test for overdispersion - massively overdispersed
# AER::dispersiontest(pglm)
# 
# # Add residuals to DF
# scaled_complete_data_sf$pglm_resid <- residuals(pglm)
# 
# # Quasi-poisson model
# qpglm <- glm(fmla, 
#              data = scaled_complete_data_sf,
#              family = quasipoisson)

# Log transformed lin model
linm <- lm(log_fmla,
           data = scaled_complete_data_sf)

# Add residuals to DF
scaled_complete_data_sf$linm_resid <- residuals(linm)

# Negative binomial model
nbglm <- MASS::glm.nb(fmla,
                      data = scaled_complete_data_sf)

# Add residuals to DF
scaled_complete_data_sf$nbglm_resid <- residuals(nbglm)

# Distribution of model residuals
ggplot() +
  geom_density(mapping = aes(x = residuals(linm),
                             colour = "LM")) +
  geom_density(mapping = aes(x = residuals(nbglm), 
                             colour = "NB"))

# Plot of predicted (linm) against actual
ggplot(scaled_complete_data_sf, aes(x = log(u14_perc_working), 
                                    y = predict(linm))) +
  geom_point() +
  geom_abline()


# Map of first model residuals
ggplot(scaled_complete_data_sf, aes(fill = residuals(linm))) +
  geom_sf() +
  scale_fill_viridis_c()

# Map of CL and residuals ---------------------------------------------------------------

cl_plot_breaks <- classIntervals(mainland_data_proj_sf$u14_perc_working,
                                 4, 
                                 style = "jenks",
                                 dataPrecision = 2)

cl_plot_breaks$brks[1] <- round(cl_plot_breaks$brks[1], digits = 4)
cl_plot_nice_breaks <- percent(cl_plot_breaks$brks,
                               accuracy = 0.01)

cl_labs <- c(paste0(cl_plot_nice_breaks[1], " to ", cl_plot_nice_breaks[2]),
             paste0(cl_plot_nice_breaks[2], " to ", cl_plot_nice_breaks[3]),
             paste0(cl_plot_nice_breaks[3], " to ", cl_plot_nice_breaks[4]),
             paste0(cl_plot_nice_breaks[4], " to ", cl_plot_nice_breaks[5]),
             "No data")


cl_plot_data <- mainland_data_proj_sf %>% 
  select(DISTRICT, u14_perc_working) %>% 
  mutate(u14_perc_working_bin = factor(ifelse(DISTRICT == "Data Not Available",
                                              "No data",
                                              cut(u14_perc_working, 
                                                  breaks = cl_plot_breaks$brks)),
                                       labels = cl_labs))


cl_pal <- c(RColorBrewer::brewer.pal(length(cl_plot_breaks$brks)-1, "OrRd"),
            "grey20")

ggplot(cl_plot_data) +
  geom_sf(aes(fill = u14_perc_working_bin),
          color = "grey8",
          linewidth = 0.15)+
  scale_fill_manual(values = cl_pal) +
  labs(fill = "% of under-15s working") +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.background = element_rect(colour = 1),
        panel.background = element_rect(fill = "white"),
        legend.key.width = unit(1, "cm"),
        legend.position = c(.82,.24),
        plot.margin=grid::unit(c(0,3,0,0), "mm"))+
  ggspatial::annotation_north_arrow(height = unit(1.5, "cm"),
                                    width = unit(0.8, "cm"),
                                    location = "tl",
                                    style = ggspatial::north_arrow_orienteering(
                                      fill = c("black","white")))

ggsave("plots/CL dist plot.png",
       dpi = 1000,
       height = 120,
       width = 120,
       units = "mm")


# Moran's I ---------------------------------------------------------------
# Create neighbour list
sf::sf_use_s2(TRUE)

nb <- poly2nb(scaled_complete_data_sf, queen = T, 
              row.names = scaled_complete_data_sf$censuscode)

# Create list of spatial weights
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

linm_resid_global_morans_i <- lm.morantest(model = linm,
                                           listw = lw,
                                           zero.policy = T)

# Calculate local Moran's I
# nbglm_local_morans_i <- localmoran(complete_data_sf$nbglm_resid,
#                              lw)
linm_local_morans_i <- localmoran(scaled_complete_data_sf$linm_resid,
                                  lw)

# Add Morans I info to sf - recode quad for districts with p > 0.05
lmi_sig <- scaled_complete_data_sf %>% 
  mutate(
    # nbglm_mean_moran_i = attributes(nbglm_local_morans_i)$quadr$mean,
    # nbglm_moran_i_pval = nbglm_local_morans_i[,5],
    # nbglm_morans_i_quad = ifelse(nbglm_moran_i_pval > 0.1, "p value > 0.05",
    #                              as.character(nbglm_mean_moran_i)),
    
    linm_mean_moran_i = attributes(linm_local_morans_i)$quadr$mean,
    linm_moran_i_pval = linm_local_morans_i[,5],
    linm_morans_i_quad = ifelse(linm_moran_i_pval > 0.1, "p value > 0.05",
                                as.character(linm_mean_moran_i)))

# Plot Morans I of lm resids
ggplot() +
  #ggspatial::annotation_map_tile(zoomin = 1) +
  geom_sf(data = scaled_complete_data_sf, fill = "grey35", colour = "grey20") +
  geom_sf(data = lmi_sig, aes(fill = linm_morans_i_quad), alpha = .9, colour = "grey20") +
  scale_fill_manual(values = c("High-High" = "firebrick1",
                               "Low-High" = "pink",
                               "High-Low" = "lightblue",
                               "Low-Low" = "royalblue3",
                               "p value > 0.05" = "lightgrey")) +
  ggspatial::annotation_north_arrow(height = unit(1.5, "cm"),
                                    width = unit(0.8, "cm"),
                                    location = "tl",
                                    style = ggspatial::north_arrow_orienteering(
                                      fill = c("black","white"))) +
  labs(fill = "Global model \nresiduals Moran's\nI quadrant") +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.82,.24))

# Save Moran's I plot
ggsave("plots/Morans I LM residual plot.png",
       width = 8,
       height = 5,
       dpi = 1000)

# Save linear model
saveRDS(object = linm, file = "outputs/linear_model.RDS")



