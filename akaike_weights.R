library(dplyr)
library(ggplot2)
library(tidyr)

# Read in BW AICcs
bw_aicc <- read.csv("outputs/bw_selector1_aicc.csv") %>% 
  select(-X)

# Create weights
aicc_wts <- bw_aicc %>% 
  mutate(across(!bw, ~.x - min(.x), .names = "{.col}_aicc_diff"),
         across(ends_with("aicc_diff"), ~exp(-0.5*.x)/sum(exp(-0.5*.x)), 
                .names = "{.col}_w"),
         across(ends_with("aicc_diff_w"), ~.x/sum(.x), 
                .names = "{.col}s"))

# Get 95% confidence intervals for BWs
bw_ci <-  data.frame(var = names(bw_aicc)[-13],
                     bw = rep(NA,12),
                     ci_min = rep(NA,12),
                     ci_max = rep(NA,12))

for (i in 1:nrow(bw_ci)){
  var_name <- paste0(bw_ci$var[i],"_aicc_diff_w")
  
  akaike_ci <- aicc_wts %>% 
    select(bw, var_name) %>% 
    arrange(desc(get(var_name))) %>% 
    mutate(cum_aicc = cumsum(get(var_name))) %>% 
    filter(cum_aicc < .95)
  
  bw_ci$ci_min[i] <- min(akaike_ci$bw)
  bw_ci$ci_max[i] <- max(akaike_ci$bw)
  bw_ci$bw[i] <- akaike_ci$bw[1]
}

indep_vars <- c("u14_perc_att_ed", "o18_perc_lit", "hh_mean_size","sc_perc", 
                "st_perc", "muslim_perc", "perc_urban_pop", "wealth_index",
                "hh_perc_f_head", "u14_perc_male", "total_fertility_rate")

covars <- c("intercept", indep_vars)

var_labels <- c("Intercept",
                "% U15 in education",
                "% 18+ literate",
                "Mean HH size",
                "% SC population",
                "% ST population",
                "% Muslim population",
                "% urban population",
                "Wealth index",
                "% female head HH",
                "% U15 male",
                "TFR")

names(var_labels) <- covars

# Plot Akaike weights with 95% CIs for bandwidths
aicc_wts_plot <- aicc_wts %>% 
  select(c(bw,ends_with("aicc_diff_ws"))) %>%  
  pivot_longer(-bw, 
               names_to = "var", 
               values_to = "aicc_ws") %>% 
  mutate(var = substr(var, 1, nchar(var)-13)) %>% 
  ggplot(aes(x = bw, y = aicc_ws)) +
  geom_line() +
  facet_wrap(~factor(var, levels = covars, ordered = T,                    
                     labels = var_labels),
             scales = "free_y",
             nrow = 2) +
  geom_vline(aes(xintercept = ci_min), bw_ci, colour = "blue") +
  geom_vline(aes(xintercept = ci_max), bw_ci, colour = "blue") +
  geom_vline(aes(xintercept = bw), bw_ci, colour = "red") +
  theme_bw() +
  theme(strip.background =element_rect(fill="grey20"),
        strip.text = element_text(colour = 'white')) +
  labs(y = "Akaike weight", x = "MGWR model bandwidth")

ggsave("output/akaike_wts_plot.png",
       aicc_wts_plot,
       width = 220, 
       height = 83, 
       units = "mm",
       dpi = 1000)



