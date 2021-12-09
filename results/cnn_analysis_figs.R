#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of CNN performance when duplicates are included or not included; using 
# simulation sets with a high and low amount of duplicates
# This script: creates figures for CNN performance comparison
# Mackenzie M. Johnson
# September 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
# library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggtext)
library(colorspace)

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'


#--------------- LOAD DATA SETS --------------------

# low duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)

load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_low_dup_unq_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)


#--------------- TIDY DATA --------------------

performance_rho_low_all %>% 
  mutate(set = "low_all") -> performance_rho_low_all

performance_rho_low_unq %>% 
  mutate(set = "low_unq") -> performance_rho_low_unq

low_dup_performance <- full_join(performance_rho_low_all, performance_rho_low_unq)


#--------------- CNN PERFORMANCE FIGURES --------------------

# figures for cnn performance on test set
r2_results_low_all
r2_results_low_unq

r2_text <- data.frame(
  label = c("*R*<sup>2</sup> = 0.790",
            "*R*<sup>2</sup> = 0.783"),
  set = c("low_all", "low_unq"),
  x = c(3, 3),
  y = c(-2.5, -2.5)
)

mse_text <- data.frame(
  label = c("*MSE* = 0.638",
            "*MSE* = 0.665"),
  set = c("low_all", "low_unq"),
  x = c(3, 3),
  y = c(-3.5, -3.5)
)

low_dup_performance %>%
  ggplot(aes(x = rho_predict, y = rho_actual)) +
  geom_point(alpha = 0.1) +
  geom_abline(color = "goldenrod", size = 1.5) +
  geom_richtext(
    data = r2_text,
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  geom_richtext(
    data = mse_text,
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(name = "Actual") +
  scale_x_continuous(name = "Estimate") +
  facet_grid(
    vars(set),
    labeller = labeller(set = c("low_all" = "All", "low_unq" = "Unique"))
  ) +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90")
  ) -> fig_low_dup

fig_low_dup


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'low_dup_performance.png'),
  fig_low_dup, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)






#--------------- CNN TRAINING FIGURES --------------------
# 
# torch_38_hist %>% 
#   ggplot(aes(x = epochs, y = sqrt(loss), color = set)) +
#   geom_point(size = 2) +
#   geom_path(size = 1.75) +
#   scale_color_viridis_d(
#     begin = 0.25, 
#     name = "Set",
#     labels = c("Train", "Validation")
#   ) +
#   scale_x_continuous(name = "Epoch") +
#   scale_y_continuous(name = "RMSE") +
#   theme_half_open() +
#   background_grid() -> fig_torch_38_hist
# 


