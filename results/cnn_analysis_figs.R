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
library(forcats)

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'


#--------------- LOAD DATA SETS --------------------

# CNN training data
# low duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_hist_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
history_low <- history_torch
rm(history_torch)

# high duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_hist_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
history_high <- history_torch
rm(history_torch)

# CNN performance data
# low duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
test_losses_low <- test_losses_all
rm(test_losses_all)

# high duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)

#--------------- TIDY DATA --------------------

# training data
history_low %>% 
  mutate(dup = "low") -> history_low

history_high %>% 
  mutate(dup = "high") -> history_high

dup_training <- full_join(history_low, history_high) %>% 
  pivot_longer(train_mean_losses:valid_mean_losses, names_to = "set", values_to = "mse")

# performance data
performance_rho_low_all %>% 
  mutate(set = "low") -> performance_rho_low_all

performance_rho_high_all %>% 
  mutate(set = "high") -> performance_rho_high_all

dup_performance <- full_join(performance_rho_low_all, performance_rho_high_all)


# back-transform rho performance data
# get mean from cnn_05_model_data_prep.R
# low mean = 3.785511
# high mean =  3.796978
performance_rho_low_all %>% 
  mutate(
    est_tmp = rho_predict + 3.785511,
    act_tmp = rho_actual + 3.785511
  ) %>% 
  mutate(
    rho_predict_trans = exp(est_tmp),
    rho_actual_trans = exp(act_tmp)
  ) -> performance_rho_low_all

performance_rho_high_all %>% 
  mutate(
    est_tmp = rho_predict + 3.796978,
    act_tmp = rho_actual + 3.796978
  ) %>% 
  mutate(
    rho_predict_trans = exp(est_tmp),
    rho_actual_trans = exp(act_tmp)
  ) -> performance_rho_high_all

dup_perf_trans <- full_join(performance_rho_low_all, performance_rho_high_all)


#--------------- CNN TRAINING FIGURE --------------------

dup_training %>%
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 2) +
  geom_path(size = 1.75, alpha = 0.8) +
  scale_color_viridis_d(
    begin = 0.25,
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epoch") +
  scale_y_continuous(name = "RMSE") +
  facet_grid(
    vars(dup),
    labeller = labeller(dup = c("low" = "Low", "high" = "High")),
    #scales = "free_y"
  ) +
  theme_half_open() +
  background_grid() +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  ) -> fig_dup_training

fig_dup_training


#--------------- CNN PERFORMANCE FIGURE --------------------

# figures for cnn performance on test set
r2_results_low_all
r2_results_high_all

r2_text <- data.frame(
  label = c("*R*<sup>2</sup> = 0.790",
            "*R*<sup>2</sup> = 0.305"),
  set = c("low", "high"),
  x = c(-2.25, -2.25),
  y = c(2.95, 2.95)
)

mse_text <- data.frame(
  label = c("*RMSE* = 0.805",
            "*RMSE* = 1.426"),
  set = c("low", "high"),
  x = c(-2.25, -2.25),
  y = c(2.35, 2.35)
)

dup_performance %>%
  mutate(set = fct_relevel(set, "low", "high")) %>% 
  ggplot(aes(x = rho_actual, y = rho_predict)) +
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
  scale_x_continuous(name = "Actual") +
  scale_y_continuous(name = "Estimate") +
  coord_fixed() +
  facet_grid(
    cols = vars(set),
    labeller = labeller(set = c("low" = "Low", "high" = "High"))
  ) +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90"),
    plot.background = element_rect(fill = "white", color = NA)
  ) -> fig_low_v_high

fig_low_v_high


#--------------- CNN BACK TRANSFORMED FIGURE --------------------

r2_text_trans <- data.frame(
  label = c("*R*<sup>2</sup> = 0.790",
            "*R*<sup>2</sup> = 0.305"),
  set = c("low", "high"),
  x = c(250, 250),
  y = c(1425, 1425)
)

mse_text_trans <- data.frame(
  label = c("*RMSE* = 0.805",
            "*RMSE* = 1.426"),
  set = c("low", "high"),
  x = c(250, 250),
  y = c(1325, 1325)
)

dup_perf_trans %>%
  ggplot(aes(x = rho_actual_trans, y = rho_predict_trans)) +
  geom_point(alpha = 0.1) +
  geom_abline(color = "goldenrod", size = 1.5) +
  geom_richtext(
    data = r2_text_trans,
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  geom_richtext(
    data = mse_text_trans,
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(name = "Actual &rho;") +
  scale_y_continuous(name = "Estimated &rho;") +
  coord_fixed() +
  facet_grid(
    cols = vars(set),
    labeller = labeller(set = c("low" = "Low", "high" = "High"))
  ) +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) -> fig_low_v_high_trans

fig_low_v_high_trans



#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_low_v_high_dup_training.png'),
  fig_dup_training, ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_low_v_high_dup_performance.png'),
  fig_low_v_high, ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_low_v_high_dup_perform_trans.png'),
  fig_low_v_high_trans, ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)





