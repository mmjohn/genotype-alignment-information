#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of CNN performance when duplicates are included or not included; using 
# simulation sets with a high and low amount of duplicates
# This script: creates figures for CNN performance comparison across datasets and architectures
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
performance_rho_low <- performance_rho_low_all
r2_results_low <- r2_results_low_all
rm(test_losses_all, performance_rho_low_all, r2_results_low_all)

# high duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
performance_rho_high <- performance_rho_high_all
r2_results_high <- r2_results_high_all
rm(performance_rho_high_all, r2_results_high_all)

# alt CNN training data
# low duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_alt_hist_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
history_alt_low <- history_torch
rm(history_torch)

# high duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_alt_hist_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
history_alt_high <- history_torch
rm(history_torch)

# alt CNN performance data
# low duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_alt_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
test_losses_alt_low <- test_losses_all
performance_rho_alt_low <- performance_rho_low_all
r2_results_alt_low <- r2_results_low_all
rm(test_losses_all, performance_rho_low_all, r2_results_low_all)

# high duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_cnn_results_alt_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData'
  )
)
test_losses_alt_high <- test_losses_all
performance_rho_alt_high <- performance_rho_high_all
r2_results_alt_high <- r2_results_high_all
rm(test_losses_all, performance_rho_high_all, r2_results_high_all)


#--------------- TIDY DATA --------------------

# training data
history_low %>% 
  mutate(dup = "low") -> history_low

history_high %>% 
  mutate(dup = "high") -> history_high

dup_training <- full_join(history_low, history_high) %>% 
  pivot_longer(
    train_mean_losses:valid_mean_losses, 
    names_to = "set", 
    values_to = "mse"
  ) %>% 
  mutate(model = "cnn")

rm(history_low, history_high)

history_alt_low %>% 
  mutate(dup = "low") -> history_alt_low

history_alt_high %>% 
  mutate(dup = "high") -> history_alt_high

dup_alt_training <- full_join(history_alt_low, history_alt_high) %>% 
  pivot_longer(
    train_mean_losses:valid_mean_losses, 
    names_to = "set", 
    values_to = "mse"
  ) %>% 
  mutate(model = "alt_cnn")

rm(history_alt_low, history_alt_high)

# performance data
performance_rho_low %>% 
  mutate(set = "low") -> performance_rho_low

performance_rho_high %>% 
  mutate(set = "high") -> performance_rho_high

dup_performance <- full_join(
  performance_rho_low, 
  performance_rho_high
) %>% 
  mutate(model = "cnn")

performance_rho_alt_low %>% 
  mutate(set = "low") -> performance_rho_alt_low

performance_rho_alt_high %>% 
  mutate(set = "high") -> performance_rho_alt_high

dup_alt_performance <- full_join(
  performance_rho_alt_low, 
  performance_rho_alt_high
) %>% 
  mutate(model = "alt_cnn")

cnn_perform <- full_join(
  dup_performance, 
  dup_alt_performance
)

rm(dup_performance, dup_alt_performance)

# back-transform rho performance data
# get mean from cnn_05_model_data_prep.R
# low mean = 3.785511
# high mean =  3.796978
performance_rho_low %>% 
  mutate(
    est_tmp = rho_predict + 3.785511,
    act_tmp = rho_actual + 3.785511
  ) %>% 
  mutate(
    rho_predict_trans = exp(est_tmp),
    rho_actual_trans = exp(act_tmp)
  ) -> performance_rho_low

performance_rho_high %>% 
  mutate(
    est_tmp = rho_predict + 3.796978,
    act_tmp = rho_actual + 3.796978
  ) %>% 
  mutate(
    rho_predict_trans = exp(est_tmp),
    rho_actual_trans = exp(act_tmp)
  ) -> performance_rho_high

dup_perf_trans <- full_join(
  performance_rho_low, 
  performance_rho_high
) %>% 
  mutate(model = "cnn")

rm(performance_rho_low, performance_rho_high)

performance_rho_alt_low %>% 
  mutate(
    est_tmp = rho_predict + 3.785511,
    act_tmp = rho_actual + 3.785511
  ) %>% 
  mutate(
    rho_predict_trans = exp(est_tmp),
    rho_actual_trans = exp(act_tmp)
  ) -> performance_rho_alt_low

performance_rho_alt_high %>% 
  mutate(
    est_tmp = rho_predict + 3.796978,
    act_tmp = rho_actual + 3.796978
  ) %>% 
  mutate(
    rho_predict_trans = exp(est_tmp),
    rho_actual_trans = exp(act_tmp)
  ) -> performance_rho_alt_high

dup_perf_alt_trans <- full_join(
  performance_rho_alt_low, 
  performance_rho_alt_high
) %>% 
  mutate(model = "alt_cnn")

rm(performance_rho_alt_low, performance_rho_alt_high)

cnn_trans_perform <- full_join(
  dup_perf_trans,
  dup_perf_alt_trans
)

rm(dup_perf_trans, dup_perf_alt_trans)


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

dup_alt_training %>%
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
  ) -> fig_dup_alt_training

fig_dup_alt_training


#--------------- CNN PERFORMANCE FIGURE --------------------

# figures for cnn performance on test set
r2_results_low
r2_results_high

r2_results_alt_low
r2_results_alt_high

r2_text <- data.frame(
  label = c("*R*<sup>2</sup> = 0.790",
            "*R*<sup>2</sup> = 0.305",
            "*R*<sup>2</sup> = 0.781",
            "*R*<sup>2</sup> = 0.383"),
  set = c("low", "high", "low", "high"),
  model = c("cnn", "cnn", "alt_cnn", "alt_cnn"),
  x = c(-2.25, -2.25),
  y = c(2.95, 2.95)
)

mse_text <- data.frame(
  label = c("*RMSE* = 0.805",
            "*RMSE* = 1.426",
            "*RMSE* = 0.809",
            "*RMSE* = 1.346"),
  set = c("low", "high", "low", "high"),
  model = c("cnn", "cnn", "alt_cnn", "alt_cnn"),
  x = c(-2.25, -2.25),
  y = c(2.35, 2.35)
)

cnn_perform %>%
  mutate(set = fct_relevel(set, "low", "high")) %>% 
  ggplot() +
  geom_point(aes(x = rho_actual, y = rho_predict), alpha = 0.1) +
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
    rows = vars(model),
    labeller = labeller(
      set = c("low" = "Low", "high" = "High"),
      model = c("cnn" = "Alignments and positions", "alt_cnn" = "Alignment only")
    )
  ) +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90"),
    plot.background = element_rect(fill = "white", color = NA)
  ) -> fig_cnn_performance

fig_cnn_performance


#--------------- CNN BACK TRANSFORMED FIGURE --------------------

# r2_text_trans <- data.frame(
#   label = c("*R*<sup>2</sup> = 0.790",
#             "*R*<sup>2</sup> = 0.305"),
#   set = c("low", "high"),
#   x = c(250, 250),
#   y = c(1425, 1425)
# )
# 
# mse_text_trans <- data.frame(
#   label = c("*RMSE* = 0.805",
#             "*RMSE* = 1.426"),
#   set = c("low", "high"),
#   x = c(250, 250),
#   y = c(1325, 1325)
# )

cnn_trans_perform %>%
  mutate(set = fct_relevel(set, "low", "high")) %>% 
  ggplot(aes(x = rho_actual_trans, y = rho_predict_trans)) +
  geom_point(alpha = 0.1) +
  geom_abline(color = "goldenrod", size = 1.5) +
  # geom_richtext(
  #   data = r2_text_trans,
  #   aes(x = x, y = y,label = label),
  #   label.color = NA,
  #   inherit.aes = FALSE
  # ) +
  # geom_richtext(
  #   data = mse_text_trans,
  #   aes(x = x, y = y,label = label),
  #   label.color = NA,
  #   inherit.aes = FALSE
  # ) +
  scale_x_continuous(name = "Actual &rho;") +
  scale_y_continuous(name = "Estimated &rho;") +
  coord_fixed() +
  facet_grid(
    cols = vars(set),
    rows = vars(model),
    labeller = labeller(
      set = c("low" = "Low", "high" = "High"),
      model = c("cnn" = "Alignments and positions", "alt_cnn" = "Alignment only")
    )
  ) +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90"),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) -> fig_cnn_trans_perform

fig_cnn_trans_perform


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_low_v_high_dup_training.png'),
  fig_dup_training, ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_alt_low_v_high_dup_training.png'),
  fig_dup_alt_training, ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_cnn_perform.png'),
  fig_cnn_performance, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_cnn_trans_perfrom.png'),
  fig_cnn_trans_perform, ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)





