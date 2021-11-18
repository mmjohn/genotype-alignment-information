
library(tidyverse)
library(cowplot)
library(ggtext)

########### TRAINING LEARNING RATE ANALYSIS ###############
# keras models

# 25 epoch, lr = 1e-4
train_mean_losses_k1 <- c(1.73, 1.33, 1.24, 1.17, 1.13, 1.09, 1.06, 1.03, 
                          1.00, 0.98, 0.95, 0.93, 0.91, 0.88, 0.86, 0.84, 
                          0.82, 0.80, 0.78, 0.75, 0.74, 0.72, 0.70, 0.68, 
                          0.67)
valid_mean_losses_k1 <- c(1.38, 1.26, 1.31, 1.16, 1.13, 1.12, 1.11, 1.10,
                          1.09, 1.09, 1.07, 1.08, 1.08, 1.16, 1.08, 1.09,
                          1.08, 1.08, 1.16, 1.09, 1.11, 1.16, 1.12, 1.11,
                          1.12)

history_k1 <- tibble(
  epochs = seq(1:length(train_mean_losses_k1)),
  train_mean_loss = train_mean_losses_k1,
  valid_mean_loss = valid_mean_losses_k1,
  lr = rep(1e-4, length(train_mean_losses_k1)),
  software = "keras"
)

# 25 epoch, lr = 1e-6
train_mean_losses_k2 <- c(3.50, 2.60, 2.06, 1.96, 1.92, 1.90, 1.88, 1.87, 
                          1.86, 1.85, 1.85, 1.84, 1.84, 1.84, 1.84, 1.84, 
                          1.83, 1.83, 1.83, 1.83, 1.83, 1.83, 1.82, 1.82, 
                          1.82)
valid_mean_losses_k2 <- c(3.08, 2.22, 1.99, 1.95, 1.92, 1.90, 1.89, 1.88, 
                          1.87, 1.87, 1.86, 1.86, 1.86, 1.86, 1.86, 1.85, 
                          1.85, 1.85, 1.85, 1.85, 1.84, 1.84, 1.84, 1.84, 
                          1.84)

history_k2 <- tibble(
  epochs = seq(1:length(train_mean_losses_k2)),
  train_mean_loss = train_mean_losses_k2,
  valid_mean_loss = valid_mean_losses_k2,
  lr = rep(1e-6, length(train_mean_losses_k2)),
  software = "keras"
)

# 25 epoch, lr = 1e-5
train_mean_losses_k3 <- c(2.13, 1.84, 1.82, 1.76, 1.62, 1.50, 1.45, 1.42, 
                          1.40, 1.38, 1.36, 1.35, 1.33, 1.32, 1.31, 1.30, 
                          1.29, 1.27, 1.26, 1.25, 1.24, 1.24, 1.23, 1.22, 
                          1.21)
valid_mean_losses_k3 <- c(1.88, 1.85, 1.82, 1.72, 1.55, 1.47, 1.43, 1.41,
                          1.38, 1.37, 1.35, 1.35, 1.34, 1.31, 1.30, 1.29,
                          1.29, 1.28, 1.27, 1.26, 1.25, 1.24, 1.24, 1.23,
                          1.22)

history_k3 <- tibble(
  epochs = seq(1:length(train_mean_losses_k3)),
  train_mean_loss = train_mean_losses_k3,
  valid_mean_loss = valid_mean_losses_k3,
  lr = rep(1e-5, length(train_mean_losses_k3)),
  software = "keras"
)

# 25 epoch, lr = 1e-7
train_mean_losses_k4 <- c(3.89, 3.83, 3.76, 3.68, 3.61, 3.53, 3.45, 3.37,
                          3.29, 3.21, 3.12, 3.03, 2.94, 2.86, 2.77, 2.69,
                          2.61, 2.53, 2.46, 2.39, 2.33, 2.27, 2.23, 2.18,
                          2.15)
valid_mean_losses_k4 <- c(3.87, 3.80, 3.73, 3.66, 3.59, 3.51, 3.43, 3.35,
                          3.27, 3.18, 3.09, 3.01, 2.92, 2.83, 2.75, 2.66,
                          2.59, 2.51, 2.44, 2.37, 2.32, 2.26, 2.22, 2.18,
                          2.14)

history_k4 <- tibble(
  epochs = seq(1:length(train_mean_losses_k4)),
  train_mean_loss = train_mean_losses_k4,
  valid_mean_loss = valid_mean_losses_k4,
  lr = rep(1e-7, length(train_mean_losses_k4)),
  software = "keras"
)


# torch models

# 25 epoch, lr = 1e-4
train_mean_losses_t1 <- c(1.861, 1.842, 1.825, 1.760, 1.492, 1.421, 1.404, 
                          1.404, 1.409, 1.401, 1.405, 1.396, 1.4219, 1.425, 
                          1.408, 1.428, 1.446, 1.420, 1.446, 1.448, 1.456, 
                          1.432, 1.460, 1.467, 1.495)
# c(1.82, 1.79, 1.79, 1.80, 1.81, 1.81, 1.82, 1.82, 
#   1.83, 1.83, 1.83, 1.84, 1.84, 1.84, 1.85, 1.85, 
#   1.85, 1.86, 1.87, 1.88, 1.90, 1.91, 1.92, 1.93, 1.94)

valid_mean_losses_t1 <- c(1.844, 1.838, 1.825, 1.600, 1.356, 1.361, 1.358, 
                          1.451, 1.494, 1.401, 1.342, 1.800, 1.620, 1.634, 
                          1.271, 1.306, 1.275, 1.333, 1.436, 1.284, 1.443, 
                          1.323, 1.296, 1.473, 1.287)
  
# c(1.95, 2.04, 2.25, 2.48, 2.60, 2.73, 2.99, 3.07, 
#   3.14, 3.04, 3.31, 3.24, 3.25, 3.34, 3.61, 3.66, 
#   3.26, 3.41, 4.02, 5.04, 5.46, 5.65, 5.73, 5.94, 5.80)

history_t1 <- tibble(
  epochs = seq(1:length(train_mean_losses_t1)),
  train_mean_loss = train_mean_losses_t1,
  valid_mean_loss = valid_mean_losses_t1,
  lr = rep(1e-4, length(train_mean_losses_t1)),
  software = "torch"
) 

# 25 epoch, lr = 1-6
train_mean_losses_t2 <- c(3.277, 2.126, 1.952, 1.920, 1.897, 1.880, 1.866, 
                          1.857, 1.849, 1.844, 1.841, 1.838, 1.836, 1.836, 
                          1.834, 1.833, 1.832, 1.831, 1.832, 1.830, 1.831, 
                          1.830, 1.830, 1.830, 1.829)
  
# 3.33, 2.47, 2.00, 1.92, 1.90, 1.89, 1.88, 1.87, 
# 1.86, 1.86, 1.85, 1.85, 1.85, 1.85, 1.84, 1.84, 
# 1.84, 1.84, 1.84, 1.84, 1.84, 1.84, 1.84, 1.84, 
# 1.84

valid_mean_losses_t2 <- c(2.529, 1.987, 1.947, 1.923, 1.906, 1.891, 1.880, 
                          1.873, 1.867, 1.863, 1.860, 1.858, 1.857, 1.856, 
                          1.855, 1.854, 1.854, 1.853, 1.853, 1.852, 1.852, 
                          1.852, 1.852, 1.852, 1.851)
  
#  2.90, 2.14, 1.95, 1.93, 1.91, 1.90, 1.89, 1.88, 
# 1.88, 1.87, 1.87, 1.87, 1.87, 1.86, 1.86, 1.86, 
# 1.86, 1.86, 1.86, 1.86, 1.86, 1.86, 1.86, 1.86, 
# 1.86

history_t2 <- tibble(
  epochs = seq(1:length(train_mean_losses_t2)),
  train_mean_loss = train_mean_losses_t2,
  valid_mean_loss = valid_mean_losses_t2,
  lr = rep(1e-6, length(train_mean_losses_t2)),
  software = "torch"
) 

# 25 epoch, lr = 1e-5
train_mean_losses_t3 <- c(2.069, 1.840, 1.833, 1.832, 1.826, 1.824,
                          1.823, 1.815, 1.813, 1.800, 1.792, 1.774, 
                          1.756, 1.726, 1.677, 1.622, 1.546, 1.474,
                          1.419, 1.380, 1.343, 1.322, 1.309, 1.295,
                          1.288)
valid_mean_losses_t3 <- c(1.870, 1.855, 1.851, 1.847, 1.845, 1.841,
                          1.838, 1.832, 1.824, 1.828, 1.799, 1.781, 
                          1.756, 1.719, 1.678, 1.605, 1.515, 1.445,
                          1.408, 1.343, 1.332, 1.299, 1.295, 1.272,
                          1.273)

history_t3 <- tibble(
  epochs = seq(1:length(train_mean_losses_t3)),
  train_mean_loss = train_mean_losses_t3,
  valid_mean_loss = valid_mean_losses_t3,
  lr = rep(1e-5, length(train_mean_losses_t3)),
  software = "torch"
) 

# 25 epoch, lr = 1e-7
train_mean_losses_t4 <- c(3.514, 3.416, 3.317, 3.216, 3.113, 3.009, 
                          2.903, 2.799, 2.696, 2.596, 2.497, 2.406, 
                          2.321, 2.242, 2.172, 2.112, 2.061, 2.019, 
                          1.988, 1.963, 1.946, 1.935, 1.926, 1.920, 
                          1.915)

# 3.68, 3.60, 3.52, 3.44, 3.36, 3.28, 3.20, 3.12, 
# 3.04, 2.95, 2.87, 2.79, 2.71, 2.63, 2.55, 2.47, 
# 2.40, 2.33, 2.27, 2.20, 2.15, 2.11, 2.07, 2.03, 2.00

valid_mean_losses_t4 <- c(3.476, 3.378, 3.278, 3.176, 3.073, 2.968, 
                          2.863, 2.760, 2.658, 2.559, 2.465, 2.376, 
                          2.294, 2.220, 2.155, 2.099, 2.054, 2.017, 
                          1.990, 1.970, 1.956, 1.945, 1.938, 1.933, 
                          1.929)

# 3.65, 3.57, 3.49, 3.41, 3.33, 3.25, 3.17, 3.09, 
# 3.01, 2.92, 2.84, 2.76, 2.68, 2.60, 2.52, 2.45, 
# 2.38, 2.31, 2.25, 2.19, 2.14, 2.10, 2.06, 2.03, 2.00

history_t4 <- tibble(
  epochs = seq(1:length(train_mean_losses_t4)),
  train_mean_loss = train_mean_losses_t4,
  valid_mean_loss = valid_mean_losses_t4,
  lr = rep(1e-7, length(train_mean_losses_t4)),
  software = "torch"
) 


history_all <- full_join(history_k1, history_k2) %>% 
  full_join(., history_k3) %>% 
  full_join(., history_k4) %>% 
  full_join(., history_t1) %>% 
  full_join(., history_t2) %>% 
  full_join(., history_t3) %>% 
  full_join(., history_t4) %>% 
  pivot_longer(
    c(train_mean_loss, valid_mean_loss), 
    names_to = "set",
    values_to = "mse"
  )

# manually set labels
labels_software <- c(
  "keras" = "Keras", 
  "torch" = "Torch"
)

labels_lr <- c(
  "1e-04" = "lr = 1e-4", 
  "1e-05" = "lr = 1e-5",
  "1e-06" = "lr = 1e-6",
  "1e-07" = "lr = 1e-7"
)

history_all %>% 
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_line(size = 1.5, alpha = 0.5) +
  facet_grid(
    cols = vars(software), 
    rows = vars(lr), 
    scales = "free_y",
    labeller = labeller(software = labels_software, lr = labels_lr)
  ) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epochs") +
  scale_y_continuous(name = "Error (RMSE)") +
  theme_bw() +
  theme(strip.text = element_markdown()) -> fig_torch_keras

fig_torch_keras

save_plot("notes/torch_keras_comparison.png", fig_torch_keras,
          ncol = 1, nrow = 1, base_height = 5.71,
          base_asp = 1.618, base_width = NULL)



########### MODEL PERFORMANCE ANALYSIS ###############

load('/stor/home/mmj2238/genotype-alignment-information/notes/keras_all_29_epoch_1e-5_lr.RData')
load('/stor/home/mmj2238/genotype-alignment-information/notes/torch_full_29_epoch_1e-5_lr.RData')

# r2_results_keras
# r2_results_torch

history_full <- tibble(
  epoch = seq(1:29),
  keras_train_loss = history$metrics$loss,
  keras_val_loss = history$metrics$val_loss,
  torch_train_loss = history_torch$train_mean_losses,
  torch_val_loss = history_torch$valid_mean_losses
) %>% 
  pivot_longer(-epoch, names_to = "set", values_to = "mse") %>% 
  mutate(software = case_when(set %in% c("keras_train_loss", "keras_val_loss") ~ "keras",
                              TRUE ~ "torch"),
         data = case_when(set %in% c("keras_train_loss", "torch_train_loss") ~ "train",
                          TRUE ~ "validation"))

history_full %>% 
  ggplot(aes(x = epoch, y = sqrt(mse), color = data)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(software)) + 
  theme_bw() -> fig_torch_keras_hist_full

fig_torch_keras_hist_full

save_plot("notes/torch_keras_hist_full.png", fig_torch_keras_hist_full,
          ncol = 1, nrow = 1, base_height = 3.71,
          base_asp = 1.618, base_width = NULL)


performance_torch %>% 
  mutate(
    estimate = rho_train_prediction,
    actual = rho_actual,
    software = "torch"
  ) %>% 
  select(sample, actual, estimate, software) -> performance_torch

performance_keras %>% 
  mutate(software = "keras") -> performance_keras

full_join(performance_torch, performance_keras) -> performance_all

r2_text <- data.frame(
  label = c("*R*<sup>2</sup> = 0.3403", 
            "*R*<sup>2</sup> = 0.3436"),
  software = c("keras", "torch"),
  x = c(2, 2),
  y = c(-3, -3)
)

ggplot() +
  geom_point(data = performance_all, aes(x = estimate, y = actual), alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "darkgreen", size = 1.5) +
  facet_grid(rows = vars(software)) +
  geom_richtext(
    data = r2_text, 
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  theme_bw() -> fig_torch_keras_performance

fig_torch_keras_performance

save_plot("notes/torch_keras_performance.png", fig_torch_keras_performance,
          ncol = 1, nrow = 1, base_height = 3.71,
          base_asp = 1.618, base_width = NULL)


########### TRAINING L2 REGULARIZATION ANALYSIS ###############

# 25 epoch, lr = 1e-5, keras
train_mean_losses_k_reg <- c(
  2.13, 1.84, 1.82, 1.76, 1.62, 1.50, 1.45, 1.42, 1.40, 1.38, 
  1.36, 1.35, 1.33, 1.32, 1.31, 1.30, 1.29, 1.27, 1.26, 1.25, 
  1.24, 1.24, 1.23, 1.22, 1.21
)

valid_mean_losses_k_reg <- c(
  1.88, 1.85, 1.82, 1.72, 1.55, 1.47, 1.43, 1.41, 1.38, 1.37, 
  1.35, 1.35, 1.34, 1.31, 1.30, 1.29, 1.29, 1.28, 1.27, 1.26, 
  1.25, 1.24, 1.24, 1.23, 1.22
)

history_k_reg <- tibble(
  epochs = seq(1:length(train_mean_losses_k_reg)),
  train_mean_loss = train_mean_losses_k_reg,
  valid_mean_loss = valid_mean_losses_k_reg,
  lr = rep(1e-5, length(train_mean_losses_k_reg)),
  software = "keras 1",
  weight_decay = NULL
)

train_mean_losses_k_no <- c(
  2.12, 1.84, 1.81, 1.71, 1.56, 1.48, 1.43, 1.40, 1.38, 1.35, 
  1.34, 1.33, 1.31, 1.29, 1.28, 1.27, 1.26, 1.24, 1.24, 1.23, 
  1.22, 1.21, 1.20, 1.19, 1.18
)

valid_mean_losses_k_no <- c(
  1.87, 1.85, 1.79, 1.63, 1.50, 1.44, 1.41, 1.37, 1.35, 1.34, 
  1.32, 1.32, 1.29, 1.28, 1.27, 1.26, 1.25, 1.24, 1.24, 1.23, 
  1.22, 1.23, 1.21, 1.20, 1.20
)

history_k_no <- tibble(
  epochs = seq(1:length(train_mean_losses_k_no)),
  train_mean_loss = train_mean_losses_k_no,
  valid_mean_loss = valid_mean_losses_k_no,
  lr = rep(1e-5, length(train_mean_losses_k_no)),
  software = "keras 2",
  weight_decay = NULL
)

# 25 epoch, lr = 1e-5, torch 
train_mean_losses_t_reg <- c(
  2.069, 1.840, 1.833, 1.832, 1.826, 1.824,
  1.823, 1.815, 1.813, 1.800, 1.792, 1.774,
  1.756, 1.726, 1.677, 1.622, 1.5463, 1.474,
  1.419, 1.380, 1.343, 1.322, 1.309, 1.295, 
  1.288
)
valid_mean_losses_t_reg <- c(
  1.870, 1.855, 1.851, 1.847, 1.845, 1.841,
  1.838, 1.832, 1.824, 1.828, 1.799, 1.781, 
  1.756, 1.719, 1.678, 1.605, 1.515, 1.445,
  1.408, 1.343, 1.332, 1.299, 1.295, 1.272, 
  1.273
)

history_t_reg <- tibble(
  epochs = seq(1:length(train_mean_losses_t_reg)),
  train_mean_loss = train_mean_losses_t_reg,
  valid_mean_loss = valid_mean_losses_t_reg,
  lr = rep(1e-5, length(train_mean_losses_t_reg)),
  software = "torch 1",
  weight_decay = 1e-4
) 

train_mean_losses_t_no <- c(
  2.034, 1.828, 1.815, 1.806, 1.794, 1.783,
  1.760, 1.721, 1.663, 1.583, 1.500, 1.424,
  1.351, 1.306, 1.277, 1.254, 1.234, 1.226,
  1.219, 1.205, 1.202, 1.199, 1.195, 1.183, 
  1.181
)

valid_mean_losses_t_no <- c(
  1.859, 1.855, 1.836, 1.826, 1.814, 1.797,
  1.772, 1.726, 1.679, 1.597, 1.560, 1.426,
  1.370, 1.326, 1.296, 1.310, 1.256, 1.245,
  1.245, 1.257, 1.237, 1.222, 1.203, 1.233, 
  1.222
)

history_t_no <- tibble(
  epochs = seq(1:length(train_mean_losses_t_no)),
  train_mean_loss = train_mean_losses_t_no,
  valid_mean_loss = valid_mean_losses_t_no,
  lr = rep(1e-5, length(train_mean_losses_t_no)),
  software = "torch 2",
  weight_decay = -99
)

train_mean_losses_t_mid <- c(
  2.598, 1.879, 1.882, 1.889, 1.900, 1.913, 1.919, 
  1.924, 1.929, 1.920, 1.914, 1.909, 1.914, 1.907, 
  1.912, 1.914, 1.909, 1.919, 1.905, 1.927, 1.919, 
  1.924, 1.936, 1.928, 1.930
)

valid_mean_losses_t_mid <-c(
  1.901, 1.893, 1.890, 1.891, 1.888, 1.886, 1.887, 
  1.899, 1.900, 1.889, 1.937, 1.933, 1.889, 1.923, 
  1.901, 1.890, 1.942, 1.916, 1.907, 1.921, 1.892, 
  1.901, 1.892, 1.974, 1.923
)

history_t_mid <- tibble(
  epochs = seq(1:length(train_mean_losses_t_mid)),
  train_mean_loss = train_mean_losses_t_mid,
  valid_mean_loss = valid_mean_losses_t_mid,
  lr = rep(1e-5, length(train_mean_losses_t_mid)),
  software = "torch 3",
  weight_decay = 1e-2
)

train_mean_losses_t_low <- c(
  2.026, 1.830, 1.817, 1.807, 1.796, 1.781, 1.758, 
  1.720, 1.659, 1.587, 1.496, 1.424, 1.359, 1.322, 
  1.295, 1.268, 1.254, 1.245, 1.234, 1.236, 1.219, 
  1.219, 1.212, 1.209, 1.203
)

valid_mean_losses_t_low <- c(
  1.860, 1.845, 1.837, 1.824, 1.813, 1.801, 1.771, 
  1.734, 1.648, 1.573, 1.505, 1.426, 1.360, 1.319, 
  1.307, 1.296, 1.264, 1.307, 1.240, 1.346, 1.234, 
  1.224, 1.227, 1.223, 1.215
)

history_t_low <- tibble(
  epochs = seq(1:length(train_mean_losses_t_low)),
  train_mean_loss = train_mean_losses_t_low,
  valid_mean_loss = valid_mean_losses_t_low,
  lr = rep(1e-5, length(train_mean_losses_t_low)),
  software = "torch 4",
  weight_decay = 1e-6
)

train_mean_losses_t_zero <- c(
  2.006, 1.830, 1.818, 1.807, 1.798, 1.781, 1.762, 
  1.719, 1.663, 1.580, 1.491, 1.406, 1.342, 1.296, 
  1.262, 1.249, 1.228, 1.222, 1.213, 1.199, 1.194, 
  1.193, 1.186, 1.184, 1.174
)

valid_mean_losses_t_zero <- c(
  1.857, 1.848, 1.834, 1.852, 1.819, 1.809, 1.789, 
  1.823, 1.647, 1.569, 1.475, 1.413, 1.357, 1.302, 
  1.384, 1.327, 1.257, 1.231, 1.231, 1.246, 1.244, 
  1.223, 1.330, 1.205, 1.229
)

history_t_zero <- tibble(
  epochs = seq(1:length(train_mean_losses_t_zero)),
  train_mean_loss = train_mean_losses_t_zero,
  valid_mean_loss = valid_mean_losses_t_zero,
  lr = rep(1e-5, length(train_mean_losses_t_zero)),
  software = "torch 5",
  weight_decay = 0
)

train_mean_losses_t_3 <- c(
  2.213, 1.870, 1.864, 1.867, 1.865, 1.867, 1.876, 
  1.872, 1.881, 1.883, 1.886, 1.879, 1.883, 1.881, 
  1.892, 1.892, 1.896, 1.892, 1.892, 1.900, 1.889, 
  1.908, 1.891, 1.912, 1.904
)

valid_mean_losses_t_3 <- c(
  1.892, 1.880, 1.878, 1.873, 1.889, 1.872, 1.876, 
  1.867, 1.885, 1.889, 1.867, 1.868, 1.870, 1.924, 
  1.879, 1.899, 1.877, 1.983, 1.935, 1.900, 1.956, 
  1.880, 1.892, 1.925, 1.962
)

history_t_3 <- tibble(
  epochs = seq(1:length(train_mean_losses_t_3)),
  train_mean_loss = train_mean_losses_t_3,
  valid_mean_loss = valid_mean_losses_t_3,
  lr = rep(1e-5, length(train_mean_losses_t_3)),
  software = "torch 6",
  weight_decay = 1e-3
)

train_mean_losses_t_5 <- c(
  1.992, 1.833, 1.825, 1.819, 1.814, 1.805, 1.796, 
  1.784, 1.767, 1.744, 1.710, 1.665, 1.612, 1.550, 
  1.489, 1.427, 1.378, 1.330, 1.296, 1.269, 1.251, 
  1.234, 1.220, 1.215, 1.207
)

valid_mean_losses_t_5 <- c(
  1.877, 1.853, 1.855, 1.840, 1.835, 1.827, 1.817, 
  1.806, 1.786, 1.760, 1.746, 1.678, 1.627, 1.564, 
  1.502, 1.433, 1.386, 1.345, 1.315, 1.301, 1.278, 
  1.254, 1.258, 1.264, 1.219
)

history_t_5 <- tibble(
  epochs = seq(1:length(train_mean_losses_t_5)),
  train_mean_loss = train_mean_losses_t_5,
  valid_mean_loss = valid_mean_losses_t_5,
  lr = rep(1e-5, length(train_mean_losses_t_5)),
  software = "torch 7",
  weight_decay = 1e-5
)

train_mean_losses_t_8 <- c(
  2.020, 1.832, 1.825, 1.824, 1.822, 1.820, 1.818, 
  1.813, 1.808, 1.801, 1.792, 1.781, 1.766, 1.745, 
  1.724, 1.695, 1.662, 1.619, 1.575, 1.527, 1.471, 
  1.434, 1.393, 1.357, 1.335
)

valid_mean_losses_t_8 <- c(
  1.861, 1.850, 1.852, 1.846, 1.845, 1.847, 1.840, 
  1.837, 1.830, 1.822, 1.817, 1.802, 1.793, 1.779, 
  1.744, 1.715, 1.678, 1.635, 1.587, 1.539, 1.485, 
  1.450, 1.385, 1.349, 1.339
)

history_t_8 <- tibble(
  epochs = seq(1:length(train_mean_losses_t_8)),
  train_mean_loss = train_mean_losses_t_8,
  valid_mean_loss = valid_mean_losses_t_8,
  lr = rep(1e-5, length(train_mean_losses_t_8)),
  software = "torch 8",
  weight_decay = 5e-5
)

full_join(history_k_reg, history_k_no) %>%
 full_join(., history_t_reg) %>%
  full_join(., history_t_no) %>% 
  full_join(., history_t_zero) %>% 
  full_join(., history_t_reg) %>% 
  full_join(., history_t_mid) %>% 
  full_join(., history_t_low) %>% 
  full_join(., history_t_3) %>% 
  full_join(., history_t_5) %>% 
  full_join(., history_t_8) %>% 
  pivot_longer(
    c(train_mean_loss, valid_mean_loss), 
    names_to = "set",
    values_to = "mse"
  ) -> comparison_df


# library(ggtext)
# 
# labels_software <- c(
#   #"keras 1" = "Keras w/ L2 reg", 
#   #"keras 2" = "Keras w/o L2 reg",
#   "torch 2" = "Torch w/o weight decay",
#   "torch 5" = "Torch w/ weight decay = 0",
#   "torch 3" = "Torch w/ weight decay = 1e-2",
#   "torch 1" = "Torch w/ weight decay = 1e-4",
#   "torch 4" = "Torch w/ weight decay = 1e-6"
# )

library(cowplot)
library(ggtext)

labels_keras <- c(
  "keras 1" = "1e-4",
  "keras 2" = " "
)

comparison_df %>% 
  filter(software %in% c("keras 1", "keras 2")) %>% 
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_line(size = 1, alpha = 0.5) +
  facet_grid(
    cols = vars(software),
    scales = "free_y",
    labeller = labeller(software = labels_keras)
  ) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epochs") +
  scale_y_continuous(name = "Error (RMSE)", limits = c(1.05, 1.65)) +
  theme_bw() +
  theme(strip.text = element_markdown(), legend.position = "none") +
  ggtitle("Keras w L2 regularization") -> plot_keras

plot_keras

save_plot("notes/keras_l2_reg.png", plot_keras,
          ncol = 1, nrow = 1, base_height = 3.71,
          base_asp = 1.618, base_width = NULL)


comparison_df %>% 
  filter(software %in% c("torch 1", "torch 2", "torch 3", 
                         "torch 4", "torch 5", "torch 6", 
                         "torch 7", "torch 8")) %>% 
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_line(size = 1, alpha = 0.5) +
  facet_grid(
    cols = vars(weight_decay),
    scales = "free_y"#,
    #labeller = labeller(software = labels_software)
  ) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epochs") +
  scale_y_continuous(name = "Error (RMSE)", limits = c(1.05, 1.65)) +
  theme_bw() +
  theme(strip.text = element_markdown(),
        legend.position = "top") +
  ggtitle("Torch w weight decay") -> plot_torch

plot_torch

save_plot("notes/torch_weight_decay.png", plot_torch,
          ncol = 1, nrow = 1, base_height = 5.71,
          base_asp = 1.618, base_width = NULL)

comparison_df %>% 
  filter(software == "keras 1") %>% 
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_line(size = 1, alpha = 0.5) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epochs") +
  scale_y_continuous(name = "Error (RMSE)", limits = c(1.05, 1.65)) +
  theme_bw() +
  theme(strip.text = element_markdown(), legend.position = "none") +
  ggtitle("Keras") -> plot_keras_min

plot_keras_min

comparison_df %>% 
  filter(software %in% c("torch 1", 
                         "torch 4", "torch 5", 
                         "torch 7", "torch 8")) %>% 
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 1.5, alpha = 0.5) +
  geom_line(size = 1, alpha = 0.5) +
  facet_grid(
    cols = vars(weight_decay),
    scales = "free_y"#,
    #labeller = labeller(software = labels_software)
  ) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epochs") +
  scale_y_continuous(name = "Error (RMSE)", limits = c(1.05, 1.65)) +
  theme_bw() +
  theme(strip.text = element_markdown()) +
  ggtitle("Torch") -> plot_torch_min

plot_torch_min

plot_grid(plot_keras_min, plot_torch_min, nrow = 1, rel_widths = c(1, 5)) -> plot_l2_weight

save_plot("notes/l2_vs_weight.png", plot_l2_weight,
          ncol = 1, nrow = 1, base_height = 5.71,
          base_asp = 1.618, base_width = NULL)


######### TRAINING ON FULL DATA W VARIABLE WEIGHT DECAY #########

load('/stor/home/mmj2238/genotype-alignment-information/notes/keras_all_29_epoch_1e-5_lr.RData')
load('/stor/home/mmj2238/genotype-alignment-information/notes/torch_full_29_epoch_1e-5_lr.RData')

history_full <- tibble(
  epoch = seq(1:29),
  keras_train_loss = history$metrics$loss,
  keras_val_loss = history$metrics$val_loss,
  torch_train_loss = history_torch$train_mean_losses,
  torch_val_loss = history_torch$valid_mean_losses,
  torch_2_train_loss = c(1.8589, 1.7543, 1.5239, 1.0378, 0.8158, 0.7219, 0.6702, 
                         0.6372, 0.6160, 0.5981, 0.5864, 0.5779, 0.5696, 0.5623, 
                         0.5559, 0.5502, 0.5465, 0.5409, 0.5365, 0.5314, 0.5289, 
                         0.5226, 0.5170, 0.5144, 0.5109, 0.5085, 0.5054, 0.5004, 0.4970),
  torch_2_val_loss = c(1.8160, 1.7333, 1.4647, 1.2428, 0.9275, 0.7822, 0.7139, 
                       0.6754, 0.6967, 0.6972, 0.7413, 0.7495, 0.6575, 0.6579, 
                       0.6369, 0.6364, 0.6176, 0.6156, 0.6114, 0.5919, 0.5912, 
                       0.6149, 0.6166, 0.6476, 0.6002, 0.6045, 0.5995, 0.5777, 0.6180),
  torch_3_train_loss = c(1.8663, 1.7513, 1.4770, 1.0004, 0.7968, 0.7120, 0.6649, 
                         0.6353, 0.6135, 0.6002, 0.5859, 0.5767, 0.5668, 0.5626, 
                         0.5553, 0.5500, 0.5426, 0.5413, 0.5352, 0.5309, 0.5271, 
                         0.5220, 0.5187, 0.5150, 0.5100, 0.5077, 0.5034, 0.5003, 0.4975),
  torch_3_val_loss = c(1.8088, 1.7037, 1.2180, 0.9762, 0.7754, 0.7330, 0.6848, 
                       0.7007, 0.6576, 0.6491, 0.6792, 0.6358, 0.6217, 0.6023, 
                       0.6153, 0.6972, 0.6383, 0.6272, 0.6047, 0.6010, 0.6061, 
                       0.6104, 0.6416, 0.6012, 0.5887, 0.5933, 0.5732, 0.5893, 0.5936)
) %>% 
  pivot_longer(-epoch, names_to = "set", values_to = "mse") %>% 
  mutate(software = case_when(set %in% c("keras_train_loss", "keras_val_loss") ~ "keras",
                              set %in% c("torch_train_loss", "torch_val_loss") ~ "torch",
                              set %in% c("torch_2_train_loss", "torch_2_val_loss") ~ "torch 2",
                              TRUE ~ "torch 3"),
         data = case_when(set %in% c("keras_train_loss", "torch_train_loss",
                                     "torch_2_train_loss", "torch_3_train_loss") ~ "train",
                          TRUE ~ "validation"))

labels_weight <- c(
  "keras" = "Keras w/ L2 reg",
  "torch" = "Torch w/ w = 1e-4",
  "torch 2" = "Torch w/ w = 1e-5",
  "torch 3" = "Torch 2 w/ val shuffle"
)


history_full %>% 
  ggplot(aes(x = epoch, y = sqrt(mse), color = data)) +
  geom_point() +
  geom_line() +
  facet_grid(
    rows = vars(software),
    labeller = labeller(software = labels_weight)
  ) + 
  theme_bw() -> fig_torch_keras_hist_full

fig_torch_keras_hist_full

save_plot("notes/torch_keras_hist_full.png", fig_torch_keras_hist_full,
          ncol = 1, nrow = 1, base_height = 5.71,
          base_asp = 1.618, base_width = NULL)

