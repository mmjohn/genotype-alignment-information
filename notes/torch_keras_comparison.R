
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




