
library(tidyverse)

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
train_mean_losses_t1 <- c(1.82, 1.79, 1.79, 1.80, 1.81, 1.81, 1.82, 1.82, 
                          1.83, 1.83, 1.83, 1.84, 1.84, 1.84, 1.85, 1.85, 
                          1.85, 1.86, 1.87, 1.88, 1.90, 1.91, 1.92, 1.93, 
                          1.94)
valid_mean_losses_t1 <- c(1.95, 2.04, 2.25, 2.48, 2.60, 2.73, 2.99, 3.07, 
                          3.14, 3.04, 3.31, 3.24, 3.25, 3.34, 3.61, 3.66, 
                          3.26, 3.41, 4.02, 5.04, 5.46, 5.65, 5.73, 5.94, 
                          5.80)

history_t1 <- tibble(
  epochs = seq(1:length(train_mean_losses_t1)),
  train_mean_loss = train_mean_losses_t1,
  valid_mean_loss = valid_mean_losses_t1,
  lr = rep(1e-4, length(train_mean_losses_t1)),
  software = "torch"
) 

# 25 epoch, lr = 1-6
train_mean_losses_t2 <- c(3.33, 2.47, 2.00, 1.92, 1.90, 1.89, 1.88, 1.87, 
                          1.86, 1.86, 1.85, 1.85, 1.85, 1.85, 1.84, 1.84, 
                          1.84, 1.84, 1.84, 1.84, 1.84, 1.84, 1.84, 1.84, 
                          1.84)
  
  # c(3.30, 2.56, 2.07, 1.95, 1.92, 1.90, 1.89, 1.88, 1.87, 1.86, 1.86, 1.85, 1.85, 1.85, 1.84, 1.84, 
  #  1.84, 1.84, 1.84, 1.84, 1.84, 1.83, 1.83, 1.83, 1.83)

valid_mean_losses_t2 <- c(2.90, 2.14, 1.95, 1.93, 1.91, 1.90, 1.89, 1.88, 
                          1.88, 1.87, 1.87, 1.87, 1.87, 1.86, 1.86, 1.86, 
                          1.86, 1.86, 1.86, 1.86, 1.86, 1.86, 1.86, 1.86, 
                          1.86)
  
  # c(2.95, 2.25, 2.02, 1.97, 1.95, 1.94, 1.93, 1.92, 1.91, 1.91, 1.90, 1.89, 1.89, 1.89, 1.88, 1.88, 
  #  1.88, 1.88, 1.87, 1.87, 1.87, 1.87, 1.87, 1.87, 1.87)

history_t2 <- tibble(
  epochs = seq(1:length(train_mean_losses_t2)),
  train_mean_loss = train_mean_losses_t2,
  valid_mean_loss = valid_mean_losses_t2,
  lr = rep(1e-6, length(train_mean_losses_t2)),
  software = "torch"
) 

# 25 epoch, lr = 1e-5
train_mean_losses_t3 <- c(2.05, 1.85, 1.84, 1.83, 1.83, 1.83, 1.83, 1.82, 
                          1.82, 1.82, 1.81, 1.81, 1.80, 1.80, 1.80, 1.80, 
                          1.79, 1.79, 1.78, 1.78, 1.78, 1.77, 1.77, 1.76, 
                          1.76)
valid_mean_losses_t3 <- c(1.88, 1.86, 1.85, 1.86, 1.86, 1.85, 1.85, 1.84, 
                          1.84, 1.84, 1.84, 1.83, 1.83, 1.83, 1.84, 1.82, 
                          1.82, 1.82, 1.81, 1.81, 1.83, 1.81, 1.82, 1.79, 
                          1.79)

history_t3 <- tibble(
  epochs = seq(1:length(train_mean_losses_t3)),
  train_mean_loss = train_mean_losses_t3,
  valid_mean_loss = valid_mean_losses_t3,
  lr = rep(1e-5, length(train_mean_losses_t3)),
  software = "torch"
) 

# 25 epoch, lr = 1e-7
train_mean_losses_t4 <- c(3.68, 3.60, 3.52, 3.44, 3.36, 3.28, 3.20, 3.12, 
                          3.04, 2.95, 2.87, 2.79, 2.71, 2.63, 2.55, 2.47, 
                          2.40, 2.33, 2.27, 2.20, 2.15, 2.11, 2.07, 2.03, 
                          2.00)

valid_mean_losses_t4 <- c(3.65, 3.57, 3.49, 3.41, 3.33, 3.25, 3.17, 3.09, 
                          3.01, 2.92, 2.84, 2.76, 2.68, 2.60, 2.52, 2.45, 
                          2.38, 2.31, 2.25, 2.19, 2.14, 2.10, 2.06, 2.03, 
                          2.00)

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

library(ggtext)

history_all %>% 
  ggplot(aes(x = epochs, y = sqrt(mse), color = set)) +
  geom_point(size = 2) +
  geom_line(size = 1.5) +
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

save_plot("torch_keras_comparison.png", fig_torch_keras,
          ncol = 1, nrow = 1, base_height = 3.71,
          base_asp = 1.618, base_width = NULL)
