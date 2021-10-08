
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
train_mean_losses_t2 <- c(3.30, 2.56, 2.07, 1.95, 1.92, 1.90, 1.89, 1.88, 
                          1.87, 1.86, 1.86, 1.85, 1.85, 1.85, 1.84, 1.84, 
                          1.84, 1.84, 1.84, 1.84, 1.84, 1.83, 1.83, 1.83, 
                          1.83)

valid_mean_losses_t2 <- c(2.95, 2.25, 2.02, 1.97, 1.95, 1.94, 1.93, 1.92, 
                          1.91, 1.91, 1.90, 1.89, 1.89, 1.89, 1.88, 1.88, 
                          1.88, 1.88, 1.87, 1.87, 1.87, 1.87, 1.87, 1.87, 
                          1.87)

history_t2 <- tibble(
  epochs = seq(1:length(train_mean_losses_t2)),
  train_mean_loss = train_mean_losses_t2,
  valid_mean_loss = valid_mean_losses_t2,
  lr = rep(1e-6, length(train_mean_losses_t2)),
  software = "torch"
) 

# 25 epoch, lr = 1-6 - shuffle = TRUE
train_mean_losses_t3 <- c(3.16, 2.33, 1.96, 1.91, 1.90, 1.88, 1.87, 1.87, 
                          1.86, 1.86, 1.85, 1.85, 1.85, 1.85)
valid_mean_losses_t3 <- c(2.72, 2.06, 1.94, 1.92, 1.91, 1.90, 1.89, 1.88,
                          1.88, 1.88, 1.87, 1.87, 1.87, 1.87)

history_t3 <- tibble(
  epochs = seq(1:length(train_mean_losses_t3)),
  train_mean_loss = train_mean_losses_t3,
  valid_mean_loss = valid_mean_losses_t3,
  lr = rep(1e-6, length(train_mean_losses_t3)),
  software = "torch"
) 


history_k1 %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_point(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  geom_line(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_line(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)") +
  ggtitle("Keras model with learning rate = 1e-4")

history_k2 %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_point(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  geom_line(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_line(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)") +
  ggtitle("Keras model with learning rate = 1e-6")

history_t1 %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_point(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  geom_line(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_line(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)") +
  ggtitle("Torch model with learning rate = 1e-4")

history_t2 %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_point(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  geom_line(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_line(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)") +
  ggtitle("Torch model with learning rate = 1e-6")

history_t3 %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_point(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  geom_line(aes(y = sqrt(train_mean_loss)), color = "purple") +
  geom_line(aes(y = sqrt(valid_mean_loss)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)") +
  ggtitle("Torch model with learning rate = 1e-6 and shuffling")


history_all <- full_join(history_k1, history_k2) %>% 
  full_join(., history_t1) %>% 
  full_join(., history_t2) %>% 
  #full_join(., history_t3) %>% 
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
  "1e-06" = "lr = 1e-6"
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

save_plot("torch_keras_comparison.png", fig_torch_keras,
          ncol = 1, nrow = 1, base_height = 3.71,
          base_asp = 1.618, base_width = NULL)
