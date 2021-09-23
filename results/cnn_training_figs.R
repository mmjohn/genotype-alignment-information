#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of CNN performance in training on training and validation sets
# for a variety of learning rates
# This script: creates figures for CNN performance in training
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


#--------------- GLOBAL PARAMETERS --------------------

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50


#--------------- LOAD DATA SETS --------------------

# torch long runs under different learning rates
load(
  file.path(
    path_to_results, 
    'models',
    'torch_model_hist_low_dup_all_150_epoch_1e-2_lr.RData'
  )
)
hist_1 <- history
rm(history)

load(
  file.path(
    path_to_results,  
    'models',
    'torch_model_hist_low_dup_all_150_epoch_1e-3_lr.RData'
  )
)
hist_2 <- history
rm(history)

load(
  file.path(
    path_to_results,  
    'models',
    'torch_model_hist_low_dup_all_150_epoch_1e-4_lr.RData'
  )
)
hist_3 <- history
rm(history)

load(
  file.path(
    path_to_results,  
    'models',
    'torch_model_hist_low_dup_all_150_epoch_1e-5_lr.RData'
  )
)
hist_4 <- history
rm(history)

load(
  file.path(
    path_to_results,  
    'models',
    'torch_model_hist_low_dup_all_150_epoch_1e-6_lr.RData'
  )
)
hist_5 <- history
rm(history)


#--------------- TIDY DATA --------------------

# torch long runs under different learning rates
hist_1 %>% 
  pivot_longer(train_losses:valid_losses, "set", "mse") %>% 
  mutate(lr = 1e-2) -> hist_1

hist_2 %>% 
  pivot_longer(train_losses:valid_losses, "set", "mse") %>% 
  mutate(lr = 1e-3) -> hist_2

hist_3 %>% 
  pivot_longer(train_losses:valid_losses, "set", "mse") %>% 
  mutate(lr = 1e-4) -> hist_3

hist_4 %>% 
  pivot_longer(train_losses:valid_losses, "set", "mse") %>% 
  mutate(lr = 1e-5) -> hist_4

hist_5 %>% 
  pivot_longer(train_losses:valid_losses, "set", "mse") %>% 
  mutate(lr = 1e-6) -> hist_5

lr_train_hist <- full_join(hist_1, hist_2) %>% 
  full_join(., hist_3) %>% 
  full_join(., hist_4) %>% 
  full_join(., hist_5)


#--------------- CNN TRAINING FIGURES --------------------

lr_train_hist %>% 
  ggplot(aes(x = epochs, y = sqrt(value), color = set)) +
  geom_point(size = 2) +
  geom_path(size = 1.75) +
  facet_grid(vars(lr), scales = "free_y") +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epoch") +
  scale_y_continuous(name = "RMSE") +
  theme_half_open() +
  background_grid() -> fig_train_lr

lr_train_hist %>% 
  filter(lr == 1e-4) %>% 
  ggplot(aes(x = epochs, y = sqrt(value), color = set)) +
  geom_point(size = 2) +
  geom_path(size = 1.75) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epoch") +
  scale_y_continuous(name = "RMSE") +
  theme_half_open() +
  background_grid()


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'train_vary_learn_rate.png'),
  fig_train_lr, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

