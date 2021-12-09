#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   120,000 simulations per set (high/low)
#   Currently using PyTorch via torch 0.5.0
# This script: defines, compiles, trains, and saves model
# Mackenzie M. Johnson
# December 2021

#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(torch)
library(coro)
library(ggplot2)

# record session info
sessionInfo()

# set paths
path_to_data <- "/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/cnn_dup/"
path_to_results <- "/stor/home/mmj2238/genotype-alignment-information/results/"
path_to_models <- "/stor/work/Wilke/mmj2238/trained_models/dup_analysis"


#--------------- LOAD IN DATA --------------------

# load data saved from cnn_05_model_data_prep.R
# load(file.path(path_to_data, 'model_data_low_dup_all.RData'))
load(file.path(path_to_data, 'model_data_low_dup_unq.RData'))
# load(file.path(path_to_data, 'model_data_high_dup_all.RData'))
# load(file.path(path_to_data, 'model_data_high_dup_unq.RData'))


#--------------- GLOBAL PARAMETERS --------------------

# number of simulations in data set
# num_sims <- 120000   # for all
num_sims <- 118512   # for low unq
# num_sims <- 60000   # for high unq

# size of alignments
# number of samples
num_chrom <- 50

# number of sites
max_size <- 174   # for low
# max_size <- 27    # for high


#--------------- DATA TO TENSORS --------------------

# should specify dtype 
# NOTE: some cost functions require specific types of data
# error messages should clearly state if this is an issue

# alignments

align_test_tensor <- torch_tensor(
  low_align_unq_test,
  requires_grad = TRUE 
)

rm(low_align_unq_test)

# positions
pos_test_tensor <- torch_tensor(
  low_pos_unq_test,
  requires_grad = TRUE 
)

rm(low_pos_unq_test)

# rhos
rho_test_tensor <- torch_tensor(
  low_rho_unq_test_centered,
  requires_grad = TRUE
)

rm(low_rho_unq_test, low_rho_unq_test_centered)


#--------------- DATA TENSORS TO DATA SETS --------------------

# data need to be combined into datasets for use with dataloaders
test_ds <- tensor_dataset(
  data_align = align_test_tensor, 
  data_pos = pos_test_tensor, 
  data_rho = rho_test_tensor
)


#--------------- DATA SETS TO DATA LOADERS --------------------

# need to transform data sets to loaders for use in batches
test_dl <- test_ds %>% dataloader(batch_size = 32, shuffle = FALSE)

#--------------- RELOAD MODEL --------------------



#--------------- VISUALIZE TRAINING --------------------

history_torch <- tibble(
  epochs = seq(1:length(train_mean_losses)),
  train_mean_losses,
  valid_mean_losses
) 

history_torch %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = sqrt(train_mean_losses)), color = "purple") +
  geom_point(aes(y = sqrt(valid_mean_losses)), color = "orange") +
  geom_path(aes(y = sqrt(train_mean_losses)), color = "purple") +
  geom_path(aes(y = sqrt(valid_mean_losses)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)")

history_torch %>% 
  ggplot2::ggplot() +
  geom_point(aes(x = epochs, y = sqrt(train_mean_losses)), color = "purple") +
  geom_point(aes(x = epochs+0.5, y = sqrt(valid_mean_losses)), color = "orange") +
  geom_path(aes(x = epochs, y = sqrt(train_mean_losses)), color = "purple") +
  geom_path(aes(x = epochs+0.5, y = sqrt(valid_mean_losses)), color = "orange") +
  theme_bw() +
  ylab("Loss (RMSE)")


# #--------------- EVALUATE MODEL --------------------
# 
# model$eval()
# 
# test_dl <- test_ds %>% dataloader(batch_size = test_ds$.length(), shuffle = FALSE)
# iter <- test_dl$.iter()
# b <- iter$.next()
# test_loss <- c()
# 
# output <- model(b[[1]], b[[2]])
# output <- torch_squeeze(output, 2)
# test_loss <- nnf_mse_loss(output, b[[3]])
# test_loss <- c(test_loss, test_loss$item())
# preds <- output %>% as.array()
# 
# actual <- test_ds$tensors$data_rho %>% as_array()
# 
# performance_rho <- tibble(
#   sample = seq(1:length(actual)),
#   rho_train_prediction = preds,
#   rho_actual = actual
# )
# 
# performance_rho %>% 
#   ggplot(aes(x = rho_train_prediction, y = rho_actual)) +
#   geom_point()
# 
# 
# library(caret)
# caret::postResample(
#   pred = preds, 
#   obs = actual
# ) -> r2_results_torch
# 
# # RMSE           Rsquared       MAE 
# # 1.1105597      0.3436091      0.9062051
# 
# performance_rho -> performance_torch
# history -> history_torch
# save(
#   history_torch, performance_torch,
#   r2_results_torch,
#   file = file.path(
#     '/stor/home/mmj2238/genotype-alignment-information/notes', 
#     'torch_subset_25_epoch_1e-5_lr.RData')
# )


# test_batch <- function(b) {
#   
#   output <- model(b[[1]], b[[2]])
#   output <- torch_squeeze(output, 2)
#   labels <- b[[3]]
#   loss <- nnf_mse_loss(output, labels)
#   
#   test_losses <<- c(test_losses, loss$item())
#   # torch_max returns a list, with position 1 containing the values
#   # and position 2 containing the respective indices
#   predicted <- output %>% as_array()
#   
# }
# 
# test_losses <- c()
# 
# for (b in enumerate(test_dl)) {
#   test_batch(b)
# }
# 
# # warning message from above
# # The `enumerate` construct is deprecated in favor of the `coro::loop` syntax.
# # * See https://github.com/mlverse/torch/issues/558 for more information. 
# 
# mean(test_losses) #1.883575


#--------------- SAVE MODEL --------------------

# save training history
save(
    history_torch,
    file = file.path(
      path_to_results, 'models',
      'torch_cnn_hist_low_dup_unq_18_epoch_1e-5_lr_1e-4_l2.RData')
  )

# save the model
model$eval()

torch_save(
  model,
  file.path(
    path_to_models,
    "torch_cnn_low_dup_unq_18_epoch_1e-5_lr_1e-4_l2.rt"
  )
)

#' 
#' # 
#' # reload_model <- torch_load(file.path(path_to_models, "torch_model_hist_low_dup_all_150.rt"))
#' # 
#' # reload_model(align_test_tensor, pos_test_tensor)


#' #--------------- SAVE MODEL PERFORMANCE --------------------
#' 
#' rho_train_prediction <- rho_pred %>% as_array()
#' rho_actual <- rho_train_tensor %>% as_array()
#' 
#' performance_rho <- tibble(
#'   sample = seq(1:72000),
#'   rho_train_prediction,
#'   rho_actual
#' )
#' 
#' performance_rho %>% 
#'   ggplot(aes(x = rho_train_prediction, y = rho_actual)) +
#'   geom_point()
#' 
#' # performance_rho %>% 
#' #   ggplot(aes(x = rho_actual, y = rho_actual)) +
#' #   geom_point()
#' 
#' performance_rho %>% 
#'   ggplot(aes(x = rho_actual)) +
#'   geom_density()
#' 
#' library(caret)
#' caret::postResample(
#'   pred = rho_train_prediction, 
#'   obs = rho_actual
#' )
#' 
#' # save training history
#' save(
#'   performance_rho,
#'   file = file.path(
#'     path_to_results, 
#'     'models', 
#'     #'torch_model_results_low_dup_all_25_epoch_1e-4_lr.RData')
#' )

