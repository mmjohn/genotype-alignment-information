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
load(file.path(path_to_data, 'model_data_low_dup_all.RData'))
# load(file.path(path_to_data, 'model_data_low_dup_unq.RData'))
# load(file.path(path_to_data, 'model_data_high_dup_all.RData'))
# load(file.path(path_to_data, 'model_data_high_dup_unq.RData'))


#--------------- DATA TO TENSORS --------------------

# should specify dtype 
# NOTE: some cost functions require specific types of data
# error messages should clearly state if this is an issue

# alignments
align_test_tensor <- torch_tensor(
  low_align_all_test,
  #low_align_unq_test,
  requires_grad = TRUE 
)

rm(low_align_all_train, low_align_all_val, low_align_all_test)
#rm(low_align_unq_train, low_align_unq_val, low_align_unq_test)

# positions
pos_test_tensor <- torch_tensor(
  low_pos_all_test,
  #low_pos_unq_test,
  requires_grad = TRUE 
)

rm(low_pos_all_train, low_pos_all_val, low_pos_all_test)
#rm(low_pos_unq_train, low_pos_unq_val, low_pos_unq_test)

# rhos
rho_test_tensor <- torch_tensor(
  low_rho_all_test_centered,
  #low_rho_unq_test_centered,
  requires_grad = TRUE
)

rm(low_rho_all_train, low_rho_all_val, low_rho_all_test, low_rho_all_train_centered, 
   low_rho_all_val_centered, low_rho_all_test_centered)
# rm(low_rho_unq_train, low_rho_unq_val, low_rho_unq_test, low_rho_unq_train_centered, 
#    low_rho_unq_val_centered, low_rho_unq_test_centered)


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

low_all_model <- torch_load(file.path(path_to_models, "torch_cnn_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.rt"))

#low_unq_model <- torch_load(file.path(path_to_models, "torch_cnn_low_dup_unq_18_epoch_1e-5_lr_1e-4_l2.rt"))


#--------------- EVALUATE MODEL --------------------

# low all model
test_batch <- function(b) {
  
  output <- low_all_model(b[[1]], b[[2]])
  output <- torch_squeeze(output, 2)
  labels <- b[[3]]
  loss <- nnf_mse_loss(output, labels)
  
  test_losses_all <<- c(test_losses_all, loss$item())
  predicted <- output %>% as_array()
  rho_predictions <<- c(rho_predictions, predicted)
  
}

test_losses_all <- c()
rho_predictions <- c()

coro::loop(for (b in test_dl) {
  test_batch(b)
})

mean(test_losses_all) # 0.6376667

actual <- test_ds$tensors$data_rho %>% as_array()

performance_rho_low_all <- tibble(
  sample = seq(1:length(actual)),
  rho_predict = rho_predictions,
  rho_actual = actual
)

performance_rho_low_all %>%
  ggplot(aes(x = rho_predict, y = rho_actual)) +
  geom_point() +
  geom_abline(slope = 1, color = "blue", size = 2)


library(caret)
caret::postResample(
  pred = rho_predictions,
  obs = actual
) -> r2_results_low_all

# RMSE           Rsquared       MAE
# 0.7985404      0.7899489      0.6052439

# low unique model
test_batch <- function(b) {

  output <- low_unq_model(b[[1]], b[[2]])
  output <- torch_squeeze(output, 2)
  labels <- b[[3]]
  loss <- nnf_mse_loss(output, labels)

  test_losses <<- c(test_losses, loss$item())
  predicted <- output %>% as_array()
  rho_predictions <<- c(rho_predictions, predicted)

}

test_losses <- c()
rho_predictions <- c()

coro::loop(for (b in test_dl) {
  test_batch(b)
})

mean(test_losses) # 0.6651793

actual <- test_ds$tensors$data_rho %>% as_array()

performance_rho_low_unq <- tibble(
  sample = seq(1:length(actual)),
  rho_predict = rho_predictions,
  rho_actual = actual
)

performance_rho_low_unq %>%
  ggplot(aes(x = rho_predict, y = rho_actual)) +
  geom_point() +
  geom_abline(slope = 1, color = "blue", size = 2)


library(caret)
caret::postResample(
  pred = rho_predictions,
  obs = actual
) -> r2_results_low_unq

# RMSE           Rsquared       MAE
# 0.8155668      0.7833350      0.6173474 


#--------------- SAVE MODEL PERFORMANCE --------------------

# low all
save(
  test_losses_all, performance_rho_low_all,
  r2_results_low_all,
  file = file.path(
    path_to_results,
    'models',
    'torch_cnn_results_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData')
)

# low unq
save(
  test_losses, performance_rho_low_unq,
  r2_results_low_unq,
  file = file.path(
    path_to_results,
    'models',
    'torch_cnn_results_low_dup_unq_18_epoch_1e-5_lr_1e-4_l2.RData')
)

