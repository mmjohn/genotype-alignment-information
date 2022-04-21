#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   Modified model from Flagel et al. (2018) - remove branch with position information
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
library(glue)
library(torch)
library(coro)
library(ggplot2)

# record session info
sessionInfo()

# set paths
path_to_data <- "/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/cnn_dup/"
path_to_results <- "/stor/home/mmj2238/genotype-alignment-information/results/"
path_to_models <- "/stor/work/Wilke/mmj2238/trained_models/dup_analysis"

# define data set
sim_set <- "high"
# sim_set <- "low"

#--------------- LOAD IN DATA --------------------

# load data saved from cnn_05_model_data_prep.R
load(file.path(path_to_data, glue('model_data_{sim_set}_dup_all.RData')))


#--------------- DATA TO TENSORS --------------------

# should specify dtype 
# NOTE: some cost functions require specific types of data
# error messages should clearly state if this is an issue

# alignments
if (sim_set == "low") {
  # test set
  align_test_tensor <- torch_tensor(
    low_align_all_test,
    requires_grad = TRUE 
  )
  # clean up
  rm(low_align_all_train, low_align_all_val, low_align_all_test)
} else {
  # test set
  align_test_tensor <- torch_tensor(
    high_align_all_test,
    requires_grad = TRUE 
  )
  # clean up
  rm(high_align_all_train, high_align_all_val, high_align_all_test)
  
}

# positions
if (sim_set == "low") {
  # remove positional vectors - not used with this model
  rm(low_pos_all_train, low_pos_all_val, low_pos_all_test)
} else {
  # remove
  rm(high_pos_all_train, high_pos_all_val, high_pos_all_test)
}

# rhos
if (sim_set == "low") {
  # test set
  rho_test_tensor <- torch_tensor(
    low_rho_all_test_centered,
    requires_grad = TRUE
  )
  # clean up
  rm(low_rho_all_train, low_rho_all_val, low_rho_all_test, low_rho_all_train_centered,
     low_rho_all_val_centered, low_rho_all_test_centered)
} else {
  # test set
  rho_test_tensor <- torch_tensor(
    high_rho_all_test_centered,
    requires_grad = TRUE
  )
  # clean up
  rm(high_rho_all_train, high_rho_all_val, high_rho_all_test, high_rho_all_train_centered,
     high_rho_all_val_centered, high_rho_all_test_centered)
}


#--------------- DATA TENSORS TO DATA SETS --------------------

# data need to be combined into datasets for use with dataloaders
test_ds <- tensor_dataset(
  data_align = align_test_tensor, 
  data_rho = rho_test_tensor
)


#--------------- DATA SETS TO DATA LOADERS --------------------

# need to transform data sets to loaders for use in batches
test_dl <- test_ds %>% dataloader(batch_size = 32, shuffle = FALSE)

#--------------- RELOAD MODEL --------------------

if (sim_set == "low") {
  # load model
  trained_model <- torch_load(
    file.path(path_to_models, "torch_cnn_alt_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.rt")
  )
} else {
  # load model
  trained_model <- torch_load(
    file.path(path_to_models, "torch_cnn_alt_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.rt")
  )
}

#--------------- EVALUATE MODEL --------------------

# test model function
test_batch <- function(b) {
  
  output <- trained_model(b[[1]])   
  output <- torch_squeeze(output, 2)
  labels <- b[[2]]
  loss <- nnf_mse_loss(output, labels)
  
  test_losses_all <<- c(test_losses_all, loss$item())
  predicted <- output %>% as_array()
  rho_predictions <<- c(rho_predictions, predicted)
  
}

test_losses_all <- c()
rho_predictions <- c()

# run all batches through model test
coro::loop(for (b in test_dl) {
  test_batch(b)
})

# record mean loss
mean_test_loss <- mean(test_losses_all) # 0.6545834 for low, 1.810497 for high

# get actual rho values
actual <- test_ds$tensors$data_rho %>% as_array()

# evaluate performance
if (sim_set == "low") {
  # make a data frame
  performance_rho_low_all <- tibble(
    sample = seq(1:length(actual)),
    rho_predict = rho_predictions,
    rho_actual = actual
  )
  # visualize actual vs estimate
  performance_rho_low_all %>%
    ggplot(aes(x = rho_predict, y = rho_actual)) +
    geom_point() +
    geom_abline(slope = 1, color = "blue", size = 2)
} else {
  # make a data frame
  performance_rho_high_all <- tibble(
    sample = seq(1:length(actual)),
    rho_predict = rho_predictions,
    rho_actual = actual
  )
  # visualize actual vs estimate
  performance_rho_high_all %>%
    ggplot(aes(x = rho_predict, y = rho_actual)) +
    geom_point() +
    geom_abline(slope = 1, color = "blue", size = 2)
}

library(caret)

if (sim_set == "low") {
  # get r2 results
  r2_results_low_all <- caret::postResample(
    pred = rho_predictions,
    obs = actual
  )
  # print results
  r2_results_low_all
} else {
  # get r2 results
  r2_results_high_all <- caret::postResample(
    pred = rho_predictions,
    obs = actual
  )
  # print results
  r2_results_high_all
}

# low duplicate dataset
# RMSE        Rsquared      MAE 
# 0.8090633   0.7813444     0.6136017 

# high duplicate dataset
# RMSE        Rsquared      MAE 
# 1.3455469   0.3828434     1.1180245 


#--------------- SAVE MODEL PERFORMANCE --------------------

if (sim_set == "low") {
  # save performance data
  save(
    test_losses_all, performance_rho_low_all,
    r2_results_low_all,
    file = file.path(
      path_to_results,
      'models',
      'torch_cnn_results_alt_low_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData')
  )
} else {
  # save performance data
  save(
    test_losses_all, performance_rho_high_all,
    r2_results_high_all,
    file = file.path(
      path_to_results,
      'models',
      'torch_cnn_results_alt_high_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData')
  )
}


