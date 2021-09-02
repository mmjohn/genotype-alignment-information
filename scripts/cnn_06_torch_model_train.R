#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   60,000 simulations per set
#   Currently using PyTorch via torch 0.5.0
# This script: defines, compiles, trains, and saves model
# Mackenzie M. Johnson
# August 2021

#--------------- CONFIGURE ENVIRONMENT --------------------
Sys.time()
cat("\nConfiguring environment.....\n")

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(glue)
library(torch)

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in dataset
num_sims <- 60000   # for all
# num_sims <- 60000   # for low unq
# num_sims <- 60000   # for high unq

# paths for script
path_to_data <- "/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/cnn_dup/"
path_to_results <- "/stor/home/mmj2238/genotype-alignment-information/results/"
path_to_models <- "/stor/work/Wilke/mmj2238/trained_models/dup_analysis"

# size of alignments
num_chrom <- 50


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
low_align_all_train_tensor <- torch_tensor(
  low_align_all_train,
  requires_grad = TRUE # this is required for nn training; tracks computations to calc. deriv.
)

low_align_all_val_tensor <- torch_tensor(
  low_align_all_val,
  requires_grad = TRUE 
)

low_align_all_test_tensor <- torch_tensor(
  low_align_all_test,
  requires_grad = TRUE 
)

rm(low_align_all_train, low_align_all_val, low_align_all_test)

# positions
low_pos_all_train_tensor <- torch_tensor(
  low_pos_all_train,
  requires_grad = TRUE 
)

low_pos_all_val_tensor <- torch_tensor(
  low_pos_all_val,
  requires_grad = TRUE 
)

low_pos_all_test_tensor <- torch_tensor(
  low_pos_all_test,
  requires_grad = TRUE 
)

rm(low_pos_all_train, low_pos_all_val, low_pos_all_test)

# rhos
low_rho_all_train_tensor <- torch_tensor(
  low_rho_all_train_centered,
  requires_grad = TRUE
)

low_rho_all_val_tensor <- torch_tensor(
  low_rho_all_val_centered,
  requires_grad = TRUE
)

low_rho_all_test_tensor <- torch_tensor(
  low_rho_all_test_centered,
  requires_grad = TRUE
)

rm(low_rho_all_train, low_rho_all_val, low_rho_all_test, low_rho_all_train_centered, 
   low_rho_all_val_centered, low_rho_all_test_centered)


#--------------- DEFINE MODEL --------------------

# create model with nn_module()

# Flagel model:
# NOTE: in Flagel code, first set of avg pooling and dropout is missing, but it's reported in paper

# need to add regularizers

flagel_cnn <- nn_module(
  "class_net",

  initialize = function() {

    #branch 1 (alignment CNN)
    self$conv1 <- nn_conv1d(
      in_channels = 106,                    
      out_channels = 1250,
      kernel_size = 2
    )
    self$dropout1 <- nn_dropout(0.25)
    self$conv2 <- nn_conv1d(
      in_channels = 1250,
      out_channels = 256,                  
      kernel_size = 2
    )
    self$dropout2 <- nn_dropout(0.25)
    self$conv3 <- nn_conv1d(
      in_channels = 256,
      out_channels = 256,                   
      kernel_size = 2
    )
    self$dropout3 <- nn_dropout(0.25)

    # branch 2 (position fc)
    self$fc1 <- nn_linear(
      in_features = 106,
      out_features = 64
    ) # dense in keras = linear in torch
    self$dropout3 <- nn_dropout(0.1)

    # full model (fc)
    self$fc2 <- nn_linear(
      in_features = 64 + 256,                 # CHECK
      out_features = 256
    ) 
    self$fc3 <- nn_linear(
      in_features = 256,
      out_features = 1
    )

  },

  forward = function(data_align, data_pos) {

    #branch 1 (alignment CNN)
    branch1 <- data_align %>%
      self$conv1() %>%
      nnf_relu() %>%
      nnf_avg_pool1d(kernel_size = 2) %>%   # MISSING IN FLAGEL CODE
      self$dropout1() %>%                   # MISSING IN FLAGEL CODE
      self$conv2() %>%
      nnf_relu() %>%
      nnf_avg_pool1d(kernel_size = 2) %>%
      self$dropout2() %>%
      self$conv3() %>%
      nnf_relu() %>%
      nnf_avg_pool1d(kernel_size = 2) %>%
      self$dropout3() %>%
      torch_flatten(start_dim = 2)                       # ISSUE??

    # branch 2 (position fc)
    branch2 <- data_pos %>% 
      self$fc1() %>% 
      self$dropout3()

    # full model (fc)
    full_model <- torch_cat(list(branch1, branch2), dim = 2)  # ISSUE??
    
    full_model %>% 
      self$fc2() %>% 
      self$fc3()

  }
)

model <- flagel_cnn()

model$forward(low_align_all_train_tensor, low_pos_all_train_tensor)


#--------------- NETWORK PARAMETERS --------------------



#--------------- TRAIN MODEL --------------------

# forward pass

# compute loss

# backpropagation

# update weights


#--------------- SAVE MODEL --------------------

# save training history
save(
  history,
  #file = file.path(path_to_results, 'models', 'cnn_model_hist_low_dup_all_60.RData')
)

# save the model 



