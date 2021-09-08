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
#library(glue)
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
align_train_tensor <- torch_tensor(
  low_align_all_train,
  requires_grad = TRUE # this is required for nn training; tracks computations to calc. deriv.
)

align_val_tensor <- torch_tensor(
  low_align_all_val,
  requires_grad = TRUE 
)

align_test_tensor <- torch_tensor(
  low_align_all_test,
  requires_grad = TRUE 
)

rm(low_align_all_train, low_align_all_val, low_align_all_test)

# positions
pos_train_tensor <- torch_tensor(
  low_pos_all_train,
  requires_grad = TRUE 
)

pos_val_tensor <- torch_tensor(
  low_pos_all_val,
  requires_grad = TRUE 
)

pos_test_tensor <- torch_tensor(
  low_pos_all_test,
  requires_grad = TRUE 
)

rm(low_pos_all_train, low_pos_all_val, low_pos_all_test)

# rhos
rho_train_tensor <- torch_tensor(
  low_rho_all_train_centered,
  requires_grad = TRUE
)

rho_val_tensor <- torch_tensor(
  low_rho_all_val_centered,
  requires_grad = TRUE
)

rho_test_tensor <- torch_tensor(
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
      in_channels = 106,            # max number of sites        
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
      in_features = 106,          # max number of sites
      out_features = 64
    )                             # dense in keras = linear in torch
    self$dropout3 <- nn_dropout(0.1)

    # full model (fc)
    self$fc2 <- nn_linear(
      in_features = 1344,             
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
      torch_flatten(start_dim = 2) 

    # branch 2 (position fc)
    branch2 <- data_pos %>% 
      self$fc1() %>% 
      self$dropout3()

    # full model (fc)
    full_model <- torch_cat(list(branch1, branch2), dim = 2)
    
    full_model %>% 
      self$fc2() %>% 
      self$fc3()

  }
)

model <- flagel_cnn()

#model$forward(align_train_tensor, pos_train_tensor)


#--------------- NETWORK PARAMETERS --------------------

# set l2 regularization parameter - WHERE DOES THIS GO?
l2_lambda <- 0.0001

# set learning rate for optimizer
learning_rate <- 0.000001        #0.08

# define optimizer
optimizer <- optim_adam(model$parameters, lr = learning_rate)

# define number of epochs for training
epochs <- 10

# NEED TO ADD BATCHES AND VALIDATION


#--------------- TRAIN MODEL --------------------

# examples: 
# https://blogs.rstudio.com/ai/posts/2020-11-03-torch-tabular/
# https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/

# training loop w validation
for (t in 1:epochs) {
  
  # -------- TRAIN --------
  
  model$train()
  train_losses <- c()
  
  # -------- forward pass -------- 
  
  # make prediction in current model state
  rho_pred <- model(align_train_tensor, pos_train_tensor)
  # reshape to match dimensions of target data
  rho_pred <- torch_squeeze(rho_pred, 2)
  
  # -------- compute loss -------- 
  
  # use mse loss to evaluation model prediction
  loss <- nnf_mse_loss(rho_pred, rho_train_tensor)
  
  #-------- backpropagation -------- 
  
  # zero out the gradients before the backward pass - move up???
  optimizer$zero_grad()
  
  # gradients are computed on the loss tensor
  loss$backward()
  
  #-------- update weights and record loss -------- 
  
  # # not sure
  # loss$backward()
  # use the optimizer to update model parameters
  optimizer$step()
  # record mse on training set
  train_losses <- c(train_losses, loss$item())
  
  # -------- VALIDATE --------
  
  model$eval()
  valid_losses <- c()
  
  # -------- forward pass -------- 
  
  # make prediction in current model state
  rho_pred_v <- model(align_val_tensor, pos_val_tensor)
  # reshape to match dimensions of target data
  rho_pred_v <- torch_squeeze(rho_pred_v, 2)
  
  # -------- compute and record loss -------- 
  
  # use mse loss to evaluation model prediction
  loss_v <- nnf_mse_loss(rho_pred_v, rho_val_tensor)
  valid_losses <- c(valid_losses, loss_v$item())
  
  # -------- OUTPUT UPDATE --------
  cat(
    sprintf(
      "Loss at epoch %d: training: %3f, validation: %3f\n", 
      t, 
      loss$item(), #mean(train_losses), 
      loss_v$item() #mean(valid_losses)
    )
  )
  
}


#--------------- SAVE MODEL --------------------

# save training history
save(
  history,
  #file = file.path(path_to_results, 'models', 'cnn_model_hist_low_dup_all_60.RData')
)

# save the model 



