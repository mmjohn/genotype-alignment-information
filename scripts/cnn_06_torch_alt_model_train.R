#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   Modified model from Flagel et al. (2018) - remove branch with position information
#   120,000 simulations per set (high/low)
#   Currently using PyTorch via torch 0.5.0
# This script: defines, compiles, trains, and saves model
# Mackenzie M. Johnson
# August 2021

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
#sim_set <- "high"
sim_set <- "low"


#--------------- LOAD IN DATA --------------------

# load data saved from cnn_05_model_data_prep.R
load(file.path(path_to_data, glue('model_data_{sim_set}_dup_all.RData')))


#--------------- GLOBAL PARAMETERS --------------------

# number of simulations in data set
num_sims <- 120000   # for all

# size of alignments
# number of samples
num_chrom <- 50

# number of sites
if (sim_set == "low") {
  max_size <- 174
} else if (sim_set == "high") {
  max_size <- 27
} else {
  print("Dataset not found")
}


#--------------- DATA TO TENSORS --------------------

# should specify dtype 
# NOTE: some cost functions require specific types of data
# error messages should clearly state if this is an issue

# alignments
if (sim_set == "low") {
  # training set
  align_train_tensor <- torch_tensor(
    low_align_all_train,
    requires_grad = TRUE # this is required for nn training; tracks computations to calc. deriv.
  )
  # validation set
  align_val_tensor <- torch_tensor(
    low_align_all_val,
    requires_grad = TRUE 
  )
  # clean up
  rm(low_align_all_train, low_align_all_val, low_align_all_test)
} else {
  # training set
  align_train_tensor <- torch_tensor(
    high_align_all_train,
    requires_grad = TRUE 
  )
  # validation set
  align_val_tensor <- torch_tensor(
    high_align_all_val,
    requires_grad = TRUE 
  )
  # clean up
  rm(high_align_all_train, high_align_all_val, high_align_all_test)
}

# positions - not needed for this model
if (sim_set == "low") {
  rm(low_pos_all_train, low_pos_all_val, low_pos_all_test)
} else {
  rm(high_pos_all_train, high_pos_all_val, high_pos_all_test)
}

# rhos
if (sim_set == "low") {
  # training set 
  rho_train_tensor <- torch_tensor(
    low_rho_all_train_centered,
    requires_grad = TRUE
  )
  # validation set
  rho_val_tensor <- torch_tensor(
    low_rho_all_val_centered,
    requires_grad = TRUE
  )
  # clean up
  rm(low_rho_all_train, low_rho_all_val, low_rho_all_test, low_rho_all_train_centered,
     low_rho_all_val_centered, low_rho_all_test_centered)
} else {
  # training set
  rho_train_tensor <- torch_tensor(
    high_rho_all_train_centered,
    requires_grad = TRUE
  )
  # validation set
  rho_val_tensor <- torch_tensor(
    high_rho_all_val_centered,
    requires_grad = TRUE
  )
  # clean up
  rm(high_rho_all_train, high_rho_all_val, high_rho_all_test, high_rho_all_train_centered, 
     high_rho_all_val_centered, high_rho_all_test_centered)
}


#--------------- DATA TENSORS TO DATA SETS --------------------

# data need to be combined into datasets for use with dataloaders
train_ds <- tensor_dataset(
  data_align = align_train_tensor,  
  data_rho = rho_train_tensor
)
validation_ds <- tensor_dataset(
  data_align = align_val_tensor, 
  data_rho = rho_val_tensor
)


#--------------- DATA SETS TO DATA LOADERS --------------------

# need to transform data sets to loaders for use in batches
train_dl <- train_ds %>% dataloader(batch_size = 32, shuffle = TRUE)
validation_dl <- validation_ds %>% dataloader(batch_size = 32, shuffle = FALSE)


#--------------- DEFINE MODEL --------------------

# create model with nn_module()

# modified Flagel model:
# NOTE: in Flagel code, first set of avg pooling and dropout is missing, but it's reported in paper

mod_flagel_cnn <- nn_module(
  "class_net",

  initialize = function() {

    #branch 1 (alignment CNN)
    self$conv1 <- nn_conv1d(
      in_channels =  max_size,            # max number of sites        
      out_channels = 1250,
      kernel_size = 2
    )
    # self$dropout1 <- nn_dropout(0.25)
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

    # # branch 2 (position fc) - remove positional information
    # self$fc1 <- nn_linear(
    #   in_features = max_size,          # max number of sites
    #   out_features = 64
    # )                             # dense in keras = linear in torch
    # self$dropout3 <- nn_dropout(0.1)

    # full model (fc)
    self$fc2 <- nn_linear(
      in_features = 2816,    # 2880    # 1344 with first pooling     
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
      # nnf_avg_pool1d(kernel_size = 2) %>%   # MISSING IN FLAGEL CODE
      # self$dropout1() %>%                   # MISSING IN FLAGEL CODE
      self$conv2() %>%
      nnf_relu() %>%
      nnf_avg_pool1d(kernel_size = 2) %>%
      self$dropout2() %>%
      self$conv3() %>%
      nnf_relu() %>%
      nnf_avg_pool1d(kernel_size = 2) %>%
      self$dropout3() %>%
      torch_flatten(start_dim = 2) 

    # # branch 2 (position fc) - remove positional information
    # branch2 <- data_pos %>% 
    #   self$fc1() %>% 
    #   self$dropout3()

    # full model (fc)
    #full_model <- torch_cat(list(branch1, branch2), dim = 2)
    
    branch1 %>% 
    #full_model %>% 
      self$fc2() %>% 
      self$fc3()

  }
)

model <- mod_flagel_cnn()

#model$forward(align_train_tensor, pos_train_tensor)


#--------------- NETWORK PARAMETERS --------------------

# set l2 regularization parameter
l2_lambda <- 0.0001

# set learning rate for optimizer
learning_rate <- 0.00001        #0.08

# define optimizer
optimizer <- optim_adam(
  model$parameters, 
  lr = learning_rate ,
  weight_decay = l2_lambda
)

# define number of epochs for training
epochs <- 18 # 54 # 18

# number of batches
train_dl$.length() # 2250
validation_dl$.length() # 750


#--------------- TRAIN MODEL --------------------

# examples: 
# https://anderfernandez.com/en/blog/how-to-create-neural-networks-with-torch-in-r/
# https://blogs.rstudio.com/ai/posts/2020-11-03-torch-tabular/
# https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/
# https://cran.r-project.org/web/packages/torch/vignettes/loading-data.html

train_mean_losses <- c()
valid_mean_losses <- c()

# training loop w validation and batches
for (t in 1:epochs) {
  
  # -------- model training -------- 
  model$train()
  train_losses <- c()  
  
  coro::loop(for (b in train_dl) {
    optimizer$zero_grad()
    rho_pred <- model(b[[1]])      # b[[1]], b[[2]]
    rho_pred <- torch_squeeze(rho_pred, 2)
    loss <- nnf_mse_loss(rho_pred, b[[2]]) # b[[3]]
    loss$backward()
    optimizer$step()
    train_losses <- c(train_losses, loss$item())
  })
  
  # -------- model validation --------
  model$eval()
  valid_losses <- c()
  
  coro::loop(for (b in validation_dl) {
    rho_pred_val <- model(b[[1]])
    rho_pred_val <- torch_squeeze(rho_pred_val, 2)
    loss_val <- nnf_mse_loss(rho_pred_val, b[[2]])
    valid_losses <- c(valid_losses, loss_val$item())
  })
  
  cat(sprintf("Loss at epoch %d: training: %3f, validation: %3f\n", t, mean(train_losses), mean(valid_losses)))
  train_mean_losses <- c(train_mean_losses, mean(train_losses))
  valid_mean_losses <- c(valid_mean_losses, mean(valid_losses))
  
  if (mean(train_mean_losses) <= 0.9) { # should actually be ~ .64
    break 
  }
 
}


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


#--------------- SAVE MODEL --------------------

# save training history
save(
    history_torch,
    file = file.path(
      path_to_results, 'models',
      glue('torch_cnn_alt_hist_{sim_set}_dup_all_18_epoch_1e-5_lr_1e-4_l2.RData')
    )
  )

# save the model
model$eval()

torch_save(
  model,
  file.path(
    path_to_models,
    glue("torch_cnn_alt_{sim_set}_dup_all_18_epoch_1e-5_lr_1e-4_l2.rt")
  )
)

