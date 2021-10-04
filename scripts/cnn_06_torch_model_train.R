#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   120,000 simulations per set (high/low)
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


#--------------- GLOBAL PARAMETERS --------------------

# number of simulations in data set
num_sims <- 120000   # for all
# num_sims <- 60000   # for low unq
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


#--------------- DATA TENSORS TO DATA SETS --------------------

# data need to be combined into datasets for use with dataloaders
train_ds <- tensor_dataset(
  data_align = align_train_tensor, 
  data_pos = pos_train_tensor, 
  data_rho = rho_train_tensor
)
validation_ds <- tensor_dataset(
  data_align = align_val_tensor, 
  data_pos = pos_val_tensor, 
  data_rho = rho_val_tensor
)
test_ds <- tensor_dataset(
  data_align = align_test_tensor, 
  data_pos = pos_test_tensor, 
  data_rho = rho_test_tensor
)

#train_ds$.getitem(1)
#train_ds$.getitem(1)$data_align

#--------------- DATA SETS TO DATA LOADERS --------------------

# need to transform data sets to loaders for use in batches
train_dl <- train_ds %>% dataloader(batch_size = 32, shuffle = FALSE)
validation_dl <- validation_ds %>% dataloader(batch_size = 32, shuffle = FALSE)
test_dl <- test_ds %>% dataloader(batch_size = 32, shuffle = FALSE)

#--------------- DEFINE MODEL --------------------

# create model with nn_module()

# Flagel model:
# NOTE: in Flagel code, first set of avg pooling and dropout is missing, but it's reported in paper

flagel_cnn <- nn_module(
  "class_net",

  initialize = function() {

    #branch 1 (alignment CNN)
    self$conv1 <- nn_conv1d(
      in_channels =  max_size,            # max number of sites        
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
      in_features = max_size,          # max number of sites
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

# set l2 regularization parameter
l2_lambda <- 0.0001

# set learning rate for optimizer
learning_rate <- 0.0001        #0.08

# define optimizer
optimizer <- optim_adam(
  model$parameters, 
  lr = learning_rate,
  weight_decay = l2_lambda
)

# define number of epochs for training
epochs <- 25

# number of batches
train_dl$.length() # 2250
validation_dl$.length() # 750


#--------------- TRAIN MODEL --------------------

# examples: 
# https://anderfernandez.com/en/blog/how-to-create-neural-networks-with-torch-in-r/
# https://blogs.rstudio.com/ai/posts/2020-11-03-torch-tabular/
# https://blogs.rstudio.com/ai/posts/2020-10-19-torch-image-classification/
# https://cran.r-project.org/web/packages/torch/vignettes/loading-data.html

# train_losses <- c()
# valid_losses <- c()
# 
# # training loop w validation
# for (t in 1:epochs) {
#   
#   # -------- TRAIN --------
#   
#   model$train()
#   
#   # -------- forward pass -------- 
#   
#   # make prediction in current model state
#   rho_pred <- model(align_train_tensor, pos_train_tensor)
#   # reshape to match dimensions of target data
#   rho_pred <- torch_squeeze(rho_pred, 2)
#   
#   # -------- compute loss -------- 
#   
#   # use mse loss to evaluation model prediction
#   loss <- nnf_mse_loss(rho_pred, rho_train_tensor)
#   
#   #-------- backpropagation -------- 
#   
#   # zero out the gradients before the backward pass - move up???
#   optimizer$zero_grad()
#   
#   # gradients are computed on the loss tensor
#   loss$backward()
#   
#   #-------- update weights and record loss -------- 
#   
#   # # not sure
#   # loss$backward()
#   # use the optimizer to update model parameters
#   optimizer$step()
#   # record mse on training set
#   train_losses <- c(train_losses, loss$item())
#   
#   # -------- VALIDATE --------
#   
#   model$eval()
#   
#   # -------- forward pass -------- 
#   
#   # make prediction in current model state
#   rho_pred_v <- model(align_val_tensor, pos_val_tensor)
#   # reshape to match dimensions of target data
#   rho_pred_v <- torch_squeeze(rho_pred_v, 2)
#   
#   # -------- compute and record loss -------- 
#   
#   # use mse loss to evaluation model prediction
#   loss_v <- nnf_mse_loss(rho_pred_v, rho_val_tensor)
#   valid_losses <- c(valid_losses, loss_v$item())
#   
#   # -------- OUTPUT UPDATE --------
#   cat(
#     sprintf(
#       "Loss at epoch %d: training: %3f, validation: %3f\n", 
#       t, 
#       loss$item(), #mean(train_losses), 
#       loss_v$item() #mean(valid_losses)
#     )
#   )
#   
# }

# training loop w validation and batches
for (t in 1:epochs) {
  
  # -------- model training -------- 
  model$train()
  train_losses <- c()  
  
  coro::loop(for (b in train_dl) {
    optimizer$zero_grad()
    rho_pred <- model(b[[1]], b[[2]])
    rho_pred <- torch_squeeze(rho_pred, 2)
    loss <- nnf_mse_loss(rho_pred, b[[3]])
    loss$backward()
    optimizer$step()
    train_losses <- c(train_losses, loss$item())
  })
  
  # -------- model validation --------
  model$eval()
  valid_losses <- c()
  
  coro::loop(for (b in validation_dl) {
    rho_pred_val <- model(b[[1]], b[[2]])
    rho_pred_v <- torch_squeeze(rho_pred_v, 2)
    loss_val <- nnf_mse_loss(rho_pred_val, b[[3]])
    valid_losses <- c(valid_losses, loss_val$item())
  })
  
  cat(sprintf("Loss at epoch %d: training: %3f, validation: %3f\n", epoch, mean(train_losses), mean(valid_losses)))
 
}


#--------------- VISUALIZE TRAINING --------------------

history <- tibble(
  epochs = seq(1:length(train_losses)),
  train_losses,
  valid_losses
) 

history %>% 
  ggplot2::ggplot(aes(x = epochs)) +
  geom_point(aes(y = train_losses), color = "purple") +
  geom_point(aes(y = valid_losses), color = "orange") +
  theme_bw() +
  ylab("Loss (MSE)")


#--------------- SAVE MODEL --------------------

# save training history
save(
  history,
  file = file.path(
    path_to_results, 
    'models', 
    #'torch_model_hist_low_dup_all_25_epoch_1e-4_lr.RData')
)

# save the model
torch_save(
  model,
  file.path(
    path_to_models,
    #"torch_model_hist_low_dup_all_25_epoch_1e-4_lr.rt"
  )
)

# 
# reload_model <- torch_load(file.path(path_to_models, "torch_model_hist_low_dup_all_150.rt"))
# 
# reload_model(align_test_tensor, pos_test_tensor)


#--------------- SAVE MODEL PERFORMANCE --------------------

rho_train_prediction <- rho_pred %>% as_array()
rho_actual <- rho_train_tensor %>% as_array()

performance_rho <- tibble(
  sample = seq(1:72000),
  rho_train_prediction,
  rho_actual
)

performance_rho %>% 
  ggplot(aes(x = rho_train_prediction, y = rho_actual)) +
  geom_point()

# performance_rho %>% 
#   ggplot(aes(x = rho_actual, y = rho_actual)) +
#   geom_point()

performance_rho %>% 
  ggplot(aes(x = rho_actual)) +
  geom_density()

library(caret)
caret::postResample(
  pred = rho_train_prediction, 
  obs = rho_actual
)

# save training history
save(
  performance_rho,
  file = file.path(
    path_to_results, 
    'models', 
    #'torch_model_results_low_dup_all_25_epoch_1e-4_lr.RData')
)

