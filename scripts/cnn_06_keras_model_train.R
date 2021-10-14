#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   120,000 simulations per set
#   Currently using tensorflow via Keras and reticulate 
# ISSUE: Cannot reload saved hdf5 models or weights
# This script: defines, compiles, and trains model; saves a hdf5
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
library(reticulate)
# call the conda environment that has keras and tensorflow installed
use_condaenv(
  "r-reticulate", 
  conda = "/stor/home/mmj2238/.local/share/r-miniconda/bin/conda"
)
library(keras) 

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in dataset
# num_sims <- 60000   # for all - old
# num_sims <- 58532   # for low unq - old
# num_sims <- 32714   # for high unq - old

# paths to data
path_to_data <- "/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/cnn_dup/"
path_to_results <- "/stor/home/mmj2238/genotype-alignment-information/results/"

# size of alignments
num_chrom <- 50


#--------------- LOAD IN DATA --------------------

# load data saved from cnn_05_model_data_prep.R
load(file.path(path_to_data, 'model_data_low_dup_all.RData'))
# load(file.path(path_to_data, 'model_data_low_dup_unq.RData'))
# load(file.path(path_to_data, 'model_data_high_dup_all.RData'))
# load(file.path(path_to_data, 'model_data_high_dup_unq.RData'))


#--------------- USE SUBSET OF DATA FOR TORCH DEBUG --------------------

#rm(low_pos_all_test, low_rho_all_test, low_rho_all_test_centered, low_align_all_test)

low_pos_all_val <- low_pos_all_val[1:8000, 1:174]
low_pos_all_train <- low_pos_all_train[1:24000, 1:174]
low_pos_all_test <- low_pos_all_test[1:8000, 1:174]

low_rho_all_val <- low_rho_all_val[1:8000]
low_rho_all_val_centered <- low_rho_all_val_centered[1:8000]
low_rho_all_train <- low_rho_all_train[1:24000]
low_rho_all_train_centered <- low_rho_all_train_centered[1:24000]
low_rho_all_test <- low_rho_all_test[1:8000]
low_rho_all_test_centered <- low_rho_all_test_centered[1:8000]

low_align_all_val <- low_align_all_val[1:8000, 1:174, 1:50]
low_align_all_train <- low_align_all_train[1:24000, 1:174, 1:50]
low_align_all_test <- low_align_all_test[1:8000, 1:174, 1:50]

#--------------- DEFINE MODEL --------------------

# set l2 regularization parameter
l2_lambda <- 0.0001

# define the inputs
genotype_input <- layer_input(
  shape = c(174, 50),               # 174 for low, 27 for high
  dtype = 'float32',
  name = 'genotype_input'
)

genotype_output <-  genotype_input %>%
  layer_conv_1d(
    filters = 256,
    kernel_size = 2,
    activation = 'relu',
    kernel_regularizer = regularizer_l2(l2_lambda)
  ) %>%
  #layer_average_pooling_1d(pool_size = 2) %>%
  #layer_dropout(rate = 0.25) %>%
  layer_conv_1d(
    filters = 256,
    kernel_size = 2,
    activation = 'relu',
    kernel_regularizer = regularizer_l2(l2_lambda)
  ) %>%
  layer_average_pooling_1d(pool_size = 2) %>%
  layer_dropout(rate = 0.25) %>%
  layer_conv_1d(
    filters = 256,
    kernel_size = 2,
    activation = 'relu',
    kernel_regularizer = regularizer_l2(l2_lambda)
  ) %>%
  layer_average_pooling_1d(pool_size = 2) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten()

position_input <- layer_input(
  shape = c(174),                # 174 for low, 27 for high
  dtype = 'float32',
  name = 'position_input'
)

position_output <- position_input %>%
  layer_dense(
    units = 64,
    activation = 'relu',
    kernel_regularizer = regularizer_l2(l2_lambda)
  ) %>%
  layer_dropout(rate = 0.1)

# merge the inputs, produce one output
full_output <- layer_concatenate(
  c(genotype_output, position_output)
) %>%
  layer_dense(
    units = 256,
    activation = 'relu',
    kernel_regularizer = regularizer_l2(l2_lambda)
  ) %>%
  layer_dense(
    units = 1,
    kernel_initializer = 'normal'
  )

# define the full model
model <- keras_model(
  inputs = c(genotype_input, position_input),
  outputs = full_output
)

summary(model)


#--------------- COMPILE MODEL --------------------

model %>%
  compile(
    loss = 'mean_squared_error',
    #optimizer = 'adam',
    optimizer = optimizer_adam(lr = 0.000001),       #  0.000001
    metrics = metric_mean_squared_error
  )


#--------------- TRAIN MODEL --------------------

model %>%
  fit(
    x = list(low_align_all_train, low_pos_all_train),
    y = low_rho_all_train_centered,
    batch = 32,
    epochs = 5,  
    validation_data = list(
      list(low_align_all_val, low_pos_all_val), 
      low_rho_all_val_centered
    )
  ) -> history

#plot(history)


#--------------- TEST AND SAVE DATA FOR EXAMPLE FIGS --------------------

# make predictions
model %>% 
  predict(
    list(low_align_all_test, low_pos_all_test)
  ) -> predictions

performance_keras <- tibble(
  sample = seq(1:length(low_rho_all_test_centered)),
  estimate = predictions,
  actual = low_rho_all_test_centered
)

performance_keras %>% 
  ggplot(aes(x = estimate, y = actual)) +
  geom_point()


library(caret)
caret::postResample(
  pred = performance_keras$estimate, 
  obs = performance_keras$actual
) -> r2_results_keras

# RMSE           Rsquared       MAE 
# 1.3709199795   0.0009230179   1.1742961743 

save(
  history, performance_keras,
  r2_results_keras,
  file = file.path(
    '/stor/home/mmj2238/genotype-alignment-information/notes', 
    'keras_subset_5_epoch_1e-6_lr.RData')
)








save(
    history, performance_keras,
    file = file.path(
      path_to_results, 'models', 
      'keras_model_results_low_dup_all_25_epoch_1e-4_lr.RData')
  )

save(
  history,
  file = file.path(
    path_to_results, 'models', 
    'keras_model_history_low_dup_all_subset_25_epoch_1e-4_lr.RData')
)

# #--------------- SAVE MODEL --------------------
# 
# # UNCOMMENT AFTER DEBUGGING
# # # save training history 
# # save(
# #   history, 
# #   file = file.path(path_to_results, 'models', 'cnn_model_hist_low_dup_all_60.RData')
# # )
# 
# # save the model 
# 
# setwd("/stor/work/Wilke/mmj2238/trained_models/dup_analysis")
# 
# # save full model without optimizer
# save_model_hdf5(
#   model, 
#   "cnn_model_full.h5", 
#   overwrite = TRUE, 
#   include_optimizer = FALSE
# )
# 
# # save JSON config to disk
# json_config <- model_to_json(model)
# writeLines(
#   json_config, 
#   "cnn_model_config_low_dup_all_10.json"
# )
# 
# # save weights to disk
# save_model_weights_hdf5(
#   model, 
#   "cnn_model_weights_low_dup_all_10.h5"
# )
# 
# 
# 
# 
# # TEST THAT MODEL SAVES CORRECTLY
# 
# # predictions on original model
# model %>% 
#   predict(
#     list(low_align_all_test, low_pos_all_test)
#   ) -> model_predictions
# 
# # reload the model from the 2 files we saved
# json_config_2 <- readLines(
#   "cnn_model_config_low_dup_all_60.json"
# )
# 
# new_model <- model_from_json(json_config_2)
# load_model_weights_hdf5(
#   new_model, 
#   "cnn_model_weights_low_dup_all_60.h5"
# )
# 
# # Check that the state is preserved
# new_predictions <- predict(new_model, list(low_align_all_test, low_pos_all_test))
# all.equal(predictions, new_predictions)
# 
# 
# # reload the model from the 1 hdf5 file
# new_model_2 <- load_model_hdf5("cnn_model_full.h5", compile = FALSE)
# 
# # Check that the state is preserved
# new_predictions_2 <- predict(new_model_2, list(low_align_all_test, low_pos_all_test))
# all.equal(predictions, new_predictions_2)
# 


