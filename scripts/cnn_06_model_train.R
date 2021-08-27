#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   60,000 simulations per set
#   Currently using tensorflow via Keras and reticulate
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
num_sims <- 60000   # for all
# num_sims <- 60000   # for low unq
# num_sims <- 60000   # for high unq

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


#--------------- DEFINE MODEL --------------------

# set l2 regularization parameter
l2_lambda <- 0.0001

# define the inputs
genotype_input <- layer_input(
  shape = c(106, 50),               # 106 for low, 17 for high
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
  shape = c(106),                # 106 for low, 17 for high
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
    optimizer = optimizer_adam(lr = 0.000001),       # LOOK
    metrics = metric_mean_squared_error
  )

#--------------- TRAIN MODEL --------------------

model %>%
  fit(
    x = list(low_align_all_train, low_pos_all_train),
    y = low_rho_all_train_centered,
    batch = 32,
    epochs = 10,  
    validation_data = list(
      list(low_align_all_val, low_pos_all_val), 
      low_rho_all_val_centered
    )
  ) -> history

#plot(history)


#--------------- SAVE MODEL --------------------

# UNCOMMENT AFTER DEBUGGING
# # save training history 
# save(
#   history, 
#   file = file.path(path_to_results, 'models', 'cnn_model_hist_low_dup_all_60.RData')
# )

# save the model 

setwd("/stor/work/Wilke/mmj2238/trained_models/dup_analysis")

# save full model without optimizer
save_model_hdf5(
  model, 
  "cnn_model_full.h5", 
  overwrite = TRUE, 
  include_optimizer = FALSE
)

# save JSON config to disk
json_config <- model_to_json(model)
writeLines(
  json_config, 
  "cnn_model_config_low_dup_all_10.json"
)

# save weights to disk
save_model_weights_hdf5(
  model, 
  "cnn_model_weights_low_dup_all_10.h5"
)




# TEST THAT MODEL SAVES CORRECTLY

# predictions on original model
model %>% 
  predict(
    list(low_align_all_test, low_pos_all_test)
  ) -> model_predictions

# reload the model from the 2 files we saved
json_config_2 <- readLines(
  "cnn_model_config_low_dup_all_60.json"
)

new_model <- model_from_json(json_config_2)
load_model_weights_hdf5(
  new_model, 
  "cnn_model_weights_low_dup_all_60.h5"
)

# Check that the state is preserved
new_predictions <- predict(new_model, list(low_align_all_test, low_pos_all_test))
all.equal(predictions, new_predictions)


# reload the model from the 1 hdf5 file
new_model_2 <- load_model_hdf5("cnn_model_full.h5", compile = FALSE)

# Check that the state is preserved
new_predictions_2 <- predict(new_model_2, list(low_align_all_test, low_pos_all_test))
all.equal(predictions, new_predictions_2)



