#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination data from msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   200,000 simulations
#   All functions defined in file (not calling package)
#   Run via command or RStudio line on wilkcomp01 or wilkcomp02
#   Currently using tensorflow via Keras and reticulate
# This script: defines, compiles, and trains model; saves a hdf5
# Mackenzie M. Johnson
# Feb 2021 - Updated May 2021 to use with msprime simulations

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
use_condaenv("r-reticulate", conda = "/stor/home/mmj2238/.local/share/r-miniconda/bin/conda")
library(keras) 

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in dataset
num_sims <- 

# paths to data
path_to_data <- "/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/"
path_to_results <- "/stor/home/mmj2238/bio-cnns/code/recombination/results/"

# size of alignments
num_chrom <- 50


#--------------- LOAD IN DATA --------------------

# load data saved from 
load(file = glue('{path_to_data}eq_alignments_training.RData'))


#--------------- PREPARE DATA --------------------

# split data into training
# transpose

#--------------- DEFINE MODEL --------------------

# set 
l2_lambda <- 0.0001

# define the inputs
genotype_input <- layer_input(
  shape = c(400, 50), # changes with data sets
  dtype = 'float32',
  name = 'genotype_input'
)

genotype_output <-  genotype_input %>%
  layer_conv_1d(
    #filters = 1250,
    filters = 256, # try to see if this is issue
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
  shape = c(max_size), # changes with data sets
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
    optimizer = optimizer_adam(lr = 0.000001),
    metrics = metric_mean_squared_error
  )

#--------------- TRAIN MODEL --------------------

model %>%
  fit(
    x = list(x1_train, x2_train),
    #y = y_train,
    y = y_train_log_centered,
    batch = 32,
    epochs = 1000,  #16, 50, 100, 150 - for eq at 59 turns
    validation_data = list(
      list(x1_val, x2_val), 
      y_val_log_centered
    )
  ) -> history

#plot(history)

save(
  history, 
  file = glue('{path_to_results}m.RData')
)


#--------------- SAVE MODEL --------------------

# Save the model
save_model_hdf5(model, "/stor/home/mmj2238/bio-cnns/code/recombination/trained_models/model_1Dcnn_eq_1000epoch.h5")

# Should probably also save model summary

