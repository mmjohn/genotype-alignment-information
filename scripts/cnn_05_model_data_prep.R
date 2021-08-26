#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# CNN for recombination rate estimation trained and tested on msprime simulations
# Using:
#   2 branch model from Flagel et al. (2018)
#   60,000 simulations per set
#   Currently using tensorflow via Keras and reticulate
# This script: prepares data for input into CNN (transpose, split, remove duplicates, etc)
# Mackenzie M. Johnson
# August 2021

#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)

# load R package
library(devtools)
devtools::load_all("/stor/home/mmj2238/popgencnn/")


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in data set
num_sims <- 60000

# paths to data
path_to_data <- "/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/cnn_dup/"
path_to_index <- "/stor/home/mmj2238/genotype-alignment-information/results/"

# size of alignments
num_chrom <- 50


#--------------- LOAD IN DATA --------------------

# load data saved from cnn_02_parse_data.R and cnn_03_aligns_process.R
# load duplicate index data from cnn_04_aligns_compare.R

# low duplicate data set
load(file.path(path_to_data, 'low_dup_align_processed.RData'))
load(file.path(path_to_data, 'low_dup_pos_processed.RData'))
load(file.path(path_to_data, 'low_dup_rho.RData'))
load(file.path(path_to_index, 'low_dup_align_indices.RData'))

# high duplicate data set
load(file.path(path_to_data, 'high_dup_align_processed.RData'))
load(file.path(path_to_data, 'high_dup_pos_processed.RData'))
load(file.path(path_to_data, 'high_dup_rho.RData'))
load(file.path(path_to_index, 'high_dup_align_indices.RData'))


#--------------- PREP DATA: REMOVE DUPLICATES --------------------

# remove duplicates from sets with them (keep both sets)

# low duplicate data set
# set n1 without duplicates (set n2 and n3 have no dup)
pop_low_n1_padded_nodup <- pop_low_n1_padded[-low_n1_index]
pop_low_n1_pos_nodup <- pop_low_n1_pos_padded[-low_n1_index, 1:106]
pop_low_n1_rho_nodup <- pop_low_n1_rho[-low_n1_index]

rm(low_n1_index, low_n2_index, low_n3_index)

# high duplicate data set
pop_high_n1_padded_nodup <- pop_high_n1_padded[-high_n1_index]
pop_high_n1_pos_nodup <- pop_high_n1_pos_padded[-high_n1_index, 1:17]
pop_high_n1_rho_nodup <- pop_high_n1_rho[-high_n1_index]

pop_high_n2_padded_nodup <- pop_high_n2_padded[-high_n2_index]
pop_high_n2_pos_nodup <- pop_high_n2_pos_padded[-high_n2_index, 1:17]
pop_high_n2_rho_nodup <- pop_high_n2_rho[-high_n2_index]

pop_high_n3_padded_nodup <- pop_high_n3_padded[-high_n3_index]
pop_high_n3_pos_nodup <- pop_high_n3_pos_padded[-high_n3_index, 1:17]
pop_high_n3_rho_nodup <- pop_high_n3_rho[-high_n3_index]

rm(high_n1_index, high_n2_index, high_n3_index)


#--------------- PREP DATA: JOIN DATA SETS --------------------

# low dup set
# join aligns - with duplicates
temp_1 <- append(pop_low_n1_padded, pop_low_n2_padded)
low_dup_aligns_all <- append(temp_1, pop_low_n3_padded)

# join aligns - without duplicates
temp_2 <- append(pop_low_n1_padded_nodup, pop_low_n2_padded)
low_dup_aligns_unq <- append(temp_2, pop_low_n3_padded)

rm(temp_1, temp_2, pop_low_n1_padded, pop_low_n1_padded_nodup,
   pop_low_n2_padded, pop_low_n3_padded)

# join positions - with duplicates
temp_3 <- rbind(pop_low_n1_pos_padded, pop_low_n2_pos_padded)
low_dup_pos_all <- rbind(temp_3, pop_low_n3_pos_padded)

# join positions - without duplicates
temp_4 <- rbind(pop_low_n1_pos_nodup, pop_low_n2_pos_padded)
low_dup_pos_unq <- rbind(temp_4, pop_low_n3_pos_padded)

rm(temp_3, temp_4, pop_low_n1_pos_nodup, pop_low_n1_pos_padded,
   pop_low_n2_pos_padded, pop_low_n3_pos_padded)

# join rho values - with duplicates
temp_5 <- c(pop_low_n1_rho, pop_low_n2_rho)
low_dup_rho_all <- c(temp_5, pop_low_n3_rho)

# join rho values - without duplicates
temp_6 <- c(pop_low_n1_rho_nodup, pop_low_n2_rho)
low_dup_rho_unq <- c(temp_6, pop_low_n3_rho)

rm(temp_5, temp_6, pop_low_n1_rho, pop_low_n1_rho_nodup, 
   pop_low_n2_rho, pop_low_n3_rho)

# high dup set
# join aligns - with duplicates
temp_7 <- append(pop_high_n1_padded, pop_high_n2_padded)
high_dup_aligns_all <- append(temp_7, pop_high_n3_padded)

# join aligns - without duplicates
temp_8 <- append(pop_high_n1_padded_nodup, pop_high_n2_padded_nodup)
high_dup_aligns_unq <- append(temp_8, pop_high_n3_padded_nodup)

rm(temp_7, temp_8, pop_high_n1_padded, pop_high_n2_padded,
   pop_high_n3_padded, pop_high_n1_padded_nodup,
   pop_high_n2_padded_nodup, pop_high_n3_padded_nodup)

# join positions - with duplicates
temp_9 <- rbind(pop_high_n1_pos_padded, pop_high_n2_pos_padded)
high_dup_pos_all <- rbind(temp_9, pop_high_n3_pos_padded)

# join positions - without duplicates
temp_10 <- rbind(pop_high_n1_pos_nodup, pop_high_n2_pos_nodup)
high_dup_pos_unq <- rbind(temp_10, pop_high_n3_pos_nodup)

rm(temp_9, temp_10, pop_high_n1_pos_padded, pop_high_n2_pos_padded,
   pop_high_n3_pos_padded, pop_high_n1_pos_nodup, 
   pop_high_n2_pos_nodup, pop_high_n3_pos_nodup)

# join rho values - with duplicates
temp_11 <- c(pop_high_n1_rho, pop_high_n2_rho)
high_dup_rho_all <- c(temp_11, pop_high_n3_rho)

# join rho values - without duplicates
temp_12 <- c(pop_high_n1_rho_nodup, pop_high_n2_rho_nodup)
high_dup_rho_unq <- c(temp_12, pop_high_n3_rho_nodup)

rm(temp_11, temp_12, pop_high_n1_rho, pop_high_n2_rho,
   pop_high_n3_rho, pop_high_n1_rho_nodup, 
   pop_high_n2_rho_nodup, pop_high_n3_rho_nodup)


#--------------- PREP DATA: CONVERT ALIGNMENTS --------------------

# convert alignments from list to array and rearrange dimensions (transpose)
# low dup set
low_dup_all_array <- popgencnn::list_to_array(
  low_dup_aligns_all, 
  dim = c(50, 106, 60000)
)

low_dup_all_align <- aperm(
  low_dup_all_array,
  c(3, 2, 1)
)

rm(low_dup_aligns_all, low_dup_all_array)

low_dup_unq_array <- popgencnn::list_to_array(
  low_dup_aligns_unq, 
  dim = c(50, 106, 58532)
)

low_dup_unq_align <- aperm(
  low_dup_unq_array,
  c(3, 2, 1)
)

rm(low_dup_aligns_unq, low_dup_unq_array)

# high dup set
high_dup_all_array <- popgencnn::list_to_array(
  high_dup_aligns_all, 
  dim = c(50, 17, 60000)
)

high_dup_all_align <- aperm(
  high_dup_all_array,
  c(3, 2, 1)
)

rm(high_dup_aligns_all, high_dup_all_array)

high_dup_unq_array <- popgencnn::list_to_array(
  high_dup_aligns_unq, 
  dim = c(50, 17, 32714)
)

high_dup_unq_align <- aperm(
  high_dup_unq_array,
  c(3, 2, 1)
)

rm(high_dup_aligns_unq, high_dup_unq_array)


#--------------- PREP DATA: SPLIT TRAIN/TEST --------------------

# split data into training, validation, and test set randomly
# get number for each set
# low dup set
length(low_dup_rho_all)*0.6   # 36,000 for training
length(low_dup_rho_all)*0.2   # 12,000 for validation
length(low_dup_rho_all)*0.2   # 12,000 for testing

length(low_dup_rho_unq)*0.6   # 35,120 for training
length(low_dup_rho_unq)*0.2   # 11,706 for validation
length(low_dup_rho_unq)*0.2   # 11,706 for testing

# high dup set
length(high_dup_rho_all)*0.6   # 36,000 for training
length(high_dup_rho_all)*0.2   # 12,000 for validation
length(high_dup_rho_all)*0.2   # 12,000 for testing

length(high_dup_rho_unq)*0.6   # 19,628 for training
length(high_dup_rho_unq)*0.2   # 6,543 for validation
length(high_dup_rho_unq)*0.2   # 6,543 for testing

# randomly select indices for each set 
all_index <- 1:60000
unq_index <- 1:58532
h_unq_index <- 1:32714

sampled_all_index <- split(
  all_index, 
  sample(rep(1:3, c(36000, 12000, 12000)))
)
sampled_unq_index <- split(
  unq_index, 
  sample(rep(1:3, c(35120, 11706, 11706)))
)

h_sampled_all_index <- split(
  all_index, 
  sample(rep(1:3, c(36000, 12000, 12000)))
)
h_sampled_unq_index <- split(
  h_unq_index, 
  sample(rep(1:3, c(19628, 6543, 6543)))
)

# split into sets based on index
# low dup set
low_align_all_train <- low_dup_all_align[sampled_all_index[[1]], 1:106, 1:50]
low_align_unq_train <- low_dup_unq_align[sampled_unq_index[[1]], 1:106, 1:50]
low_pos_all_train <- low_dup_pos_all[sampled_all_index[[1]], 1:106]
low_pos_unq_train <- low_dup_pos_unq[sampled_unq_index[[1]], 1:106]
low_rho_all_train <- low_dup_rho_all[sampled_all_index[[1]]]
low_rho_unq_train <- low_dup_rho_unq[sampled_unq_index[[1]]]

low_align_all_val <- low_dup_all_align[sampled_all_index[[2]], 1:106, 1:50]
low_align_unq_val <- low_dup_unq_align[sampled_unq_index[[2]], 1:106, 1:50]
low_pos_all_val <- low_dup_pos_all[sampled_all_index[[2]], 1:106]
low_pos_unq_val <- low_dup_pos_unq[sampled_unq_index[[2]], 1:106]
low_rho_all_val <- low_dup_rho_all[sampled_all_index[[2]]]
low_rho_unq_val <- low_dup_pos_unq[sampled_unq_index[[2]]]

low_align_all_test <- low_dup_all_align[sampled_all_index[[3]], 1:106, 1:50]
low_align_unq_test <- low_dup_unq_align[sampled_unq_index[[3]], 1:106, 1:50]
low_pos_all_test <- low_dup_pos_all[sampled_all_index[[3]], 1:106]
low_pos_unq_test <- low_dup_pos_unq[sampled_unq_index[[3]], 1:106]
low_rho_all_test <- low_dup_rho_all[sampled_all_index[[3]]]
low_rho_unq_test <- low_dup_rho_unq[sampled_unq_index[[3]]]

rm(low_dup_all_align, low_dup_pos_all, low_dup_rho_all,
   all_index, sampled_all_index)
rm(low_dup_unq_align, low_dup_pos_unq, low_dup_rho_unq, 
   unq_index, sampled_unq_index)

# high dup set
high_align_all_train <- high_dup_all_align[h_sampled_all_index[[1]], 1:17, 1:50]
high_align_unq_train <- high_dup_unq_align[h_sampled_unq_index[[1]], 1:17, 1:50]
high_pos_all_train <- high_dup_pos_all[h_sampled_all_index[[1]], 1:17]
high_pos_unq_train <- high_dup_pos_unq[h_sampled_unq_index[[1]], 1:17]
high_rho_all_train <- high_dup_rho_all[h_sampled_all_index[[1]]]
high_rho_unq_train <- high_dup_rho_unq[h_sampled_unq_index[[1]]]

high_align_all_val <- high_dup_all_align[h_sampled_all_index[[2]], 1:17, 1:50]
high_align_unq_val <- high_dup_unq_align[h_sampled_unq_index[[2]], 1:17, 1:50]
high_pos_all_val <- high_dup_pos_all[h_sampled_all_index[[2]], 1:17]
high_pos_unq_val <- high_dup_pos_unq[h_sampled_unq_index[[2]], 1:17]
high_rho_all_val <- high_dup_rho_all[h_sampled_all_index[[2]]]
high_rho_unq_val <- high_dup_rho_unq[h_sampled_unq_index[[2]]]

high_align_all_test <- high_dup_all_align[h_sampled_all_index[[3]], 1:17, 1:50]
high_align_unq_test <- high_dup_unq_align[h_sampled_unq_index[[3]], 1:17, 1:50]
high_pos_all_test <- high_dup_pos_all[h_sampled_all_index[[3]], 1:17]
high_pos_unq_test <- high_dup_pos_unq[h_sampled_unq_index[[3]], 1:17]
high_rho_all_test <- high_dup_rho_all[h_sampled_all_index[[3]]]
high_rho_unq_test <- high_dup_rho_unq[h_sampled_unq_index[[3]]]

rm(high_dup_all_align, high_dup_pos_all, high_dup_rho_all,
   all_index, h_sampled_all_index)
rm(high_dup_unq_align, high_dup_pos_unq, high_dup_rho_unq, 
   h_unq_index, h_sampled_unq_index)


#--------------- PREP DATA: TRANSFORM RHO --------------------

# currently using the mean of the training set for all normalization - seems wrong ??
mean_all_train <- mean(log(low_rho_all_train))
mean_unq_train <- mean(log(low_rho_unq_train))

h_mean_all_train <- mean(log(high_rho_all_train))
h_mean_unq_train <- mean(log(high_rho_unq_train))

# normalize all rho data sets
# low dup set
low_rho_all_train_centered <- popgencnn::rho_normalize(
  low_rho_all_train, mean_all_train
)
low_rho_all_val_centered <- popgencnn::rho_normalize(
  low_rho_all_val, mean_all_train
)
low_rho_all_test_centered <- popgencnn::rho_normalize(
  low_rho_all_test, mean_all_train
)

low_rho_unq_train_centered <- popgencnn::rho_normalize(
  low_rho_unq_train, mean_unq_train
)
low_rho_unq_val_centered <- popgencnn::rho_normalize(
  low_rho_unq_val, mean_unq_train
)
low_rho_unq_test_centered <- popgencnn::rho_normalize(
  low_rho_unq_test, mean_unq_train
)

rm(mean_all_train, mean_unq_train)

# high dup set
high_rho_all_train_centered <- popgencnn::rho_normalize(
  high_rho_all_train, h_mean_all_train
)
high_rho_all_val_centered <- popgencnn::rho_normalize(
  high_rho_all_val, h_mean_all_train
)
high_rho_all_test_centered <- popgencnn::rho_normalize(
  high_rho_all_test, h_mean_all_train
)

high_rho_unq_train_centered <- popgencnn::rho_normalize(
  high_rho_unq_train, h_mean_unq_train
)
high_rho_unq_val_centered <- popgencnn::rho_normalize(
  high_rho_unq_val, h_mean_unq_train
)
high_rho_unq_test_centered <- popgencnn::rho_normalize(
  high_rho_unq_test, h_mean_unq_train
)

rm(h_mean_all_train, h_mean_unq_train)


#--------------- SAVE DATA --------------------

# Save data for input into CNN
# low dup set with all sim
save(
  low_align_all_train, low_align_all_val, low_align_all_test,
  low_pos_all_train, low_pos_all_val, low_pos_all_test,
  low_rho_all_train, low_rho_all_val, low_rho_all_test,
  low_rho_all_train_centered, low_rho_all_val_centered, low_rho_all_test_centered, 
  file = file.path(
    path_to_data, 
    'model_data_low_dup_all.RData')
)

# low dup set with only unique sim
save(
  low_align_unq_train, low_align_unq_val, low_align_unq_test,
  low_pos_unq_train, low_pos_unq_val, low_pos_unq_test,
  low_rho_unq_train, low_rho_unq_val, low_rho_unq_test,
  low_rho_unq_train_centered, low_rho_unq_val_centered, low_rho_unq_test_centered, 
  file = file.path(
    path_to_data, 
    'model_data_low_dup_unq.RData')
)

# high dup set with all sim
save(
  high_align_all_train, high_align_all_val, high_align_all_test,
  high_pos_all_train, high_pos_all_val, high_pos_all_test,
  high_rho_all_train, high_rho_all_val, high_rho_all_test,
  high_rho_all_train_centered, high_rho_all_val_centered, 
  high_rho_all_test_centered, 
  file = file.path(
    path_to_data, 
    'model_data_high_dup_all.RData')
)

# high dup set with only unique sim
save(
  high_align_unq_train, high_align_unq_val, high_align_unq_test,
  high_pos_unq_train, high_pos_unq_val, high_pos_unq_test,
  high_rho_unq_train, high_rho_unq_val, high_rho_unq_test,
  high_rho_unq_train_centered, high_rho_unq_val_centered, 
  high_rho_unq_test_centered, 
  file = file.path(
    path_to_data, 
    'model_data_high_dup_unq.RData')
)

