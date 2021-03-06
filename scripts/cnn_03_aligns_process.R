#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: pre-processing steps on alignment data (sorting and padding)
# Note that alignments are not transposed, but will be before use in CNN
# Mackenzie M. Johnson
# August 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(reticulate)
# call the conda environment that has keras and tensorflow installed
use_condaenv("r-reticulate", conda = "/stor/home/mmj2238/.local/share/r-miniconda/bin/conda")
library(keras) 

# load R package
library(devtools)
devtools::load_all("/stor/home/mmj2238/popgencnn/")

# record session info
sessionInfo()


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_data <- '/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/'

# size of alignments
num_chrom <- 50


#--------------- READ IN DATA --------------------

# load alignments
load(
  file = file.path(
    path_to_data, 
    'cnn_dup',
    'low_dup_align.RData'
  )
)

load(
  file = file.path(
    path_to_data, 
    'cnn_dup',
    'high_dup_align.RData'
  )
)

# load rho values
load(
  file = file.path(
    path_to_data, 
    'cnn_dup',
    'low_dup_pos.RData'
  )
)

load(
  file = file.path(
    path_to_data, 
    'cnn_dup',
    'high_dup_pos.RData'
  )
)


#--------------- SORT ALIGNMENTS --------------------

# sort by similarity
pop_low_n1_data <- lapply(pop_low_n1_unsorted, sort_align)
rm(pop_low_n1_unsorted)

pop_low_n2_data <- lapply(pop_low_n2_unsorted, sort_align)
rm(pop_low_n2_unsorted)

pop_low_n3_data <- lapply(pop_low_n3_unsorted, sort_align)
rm(pop_low_n3_unsorted)

pop_low_n4_data <- lapply(pop_low_n4_unsorted, sort_align)
rm(pop_low_n4_unsorted)

pop_low_n5_data <- lapply(pop_low_n5_unsorted, sort_align)
rm(pop_low_n5_unsorted)

pop_low_n6_data <- lapply(pop_low_n6_unsorted, sort_align)
rm(pop_low_n6_unsorted)


pop_high_n1_data <- lapply(pop_high_n1_unsorted, sort_align)
rm(pop_high_n1_unsorted)

pop_high_n2_data <- lapply(pop_high_n2_unsorted, sort_align)
rm(pop_high_n2_unsorted)

pop_high_n3_data <- lapply(pop_high_n3_unsorted, sort_align)
rm(pop_high_n3_unsorted)

pop_high_n4_data <- lapply(pop_high_n4_unsorted, sort_align)
rm(pop_high_n4_unsorted)

pop_high_n5_data <- lapply(pop_high_n5_unsorted, sort_align)
rm(pop_high_n5_unsorted)

pop_high_n6_data <- lapply(pop_high_n6_unsorted, sort_align)
rm(pop_high_n6_unsorted)


#--------------- PAD ALIGNMENTS --------------------

# use keras built-in function to pad alignments

max(lengths(pop_low_n1_data)/num_chrom)   # 20
max(lengths(pop_low_n2_data)/num_chrom)   # 31
max(lengths(pop_low_n3_data)/num_chrom)   # 58
max(lengths(pop_low_n4_data)/num_chrom)   # 106
max(lengths(pop_low_n5_data)/num_chrom)   # 134
max(lengths(pop_low_n6_data)/num_chrom)   # 174 - PAD

pop_low_n1_padded <- lapply(
  pop_low_n1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 174,  
  dtype = "float32",
  padding = "post"
)

rm(pop_low_n1_data)

pop_low_n2_padded <- lapply(
  pop_low_n2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 174,  
  dtype = "float32",
  padding = "post"
)

rm(pop_low_n2_data)

pop_low_n3_padded <- lapply(
  pop_low_n3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 174,  
  dtype = "float32",
  padding = "post"
)

rm(pop_low_n3_data)

pop_low_n4_padded <- lapply(
  pop_low_n4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 174,  
  dtype = "float32",
  padding = "post"
)

rm(pop_low_n4_data)

pop_low_n5_padded <- lapply(
  pop_low_n5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 174,  
  dtype = "float32",
  padding = "post"
)

rm(pop_low_n5_data)

pop_low_n6_padded <- lapply(
  pop_low_n6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 174,  
  dtype = "float32",
  padding = "post"
)

rm(pop_low_n6_data)

max(lengths(pop_high_n1_data)/num_chrom)   # 6
max(lengths(pop_high_n2_data)/num_chrom)   # 8
max(lengths(pop_high_n3_data)/num_chrom)   # 11
max(lengths(pop_high_n4_data)/num_chrom)   # 17
max(lengths(pop_high_n5_data)/num_chrom)   # 24
max(lengths(pop_high_n6_data)/num_chrom)   # 27 - PAD

pop_high_n1_padded <- lapply(
  pop_high_n1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 27,  
  dtype = "float32",
  padding = "post"
)

rm(pop_high_n1_data)

pop_high_n2_padded <- lapply(
  pop_high_n2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 27,  
  dtype = "float32",
  padding = "post"
)

rm(pop_high_n2_data)

pop_high_n3_padded <- lapply(
  pop_high_n3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 27,  
  dtype = "float32",
  padding = "post"
)

rm(pop_high_n3_data)

pop_high_n4_padded <- lapply(
  pop_high_n4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 27,  
  dtype = "float32",
  padding = "post"
)

rm(pop_high_n4_data)

pop_high_n5_padded <- lapply(
  pop_high_n5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 27,  
  dtype = "float32",
  padding = "post"
)

rm(pop_high_n5_data)

pop_high_n6_padded <- lapply(
  pop_high_n6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 27,  
  dtype = "float32",
  padding = "post"
)

rm(pop_high_n6_data)


#--------------- PAD POSITIONS --------------------

# pad position vectors to match alignments

pop_low_n1_pos_padded <- pad_sequences(
  pop_low_n1_pos,
  maxlen = 174,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_low_n2_pos_padded <- pad_sequences(
  pop_low_n2_pos,
  maxlen = 174,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_low_n3_pos_padded <- pad_sequences(
  pop_low_n3_pos,
  maxlen = 174,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_low_n4_pos_padded <- pad_sequences(
  pop_low_n4_pos,
  maxlen = 174,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_low_n5_pos_padded <- pad_sequences(
  pop_low_n5_pos,
  maxlen = 174,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_low_n6_pos_padded <- pad_sequences(
  pop_low_n6_pos,
  maxlen = 174,
  dtype = "float32",
  padding = "post",
  value = -1.0
)


pop_high_n1_pos_padded <- pad_sequences(
  pop_high_n1_pos,
  maxlen = 27,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_high_n2_pos_padded <- pad_sequences(
  pop_high_n2_pos,
  maxlen = 27,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_high_n3_pos_padded <- pad_sequences(
  pop_high_n3_pos,
  maxlen = 27,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_high_n4_pos_padded <- pad_sequences(
  pop_high_n4_pos,
  maxlen = 27,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_high_n5_pos_padded <- pad_sequences(
  pop_high_n5_pos,
  maxlen = 27,
  dtype = "float32",
  padding = "post",
  value = -1.0
)

pop_high_n6_pos_padded <- pad_sequences(
  pop_high_n6_pos,
  maxlen = 27,
  dtype = "float32",
  padding = "post",
  value = -1.0
)


#--------------- SAVE DATA SETS --------------------

save(
  pop_low_n1_padded, pop_low_n2_padded, pop_low_n3_padded, 
  pop_low_n4_padded, pop_low_n5_padded, pop_low_n6_padded,
  file = file.path(
    path_to_data, 
    'cnn_dup', 
    'low_dup_align_processed.RData')
)

save(
  pop_low_n1_pos_padded, pop_low_n2_pos_padded, pop_low_n3_pos_padded,
  pop_low_n4_pos_padded, pop_low_n5_pos_padded, pop_low_n6_pos_padded,
  file = file.path(
    path_to_data, 
    'cnn_dup', 
    'low_dup_pos_processed.RData')
)

save(
  pop_high_n1_padded, pop_high_n2_padded, pop_high_n3_padded,
  pop_high_n4_padded, pop_high_n5_padded, pop_high_n6_padded,
  file = file.path(
    path_to_data, 
    'cnn_dup', 
    'high_dup_align_processed.RData')
)

save(
  pop_high_n1_pos_padded, pop_high_n2_pos_padded, pop_high_n3_pos_padded, 
  pop_high_n4_pos_padded, pop_high_n5_pos_padded, pop_high_n6_pos_padded,
  file = file.path(
    path_to_data, 
    'cnn_dup', 
    'high_dup_pos_processed.RData')
)

