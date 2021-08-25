#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: pre-processing steps on alignment data (sorting and padding)
# Note that alignments are not transposed, but will be before use in CNN
# Mackenzie M. Johnson
# July 2021 


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

# check that the right environment loaded
reticulate::py_config()

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_data <- '/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/'

# size of alignments
num_chrom <- 50

# go back to padding to maximum size of alignments in set to accommodate large pops with high mu
# # max_size - need to use one standard size for consistency across training and test sets
# max_size <- 400 

#--------------- READ IN DATA - FIXED MU --------------------

# load alignments - fixed mu
load(
  file = file.path(
    path_to_data, 
    'fixed_mu',
    'fixed_mu1_vary_n_align.RData'
  )
)

load(
  file = file.path(
    path_to_data, 
    'fixed_mu',
    'fixed_mu2_vary_n_align.RData'
  )
)

load(
  file = file.path(
    path_to_data, 
    'fixed_mu',
    'fixed_mu3_vary_n_align.RData'
  )
)


#--------------- SORT ALIGNMENTS - FIXED MU --------------------

# fixed mu, vary n
# set 1
pop_mu1_n1_data <- lapply(pop_mu1_n1_unsorted, sort_align)
rm(pop_mu1_n1_unsorted)

pop_mu1_n2_data <- lapply(pop_mu1_n2_unsorted, sort_align)
rm(pop_mu1_n2_unsorted)

pop_mu1_n3_data <- lapply(pop_mu1_n3_unsorted, sort_align)
rm(pop_mu1_n3_unsorted)

pop_mu1_n4_data <- lapply(pop_mu1_n4_unsorted, sort_align)
rm(pop_mu1_n4_unsorted)

pop_mu1_n5_data <- lapply(pop_mu1_n5_unsorted, sort_align)
rm(pop_mu1_n5_unsorted)

pop_mu1_n6_data <- lapply(pop_mu1_n6_unsorted, sort_align)
rm(pop_mu1_n6_unsorted)

# set 2
pop_mu2_n1_data <- lapply(pop_mu2_n1_unsorted, sort_align)
rm(pop_mu2_n1_unsorted)

pop_mu2_n2_data <- lapply(pop_mu2_n2_unsorted, sort_align)
rm(pop_mu2_n2_unsorted)

pop_mu2_n3_data <- lapply(pop_mu2_n3_unsorted, sort_align)
rm(pop_mu2_n3_unsorted)

pop_mu2_n4_data <- lapply(pop_mu2_n4_unsorted, sort_align)
rm(pop_mu2_n4_unsorted)

pop_mu2_n5_data <- lapply(pop_mu2_n5_unsorted, sort_align)
rm(pop_mu2_n5_unsorted)

pop_mu2_n6_data <- lapply(pop_mu2_n6_unsorted, sort_align)
rm(pop_mu2_n6_unsorted)

# set 3
pop_mu3_n1_data <- lapply(pop_mu3_n1_unsorted, sort_align)
rm(pop_mu3_n1_unsorted)

pop_mu3_n2_data <- lapply(pop_mu3_n2_unsorted, sort_align)
rm(pop_mu3_n2_unsorted)

pop_mu3_n3_data <- lapply(pop_mu3_n3_unsorted, sort_align)
rm(pop_mu3_n3_unsorted)

pop_mu3_n4_data <- lapply(pop_mu3_n4_unsorted, sort_align)
rm(pop_mu3_n4_unsorted)

pop_mu3_n5_data <- lapply(pop_mu3_n5_unsorted, sort_align)
rm(pop_mu3_n5_unsorted)

pop_mu3_n6_data <- lapply(pop_mu3_n6_unsorted, sort_align)
rm(pop_mu3_n6_unsorted)


#--------------- PAD ALIGNMENTS - FIXED MU --------------------

# use keras built-in function to pad alignments

# fixed mu, vary n
# set 1
max(lengths(pop_mu1_n1_data)/num_chrom)   # 6

pop_mu1_n1_padded <- lapply(
  pop_mu1_n1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 6,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu1_n1_data)

max(lengths(pop_mu1_n2_data)/num_chrom)   # 10

pop_mu1_n2_padded <- lapply(
  pop_mu1_n2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 10,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu1_n2_data)

max(lengths(pop_mu1_n3_data)/num_chrom)   # 19

pop_mu1_n3_padded <- lapply(
  pop_mu1_n3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 19,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu1_n3_data)

max(lengths(pop_mu1_n4_data)/num_chrom)   # 45

pop_mu1_n4_padded <- lapply(
  pop_mu1_n4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 45,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu1_n4_data)

max(lengths(pop_mu1_n5_data)/num_chrom)   # 114

pop_mu1_n5_padded <- lapply(
  pop_mu1_n5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 114,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu1_n5_data)

max(lengths(pop_mu1_n6_data)/num_chrom)   # 266

pop_mu1_n6_padded <- lapply(
  pop_mu1_n6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 266,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu1_n6_data)

# set 2
max(lengths(pop_mu2_n1_data)/num_chrom)   # 2

pop_mu2_n1_padded <- lapply(
  pop_mu2_n1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 2,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu2_n1_data)

max(lengths(pop_mu2_n2_data)/num_chrom)   # 4

pop_mu2_n2_padded <- lapply(
  pop_mu2_n2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 4,   
  dtype = "float32",
  padding = "post"
)

rm(pop_mu2_n2_data)

max(lengths(pop_mu2_n3_data)/num_chrom)   # 5

pop_mu2_n3_padded <- lapply(
  pop_mu2_n3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 5,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu2_n3_data)

max(lengths(pop_mu2_n4_data)/num_chrom)   # 10

pop_mu2_n4_padded <- lapply(
  pop_mu2_n4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 10,
  dtype = "float32",
  padding = "post"
)

rm(pop_mu2_n4_data)

max(lengths(pop_mu2_n5_data)/num_chrom)   # 17

pop_mu2_n5_padded <- lapply(
  pop_mu2_n5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 17, 
  dtype = "float32",
  padding = "post"
)

rm(pop_mu2_n5_data)

max(lengths(pop_mu2_n6_data)/num_chrom)   # 37

pop_mu2_n6_padded <- lapply(
  pop_mu2_n6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 37,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu2_n6_data)

# set 3
max(lengths(pop_mu3_n1_data)/num_chrom)   # 19

pop_mu3_n1_padded <- lapply(
  pop_mu3_n1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 19,
  dtype = "float32",
  padding = "post"
)

rm(pop_mu3_n1_data)

max(lengths(pop_mu3_n2_data)/num_chrom)   # 53

pop_mu3_n2_padded <- lapply(
  pop_mu3_n2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 53,  
  dtype = "float32",
  padding = "post"
)

rm(pop_mu3_n2_data)

max(lengths(pop_mu3_n3_data)/num_chrom)   # 130

pop_mu3_n3_padded <- lapply(
  pop_mu3_n3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 130, 
  dtype = "float32",
  padding = "post"
)

rm(pop_mu3_n3_data)

max(lengths(pop_mu3_n4_data)/num_chrom)   # 350

pop_mu3_n4_padded <- lapply(
  pop_mu3_n4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 350,
  dtype = "float32",
  padding = "post"
)

rm(pop_mu3_n4_data)

max(lengths(pop_mu3_n5_data)/num_chrom)   # 982

pop_mu3_n5_padded <- lapply(
  pop_mu3_n5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 982, 
  dtype = "float32",
  padding = "post"
)

rm(pop_mu3_n5_data)

max(lengths(pop_mu3_n6_data)/num_chrom)   # 2473

pop_mu3_n6_padded <- lapply(
  pop_mu3_n6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 2473,
  dtype = "float32",
  padding = "post"
)

rm(pop_mu3_n6_data)


#--------------- SAVE DATA SETS - FIXED MU --------------------

# set 1
save(
  pop_mu1_n1_padded, pop_mu1_n2_padded, pop_mu1_n3_padded,
  pop_mu1_n4_padded, pop_mu1_n5_padded, pop_mu1_n6_padded, 
  file = file.path(
    path_to_data, 
    'fixed_mu', 
    'fixed_mu1_vary_n_align_processed.RData')
)

# set 2
save(
  pop_mu2_n1_padded, pop_mu2_n2_padded, pop_mu2_n3_padded,
  pop_mu2_n4_padded, pop_mu2_n5_padded, pop_mu2_n6_padded, 
  file = file.path(
    path_to_data, 
    'fixed_mu', 
    'fixed_mu2_vary_n_align_processed.RData')
)

# set 3
save(
  pop_mu3_n1_padded, pop_mu3_n2_padded, pop_mu3_n3_padded,
  pop_mu3_n4_padded, pop_mu3_n5_padded, pop_mu3_n6_padded, 
  file = file.path(
    path_to_data, 
    'fixed_mu', 
    'fixed_mu3_vary_n_align_processed.RData')
)

# rm data sets
rm(pop_mu1_n1_padded, pop_mu1_n2_padded, pop_mu1_n3_padded,
   pop_mu1_n4_padded, pop_mu1_n5_padded, pop_mu1_n6_padded)

rm(pop_mu2_n1_padded, pop_mu2_n2_padded, pop_mu2_n3_padded,
   pop_mu2_n4_padded, pop_mu2_n5_padded, pop_mu2_n6_padded)

rm(pop_mu3_n1_padded, pop_mu3_n2_padded, pop_mu3_n3_padded,
   pop_mu3_n4_padded, pop_mu3_n5_padded, pop_mu3_n6_padded)


#--------------- READ IN DATA - FIXED N --------------------

# load alignments - fixed N
load(
  file = file.path(
    path_to_data, 
    'fixed_n',
    'fixed_n1_vary_mu_align.RData'
  )
)

load(
  file = file.path(
    path_to_data, 
    'fixed_n',
    'fixed_n2_vary_mu_align.RData'
  )
)

load(
  file = file.path(
    path_to_data, 
    'fixed_n',
    'fixed_n3_vary_mu_align.RData'
  )
)

#--------------- SORT ALIGNMENTS - FIXED N --------------------

# fixed n, vary mu
# set 1
pop_n1_mu1_data <- lapply(pop_n1_mu1_unsorted, sort_align)
rm(pop_n1_mu1_unsorted)

pop_n1_mu2_data <- lapply(pop_n1_mu2_unsorted, sort_align)
rm(pop_n1_mu2_unsorted)

pop_n1_mu3_data <- lapply(pop_n1_mu3_unsorted, sort_align)
rm(pop_n1_mu3_unsorted)

pop_n1_mu4_data <- lapply(pop_n1_mu4_unsorted, sort_align)
rm(pop_n1_mu4_unsorted)

pop_n1_mu5_data <- lapply(pop_n1_mu5_unsorted, sort_align)
rm(pop_n1_mu5_unsorted)

pop_n1_mu6_data <- lapply(pop_n1_mu6_unsorted, sort_align)
rm(pop_n1_mu6_unsorted)

# set 2
pop_n2_mu1_data <- lapply(pop_n2_mu1_unsorted, sort_align)
rm(pop_n2_mu1_unsorted)

pop_n2_mu2_data <- lapply(pop_n2_mu2_unsorted, sort_align)
rm(pop_n2_mu2_unsorted)

pop_n2_mu3_data <- lapply(pop_n2_mu3_unsorted, sort_align)
rm(pop_n2_mu3_unsorted)

pop_n2_mu4_data <- lapply(pop_n2_mu4_unsorted, sort_align)
rm(pop_n2_mu4_unsorted)

pop_n2_mu5_data <- lapply(pop_n2_mu5_unsorted, sort_align)
rm(pop_n2_mu5_unsorted)

pop_n2_mu6_data <- lapply(pop_n2_mu6_unsorted, sort_align)
rm(pop_n2_mu6_unsorted)

# set 3
pop_n3_mu1_data <- lapply(pop_n3_mu1_unsorted, sort_align)
rm(pop_n3_mu1_unsorted)

pop_n3_mu2_data <- lapply(pop_n3_mu2_unsorted, sort_align)
rm(pop_n3_mu2_unsorted)

pop_n3_mu3_data <- lapply(pop_n3_mu3_unsorted, sort_align)
rm(pop_n3_mu3_unsorted)

pop_n3_mu4_data <- lapply(pop_n3_mu4_unsorted, sort_align)
rm(pop_n3_mu4_unsorted)

pop_n3_mu5_data <- lapply(pop_n3_mu5_unsorted, sort_align)
rm(pop_n3_mu5_unsorted)

pop_n3_mu6_data <- lapply(pop_n3_mu6_unsorted, sort_align)
rm(pop_n3_mu6_unsorted)


#--------------- PAD ALIGNMENTS - FIXED N --------------------

# use keras built-in function to pad alignments

# fixed n, vary mu
# set 1
max(lengths(pop_n1_mu1_data)/num_chrom)   # 5

pop_n1_mu1_padded <- lapply(
  pop_n1_mu1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 5,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n1_mu1_data)

max(lengths(pop_n1_mu2_data)/num_chrom)   # 8

pop_n1_mu2_padded <- lapply(
  pop_n1_mu2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 8, 
  dtype = "float32",
  padding = "post"
)

rm(pop_n1_mu2_data)

max(lengths(pop_n1_mu3_data)/num_chrom)   # 13

pop_n1_mu3_padded <- lapply(
  pop_n1_mu3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 13,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n1_mu3_data)

max(lengths(pop_n1_mu4_data)/num_chrom)   # 29

pop_n1_mu4_padded <- lapply(
  pop_n1_mu4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 29,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n1_mu4_data)

max(lengths(pop_n1_mu5_data)/num_chrom)   # 79

pop_n1_mu5_padded <- lapply(
  pop_n1_mu5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 79,  
  dtype = "float32",
  padding = "post"
)

rm(pop_n1_mu5_data)

max(lengths(pop_n1_mu6_data)/num_chrom)   # 208

pop_n1_mu6_padded <- lapply(
  pop_n1_mu6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 208,  
  dtype = "float32",
  padding = "post"
)

rm(pop_n1_mu6_data)

# set 2
max(lengths(pop_n2_mu1_data)/num_chrom)   # 2 

pop_n2_mu1_padded <- lapply(
  pop_n2_mu1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 2,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n2_mu1_data)

max(lengths(pop_n2_mu2_data)/num_chrom)   # 3

pop_n2_mu2_padded <- lapply(
  pop_n2_mu2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 3, 
  dtype = "float32",
  padding = "post"
)

rm(pop_n2_mu2_data)

max(lengths(pop_n2_mu3_data)/num_chrom)   # 5

pop_n2_mu3_padded <- lapply(
  pop_n2_mu3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 5,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n2_mu3_data)

max(lengths(pop_n2_mu4_data)/num_chrom)   # 8

pop_n2_mu4_padded <- lapply(
  pop_n2_mu4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 8,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n2_mu4_data)

max(lengths(pop_n2_mu5_data)/num_chrom)   # 15

pop_n2_mu5_padded <- lapply(
  pop_n2_mu5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 15,  
  dtype = "float32",
  padding = "post"
)

rm(pop_n2_mu5_data)

max(lengths(pop_n2_mu6_data)/num_chrom)   # 38

pop_n2_mu6_padded <- lapply(
  pop_n2_mu6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 38,  
  dtype = "float32",
  padding = "post"
)

rm(pop_n2_mu6_data)

# set 3
max(lengths(pop_n3_mu1_data)/num_chrom)   # 13

pop_n3_mu1_padded <- lapply(
  pop_n3_mu1_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 13,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n3_mu1_data)

max(lengths(pop_n3_mu2_data)/num_chrom)   # 33

pop_n3_mu2_padded <- lapply(
  pop_n3_mu2_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 33, 
  dtype = "float32",
  padding = "post"
)

rm(pop_n3_mu2_data)

max(lengths(pop_n3_mu3_data)/num_chrom)   # 62

pop_n3_mu3_padded <- lapply(
  pop_n3_mu3_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 62,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n3_mu3_data)

max(lengths(pop_n3_mu4_data)/num_chrom)   # 165

pop_n3_mu4_padded <- lapply(
  pop_n3_mu4_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 165,   
  dtype = "float32",
  padding = "post"
)

rm(pop_n3_mu4_data)

max(lengths(pop_n3_mu5_data)/num_chrom)   # 499

pop_n3_mu5_padded <- lapply(
  pop_n3_mu5_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 499,  
  dtype = "float32",
  padding = "post"
)

rm(pop_n3_mu5_data)

max(lengths(pop_n3_mu6_data)/num_chrom)   # 1503

pop_n3_mu6_padded <- lapply(
  pop_n3_mu6_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = 1503,  
  dtype = "float32",
  padding = "post"
)

rm(pop_n3_mu6_data)


#--------------- SAVE DATA SETS - FIXED N --------------------

# set 1
save(
  pop_n1_mu1_padded, pop_n1_mu2_padded, pop_n1_mu3_padded,
  pop_n1_mu4_padded, pop_n1_mu5_padded, pop_n1_mu6_padded, 
  file = file.path(
    path_to_data, 
    'fixed_n', 
    'fixed_n1_vary_mu_align_processed.RData')
)

# set 2
save(
  pop_n2_mu1_padded, pop_n2_mu2_padded, pop_n2_mu3_padded,
  pop_n2_mu4_padded, pop_n2_mu5_padded, pop_n2_mu6_padded, 
  file = file.path(
    path_to_data, 
    'fixed_n', 
    'fixed_n2_vary_mu_align_processed.RData')
)

# set 3
save(
  pop_n3_mu1_padded, pop_n3_mu2_padded, pop_n3_mu3_padded,
  pop_n3_mu4_padded, pop_n3_mu5_padded, pop_n3_mu6_padded, 
  file = file.path(
    path_to_data, 
    'fixed_n', 
    'fixed_n3_vary_mu_align_processed.RData')
)

# remove data sets
rm(pop_n1_mu1_padded, pop_n1_mu2_padded, pop_n1_mu3_padded,
   pop_n1_mu4_padded, pop_n1_mu5_padded, pop_n1_mu6_padded)

rm(pop_n2_mu1_padded, pop_n2_mu2_padded, pop_n2_mu3_padded,
   pop_n2_mu4_padded, pop_n2_mu5_padded, pop_n2_mu6_padded)

rm(pop_n3_mu1_padded, pop_n3_mu2_padded, pop_n3_mu3_padded,
   pop_n3_mu4_padded, pop_n3_mu5_padded, pop_n3_mu6_padded)

