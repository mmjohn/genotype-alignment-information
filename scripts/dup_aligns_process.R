#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: pre-processing steps on alignment data and compares all alignments to find duplicates
# Mackenzie M. Johnson
# July 2021 


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
#use_condaenv(condaenv = "r-reticulate", conda = "/home1/05072/mmjohn/.local/share/r-miniconda/conda.exe")
use_condaenv("r-reticulate", conda = "/stor/home/mmj2238/.local/share/r-miniconda/bin/conda")
library(keras) 

# load R package
library(devtools)
#devtools::load_all("/Users/mackenziejohnson/Documents/grad_school/wilke_lab/popgencnn")
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

# max_size - need to use one standard size for consistency across training and test sets
max_size <- 400 

#--------------- READ IN DATA --------------------
Sys.time()
cat("\nReading in data.....\n")

# load alignments - vary n
load(glue('{path_to_data}fixed_mu_vary_n_align.RData'))

# load alignments - vary mu
load(glue('{path_to_data}fixed_n_vary_mu_align.RData'))


#--------------- SORT ALIGNMENTS --------------------
Sys.time()
cat("\nSorting alignments.....\n")

# fixed mu, vary n
sm1_pop_data <- lapply(sm1_pop_unsorted, sort_align)
rm(sm1_pop_unsorted)

sm2_pop_data <- lapply(sm2_pop_unsorted, sort_align)
rm(sm2_pop_unsorted)

sm3_pop_data <- lapply(sm3_pop_unsorted, sort_align)
rm(sm3_pop_unsorted)

sm_pop_data <- lapply(sm_pop_unsorted, sort_align)
rm(sm_pop_unsorted)

md1_pop_data <- lapply(md1_pop_unsorted, sort_align)
rm(md1_pop_unsorted)

md2_pop_data <- lapply(md2_pop_unsorted, sort_align)
rm(md2_pop_unsorted)

md_pop_data <- lapply(md_pop_unsorted, sort_align)
rm(md_pop_unsorted)

lg_pop_data <- lapply(lg_pop_unsorted, sort_align)
rm(lg_pop_unsorted)


# fixed n, vary mu
lw1_mu_data <- lapply(lw1_mu_unsorted, sort_align)
rm(lw1_mu_unsorted)

lw2_mu_data <- lapply(lw2_mu_unsorted, sort_align)
rm(lw2_mu_unsorted)

lw_mu_data <- lapply(lw_mu_unsorted, sort_align)
rm(lw_mu_unsorted)

md1_mu_data <- lapply(md1_mu_unsorted, sort_align)
rm(md1_mu_unsorted)

md_mu_data <- lapply(md_mu_unsorted, sort_align)
rm(md_mu_unsorted)

hg_mu_data <- lapply(hg_mu_unsorted, sort_align)
rm(hg_mu_unsorted)


#--------------- PAD ALIGNMENTS --------------------
Sys.time()
cat("\nPadding alignments.....\n")

# use keras built-in function to pad alignments

# fixed mu, vary n
sm1_pop_padded <- lapply(
  sm1_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(sm1_pop_data)

sm2_pop_padded <- lapply(
  sm2_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(sm2_pop_data)

sm3_pop_padded <- lapply(
  sm3_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(sm3_pop_data)

sm_pop_padded <- lapply(
  sm_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(sm_pop_data)

md1_pop_padded <- lapply(
  md1_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md1_pop_data)

md2_pop_padded <- lapply(
  md2_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md2_pop_data)

md_pop_padded <- lapply(
  md_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md_pop_data)

lg_pop_padded <- lapply(
  lg_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, 
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(lg_pop_data)


# fixed n, vary mu
lw1_mu_padded <- lapply(
  lw1_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(lw1_mu_data)

lw2_mu_padded <- lapply(
  lw2_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(lw2_mu_data)

lw_mu_padded <- lapply(
  lw_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(lw_mu_data)

md1_mu_padded <- lapply(
  md1_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md1_mu_data)

md_mu_padded <- lapply(
  md_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md_mu_data)

hg_mu_padded <- lapply(
  hg_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(hg_mu_data)


#--------------- SAVE DATA SETS --------------------

save(
  sm1_pop_padded, sm2_pop_padded, sm3_pop_padded, sm_pop_padded,
  md1_pop_padded, md2_pop_padded, md_pop_padded, lg_pop_padded, 
  file = glue('{path_to_data}fixed_mu_vary_n_align_processed.RData')
)

save(
  lw1_mu_padded, lw2_mu_padded, lw_mu_padded, 
  md1_mu_padded, md_mu_padded, hg_mu_padded,
  file = glue('{path_to_data}fixed_n_vary_mu_align_processed.RData')
)

