#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Generate msprime simulations under different parameter conditions to be used in keras CNN
# This script: create function calls and output shell scripts
# Mackenzie M. Johnson
# August 2021 

#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(glue)

#--------------- GLOBAL PARAMETERS --------------------

# general msprime function call
msp_sim_call <- function(morgans_per_bp, ne, mu, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50                          # number of chromosomes
  num_loci <- 20001
  num_replicates <- 1   
  
  # variable parameters
  rho <- 4 * ne * morgans_per_bp * (num_loci - 1)
  theta <- 4 * ne * mu * num_loci
  
  # switch to use glue package
  sim_call <- glue::glue(
    '~/.local/bin/mspms ',                   # fuction call for TACC env
    '--mutation-rate {theta} ',
    '--recombination {rho} {num_loci} ',
    '--growth-rate {alpha} ',
    '{sample_size} {num_replicates} ',
    '>> {output}'
  )
  
  return(sim_call)
  
}

# function randomly selects mbp and passes parameters to function call
msp_params <- function(index, sim_ne, sim_mu, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  msp_sim_call(
    morgans_per_bp = mbp,
    ne = sim_ne,
    mu = sim_mu,
    output = glue::glue(
      '{filename}_sim{index}.txt'           # added {index} for use with TACC launcher
    ), 
    ...
  )
}


#--------------- GENERATE SCRIPTS --------------------

# simulations for duplicate CNN analysis
# parameters used:
# fixed mu: 1.5e-8 or 1.5e-9 (low vs high duplicate range)
# N used: 1,000, 2,000, 5,000, 10,000, 15,000, 20,000 (increase range to improve model training)


# generate script for low duplicate range
low_dup_n1_set <- tibble(
  sim_num = seq(1:20000)                  # number of simulations for parameter set
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 1000, 
      sim_mu = 1.5e-8,
      filename = 'n_1000_low_dup' 
    )
  )

low_dup_n2_set <- tibble(
  sim_num = seq(1:20000)  
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 2000, 
      sim_mu = 1.5e-8,
      filename = 'n_2000_low_dup' 
    )
  )

low_dup_n3_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 5000, 
      sim_mu = 1.5e-8,
      filename = 'n_5000_low_dup' 
    )
  )

low_dup_n4_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 10000, 
      sim_mu = 1.5e-8,
      filename = 'n_10000_low_dup' 
    )
  )

low_dup_n5_set <- tibble(
  sim_num = seq(1:20000)  
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 15000, 
      sim_mu = 1.5e-8,
      filename = 'n_15000_low_dup' 
    )
  )

low_dup_n6_set <- tibble(
  sim_num = seq(1:20000)  
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 20000, 
      sim_mu = 1.5e-8,
      filename = 'n_20000_low_dup' 
    )
  )

# generate script for high duplicate range
high_dup_n1_set <- tibble(
  sim_num = seq(1:20000)                 
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 1000, 
      sim_mu = 1.5e-9,
      filename = 'n_1000_high_dup' 
    )
  )

high_dup_n2_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 2000, 
      sim_mu = 1.5e-9,
      filename = 'n_2000_high_dup' 
    )
  )

high_dup_n3_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 5000, 
      sim_mu = 1.5e-9,
      filename = 'n_5000_high_dup' 
    )
  )

high_dup_n4_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 10000, 
      sim_mu = 1.5e-9,
      filename = 'n_10000_high_dup' 
    )
  )

high_dup_n5_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 15000, 
      sim_mu = 1.5e-9,
      filename = 'n_15000_high_dup' 
    )
  )

high_dup_n6_set <- tibble(
  sim_num = seq(1:20000)
) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      msp_params, 
      sim_ne = 20000, 
      sim_mu = 1.5e-9,
      filename = 'n_20000_high_dup' 
    )
  )

#--------------- WRITE TO FILE --------------------

# write to bash shell script 
low_dup_n1_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_low_n1000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

low_dup_n2_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_low_n2000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

low_dup_n3_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_low_n5000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

low_dup_n4_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_low_n10000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

low_dup_n5_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_low_n15000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

low_dup_n6_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_low_n20000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_dup_n1_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_high_n1000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_dup_n2_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_high_n2000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_dup_n3_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_high_n5000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_dup_n4_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_high_n10000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_dup_n5_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_high_n15000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_dup_n6_set %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_cnn_high_n20000.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

