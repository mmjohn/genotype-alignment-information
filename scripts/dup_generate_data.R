#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Generate msprime simulations under different parameter conditions
# This script: create function calls and output shell scripts
# Mackenzie M. Johnson
# July 2021 

#--------------- CONFIGURE ENVIRONMENT --------------------
Sys.time()
cat("\nConfiguring environment.....\n")

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(readr)
library(glue)

# load R package
library(devtools)
devtools::load_all("/stor/home/mmj2238/popgencnn/") # path for Wilk comp

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# paths to data
path_to_data_TACC <- 'Ignore'
path_to_scripts_TACC <- 'Ignore'

path_to_data_Wilkcomp <- '/stor/work/Wilke/mmj2238/rho_cnn_data/raw/'
path_to_scripts_Wilkcomp <- '/stor/home/mmj2238/bio-cnns/code/recombination/scripts/'


#--------------- GENERATE SCRIPTS --------------------
Sys.time()
cat("\nGenerating scripts for simulations.....\n")

# simulations for duplicate parameter analysis

# SET A ------ fixed mu, vary N

# general msprime function call used for all sims with fixed mu
msp_sim_fixed_mu <- function(morgans_per_bp, ne, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50     # number of chromosomes
  mu <- 1.5e-8          # fixed only for set A
  num_loci <- 20001
  num_replicates <- 1   # right now this just generates one sim per rho, mu, ne combo, but could be modified
  
  # variable parameters
  rho <- 4 * ne * morgans_per_bp * (num_loci - 1)
  theta <- 4 * ne * mu * num_loci
  
  # switch to use glue package
  sim_call <- glue::glue('mspms --mutation-rate {theta} ',
                         '--recombination {rho} {num_loci} ',
                         '--growth-rate {alpha} ', # exponential per-generation growth rate (0=constant N)
                         '{sample_size} {num_replicates} ',
                         '>> {output}')
  
  return(sim_call)
  
}

# general function to vary N
vary_pop <- function(index, path, sim_ne, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  #sim_ne <- 1000
  msp_sim_fixed_mu(
    morgans_per_bp = mbp,
    ne = sim_ne,
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}


# generate script for different population sizes
small_n_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 1000, 
      filename = 'n_1000_mu_fixed' # updated to use with launcher on TACC
    )
  )

med_n_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      filename = 'n_10000_mu_fixed' 
    )
  )

large_n_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 50000,
      filename = 'n_50000_mu_fixed' 
    )
  )

# update function call for TACC only
small_n_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> small_n_set_tidy$call

med_n_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> med_n_set_tidy$call

large_n_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> large_n_set_tidy$call


# write to bash shell script
small_n_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_small_n.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

med_n_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_med_n.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

large_n_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_large_n.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )




# SET B ------ fixed N, vary mu

# general msprime function call used for all sims with fixed N
msp_sim_fixed_n <- function(morgans_per_bp, mu, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50 # number of chromosomes
  #mu <- 1.5e-8 # fixed only for set A
  ne <- 10000
  num_loci <- 20001
  num_replicates <- 1   # right now this just generates one sim per rho, mu, ne combo, but could be modified
  
  # variable parameters
  rho <- 4 * ne * morgans_per_bp * (num_loci - 1)
  theta <- 4 * ne * mu * num_loci
  
  # switch to use glue package
  sim_call <- glue::glue('mspms --mutation-rate {theta} ',
                         '--recombination {rho} {num_loci} ',
                         '--growth-rate {alpha} ', # exponential per-generation growth rate (0=constant N)
                         '{sample_size} {num_replicates} ',
                         '>> {output}')
  
  return(sim_call)
  
}

# generate low mutation population
vary_mu <- function(index, path, sim_mu, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  #sim_mu <- 1.5e-9
  msp_sim_fixed_n(
    morgans_per_bp = mbp,
    mu = sim_mu,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for populations with low mutation
low_mu_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_mu = 1.5-9,
      filename = 'mu_low_n_fixed' # updated to use with launcher on TACC
    )
  )

med_mu_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_mu = 1.5-8,
      filename = 'mu_med_n_fixed' # updated to use with launcher on TACC
    )
  )

high_mu_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_mu = 1.5-7,
      filename = 'mu_high_n_fixed' # updated to use with launcher on TACC
    )
  )


# update function call for TACC only
low_mu_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> low_mu_set_tidy$call

med_mu_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> med_mu_set_tidy$call

high_mu_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> high_mu_set_tidy$call


# write to bash shell script
low_mu_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_low_mu.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

med_mu_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_med_mu.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

high_mu_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_high_mu.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

