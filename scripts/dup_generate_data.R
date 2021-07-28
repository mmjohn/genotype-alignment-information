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

# FOR WILK COMP
# should generate bash scripts with calls like this
# eval "$(conda shell.bash hook)"
# conda activate msprime-env
# mspms ....
# .
# .
# .
# .

# FOR TACC
# should generate bash scripts called by launcher
# ~/.local/bin/mspms .....
# .
# .
# .
# .

# general msprime function call used for all models
msprime_sim <- function(morgans_per_bp, ne, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50 # number of chromosomes
  mu <- 1.5e-8
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

# generate equilibrium models with constant N
equilibrium_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_ne <- sample(c(1000, 2000, 5000, 10000, 15000, 20000, 50000), 1)
  msprime_sim(
    morgans_per_bp = mbp,
    ne = sim_ne,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# # test case
# equilibrium_pop(
#   index = 1, 
#   path = path_to_data_TACC, 
#   filename = 'msprime_equilibrium_sims_2000'
# )

# generate script for equilibrium populations across a range of N values
eq_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      equilibrium_pop, 
      path = path_to_data_TACC,
      #filename = 'msprime_equilibrium_sims_2000' # format used on Wilkcomp
      filename = 'eq_batch5' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
eq_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> eq_set_tidy$call

# write to bash shell script
eq_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_eq_5.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )


# generate  models with exponential population growth
exponential_pop <- function(index, path, filename, pop_sizes = c(1000, 2000, 5000, 10000, 15000, 20000, 50000)){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_ne <- sample(c(1000, 2000, 5000, 10000, 15000, 20000, 50000), 1)
  sim_ne <- sample(pop_sizes,1)
  sim_alpha <- sample(c(0.2, 0.4, 0.6, 0.8, 1), 1)
  
  msprime_sim(
    morgans_per_bp = mbp,
    ne = sim_ne,
    alpha = sim_alpha,
    # output = glue::glue('{path}{filename}') general
    output = glue::glue('{filename}_sim{index}.txt') # added {index} for use with launcher (TACC-specific)
  )
}

# # test case
# exponential_pop(
#   index = 1, 
#   path = path_to_data_TACC, 
#   filename = 'msprime_equilibrium_sims_2000.txt'
# )

# generate script for exponential growth populations across a range of N and alpha values
exp_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      exponential_pop, 
      path = path_to_data_TACC,
      #filename = 'msprime_equilibrium_sims_2000' # format used on Wilkcomp
      filename = 'exp_batch5' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
exp_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> exp_set_tidy$call

# write to bash shell script
exp_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    #glue::glue({path_to_scripts_Wilkcomp}'run_msprime_eq.sh'),
    'msprime_exp_5.sh', # updated for TACC
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )







# usage: mspms [-h] [--mutation-rate theta] [--trees]
# [--recombination rho num_loci]
# [--gene-conversion gc_recomb_ratio tract_length]
# [--hotspots HOTSPOTS [HOTSPOTS ...]]
# [--structure value [value ...]]
# [--migration-matrix-entry i j rate]
# [--migration-matrix entry [entry ...]]
# [--migration-rate-change t x]
# [--migration-matrix-entry-change time i j rate]
# [--migration-matrix-change entry [entry ...]]
# [--growth-rate alpha]
# [--population-growth-rate population_id alpha]
# [--population-size population_id size]
# [--growth-rate-change t alpha]
# [--population-growth-rate-change t population_id alpha]
# [--size-change t x] [--population-size-change t population_id x]
# [--population-split t i j]
# [--admixture t population_id proportion]
# [--random-seeds x1 x2 x3] [--precision PRECISION] [-V]
# [-f FILENAME]
# sample_size num_replicates





# simulations for duplicate parameter analysis

# SET A ------ fixed mu, vary N

# general msprime function call used for all sims with fixed mu
msprime_sim <- function(morgans_per_bp, ne, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50 # number of chromosomes
  mu <- 1.5e-8 # fixed only for set A
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

# generate small populations
small_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_ne <- 1000
  msprime_sim(
    morgans_per_bp = mbp,
    ne = sim_ne,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for small populations
small_n_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      small_pop, 
      path = path_to_data_TACC,
      filename = 'n_1000_mu_fixed' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
small_n_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> small_n_set_tidy$call

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


# generate medium sized populations
med_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_ne <- 10000
  msprime_sim(
    morgans_per_bp = mbp,
    ne = sim_ne,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for medium populations
med_n_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      med_pop, 
      path = path_to_data_TACC,
      filename = 'n_10000_mu_fixed' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
med_n_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> med_n_set_tidy$call

# write to bash shell script
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


# generate large populations
large_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_ne <- 50000
  msprime_sim(
    morgans_per_bp = mbp,
    ne = sim_ne,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for large populations
large_n_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      large_pop, 
      path = path_to_data_TACC,
      filename = 'n_50000_mu_fixed' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
large_n_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> large_n_set_tidy$call

# write to bash shell script
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
msprime_sim_b <- function(morgans_per_bp, mu, alpha = 0, output){
  
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
low_mu_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_mu <- 1.5e-9
  msprime_sim_b(
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
      low_mu_pop, 
      path = path_to_data_TACC,
      filename = 'mu_low_n_fixed' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
low_mu_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> low_mu_set_tidy$call

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


# generate medium mutation population
med_mu_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_mu <- 1.5e-8
  msprime_sim_b(
    morgans_per_bp = mbp,
    mu = sim_mu,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for populations with medium mutation
med_mu_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      med_mu_pop, 
      path = path_to_data_TACC,
      filename = 'mu_med_n_fixed' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
med_mu_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> med_mu_set_tidy$call

# write to bash shell script
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


# generate high mutation population
high_mu_pop <- function(index, path, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  sim_mu <- 1.5e-7
  msprime_sim_b(
    morgans_per_bp = mbp,
    mu = sim_mu,
    #output = glue::glue('{path}{filename}') # format used on Wilkcomp, one output file
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for populations with low mutation
high_mu_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      high_mu_pop, 
      path = path_to_data_TACC,
      filename = 'mu_high_n_fixed' # updated to use with launcher on TACC
    )
  )

# update function call for TACC only
high_mu_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> high_mu_set_tidy$call

# write to bash shell script
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

