#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Generate msprime simulations under different parameter conditions
# This script: create function calls and output shell scripts
# Mackenzie M. Johnson
# July 2021 

#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(readr)
library(glue)

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# paths to data
path_to_data_TACC <- 'Ignore'
path_to_scripts_TACC <- 'Ignore'


#--------------- GENERATE SCRIPTS --------------------

# simulations for duplicate parameter analysis

# fixed mu, vary N
# N values used: 100, 316, 1000, 3160, 10000, 31600
# fixed mu set 1: 1.5e-8
# fixed mu set 2: 1.5e-9
# fixed mu set 3: 1.5e-7

# general msprime function call used for all sims with fixed mu in set 1
msp_sim_fixed_mu <- function(morgans_per_bp, ne, mu, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50     # number of chromosomes
  num_loci <- 20001
  num_replicates <- 1   
  
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

# general function to vary N - set 1
vary_pop <- function(index, path, sim_ne, sim_mu, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  msp_sim_fixed_mu(
    morgans_per_bp = mbp,
    ne = sim_ne,
    mu = sim_mu,
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}

# generate script for different population sizes - set 1
mu1_n1_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 100, 
      sim_mu = 1.5e-8,
      filename = 'n_100_fixed_mu1' 
    )
  )

mu1_n2_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 316, 
      sim_mu = 1.5e-8,
      filename = 'n_316_fixed_mu1' 
    )
  )

mu1_n3_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 1000, 
      sim_mu = 1.5e-8,
      filename = 'n_1000_fixed_mu1' 
    )
  )

mu1_n4_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 3160, 
      sim_mu = 1.5e-8,
      filename = 'n_3160_fixed_mu1' 
    )
  )

mu1_n5_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 10000, 
      sim_mu = 1.5e-8,
      filename = 'n_10000_fixed_mu1' 
    )
  )

mu1_n6_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 31600, 
      sim_mu = 1.5e-8,
      filename = 'n_31600_fixed_mu1' 
    )
  )

# generate script for different population sizes - set 2
mu2_n1_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 100, 
      sim_mu = 1.5e-9,
      filename = 'n_100_fixed_mu2' 
    )
  )

mu2_n2_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 316, 
      sim_mu = 1.5e-9,
      filename = 'n_316_fixed_mu2' 
    )
  )

mu2_n3_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 1000, 
      sim_mu = 1.5e-9,
      filename = 'n_1000_fixed_mu2' 
    )
  )

mu2_n4_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 3160, 
      sim_mu = 1.5e-9,
      filename = 'n_3160_fixed_mu2' 
    )
  )

mu2_n5_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 10000, 
      sim_mu = 1.5e-9,
      filename = 'n_10000_fixed_mu2' 
    )
  )

mu2_n6_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 31600, 
      sim_mu = 1.5e-9,
      filename = 'n_31600_fixed_mu2' 
    )
  )

# generate script for different population sizes - set 3
mu3_n1_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 100, 
      sim_mu = 1.5e-7,
      filename = 'n_100_fixed_mu3' 
    )
  )

mu3_n2_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 316, 
      sim_mu = 1.5e-7,
      filename = 'n_316_fixed_mu3' 
    )
  )

mu3_n3_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 1000, 
      sim_mu = 1.5e-7,
      filename = 'n_1000_fixed_mu3' 
    )
  )

mu3_n4_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 3160, 
      sim_mu = 1.5e-7,
      filename = 'n_3160_fixed_mu3' 
    )
  )

mu3_n5_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 10000, 
      sim_mu = 1.5e-7,
      filename = 'n_10000_fixed_mu3' 
    )
  )

mu3_n6_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_pop, 
      path = path_to_data_TACC,
      sim_ne = 31600, 
      sim_mu = 1.5e-7,
      filename = 'n_31600_fixed_mu3' 
    )
  )


# update function call for TACC - set 1
mu1_n1_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu1_n1_set_tidy$call
mu1_n2_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu1_n2_set_tidy$call
mu1_n3_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu1_n3_set_tidy$call
mu1_n4_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu1_n4_set_tidy$call
mu1_n5_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu1_n5_set_tidy$call
mu1_n6_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu1_n6_set_tidy$call

# update function call for TACC - set 2
mu2_n1_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu2_n1_set_tidy$call
mu2_n2_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu2_n2_set_tidy$call
mu2_n3_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu2_n3_set_tidy$call
mu2_n4_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu2_n4_set_tidy$call
mu2_n5_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu2_n5_set_tidy$call
mu2_n6_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu2_n6_set_tidy$call

# update function call for TACC - set 3
mu3_n1_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu3_n1_set_tidy$call
mu3_n2_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu3_n2_set_tidy$call
mu3_n3_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu3_n3_set_tidy$call
mu3_n4_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu3_n4_set_tidy$call
mu3_n5_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu3_n5_set_tidy$call
mu3_n6_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> mu3_n6_set_tidy$call


# write to bash shell script - set 1
mu1_n1_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu1_n1.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu1_n2_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu1_n2.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu1_n3_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu1_n3.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu1_n4_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu1_n4.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu1_n5_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu1_n5.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu1_n6_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu1_n6.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

# write to bash shell script - set 2
mu2_n1_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu2_n1.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu2_n2_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu2_n2.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu2_n3_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu2_n3.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu2_n4_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu2_n4.sh' 
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu2_n5_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu2_n5.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu2_n6_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu2_n6.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

# write to bash shell script - set 3
mu3_n1_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu3_n1.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu3_n2_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu3_n2.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu3_n3_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu3_n3.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu3_n4_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu3_n4.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu3_n5_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu3_n5.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

mu3_n6_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedmu3_n6.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )


#################
# fixed N, vary mu
# mu values used: 1.0e-10, 3.16e-10, 1.0e-9, 3.16e-9, 1.0e-8, 3.16e-8
# fixed N set 1: 10,000
# fixed N set 2: 1,000
# fixed N set 3: 100,000

# general msprime function call used for all sims with fixed N - set 1
msp_sim_fixed_n <- function(morgans_per_bp, ne, mu, alpha = 0, output){
  
  # fixed parameters
  sample_size <- 50 # number of chromosomes
  num_loci <- 20001
  num_replicates <- 1   
  
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


# generate population with fixed N, variable mu
vary_mu <- function(index, path, sim_ne, sim_mu, filename, ...){
  # picks parameters
  mbp   <- 10^(runif(n = 1,min = -8, max = -6))
  #sim_mu <- 1.5e-9
  msp_sim_fixed_n(
    morgans_per_bp = mbp,
    ne = sim_ne,
    mu = sim_mu,
    output = glue::glue('{filename}_sim{index}.txt'), # added {index} for use with launcher
    ...
  )
}


# generate script for populations with different mutation rates - set 1
n1_mu1_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      sim_mu = 1.0e-10,
      filename = 'mu1_fixed_n1' 
    )
  )

n1_mu2_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      sim_mu = 3.16e-10,
      filename = 'mu2_fixed_n1' 
    )
  )

n1_mu3_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      sim_mu = 1.0e-9,
      filename = 'mu3_fixed_n1' 
    )
  )

n1_mu4_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      sim_mu = 3.16e-9,
      filename = 'mu4_fixed_n1' 
    )
  )

n1_mu5_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      sim_mu = 1.0e-8,
      filename = 'mu5_fixed_n1'
    )
  )

n1_mu6_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 10000,
      sim_mu = 3.16e-8,
      filename = 'mu6_fixed_n1' 
    )
  )

# generate script for populations with different mutation rates - set 2
n2_mu1_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 1000,
      sim_mu = 1.0e-10,
      filename = 'mu1_fixed_n2' 
    )
  )

n2_mu2_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 1000,
      sim_mu = 3.16e-10,
      filename = 'mu2_fixed_n2' 
    )
  )

n2_mu3_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 1000,
      sim_mu = 1.0e-9,
      filename = 'mu3_fixed_n2' 
    )
  )

n2_mu4_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 1000,
      sim_mu = 3.16e-9,
      filename = 'mu4_fixed_n2' 
    )
  )

n2_mu5_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 1000,
      sim_mu = 1.0e-8,
      filename = 'mu5_fixed_n2' 
    )
  )

n2_mu6_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 1000,
      sim_mu = 3.16e-8,
    )
  )

# generate script for populations with different mutation rates - set 3
n3_mu1_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 100000,
      sim_mu = 1.0e-10,
      filename = 'mu1_fixed_n3' 
    )
  )

n3_mu2_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 100000,
      sim_mu = 3.16e-10,
      filename = 'mu2_fixed_n3' 
    )
  )

n3_mu3_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 100000,
      sim_mu = 1.0e-9,
      filename = 'mu3_fixed_n3' 
    )
  )

n3_mu4_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 100000,
      sim_mu = 3.16e-9,
      filename = 'mu4_fixed_n3' 
    )
  )

n3_mu5_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 100000,
      sim_mu = 1.0e-8,
      filename = 'mu5_fixed_n3' 
    )
  )

n3_mu6_set_tidy <- tibble(sim_num = seq(1:20000)) %>% 
  dplyr::mutate(
    call = purrr::map_chr(
      .$sim_num, 
      vary_mu, 
      path = path_to_data_TACC,
      sim_ne = 100000,
      sim_mu = 3.16e-8,
      filename = 'mu6_fixed_n3' 
    )
  )


# update function call for TACC - set 1
n1_mu1_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n1_mu1_set_tidy$call
n1_mu2_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n1_mu2_set_tidy$call
n1_mu3_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n1_mu3_set_tidy$call
n1_mu4_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n1_mu4_set_tidy$call
n1_mu5_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n1_mu5_set_tidy$call
n1_mu6_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n1_mu6_set_tidy$call

# update function call for TACC - set 2
n2_mu1_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n2_mu1_set_tidy$call
n2_mu2_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n2_mu2_set_tidy$call
n2_mu3_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n2_mu3_set_tidy$call
n2_mu4_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n2_mu4_set_tidy$call
n2_mu5_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n2_mu5_set_tidy$call
n2_mu6_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n2_mu6_set_tidy$call

# update function call for TACC - set 3
n3_mu1_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n3_mu1_set_tidy$call
n3_mu2_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n3_mu2_set_tidy$call
n3_mu3_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n3_mu3_set_tidy$call
n3_mu4_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n3_mu4_set_tidy$call
n3_mu5_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n3_mu5_set_tidy$call
n3_mu6_set_tidy$call %>% 
  stringr::str_replace(., "mspms", "~/.local/bin/mspms") -> n3_mu6_set_tidy$call


# write to bash shell script - set 1
n1_mu1_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn1_mu1.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n1_mu2_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn1_mu2.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n1_mu3_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn1_mu3.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n1_mu4_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn1_mu4.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n1_mu5_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn1_mu5.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n1_mu6_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn1_mu6.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

# write to bash shell script - set 2
n2_mu1_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn2_mu1.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n2_mu2_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn2_mu2.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n2_mu3_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn2_mu3.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n2_mu4_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn2_mu4.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n2_mu5_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn2_mu5.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n2_mu6_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn2_mu6.sh'
    ),
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

# write to bash shell script - set 3
n3_mu1_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn3_mu1.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n3_mu2_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn3_mu2.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n3_mu3_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn3_mu3.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n3_mu4_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn3_mu4.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n3_mu5_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn3_mu5.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

n3_mu6_set_tidy %>% 
  select(call) %>% 
  readr::write_delim(
    .,
    file.path(
      'data',
      'msprime_simulations', 
      'msprime_fixedn3_mu6.sh'
    ), 
    delim = "",
    col_names = FALSE,
    quote_escape = FALSE,
    eol = '\n'
  )

