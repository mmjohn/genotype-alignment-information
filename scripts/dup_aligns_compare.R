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
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50

# max_size - need to use one standard size for consistency across training and test sets
max_size <- 400 

#--------------- READ IN DATA --------------------
Sys.time()
cat("\nReading in data.....\n")

# load alignments
load(glue('{path_to_data}fixed_mu_vary_n_align.RData'))
load(glue('{path_to_data}fixed_n_vary_mu_align.RData'))



#--------------- FIND DUPLICATES --------------------

Sys.time()
cat("\nIdentifying duplicates in parameter sets.....\n")

# compare each parameter set
true_cntr_1 <- 0
num_dupl_1 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        sm_pop_padded[[i]],
        sm_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_1 <- true_cntr_1 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_1 <- num_dupl_1 + 1 
  }
}

true_cntr_1     # 31,921
num_dupl_1      # 1,505
# total_cntr_1    # 199,990,000

rm(does_match, i, j, has_duplicate)

true_cntr_2 <- 0
num_dupl_2 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md_pop_padded[[i]],
        md_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_2 <- true_cntr_2 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_2 <- num_dupl_2 + 1 
  }
}

true_cntr_2     # 0
num_dupl_2      # 0

rm(does_match, i, j, has_duplicate)

true_cntr_3 <- 0
num_dupl_3 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lg_pop_padded[[i]],
        lg_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_3 <- true_cntr_3 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_3 <- num_dupl_3 + 1 
  }
}

true_cntr_3     # 0
num_dupl_3      # 
 
rm(does_match, i, j, has_duplicate)

fixed_mu_vary_n <- tibble(
  mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8),
  pop_size = c(1000, 10000, 50000),
  num_match = c(true_cntr_1, true_cntr_2, true_cntr_3),
  total_dupl = c(num_dupl_1, num_dupl_2, num_dupl_3)
)

rm(sm_pop_padded, md_pop_padded, lg_pop_padded)

true_cntr_4 <- 0
num_dupl_4 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lw_mu_padded[[i]],
        lw_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_4 <- true_cntr_4 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_4 <- num_dupl_4 + 1 
  }
}

true_cntr_4     # 21,897
num_dupl_4      # 1,170
 
rm(does_match, i, j, has_duplicate)

true_cntr_5 <- 0
num_dupl_5 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md_mu_padded[[i]],
        md_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_5 <- true_cntr_5 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_5 <- num_dupl_5 + 1 
  }
}

true_cntr_5     # 0
num_dupl_5

rm(does_match, i, j, has_duplicate)

true_cntr_6 <- 0
num_dupl_6 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        hg_mu_padded[[i]],
        hg_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_6 <- true_cntr_6 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_6 <- num_dupl_6 + 1 
  }
}

true_cntr_6     # 0
num_dupl_6      # 0

rm(does_match, i, j, has_duplicate)

fixed_n_vary_mu <- tibble(
  mut_rate = c(1.5e-9, 1.5e-8, 1.5e-7),
  pop_size = c(10000, 10000, 10000),
  num_match = c(true_cntr_4, true_cntr_5, true_cntr_6),
  total_dupl = c(num_dupl_4, num_dupl_5, num_dupl_6)
)

rm(lw_mu_padded, md_mu_padded, hg_mu_padded)


#--------------- TIDY DATA SETS --------------------

# fixed_mu_vary_n <- tibble(
#   mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8),
#   pop_size = c(1000, 10000, 50000),
#   num_match = c(31921, 0, 0),
#   total_dupl = c(1505, 0, 0)
# )
# 
# fixed_n_vary_mu <- tibble(
#   mut_rate = c(1.5e-9, 1.5e-8, 1.5e-7),
#   pop_size = c(10000, 10000, 10000),
#   num_match = c(21897, 0, 0),
#   total_dupl = c(1170, 0, 0)
# )

fixed_mu_vary_n %>% 
  mutate(
    set = "fixed_mu",
    prop_dup = total_dupl/20000
  ) -> fixed_mu_vary_n

fixed_n_vary_mu %>% 
  mutate(
    set = "fixed_n",
    prop_dup = total_dupl/20000
  ) -> fixed_n_vary_mu

dup_df <- full_join(fixed_mu_vary_n, fixed_n_vary_mu)

rho_df <- tibble(
  small_pop_rho, 
  med_pop_rho, 
  large_pop_rho,
  low_mu_rho,
  med_mu_rho,
  high_mu_rho
) %>% pivot_longer(
  cols = small_pop_rho:high_mu_rho,
  names_to = "param_set",
  values_to = "rho"
)

rho_df %>% 
  mutate(
    pop_size = case_when(
      param_set == "small_pop_rho" ~ 1000,
      param_set == "med_pop_rho" ~ 10000,
      param_set == "large_pop_rho" ~ 50000,
      param_set == "low_mu_rho" | 
        param_set == "med_mu_rho" | 
        param_set == "high_mu_rho" ~ 10000
    ),
    mut_rate = case_when(
      param_set == "low_mu_rho" ~ 1.5e-9,
      param_set == "med_mu_rho" ~ 1.5e-8,
      param_set == "high_mu_rho" ~ 1.5e-7,
      param_set == "small_pop_rho" | 
        param_set == "med_pop_rho" |
        param_set == "large_pop_rho" ~ 1.5e-8,
    ),
    set = case_when(
      param_set == "low_mu_rho" | 
        param_set == "med_mu_rho" | 
        param_set == "high_mu_rho" ~ "fixed_n",
      param_set == "small_pop_rho" | 
        param_set == "med_pop_rho" |
        param_set == "large_pop_rho" ~ "fixed_mu"
    )
  ) -> rho_df


#--------------- SAVE DATA SETS --------------------

save(
  dup_df, rho_df, 
  file = glue('{path_to_results}dup_analysis_results.RData')
)


