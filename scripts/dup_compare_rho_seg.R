#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: compares all alignments to find duplicates
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

# load in rho data
load(glue('{path_to_data}fixed_mu_vary_n_rho.RData'))
load(glue('{path_to_data}fixed_n_vary_mu_rho.RData'))

# load in seg sites data


#--------------- TIDY RHO DATA --------------------


# join data sets
rho_df <- tibble(
  sm1_pop_rho,
  sm2_pop_rho, 
  sm3_pop_rho,
  small_pop_rho,
  med1_pop_rho, 
  med2_pop_rho,
  med_pop_rho, 
  large_pop_rho,
  lo1_mu_rho,
  lo2_mu_rho,
  low_mu_rho,
  me1_mu_rho,
  med_mu_rho,
  high_mu_rho
) %>% pivot_longer(
  cols = sm1_pop_rho:high_mu_rho,
  names_to = "param_set",
  values_to = "rho"
)

rho_df %>% 
  mutate(
    pop_size = case_when(
      param_set == "sm1_pop_rho" ~ 50,
      param_set == "sm2_pop_rho" ~ 100,
      param_set == "sm3_pop_rho" ~ 500,
      param_set == "small_pop_rho" ~ 1000,
      param_set == "med1_pop_rho" ~ 2000,
      param_set == "med2_pop_rho" ~ 5000,
      param_set == "med_pop_rho" ~ 10000,
      param_set == "large_pop_rho" ~ 50000,
      param_set == "low_mu_rho" |
        param_set == "lo1_mu_rho" |
        param_set == "lo2_mu_rho" |
        param_set == "me1_mu_rho" |
        param_set == "med_mu_rho" | 
        param_set == "high_mu_rho" ~ 10000
    ),
    mut_rate = case_when(
      param_set == "lo1_mu_rho" ~ 1.5e-11,
      param_set == "lo2_mu_rho" ~ 1.5e-10,
      param_set == "low_mu_rho" ~ 1.5e-9,
      param_set == "me1_mu_rho" ~ 0.5e-8,
      param_set == "med_mu_rho" ~ 1.5e-8,
      param_set == "high_mu_rho" ~ 1.5e-7,
      param_set == "small_pop_rho" |
        param_set == "sm1_pop_rho" |
        param_set == "sm2_pop_rho" |
        param_set == "sm3_pop_rho" |
        param_set == "med1_pop_rho" |
        param_set == "med2_pop_rho" |
        param_set == "med_pop_rho" |
        param_set == "large_pop_rho" ~ 1.5e-8,
    ),
    set = case_when(
      param_set == "low_mu_rho" | 
        param_set == "lo1_mu_rho" | 
        param_set == "lo2_mu_rho" | 
        param_set == "me1_mu_rho" |
        param_set == "med_mu_rho" | 
        param_set == "high_mu_rho" ~ "fixed_n",
      param_set == "small_pop_rho" | 
        param_set == "sm1_pop_rho" | 
        param_set == "sm2_pop_rho" | 
        param_set == "sm3_pop_rho" | 
        param_set == "med1_pop_rho" |
        param_set == "med2_pop_rho" |
        param_set == "med_pop_rho" |
        param_set == "large_pop_rho" ~ "fixed_mu"
    )
  ) -> rho_df

rho_df$param_set <- as.factor(rho_df$param_set)
rho_df$set <- as.factor(rho_df$set)


#--------------- SAVE DATA SETS --------------------

save(
  rho_df, 
  file = glue('{path_to_results}dup_analysis_rho_results.RData')
)


