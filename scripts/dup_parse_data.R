#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Parse data sets for comparisons of msprime simulation under different parameter conditions
# This script: parse and save simulation data
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
path_to_raw <- '/stor/work/Wilke/mmj2238/rho_cnn_data/raw/dup_analysis/'
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# filenames
s1_n <- 'fixed_mu_n_50_sims_all.txt'
s2_n <- 'fixed_mu_n_100_sims_all.txt'
s3_n <- 'fixed_mu_n_500_sims_all.txt'
s_n <- 'fixed_mu_n_1000_sims_all.txt'
m1_n <- 'fixed_mu_n_2000_sims_all.txt'
m2_n <- 'fixed_mu_n_5000_sims_all.txt'
m_n <- 'fixed_mu_n_10000_sims_all.txt'
l_n <- 'fixed_mu_n_50000_sims_all.txt'

l1_mu <- 'fixed_n_mu_l1_sims_all.txt'
l1_mu <- 'fixed_n_mu_l2_sims_all.txt'
l_mu <- 'fixed_n_low_mu_sims_all.txt'
m1_mu <- 'fixed_n_mu_m1_sims_all.txt'
m_mu <- 'fixed_n_med_mu_sims_all.txt'
h_mu <- 'fixed_n_high_mu_sims_all.txt'

# size of alignments
num_chrom <- 50

# max_size - need to use one standard size for consistency across training and test sets
max_size <- 400 

#--------------- READ IN DATA --------------------
Sys.time()
cat("\nReading in data.....\n")

# read in data for set a
sm1_pop <- readLines(glue('{path_to_raw}{s1_n}'))
sm2_pop <- readLines(glue('{path_to_raw}{s2_n}'))
sm3_pop <- readLines(glue('{path_to_raw}{s3_n}'))
small_pop <- readLines(glue('{path_to_raw}{s_n}'))
m1_pop <- readLines(glue('{path_to_raw}{m1_n}'))
m2_pop <- readLines(glue('{path_to_raw}{m2_n}'))
medium_pop <- readLines(glue('{path_to_raw}{m_n}'))
large_pop <- readLines(glue('{path_to_raw}{l_n}'))

# read in data for set b
lo1_mu <- readLines(glue('{path_to_raw}{l1_mu}'))
lo2_mu <- readLines(glue('{path_to_raw}{l2_mu}'))
low_mu <- readLines(glue('{path_to_raw}{l_mu}'))
me1_mu <- readLines(glue('{path_to_raw}{m1_mu}'))
medium_mu <- readLines(glue('{path_to_raw}{m_mu}'))
high_mu <- readLines(glue('{path_to_raw}{h_mu}'))

rm(s1_n, s2_n, s3_n, s_n, m1_n, m2_n, m_n, l_n, 
   l1_mu, l2_mu, l_mu, m1_mu, m_mu, l_mu)


#--------------- PARSE DATA (ALIGNMENTS AND RHOS) --------------------
Sys.time()
cat("\nParsing data.....\n")

# read in alignments
sm_pop_unsorted <- get_alignment_data(small_pop)
md_pop_unsorted <- get_alignment_data(medium_pop)
lg_pop_unsorted <- get_alignment_data(large_pop)

lw_mu_unsorted <- get_alignment_data(low_mu)
md_mu_unsorted <- get_alignment_data(medium_mu)
hg_mu_unsorted <- get_alignment_data(high_mu)

# read in rhos

# read in rho value (y)  
#get_rho_data() not working - NEED TO MOVE NEW FUNCTION TO PACKAGE
get_rho_msp <- function(all_data) {
  
  # define pattern
  y_pattern <- "^.*-r"
  # get relevant line numbers
  y_linenum <- which(grepl(y_pattern, all_data))
  # read those lines
  y_lines <- all_data[y_linenum]
  # extract just the rho value
  y_vec_rho <- y_lines %>%
    dplyr::tibble() %>%
    tidyr::extract(., ., "(.* --recombination) (.*)",
                   into = c("sim", "keep")) %>%
    dplyr::select(keep) %>%
    tidyr::extract(., keep, "(.*) (20001)",
                   into = c("rho", "constant")) %>%
    dplyr::pull(rho)
  # convert to numeric
  y_data_rho <- as.double(y_vec_rho)
  # return
  return(y_data_rho)
  
}

small_pop_rho <- get_rho_msp(small_pop)
med_pop_rho <- get_rho_msp(medium_pop)
large_pop_rho <- get_rho_msp(large_pop)

low_mu_rho <- get_rho_msp(low_mu)
med_mu_rho <- get_rho_msp(medium_mu)
high_mu_rho <- get_rho_msp(high_mu)

# remove full data set
rm(small_pop, medium_pop, large_pop, low_mu, medium_mu, high_mu)


#--------------- SAVE DATA SETS --------------------

save(
  dup_df, rho_df, 
  file = glue('{path_to_results}dup_analysis_results.RData')
)



