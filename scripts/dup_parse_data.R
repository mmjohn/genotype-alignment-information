#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Parse data sets for comparisons of msprime simulation under different parameter conditions
# This script: parse and save simulation data (alignments, rhos, seg sites)
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

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_raw <- '/stor/work/Wilke/mmj2238/rho_cnn_data/raw/dup_analysis/'
path_to_parsed <- '/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/'

# filenames
n_s1 <- 'fixed_mu_n_50_sims_all.txt'
n_s2 <- 'fixed_mu_n_100_sims_all.txt'
n_s3 <- 'fixed_mu_n_500_sims_all.txt'
n_s <- 'fixed_mu_n_1000_sims_all.txt'
n_m1 <- 'fixed_mu_n_2000_sims_all.txt'
n_m2 <- 'fixed_mu_n_5000_sims_all.txt'
n_m <- 'fixed_mu_n_10000_sims_all.txt'
n_l <- 'fixed_mu_n_50000_sims_all.txt'

mu_l1 <- 'fixed_n_mu_l1_sims_all.txt'
mu_l2 <- 'fixed_n_mu_l2_sims_all.txt'
mu_l <- 'fixed_n_low_mu_sims_all.txt'
mu_m1 <- 'fixed_n_mu_m1_sims_all.txt'
mu_m <- 'fixed_n_med_mu_sims_all.txt'
mu_h <- 'fixed_n_high_mu_sims_all.txt'

# size of alignments
num_chrom <- 50


#--------------- READ IN DATA --------------------
Sys.time()
cat("\nReading in data.....\n")

# read in data for set a
sm1_pop <- readLines(glue('{path_to_raw}{n_s1}'))
sm2_pop <- readLines(glue('{path_to_raw}{n_s2}'))
sm3_pop <- readLines(glue('{path_to_raw}{n_s3}'))
small_pop <- readLines(glue('{path_to_raw}{n_s}'))
m1_pop <- readLines(glue('{path_to_raw}{n_m1}'))
m2_pop <- readLines(glue('{path_to_raw}{n_m2}'))
medium_pop <- readLines(glue('{path_to_raw}{n_m}'))
large_pop <- readLines(glue('{path_to_raw}{n_l}'))

# read in data for set b
lo1_mu <- readLines(glue('{path_to_raw}{mu_l1}'))
lo2_mu <- readLines(glue('{path_to_raw}{mu_l2}'))
low_mu <- readLines(glue('{path_to_raw}{mu_l}'))
me1_mu <- readLines(glue('{path_to_raw}{mu_m1}'))
medium_mu <- readLines(glue('{path_to_raw}{mu_m}'))
high_mu <- readLines(glue('{path_to_raw}{mu_h}'))

rm(n_s1, n_s2, n_s, n_m1, n_m2, n_m, n_l)
rm(mu_l1, mu_l2, mu_l, mu_m1, mu_m, mu_h)


#--------------- PARSE DATA (ALIGNMENTS, RHOS, SEG SITES) --------------------
Sys.time()
cat("\nParsing data.....\n")

# read in alignments
sm1_pop_unsorted <- get_alignment_data(sm1_pop)
sm2_pop_unsorted <- get_alignment_data(sm2_pop)
sm3_pop_unsorted <- get_alignment_data(sm3_pop)
sm_pop_unsorted <- get_alignment_data(small_pop)
md1_pop_unsorted <- get_alignment_data(m1_pop)
md2_pop_unsorted <- get_alignment_data(m2_pop)
md_pop_unsorted <- get_alignment_data(medium_pop)
lg_pop_unsorted <- get_alignment_data(large_pop)

lw1_mu_unsorted <- get_alignment_data(lo1_mu)
lw2_mu_unsorted <- get_alignment_data(lo2_mu)
lw_mu_unsorted <- get_alignment_data(low_mu)
md1_mu_unsorted <- get_alignment_data(me1_mu)
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

sm1_pop_rho <- get_rho_msp(sm1_pop)
sm2_pop_rho <- get_rho_msp(sm2_pop)
sm3_pop_rho <- get_rho_msp(sm3_pop)
small_pop_rho <- get_rho_msp(small_pop)
med1_pop_rho <- get_rho_msp(m1_pop)
med2_pop_rho <- get_rho_msp(m2_pop)
med_pop_rho <- get_rho_msp(medium_pop)
large_pop_rho <- get_rho_msp(large_pop)

lo1_mu_rho <- get_rho_msp(lo1_mu)
lo2_mu_rho <- get_rho_msp(lo2_mu)
low_mu_rho <- get_rho_msp(low_mu)
me1_mu_rho <- get_rho_msp(me1_mu)
med_mu_rho <- get_rho_msp(medium_mu)
high_mu_rho <- get_rho_msp(high_mu)

# remove full data set
rm(sm1_pop, sm2_pop, sm3_pop, small_pop, m1_pop, m2_pop, medium_pop, large_pop)
rm(lo1_mu, lo2_mu, low_mu, me1_mu, medium_mu, high_mu)


#--------------- SAVE DATA SETS --------------------

save(
  sm1_pop_unsorted, sm2_pop_unsorted, sm3_pop_unsorted,
  sm_pop_unsorted, md1_pop_unsorted, md2_pop_unsorted, 
  md_pop_unsorted, lg_pop_unsorted, 
  file = glue('{path_to_parsed}fixed_mu_vary_n_align.RData')
)

save(
  sm1_pop_rho, sm2_pop_rho, sm3_pop_rho, small_pop_rho, 
  med1_pop_rho, med2_pop_rho, med_pop_rho, large_pop_rho,
  file = glue('{path_to_parsed}fixed_mu_vary_n_rho.RData')
)

save(
  lw1_mu_unsorted, lw2_mu_unsorted, lw_mu_unsorted, 
  md1_mu_unsorted, md_mu_unsorted, hg_mu_unsorted,
  file = glue('{path_to_parsed}fixed_n_vary_mu_align.RData')
)

save(
  lo1_mu_rho, lo2_mu_rho, low_mu_rho, 
  me1_mu_rho, med_mu_rho, high_mu_rho,
  file = glue('{path_to_parsed}fixed_n_vary_mu_rho.RData')
)


