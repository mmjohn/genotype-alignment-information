#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Parse data sets for comparisons of msprime simulation under different parameter conditions
# This script: parse and save simulation data (alignments, rhos, seg sites)
# Mackenzie M. Johnson
# August 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)

# load R package
library(devtools)
devtools::load_all("/stor/home/mmj2238/popgencnn/")


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_raw <- '/stor/work/Wilke/mmj2238/rho_cnn_data/raw/dup_analysis/'
path_to_parsed <- '/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/'

# size of alignments
num_chrom <- 50


#--------------- READ IN DATA --------------------

# read in data for set: low proportion duplicates
pop_low_n1 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_1000_sims_all.txt')
)
pop_low_n2 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_5000_sims_all.txt')
)
pop_low_n3 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_10000_sims_all.txt')
)


# read in data for set: high proportion duplicates
pop_high_n1 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_1000_sims_all.txt')
)
pop_high_n2 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_5000_sims_all.txt')
)
pop_high_n3 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_10000_sims_all.txt')
)


#--------------- PARSE DATA: ALIGNMENTS, RHOS, SEG SITES --------------------

# parse alignments: low proportion duplicates
pop_low_n1_unsorted <- get_alignment_data(pop_low_n1)
pop_low_n2_unsorted <- get_alignment_data(pop_low_n2)
pop_low_n3_unsorted <- get_alignment_data(pop_low_n3)

# parse alignments: high proportion duplicates
pop_high_n1_unsorted <- get_alignment_data(pop_high_n1)
pop_high_n2_unsorted <- get_alignment_data(pop_high_n2)
pop_high_n3_unsorted <- get_alignment_data(pop_high_n3)

# save alignment data
save(
  pop_low_n1_unsorted, pop_low_n2_unsorted, pop_low_n3_unsorted,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_align.RData')
)

save(
  pop_high_n1_unsorted, pop_high_n2_unsorted, pop_high_n3_unsorted, 
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_align.RData')
)

# remove alignments
rm(pop_low_n1_unsorted, pop_low_n2_unsorted, pop_low_n3_unsorted)
rm(pop_high_n1_unsorted, pop_high_n2_unsorted, pop_high_n3_unsorted)


#--------------- PARSE DATA: RHOS --------------------

# read in rho values
# NEED TO MOVE NEW FUNCTION TO PACKAGE

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

# low dup set
pop_low_n1_rho <- get_rho_msp(pop_low_n1)
pop_low_n2_rho <- get_rho_msp(pop_low_n2)
pop_low_n3_rho <- get_rho_msp(pop_low_n3)

# high dup set
pop_high_n1_rho <- get_rho_msp(pop_high_n1)
pop_high_n2_rho <- get_rho_msp(pop_high_n2)
pop_high_n3_rho <- get_rho_msp(pop_high_n3)

# save rho data
save(
  pop_low_n1_rho, pop_low_n2_rho, pop_low_n3_rho,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_rho.RData')
)

save(
  pop_high_n1_rho, pop_high_n2_rho, pop_high_n3_rho,
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_rho.RData')
)

# remove rho data
rm(pop_low_n1_rho, pop_low_n2_rho, pop_low_n3_rho)
rm(pop_high_n1_rho, pop_high_n2_rho, pop_high_n3_rho)


#--------------- PARSE DATA: SEG SITES --------------------

# read in seg. sites
get_seg_sites <- function(all_data){
  
  # get relevant line numbers
  ss_linenum <- which(grepl("^segsites: ", all_data))
  # read those lines
  ss_lines <- all_data[ss_linenum]
  # remove "positions: "
  ss_vec <- ss_lines %>%
    dplyr::tibble() %>%
    tidyr::extract(., ., "(segsites): (.*)",
                   into = c("pos", "keep")) %>%
    dplyr::pull(keep)
  # convert to numeric vectors
  ss_data <- as.numeric(ss_vec)
  
}

# low dup set
pop_low_n1_sites <- get_seg_sites(pop_low_n1)
pop_low_n2_sites <- get_seg_sites(pop_low_n2)
pop_low_n3_sites <- get_seg_sites(pop_low_n3)

# high dup set
pop_high_n1_sites <- get_seg_sites(pop_high_n1)
pop_high_n2_sites <- get_seg_sites(pop_high_n2)
pop_high_n3_sites <- get_seg_sites(pop_high_n3)

# save seg sites data
save(
  pop_low_n1_sites, pop_low_n2_sites, pop_low_n3_sites,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_sites.RData')
)

save(
  pop_high_n1_sites, pop_high_n2_sites, pop_high_n3_sites,
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_sites.RData')
)

# remove data 
rm(pop_low_n1_sites, pop_low_n2_sites, pop_low_n3_sites)
rm(pop_high_n1_sites, pop_high_n2_sites, pop_high_n3_sites)

rm(pop_low_n1, pop_low_n2, pop_low_n3)
rm(pop_high_n1, pop_high_n2, pop_high_n3)


