#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Parse data sets for comparisons of msprime simulation under different parameter conditions
# This script: parse and save simulation data (alignments, rhos, seg sites (# and position))
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
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_2000_sims_all.txt')
)
pop_low_n3 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_5000_sims_all.txt')
)
pop_low_n4 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_10000_sims_all.txt')
)
pop_low_n5 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_15000_sims_all.txt')
)
pop_low_n6 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'low_dup_n_20000_sims_all.txt')
)


# read in data for set: high proportion duplicates
pop_high_n1 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_1000_sims_all.txt')
)
pop_high_n2 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_2000_sims_all.txt')
)
pop_high_n3 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_5000_sims_all.txt')
)
pop_high_n4 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_10000_sims_all.txt')
)
pop_high_n5 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_15000_sims_all.txt')
)
pop_high_n6 <- readLines(
  file.path(path_to_raw, 'cnn_dup', 'high_dup_n_20000_sims_all.txt')
)


#--------------- PARSE DATA: ALIGNMENTS, RHOS, SEG SITES --------------------

# parse alignments: low proportion duplicates
pop_low_n1_unsorted <- popgencnn::get_alignment_data(pop_low_n1)
pop_low_n2_unsorted <- popgencnn::get_alignment_data(pop_low_n2)
pop_low_n3_unsorted <- popgencnn::get_alignment_data(pop_low_n3)
pop_low_n4_unsorted <- popgencnn::get_alignment_data(pop_low_n4)
pop_low_n5_unsorted <- popgencnn::get_alignment_data(pop_low_n5)
pop_low_n6_unsorted <- popgencnn::get_alignment_data(pop_low_n6)

# parse alignments: high proportion duplicates
pop_high_n1_unsorted <- popgencnn::get_alignment_data(pop_high_n1)
pop_high_n2_unsorted <- popgencnn::get_alignment_data(pop_high_n2)
pop_high_n3_unsorted <- popgencnn::get_alignment_data(pop_high_n3)
pop_high_n4_unsorted <- popgencnn::get_alignment_data(pop_high_n4)
pop_high_n5_unsorted <- popgencnn::get_alignment_data(pop_high_n5)
pop_high_n6_unsorted <- popgencnn::get_alignment_data(pop_high_n6)

# save alignment data
save(
  pop_low_n1_unsorted, pop_low_n2_unsorted, pop_low_n3_unsorted,
  pop_low_n4_unsorted, pop_low_n5_unsorted, pop_low_n6_unsorted,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_align.RData')
)

save(
  pop_high_n1_unsorted, pop_high_n2_unsorted, pop_high_n3_unsorted, 
  pop_high_n4_unsorted, pop_high_n5_unsorted, pop_high_n6_unsorted, 
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_align.RData')
)

# remove alignments
rm(pop_low_n1_unsorted, pop_low_n2_unsorted, pop_low_n3_unsorted,
   pop_low_n4_unsorted, pop_low_n5_unsorted, pop_low_n6_unsorted)
rm(pop_high_n1_unsorted, pop_high_n2_unsorted, pop_high_n3_unsorted, 
   pop_high_n4_unsorted, pop_high_n5_unsorted, pop_high_n6_unsorted)


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
pop_low_n4_rho <- get_rho_msp(pop_low_n4)
pop_low_n5_rho <- get_rho_msp(pop_low_n5)
pop_low_n6_rho <- get_rho_msp(pop_low_n6)

# high dup set
pop_high_n1_rho <- get_rho_msp(pop_high_n1)
pop_high_n2_rho <- get_rho_msp(pop_high_n2)
pop_high_n3_rho <- get_rho_msp(pop_high_n3)
pop_high_n4_rho <- get_rho_msp(pop_high_n4)
pop_high_n5_rho <- get_rho_msp(pop_high_n5)
pop_high_n6_rho <- get_rho_msp(pop_high_n6)

# save rho data
save(
  pop_low_n1_rho, pop_low_n2_rho, pop_low_n3_rho,
  pop_low_n4_rho, pop_low_n5_rho, pop_low_n6_rho,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_rho.RData')
)

save(
  pop_high_n1_rho, pop_high_n2_rho, pop_high_n3_rho,
  pop_high_n4_rho, pop_high_n5_rho, pop_high_n6_rho,
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_rho.RData')
)

# remove rho data
rm(pop_low_n1_rho, pop_low_n2_rho, pop_low_n3_rho,
   pop_low_n4_rho, pop_low_n5_rho, pop_low_n6_rho)
rm(pop_high_n1_rho, pop_high_n2_rho, pop_high_n3_rho,
   pop_high_n4_rho, pop_high_n5_rho, pop_high_n6_rho)


#--------------- PARSE DATA: SEG SITES (#) --------------------

# read in seg. sites - MOVE TO PACKAGE
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
pop_low_n4_sites <- get_seg_sites(pop_low_n4)
pop_low_n5_sites <- get_seg_sites(pop_low_n5)
pop_low_n6_sites <- get_seg_sites(pop_low_n6)

# high dup set
pop_high_n1_sites <- get_seg_sites(pop_high_n1)
pop_high_n2_sites <- get_seg_sites(pop_high_n2)
pop_high_n3_sites <- get_seg_sites(pop_high_n3)
pop_high_n4_sites <- get_seg_sites(pop_high_n4)
pop_high_n5_sites <- get_seg_sites(pop_high_n5)
pop_high_n6_sites <- get_seg_sites(pop_high_n6)

# save seg sites data
save(
  pop_low_n1_sites, pop_low_n2_sites, pop_low_n3_sites,
  pop_low_n4_sites, pop_low_n5_sites, pop_low_n6_sites,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_sites.RData')
)

save(
  pop_high_n1_sites, pop_high_n2_sites, pop_high_n3_sites,
  pop_high_n4_sites, pop_high_n5_sites, pop_high_n6_sites,
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_sites.RData')
)

# remove data 
rm(pop_low_n1_sites, pop_low_n2_sites, pop_low_n3_sites,
   pop_low_n4_sites, pop_low_n5_sites, pop_low_n6_sites)
rm(pop_high_n1_sites, pop_high_n2_sites, pop_high_n3_sites,
   pop_high_n4_sites, pop_high_n5_sites, pop_high_n6_sites)


#--------------- PARSE DATA: SEG SITES (POS) --------------------

# read in variable site positions
# low dup set
pop_low_n1_pos <- popgencnn::get_pos_data(pop_low_n1)
pop_low_n2_pos <- popgencnn::get_pos_data(pop_low_n2)
pop_low_n3_pos <- popgencnn::get_pos_data(pop_low_n3)
pop_low_n4_pos <- popgencnn::get_pos_data(pop_low_n4)
pop_low_n5_pos <- popgencnn::get_pos_data(pop_low_n5)
pop_low_n6_pos <- popgencnn::get_pos_data(pop_low_n6)

# high dup set
pop_high_n1_pos <- popgencnn::get_pos_data(pop_high_n1)
pop_high_n2_pos <- popgencnn::get_pos_data(pop_high_n2)
pop_high_n3_pos <- popgencnn::get_pos_data(pop_high_n3)
pop_high_n4_pos <- popgencnn::get_pos_data(pop_high_n4)
pop_high_n5_pos <- popgencnn::get_pos_data(pop_high_n5)
pop_high_n6_pos <- popgencnn::get_pos_data(pop_high_n6)

# save position data
save(
  pop_low_n1_pos, pop_low_n2_pos, pop_low_n3_pos,
  pop_low_n4_pos, pop_low_n5_pos, pop_low_n6_pos,
  file = file.path(path_to_parsed, 'cnn_dup', 'low_dup_pos.RData')
)

save(
  pop_high_n1_pos, pop_high_n2_pos, pop_high_n3_pos,
  pop_high_n4_pos, pop_high_n5_pos, pop_high_n6_pos,
  file = file.path(path_to_parsed, 'cnn_dup', 'high_dup_pos.RData')
)

# remove data
rm(pop_low_n1_pos, pop_low_n2_pos, pop_low_n3_pos,
   pop_low_n4_pos, pop_low_n5_pos, pop_low_n6_pos)
rm(pop_high_n1_pos, pop_high_n2_pos, pop_high_n3_pos,
   pop_high_n4_pos, pop_high_n5_pos, pop_high_n6_pos)

rm(pop_low_n1, pop_low_n2, pop_low_n3, pop_low_n4, pop_low_n5, pop_low_n6)
rm(pop_high_n1, pop_high_n2, pop_high_n3, pop_high_n4, pop_high_n5, pop_high_n6)


