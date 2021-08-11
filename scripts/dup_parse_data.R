#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Parse data sets for comparisons of msprime simulation under different parameter conditions
# This script: parse and save simulation data (alignments, rhos, seg sites)
# Mackenzie M. Johnson
# July 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(glue)

# load R package
library(devtools)
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
# fixed mu sims
mu1_n1 <- 'fixed_mu/fixed_mu1_n_100_sims_all.txt'
mu1_n2 <- 'fixed_mu/fixed_mu1_n_316_sims_all.txt'
mu1_n3 <- 'fixed_mu/fixed_mu1_n_1000_sims_all.txt'
mu1_n4 <- 'fixed_mu/fixed_mu1_n_3160_sims_all.txt'
mu1_n5 <- 'fixed_mu/fixed_mu1_n_10000_sims_all.txt'
mu1_n6 <- 'fixed_mu/fixed_mu1_n_31600_sims_all.txt'

mu2_n1 <- 'fixed_mu/fixed_mu2_n_100_sims_all.txt'
mu2_n2 <- 'fixed_mu/fixed_mu2_n_316_sims_all.txt'
mu2_n3 <- 'fixed_mu/fixed_mu2_n_1000_sims_all.txt'
mu2_n4 <- 'fixed_mu/fixed_mu2_n_3160_sims_all.txt'
mu2_n5 <- 'fixed_mu/fixed_mu2_n_10000_sims_all.txt'
mu2_n6 <- 'fixed_mu/fixed_mu2_n_31600_sims_all.txt'

mu3_n1 <- 'fixed_mu/fixed_mu3_n_100_sims_all.txt'
mu3_n2 <- 'fixed_mu/fixed_mu3_n_316_sims_all.txt'
mu3_n3 <- 'fixed_mu/fixed_mu3_n_1000_sims_all.txt'
mu3_n4 <- 'fixed_mu/fixed_mu3_n_3160_sims_all.txt'
mu3_n5 <- 'fixed_mu/fixed_mu3_n_10000_sims_all.txt'
mu3_n6 <- 'fixed_mu/fixed_mu3_n_31600_sims_all.txt'

# fixed n sims
n1_mu1 <- 'fixed_n/fixed_n1_mu1_sims_all.txt'
n1_mu2 <- 'fixed_n/fixed_n1_mu2_sims_all.txt'
n1_mu3 <- 'fixed_n/fixed_n1_mu3_sims_all.txt'
n1_mu4 <- 'fixed_n/fixed_n1_mu4_sims_all.txt'
n1_mu5 <- 'fixed_n/fixed_n1_mu5_sims_all.txt'
n1_mu6 <- 'fixed_n/fixed_n1_mu6_sims_all.txt'

n2_mu1 <- 'fixed_n/fixed_n2_mu1_sims_all.txt'
n2_mu2 <- 'fixed_n/fixed_n2_mu2_sims_all.txt'
n2_mu3 <- 'fixed_n/fixed_n2_mu3_sims_all.txt'
n2_mu4 <- 'fixed_n/fixed_n2_mu4_sims_all.txt'
n2_mu5 <- 'fixed_n/fixed_n2_mu5_sims_all.txt'
n2_mu6 <- 'fixed_n/fixed_n2_mu6_sims_all.txt'

n3_mu1 <- 'fixed_n/fixed_n3_mu1_sims_all.txt'
n3_mu2 <- 'fixed_n/fixed_n3_mu2_sims_all.txt'
n3_mu3 <- 'fixed_n/fixed_n3_mu3_sims_all.txt'
n3_mu4 <- 'fixed_n/fixed_n3_mu4_sims_all.txt'
n3_mu5 <- 'fixed_n/fixed_n3_mu5_sims_all.txt'
n3_mu6 <- 'fixed_n/fixed_n3_mu6_sims_all.txt'

# size of alignments
num_chrom <- 50


#--------------- READ IN DATA --------------------

# read in data for set: fixed mu
pop_mu1_n1 <- readLines(file.path(path_to_raw, mu1_n1))
pop_mu1_n2 <- readLines(file.path(path_to_raw, mu1_n2))
pop_mu1_n3 <- readLines(file.path(path_to_raw, mu1_n3))
pop_mu1_n4 <- readLines(file.path(path_to_raw, mu1_n4))
pop_mu1_n5 <- readLines(file.path(path_to_raw, mu1_n5))
pop_mu1_n6 <- readLines(file.path(path_to_raw, mu1_n6))

pop_mu2_n1 <- readLines(file.path(path_to_raw, mu2_n1))
pop_mu2_n2 <- readLines(file.path(path_to_raw, mu2_n2))
pop_mu2_n3 <- readLines(file.path(path_to_raw, mu2_n3))
pop_mu2_n4 <- readLines(file.path(path_to_raw, mu2_n4))
pop_mu2_n5 <- readLines(file.path(path_to_raw, mu2_n5))
pop_mu2_n6 <- readLines(file.path(path_to_raw, mu2_n6))

pop_mu3_n1 <- readLines(file.path(path_to_raw, mu3_n1))
pop_mu3_n2 <- readLines(file.path(path_to_raw, mu3_n2))
pop_mu3_n3 <- readLines(file.path(path_to_raw, mu3_n3))
pop_mu3_n4 <- readLines(file.path(path_to_raw, mu3_n4))
pop_mu3_n5 <- readLines(file.path(path_to_raw, mu3_n5))
pop_mu3_n6 <- readLines(file.path(path_to_raw, mu3_n6))

# read in data for set: fixed n
pop_n1_mu1 <- readLines(file.path(path_to_raw, n1_mu1))
pop_n1_mu2 <- readLines(file.path(path_to_raw, n1_mu2))
pop_n1_mu3 <- readLines(file.path(path_to_raw, n1_mu3))
pop_n1_mu4 <- readLines(file.path(path_to_raw, n1_mu4))
pop_n1_mu5 <- readLines(file.path(path_to_raw, n1_mu5))
pop_n1_mu6 <- readLines(file.path(path_to_raw, n1_mu6))

pop_n2_mu1 <- readLines(file.path(path_to_raw, n2_mu1))
pop_n2_mu2 <- readLines(file.path(path_to_raw, n2_mu2))
pop_n2_mu3 <- readLines(file.path(path_to_raw, n2_mu3))
pop_n2_mu4 <- readLines(file.path(path_to_raw, n2_mu4))
pop_n2_mu5 <- readLines(file.path(path_to_raw, n2_mu5))
pop_n2_mu6 <- readLines(file.path(path_to_raw, n2_mu6))

pop_n3_mu1 <- readLines(file.path(path_to_raw, n3_mu1))
pop_n3_mu2 <- readLines(file.path(path_to_raw, n3_mu2))
pop_n3_mu3 <- readLines(file.path(path_to_raw, n3_mu3))
pop_n3_mu4 <- readLines(file.path(path_to_raw, n3_mu4))
pop_n3_mu5 <- readLines(file.path(path_to_raw, n3_mu5))
pop_n3_mu6 <- readLines(file.path(path_to_raw, n3_mu6))

# remove file names
rm(mu1_n1, mu1_n2, mu1_n3, mu1_n4, mu1_n5, mu1_n6)
rm(mu2_n1, mu2_n2, mu2_n3, mu2_n4, mu2_n5, mu2_n6)
rm(mu3_n1, mu3_n2, mu3_n3, mu3_n4, mu3_n5, mu3_n6)

rm(n1_mu1, n1_mu2, n1_mu3, n1_mu4, n1_mu5, n1_mu6)
rm(n2_mu1, n2_mu2, n2_mu3, n2_mu4, n2_mu5, n2_mu6)
rm(n3_mu1, n3_mu2, n3_mu3, n3_mu4, n3_mu5, n3_mu6)


#--------------- PARSE DATA: ALIGNMENTS, RHOS, SEG SITES --------------------

# read in alignments
# fixed mu
pop_mu1_n1_unsorted <- get_alignment_data(pop_mu1_n1)
pop_mu1_n2_unsorted <- get_alignment_data(pop_mu1_n2)
pop_mu1_n3_unsorted <- get_alignment_data(pop_mu1_n3)
pop_mu1_n4_unsorted <- get_alignment_data(pop_mu1_n4)
pop_mu1_n5_unsorted <- get_alignment_data(pop_mu1_n5)
pop_mu1_n6_unsorted <- get_alignment_data(pop_mu1_n6)

pop_mu2_n1_unsorted <- get_alignment_data(pop_mu2_n1)
pop_mu2_n2_unsorted <- get_alignment_data(pop_mu2_n2)
pop_mu2_n3_unsorted <- get_alignment_data(pop_mu2_n3)
pop_mu2_n4_unsorted <- get_alignment_data(pop_mu2_n4)
pop_mu2_n5_unsorted <- get_alignment_data(pop_mu2_n5)
pop_mu2_n6_unsorted <- get_alignment_data(pop_mu2_n6)

pop_mu3_n1_unsorted <- get_alignment_data(pop_mu3_n1)
pop_mu3_n2_unsorted <- get_alignment_data(pop_mu3_n2)
pop_mu3_n3_unsorted <- get_alignment_data(pop_mu3_n3)
pop_mu3_n4_unsorted <- get_alignment_data(pop_mu3_n4)
pop_mu3_n5_unsorted <- get_alignment_data(pop_mu3_n5)
pop_mu3_n6_unsorted <- get_alignment_data(pop_mu3_n6)

# fixed n
pop_n1_mu1_unsorted <- get_alignment_data(pop_n1_mu1)
pop_n1_mu2_unsorted <- get_alignment_data(pop_n1_mu2)
pop_n1_mu3_unsorted <- get_alignment_data(pop_n1_mu3)
pop_n1_mu4_unsorted <- get_alignment_data(pop_n1_mu4)
pop_n1_mu5_unsorted <- get_alignment_data(pop_n1_mu5)
pop_n1_mu6_unsorted <- get_alignment_data(pop_n1_mu6)

pop_n2_mu1_unsorted <- get_alignment_data(pop_n2_mu1)
pop_n2_mu2_unsorted <- get_alignment_data(pop_n2_mu2)
pop_n2_mu3_unsorted <- get_alignment_data(pop_n2_mu3)
pop_n2_mu4_unsorted <- get_alignment_data(pop_n2_mu4)
pop_n2_mu5_unsorted <- get_alignment_data(pop_n2_mu5)
pop_n2_mu6_unsorted <- get_alignment_data(pop_n2_mu6)

pop_n3_mu1_unsorted <- get_alignment_data(pop_n3_mu1)
pop_n3_mu2_unsorted <- get_alignment_data(pop_n3_mu2)
pop_n3_mu3_unsorted <- get_alignment_data(pop_n3_mu3)
pop_n3_mu4_unsorted <- get_alignment_data(pop_n3_mu4)
pop_n3_mu5_unsorted <- get_alignment_data(pop_n3_mu5)
pop_n3_mu6_unsorted <- get_alignment_data(pop_n3_mu6)


# save alignment data
# fixed mu
save(
  pop_mu1_n1_unsorted, pop_mu1_n2_unsorted, pop_mu1_n3_unsorted,
  pop_mu1_n4_unsorted, pop_mu1_n5_unsorted, pop_mu1_n6_unsorted,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu1_vary_n_align.RData')
)

save(
  pop_mu2_n1_unsorted, pop_mu2_n2_unsorted, pop_mu2_n3_unsorted,
  pop_mu2_n4_unsorted, pop_mu2_n5_unsorted, pop_mu2_n6_unsorted,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu2_vary_n_align.RData')
)

save(
  pop_mu3_n1_unsorted, pop_mu3_n2_unsorted, pop_mu3_n3_unsorted,
  pop_mu3_n4_unsorted, pop_mu3_n5_unsorted, pop_mu3_n6_unsorted,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu3_vary_n_align.RData')
)

# fixed n
save(
  pop_n1_mu1_unsorted, pop_n1_mu2_unsorted, pop_n1_mu3_unsorted, 
  pop_n1_mu4_unsorted, pop_n1_mu5_unsorted, pop_n1_mu6_unsorted,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n1_vary_mu_align.RData')
)

save(
  pop_n2_mu1_unsorted, pop_n2_mu2_unsorted, pop_n2_mu3_unsorted, 
  pop_n2_mu4_unsorted, pop_n2_mu5_unsorted, pop_n2_mu6_unsorted,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n2_vary_mu_align.RData')
)

save(
  pop_n3_mu1_unsorted, pop_n3_mu2_unsorted, pop_n3_mu3_unsorted, 
  pop_n3_mu4_unsorted, pop_n3_mu5_unsorted, pop_n3_mu6_unsorted,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n3_vary_mu_align.RData')
)


# remove alignments
rm(pop_mu1_n1_unsorted, pop_mu1_n2_unsorted, pop_mu1_n3_unsorted,
   pop_mu1_n4_unsorted, pop_mu1_n5_unsorted, pop_mu1_n6_unsorted)

rm(pop_mu2_n1_unsorted, pop_mu2_n2_unsorted, pop_mu2_n3_unsorted,
   pop_mu2_n4_unsorted, pop_mu2_n5_unsorted, pop_mu2_n6_unsorted)

rm(pop_mu3_n1_unsorted, pop_mu3_n2_unsorted, pop_mu3_n3_unsorted,
   pop_mu3_n4_unsorted, pop_mu3_n5_unsorted, pop_mu3_n6_unsorted)

rm(pop_n1_mu1_unsorted, pop_n1_mu2_unsorted, pop_n1_mu3_unsorted,
   pop_n1_mu4_unsorted, pop_n1_mu5_unsorted, pop_n1_mu6_unsorted)

rm(pop_n2_mu1_unsorted, pop_n2_mu2_unsorted, pop_n2_mu3_unsorted, 
   pop_n2_mu4_unsorted, pop_n2_mu5_unsorted, pop_n2_mu6_unsorted)

rm(pop_n3_mu1_unsorted, pop_n3_mu2_unsorted, pop_n3_mu3_unsorted, 
   pop_n3_mu4_unsorted, pop_n3_mu5_unsorted, pop_n3_mu6_unsorted)


#--------------- PARSE DATA: RHOS --------------------
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

# fixed mu set
pop_mu1_n1_rho <- get_rho_msp(pop_mu1_n1)
pop_mu1_n2_rho <- get_rho_msp(pop_mu1_n2)
pop_mu1_n3_rho <- get_rho_msp(pop_mu1_n3)
pop_mu1_n4_rho <- get_rho_msp(pop_mu1_n4)
pop_mu1_n5_rho <- get_rho_msp(pop_mu1_n5)
pop_mu1_n6_rho <- get_rho_msp(pop_mu1_n6)

pop_mu2_n1_rho <- get_rho_msp(pop_mu2_n1)
pop_mu2_n2_rho <- get_rho_msp(pop_mu2_n2)
pop_mu2_n3_rho <- get_rho_msp(pop_mu2_n3)
pop_mu2_n4_rho <- get_rho_msp(pop_mu2_n4)
pop_mu2_n5_rho <- get_rho_msp(pop_mu2_n5)
pop_mu2_n6_rho <- get_rho_msp(pop_mu2_n6)

pop_mu3_n1_rho <- get_rho_msp(pop_mu3_n1)
pop_mu3_n2_rho <- get_rho_msp(pop_mu3_n2)
pop_mu3_n3_rho <- get_rho_msp(pop_mu3_n3)
pop_mu3_n4_rho <- get_rho_msp(pop_mu3_n4)
pop_mu3_n5_rho <- get_rho_msp(pop_mu3_n5)
pop_mu3_n6_rho <- get_rho_msp(pop_mu3_n6)

# fixed n set
pop_n1_mu1_rho <- get_rho_msp(pop_n1_mu1)
pop_n1_mu2_rho <- get_rho_msp(pop_n1_mu2)
pop_n1_mu3_rho <- get_rho_msp(pop_n1_mu3)
pop_n1_mu4_rho <- get_rho_msp(pop_n1_mu4)
pop_n1_mu5_rho <- get_rho_msp(pop_n1_mu5)
pop_n1_mu6_rho <- get_rho_msp(pop_n1_mu6)

pop_n2_mu1_rho <- get_rho_msp(pop_n2_mu1)
pop_n2_mu2_rho <- get_rho_msp(pop_n2_mu2)
pop_n2_mu3_rho <- get_rho_msp(pop_n2_mu3)
pop_n2_mu4_rho <- get_rho_msp(pop_n2_mu4)
pop_n2_mu5_rho <- get_rho_msp(pop_n2_mu5)
pop_n2_mu6_rho <- get_rho_msp(pop_n2_mu6)

pop_n3_mu1_rho <- get_rho_msp(pop_n3_mu1)
pop_n3_mu2_rho <- get_rho_msp(pop_n3_mu2)
pop_n3_mu3_rho <- get_rho_msp(pop_n3_mu3)
pop_n3_mu4_rho <- get_rho_msp(pop_n3_mu4)
pop_n3_mu5_rho <- get_rho_msp(pop_n3_mu5)
pop_n3_mu6_rho <- get_rho_msp(pop_n3_mu6)


# save rho data
# fixed mu
save(
  pop_mu1_n1_rho, pop_mu1_n2_rho, pop_mu1_n3_rho, 
  pop_mu1_n4_rho, pop_mu1_n5_rho, pop_mu1_n6_rho,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu1_vary_n_rho.RData')
)

save(
  pop_mu2_n1_rho, pop_mu2_n2_rho, pop_mu2_n3_rho, 
  pop_mu2_n4_rho, pop_mu2_n5_rho, pop_mu2_n6_rho,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu2_vary_n_rho.RData')
)

save(
  pop_mu3_n1_rho, pop_mu3_n2_rho, pop_mu3_n3_rho, 
  pop_mu3_n4_rho, pop_mu3_n5_rho, pop_mu3_n6_rho,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu3_vary_n_rho.RData')
)

# fixed n
save(
  pop_n1_mu1_rho, pop_n1_mu2_rho, pop_n1_mu3_rho, 
  pop_n1_mu4_rho, pop_n1_mu5_rho, pop_n1_mu6_rho,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n1_vary_mu_rho.RData')
)

save(
  pop_n2_mu1_rho, pop_n2_mu2_rho, pop_n2_mu3_rho, 
  pop_n2_mu4_rho, pop_n2_mu5_rho, pop_n2_mu6_rho,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n2_vary_mu_rho.RData')
)

save(
  pop_n3_mu1_rho, pop_n3_mu2_rho, pop_n3_mu3_rho, 
  pop_n3_mu4_rho, pop_n3_mu5_rho, pop_n3_mu6_rho,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n3_vary_mu_rho.RData')
)


# remove rho data
rm(pop_mu1_n1_rho, pop_mu1_n2_rho, pop_mu1_n3_rho, 
   pop_mu1_n4_rho, pop_mu1_n5_rho, pop_mu1_n6_rho)

rm(pop_mu2_n1_rho, pop_mu2_n2_rho, pop_mu2_n3_rho, 
   pop_mu2_n4_rho, pop_mu2_n5_rho, pop_mu2_n6_rho)

rm(pop_mu3_n1_rho, pop_mu3_n2_rho, pop_mu3_n3_rho, 
   pop_mu3_n4_rho, pop_mu3_n5_rho, pop_mu3_n6_rho)

rm(pop_n1_mu1_rho, pop_n1_mu2_rho, pop_n1_mu3_rho, 
   pop_n1_mu4_rho, pop_n1_mu5_rho, pop_n1_mu6_rho)

rm(pop_n2_mu1_rho, pop_n2_mu2_rho, pop_n2_mu3_rho, 
   pop_n2_mu4_rho, pop_n2_mu5_rho, pop_n2_mu6_rho)

rm(pop_n3_mu1_rho, pop_n3_mu2_rho, pop_n3_mu3_rho, 
   pop_n3_mu4_rho, pop_n3_mu5_rho, pop_n3_mu6_rho)


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

# fixed mu set
pop_mu1_n1_segsites <- get_seg_sites(pop_mu1_n1)
pop_mu1_n2_segsites <- get_seg_sites(pop_mu1_n2)
pop_mu1_n3_segsites <- get_seg_sites(pop_mu1_n3)
pop_mu1_n4_segsites <- get_seg_sites(pop_mu1_n4)
pop_mu1_n5_segsites <- get_seg_sites(pop_mu1_n5)
pop_mu1_n6_segsites <- get_seg_sites(pop_mu1_n6)

pop_mu2_n1_segsites <- get_seg_sites(pop_mu2_n1)
pop_mu2_n2_segsites <- get_seg_sites(pop_mu2_n2)
pop_mu2_n3_segsites <- get_seg_sites(pop_mu2_n3)
pop_mu2_n4_segsites <- get_seg_sites(pop_mu2_n4)
pop_mu2_n5_segsites <- get_seg_sites(pop_mu2_n5)
pop_mu2_n6_segsites <- get_seg_sites(pop_mu2_n6)

pop_mu3_n1_segsites <- get_seg_sites(pop_mu3_n1)
pop_mu3_n2_segsites <- get_seg_sites(pop_mu3_n2)
pop_mu3_n3_segsites <- get_seg_sites(pop_mu3_n3)
pop_mu3_n4_segsites <- get_seg_sites(pop_mu3_n4)
pop_mu3_n5_segsites <- get_seg_sites(pop_mu3_n5)
pop_mu3_n6_segsites <- get_seg_sites(pop_mu3_n6)

# fixed n set
pop_n1_mu1_segsites <- get_seg_sites(pop_n1_mu1)
pop_n1_mu2_segsites <- get_seg_sites(pop_n1_mu2)
pop_n1_mu3_segsites <- get_seg_sites(pop_n1_mu3)
pop_n1_mu4_segsites <- get_seg_sites(pop_n1_mu4)
pop_n1_mu5_segsites <- get_seg_sites(pop_n1_mu5)
pop_n1_mu6_segsites <- get_seg_sites(pop_n1_mu6)

pop_n2_mu1_segsites <- get_seg_sites(pop_n2_mu1)
pop_n2_mu2_segsites <- get_seg_sites(pop_n2_mu2)
pop_n2_mu3_segsites <- get_seg_sites(pop_n2_mu3)
pop_n2_mu4_segsites <- get_seg_sites(pop_n2_mu4)
pop_n2_mu5_segsites <- get_seg_sites(pop_n2_mu5)
pop_n2_mu6_segsites <- get_seg_sites(pop_n2_mu6)

pop_n3_mu1_segsites <- get_seg_sites(pop_n3_mu1)
pop_n3_mu2_segsites <- get_seg_sites(pop_n3_mu2)
pop_n3_mu3_segsites <- get_seg_sites(pop_n3_mu3)
pop_n3_mu4_segsites <- get_seg_sites(pop_n3_mu4)
pop_n3_mu5_segsites <- get_seg_sites(pop_n3_mu5)
pop_n3_mu6_segsites <- get_seg_sites(pop_n3_mu6)


# save seg sites data
# fixed mu
save(
  pop_mu1_n1_segsites, pop_mu1_n2_segsites, pop_mu1_n3_segsites, 
  pop_mu1_n4_segsites, pop_mu1_n5_segsites, pop_mu1_n6_segsites,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu1_vary_n_sites.RData')
)

save(
  pop_mu2_n1_segsites, pop_mu2_n2_segsites, pop_mu2_n3_segsites, 
  pop_mu2_n4_segsites, pop_mu2_n5_segsites, pop_mu2_n6_segsites,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu2_vary_n_sites.RData')
)

save(
  pop_mu3_n1_segsites, pop_mu3_n2_segsites, pop_mu3_n3_segsites, 
  pop_mu3_n4_segsites, pop_mu3_n5_segsites, pop_mu3_n6_segsites,
  file = file.path(path_to_parsed, 'fixed_mu/fixed_mu3_vary_n_sites.RData')
)

# fixed n
save(
  pop_n1_mu1_segsites, pop_n1_mu2_segsites, pop_n1_mu3_segsites,
  pop_n1_mu4_segsites, pop_n1_mu5_segsites, pop_n1_mu6_segsites,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n1_vary_mu_sites.RData')
)

save(
  pop_n2_mu1_segsites, pop_n2_mu2_segsites, pop_n2_mu3_segsites,
  pop_n2_mu4_segsites, pop_n2_mu5_segsites, pop_n2_mu6_segsites,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n2_vary_mu_sites.RData')
)

save(
  pop_n3_mu1_segsites, pop_n3_mu2_segsites, pop_n3_mu3_segsites,
  pop_n3_mu4_segsites, pop_n3_mu5_segsites, pop_n3_mu6_segsites,
  file = file.path(path_to_parsed, 'fixed_n/fixed_n3_vary_mu_sites.RData')
)


# remove data 
rm(pop_mu1_n1_segsites, pop_mu1_n2_segsites, pop_mu1_n3_segsites,
   pop_mu1_n4_segsites, pop_mu1_n5_segsites, pop_mu1_n6_segsites)

rm(pop_mu2_n1_segsites, pop_mu2_n2_segsites, pop_mu2_n3_segsites, 
   pop_mu2_n4_segsites, pop_mu2_n5_segsites, pop_mu2_n6_segsites)

rm(pop_mu3_n1_segsites, pop_mu3_n2_segsites, pop_mu3_n3_segsites, 
   pop_mu3_n4_segsites, pop_mu3_n5_segsites, pop_mu3_n6_segsites)

rm(pop_mu1_n1, pop_mu1_n2, pop_mu1_n3, pop_mu1_n4, pop_mu1_n5, pop_mu1_n6)

rm(pop_mu2_n1, pop_mu2_n2, pop_mu2_n3, pop_mu2_n4, pop_mu2_n5, pop_mu2_n6)

rm(pop_mu3_n1, pop_mu3_n2, pop_mu3_n3, pop_mu3_n4, pop_mu3_n5, pop_mu3_n6)

rm(pop_n1_mu1_segsites, pop_n1_mu2_segsites, pop_n1_mu3_segsites,
   pop_n1_mu4_segsites, pop_n1_mu5_segsites, pop_n1_mu6_segsites)

rm(pop_n2_mu1_segsites, pop_n2_mu2_segsites, pop_n2_mu3_segsites,
   pop_n2_mu4_segsites, pop_n2_mu5_segsites, pop_n2_mu6_segsites)

rm(pop_n3_mu1_segsites, pop_n3_mu2_segsites, pop_n3_mu3_segsites,
   pop_n3_mu4_segsites, pop_n3_mu5_segsites, pop_n3_mu6_segsites)

rm(pop_n1_mu1, pop_n1_mu2, pop_n1_mu3, pop_n1_mu4, pop_n1_mu5, pop_n1_mu6)

rm(pop_n2_mu1, pop_n2_mu2, pop_n2_mu3, pop_n2_mu4, pop_n2_mu5, pop_n2_mu6)

rm(pop_n3_mu1, pop_n3_mu2, pop_n3_mu3, pop_n3_mu4, pop_n3_mu5, pop_n3_mu6)

