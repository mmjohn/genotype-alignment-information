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
Sys.time()
cat("\nReading in data.....\n")

# load alignments - fixed mu
load(glue('{path_to_data}fixed_mu_vary_n_align_processed.RData'))

# load alignments - fixed n
load(glue('{path_to_data}fixed_n_vary_mu_align_processed.RData'))



#--------------- FIND DUPLICATES - FIXED MU --------------------

Sys.time()
cat("\nIdentifying duplicates in parameter sets.....\n")

# compare each parameter set
#true_cntr_sm1 <- 0
num_dupl_sm1 <- 0
sm1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        sm1_pop_padded[[i]],
        sm1_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_sm1 <- true_cntr_sm1 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_sm1 <- num_dupl_sm1 + 1 
    sm1_index <- c(sm1_index, i)
  }
}

#true_cntr_sm1     # 118,657,920
num_dupl_sm1      # 19,431

rm(does_match, i, j, has_duplicate)
rm(sm1_pop_padded)

# SAVE INDEX

#true_cntr_sm2 <- 0
num_dupl_sm2 <- 0
sm2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        sm2_pop_padded[[i]],
        sm2_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_sm2 <- true_cntr_sm2 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_sm2 <- num_dupl_sm2 + 1 
    sm2_index <- c(sm2_index, i)
  }
}

#true_cntr_sm2     # 71,130,698
num_dupl_sm2      # 18,499

rm(does_match, i, j, has_duplicate)
rm(sm2_pop_padded)

#true_cntr_sm3 <- 0
num_dupl_sm3 <- 0
sm3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        sm3_pop_padded[[i]],
        sm3_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_sm3 <- true_cntr_sm3 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_sm3 <- num_dupl_sm3 + 1 
    sm3_index <- c(sm3_index, i)
  }
}

#true_cntr_sm3     # 1,925,461
num_dupl_sm3      # 8,209

rm(does_match, i, j, has_duplicate)
rm(sm3_pop_padded)

#true_cntr_sm <- 0
num_dupl_sm <- 0
sm_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        sm_pop_padded[[i]],
        sm_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_sm <- true_cntr_sm + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_sm <- num_dupl_sm + 1 
    sm_index <- c(sm_index, i)
  }
}

#true_cntr_sm     # 31,921
num_dupl_sm      # 1,505

rm(does_match, i, j, has_duplicate)
rm(sm_pop_padded)

#true_cntr_md1 <- 0
num_dupl_md1 <- 0
md1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md1_pop_padded[[i]],
        md1_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_md1 <- true_cntr_md1 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_md1 <- num_dupl_md1 + 1 
    md1_index <- c(md1_index, i)
  }
}

#true_cntr_md1     # 14
num_dupl_md1      # 11

rm(does_match, i, j, has_duplicate)
rm(md1_pop_padded)

#true_cntr_md2 <- 0
num_dupl_md2 <- 0
md2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md2_pop_padded[[i]],
        md2_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_md2 <- true_cntr_md2 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_md2 <- num_dupl_md2 + 1 
  }
}

true_cntr_md2     # 0
num_dupl_md2      # 0

rm(does_match, i, j, has_duplicate)
rm(md2_pop_padded)

#true_cntr_md <- 0
num_dupl_md <- 0
md_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md_pop_padded[[i]],
        md_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_md <- true_cntr_md + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_md <- num_dupl_md + 1 
  }
}

true_cntr_md     # 0
num_dupl_md      # 0

rm(does_match, i, j, has_duplicate)
rm(md_pop_padded)

#true_cntr_lg <- 0
num_dupl_lg <- 0
lg_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lg_pop_padded[[i]],
        lg_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_lg <- true_cntr_lg + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_lg <- num_dupl_lg + 1 
  }
}

true_cntr_lg     # 0
num_dupl_lg      # 0
 
rm(does_match, i, j, has_duplicate)
rm(lg_pop_padded)

fixed_mu_vary_n <- tibble(
  mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8),
  pop_size = c(50, 100, 500, 1000, 2000, 5000, 10000, 50000),
  num_match = c(true_cntr_sm1, true_cntr_sm2, true_cntr_sm3,
                true_cntr_sm, true_cntr_md1, true_cntr_md2,
                true_cntr_md, true_cntr_lg),
  total_dupl = c(num_dupl_sm1, num_dupl_sm2, num_dupl_sm3,
                 num_dupl_sm, num_dupl_md1, num_dupl_md2,
                 num_dupl_md, num_dupl_lg)
)



#--------------- FIND DUPLICATES - FIXED N --------------------

#true_cntr_lw1 <- 0
num_dupl_lw1 <- 0
lw1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lw1_mu_padded[[i]],
        lw1_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_lw1 <- true_cntr_lw1 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_lw1 <- num_dupl_lw1 + 1 
    lw1_index <- c(lw1_index, i)
  }
}

#true_cntr_lw1     # 178,525,285
num_dupl_lw1      # 19,886

rm(does_match, i, j, has_duplicate)
rm(lw1_mu_padded)

#true_cntr_lw2 <- 0
num_dupl_lw2 <- 0
lw2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lw2_mu_padded[[i]],
        lw2_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_lw2 <- true_cntr_lw2 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_lw2 <- num_dupl_lw2 + 1 
    lw2_index <- c(lw2_index, i)
  }
}

#true_cntr_lw2     # 71,595,159
num_dupl_lw2      # 18478

rm(does_match, i, j, has_duplicate)
rm(lw2_mu_padded)

#true_cntr_lw <- 0
num_dupl_lw <- 0
lw_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lw_mu_padded[[i]],
        lw_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        #true_cntr_lw <- true_cntr_lw + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_lw <- num_dupl_lw + 1 
    lw_index <- c(lw_index, i)
  }
}

#true_cntr_lw     # 21,897
num_dupl_lw      # 1,170
 
rm(does_match, i, j, has_duplicate)
rm(lw_mu_padded)

#true_cntr_md1 <- 0
num_dupl_md1 <- 0
md1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md1_mu_padded[[i]],
        md1_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_md1 <- true_cntr_md1 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_md1 <- num_dupl_md1 + 1 
  }
}

true_cntr_md1     # 0
num_dupl_md1      # 0

rm(does_match, i, j, has_duplicate)
rm(md1_mu_padded)

#true_cntr_md <- 0
num_dupl_md <- 0
md_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md_mu_padded[[i]],
        md_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_md <- true_cntr_md + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_md <- num_dupl_md + 1 
  }
}

true_cntr_md     # 0
num_dupl_md      # 0

rm(does_match, i, j, has_duplicate)
rm(md_mu_padded)

#true_cntr_hg <- 0
num_dupl_hg <- 0
hg_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        hg_mu_padded[[i]],
        hg_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_hg <- true_cntr_hg + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_hg <- num_dupl_hg + 1 
  }
}

true_cntr_hg     # 0
num_dupl_hg      # 0

rm(does_match, i, j, has_duplicate)
rm(hg_mu_padded)

fixed_n_vary_mu <- tibble(
  mut_rate = c(1.5e-11, 1.5e-10, 1.5e-9, 0.5e-8, 1.5e-8, 1.5e-7),
  pop_size = c(10000, 10000, 10000, 10000, 10000, 10000),
  num_match = c(true_cntr_lw1, true_cntr_lw2, true_cntr_lw, 
                true_cntr_md1, true_cntr_md, true_cntr_hg),
  total_dupl = c(num_dupl_lw1, num_dupl_lw2, num_dupl_lw,
                 num_dupl_md1, num_dupl_md, num_dupl_hg)
)



#--------------- TIDY ALIGN DATA --------------------

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


#--------------- SAVE DATA SETS --------------------

save(
  fixed_mu_vary_n, 
  file = glue('{path_to_results}dup_analysis_fixed_mu_align_results.RData')
)

save(
  fixed_n_vary_mu, 
  file = glue('{path_to_results}dup_analysis_fixed_n_align_results.RData')
)

save(
  sm1_index, sm2_index, sm3_index,
  sm_index, md1_index, md2_index, 
  md_index, lg_index,
  file = glue('{path_to_results}fixed_mu_align_indices.RData')
)

save(
  lw1_index, lw2_index, lw_index, 
  md1_index, md_index, hg_index,
  file = glue('{path_to_results}fixed_n_align_indices.RData')
)


