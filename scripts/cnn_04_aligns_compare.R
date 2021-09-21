#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: compares all alignments to find duplicates after sorting and padding
# Mackenzie M. Johnson
# August 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_data <- '/stor/work/Wilke/mmj2238/rho_cnn_data/parsed/dup_analysis/'
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50


#--------------- READ IN DATA --------------------

# load alignments 
load(
  file = file.path(
    path_to_data,
    'cnn_dup',
    'low_dup_align_processed.RData'
  )
)

load(
  file = file.path(
    path_to_data,
    'cnn_dup',
    'high_dup_align_processed.RData'
  )
)


#--------------- FIND DUPLICATES --------------------

# compare each parameter set - low duplicate set
low_n1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_low_n1_padded[[i]],
        pop_low_n1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    low_n1_index <- c(low_n1_index, i)
  }
}

num_dupl_low_n1 <- length(low_n1_index)     # 1,468

rm(does_match, i, j, has_duplicate)
rm(pop_low_n1_padded)

low_n2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_low_n2_padded[[i]],
        pop_low_n2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    low_n2_index <- c(low_n2_index, i)
  }
}

num_dupl_low_n2 <- length(low_n2_index)     # 20

rm(does_match, i, j, has_duplicate)
rm(pop_low_n2_padded)

low_n3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_low_n3_padded[[i]],
        pop_low_n3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    low_n3_index <- c(low_n3_index, i)
  }
}

num_dupl_low_n3 <- length(low_n3_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_low_n3_padded)

low_n4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_low_n4_padded[[i]],
        pop_low_n4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    low_n4_index <- c(low_n4_index, i)
  }
}

num_dupl_low_n4 <- length(low_n4_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_low_n4_padded)

low_n5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_low_n5_padded[[i]],
        pop_low_n5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    low_n5_index <- c(low_n5_index, i)
  }
}

num_dupl_low_n5 <- length(low_n5_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_low_n5_padded)

low_n6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_low_n6_padded[[i]],
        pop_low_n6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    low_n6_index <- c(low_n6_index, i)
  }
}

num_dupl_low_n6 <- length(low_n6_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_low_n6_padded)


# compare each parameter set - high duplicate set
high_n1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_high_n1_padded[[i]],
        pop_high_n1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    high_n1_index <- c(high_n1_index, i)
  }
}

num_dupl_high_n1 <- length(high_n1_index)     # 18,465

rm(does_match, i, j, has_duplicate)
rm(pop_high_n1_padded)

high_n2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_high_n2_padded[[i]],
        pop_high_n2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    high_n2_index <- c(high_n2_index, i)
  }
}

num_dupl_high_n2 <- length(high_n2_index)     # 15,951

rm(does_match, i, j, has_duplicate)
rm(pop_high_n2_padded)

high_n3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_high_n3_padded[[i]],
        pop_high_n3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    high_n3_index <- c(high_n3_index, i)
  }
}

num_dupl_high_n3 <- length(high_n3_index)     # 7,670

rm(does_match, i, j, has_duplicate)
rm(pop_high_n3_padded)

high_n4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_high_n4_padded[[i]],
        pop_high_n4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    high_n4_index <- c(high_n4_index, i)
  }
}

num_dupl_high_n4 <- length(high_n4_index)     # 1,151

rm(does_match, i, j, has_duplicate)
rm(pop_high_n4_padded)

high_n5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_high_n5_padded[[i]],
        pop_high_n5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    high_n5_index <- c(high_n5_index, i)
  }
}

num_dupl_high_n5 <- length(high_n5_index)     # 113

rm(does_match, i, j, has_duplicate)
rm(pop_high_n5_padded)

high_n6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_high_n6_padded[[i]],
        pop_high_n6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    high_n6_index <- c(high_n6_index, i)
  }
}

num_dupl_high_n6 <- length(high_n6_index)     # 5

rm(does_match, i, j, has_duplicate)
rm(pop_high_n6_padded)



#--------------- TIDY ALIGN DATA --------------------

low_dup_set <- tibble(
  mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8),
  pop_size = c(1000, 2000, 5000, 10000, 15000, 20000),
  total_dupl = c(num_dupl_low_n1, num_dupl_low_n2, num_dupl_low_n3,
                 num_dupl_low_n4, num_dupl_low_n5, num_dupl_low_n6)
)

low_dup_set %>% 
  mutate(
    set = "low_dup",
    prop_dup = total_dupl/20000
  ) -> low_dup_set

high_dup_set <- tibble(
  mut_rate = c(1.5e-9, 1.5e-9, 1.5e-9, 1.5e-9, 1.5e-9, 1.5e-9),
  pop_size = c(1000, 2000, 5000, 10000, 15000, 20000),
  total_dupl = c(num_dupl_high_n1, num_dupl_high_n2, num_dupl_high_n3,
                 num_dupl_high_n4, num_dupl_high_n5, num_dupl_high_n6)
)

high_dup_set %>% 
  mutate(
    set = "high_dup",
    prop_dup = total_dupl/20000
  ) -> high_dup_set


#--------------- SAVE DATA SETS --------------------

# save results data frame for duplicate comparison figures
save(
  low_dup_set, 
  file = file.path(
    path_to_results,
    'low_dup_align_results.RData'
  )
)

save(
  high_dup_set, 
  file = file.path(
    path_to_results,
    'high_dup_align_results.RData'
  )
)

# save duplicate indices to compare parameter spaces
save(
  low_n1_index, low_n2_index, low_n3_index,
  low_n4_index, low_n5_index, low_n6_index,
  file = file.path(
    path_to_results,
    'low_dup_align_indices.RData'
  )
)

save(
  high_n1_index, high_n2_index, high_n3_index,
  high_n4_index, high_n5_index, high_n6_index,
  file = file.path(
    path_to_results,
    'high_dup_align_indices.RData'
  )
)

