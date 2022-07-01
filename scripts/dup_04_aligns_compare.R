#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: compares all alignments to find duplicates after sorting and padding
# Mackenzie M. Johnson
# July 2021 


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


#--------------- READ IN DATA - FIXED MU --------------------

# load alignments 
load(
  file = file.path(
    path_to_data,
    'fixed_mu',
    'fixed_mu1_vary_n_align_processed.RData'
  )
)

load(
  file = file.path(
    path_to_data,
    'fixed_mu',
    'fixed_mu2_vary_n_align_processed.RData'
  )
)

load(
  file = file.path(
    path_to_data,
    'fixed_mu',
    'fixed_mu3_vary_n_align_processed.RData'
  )
)


#--------------- FIND DUPLICATES - FIXED MU --------------------

# compare each parameter set (vary n for 3 fixed mu values)
# set 1
mu1_n1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu1_n1_padded[[i]],
        pop_mu1_n1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu1_n1_index <- c(mu1_n1_index, i)
  }
}

num_dupl_mu1_n1 <- length(mu1_n1_index)     # 18,530

rm(does_match, i, j, has_duplicate)
rm(pop_mu1_n1_padded)

mu1_n2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu1_n2_padded[[i]],
        pop_mu1_n2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu1_n2_index <- c(mu1_n2_index, i)
  }
}

num_dupl_mu1_n2 <- length(mu1_n2_index)     # 12,967

rm(does_match, i, j, has_duplicate)
rm(pop_mu1_n2_padded)

mu1_n3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu1_n3_padded[[i]],
        pop_mu1_n3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu1_n3_index <- c(mu1_n3_index, i)
  }
}

num_dupl_mu1_n3 <- length(mu1_n3_index)     # 1,553

rm(does_match, i, j, has_duplicate)
rm(pop_mu1_n3_padded)

mu1_n4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu1_n4_padded[[i]],
        pop_mu1_n4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu1_n4_index <- c(mu1_n4_index, i)
  }
}

num_dupl_mu1_n4 <- length(mu1_n4_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu1_n4_padded)

mu1_n5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu1_n5_padded[[i]],
        pop_mu1_n5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu1_n5_index <- c(mu1_n5_index, i)
  }
}

num_dupl_mu1_n5 <- length(mu1_n5_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu1_n5_padded)

mu1_n6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu1_n6_padded[[i]],
        pop_mu1_n6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu1_n6_index <- c(mu1_n6_index, i)
  }
}

num_dupl_mu1_n6 <- length(mu1_n6_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu1_n6_padded)

# set 2
mu2_n1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu2_n1_padded[[i]],
        pop_mu2_n1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu2_n1_index <- c(mu2_n1_index, i)
  }
}

num_dupl_mu2_n1 <- length(mu2_n1_index)     # 19,880

rm(does_match, i, j, has_duplicate)
rm(pop_mu2_n1_padded)

mu2_n2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu2_n2_padded[[i]],
        pop_mu2_n2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu2_n2_index <- c(mu2_n2_index, i)
  }
}

num_dupl_mu2_n2 <- length(mu2_n2_index)     # 19,672

rm(does_match, i, j, has_duplicate)
rm(pop_mu2_n2_padded)

mu2_n3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu2_n3_padded[[i]],
        pop_mu2_n3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu2_n3_index <- c(mu2_n3_index, i)
  }
}

num_dupl_mu2_n3 <- length(mu2_n3_index)     # 18,388

rm(does_match, i, j, has_duplicate)
rm(pop_mu2_n3_padded)

mu2_n4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu2_n4_padded[[i]],
        pop_mu2_n4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu2_n4_index <- c(mu2_n4_index, i)
  }
}

num_dupl_mu2_n4 <- length(mu2_n4_index)     # 12,575

rm(does_match, i, j, has_duplicate)
rm(pop_mu2_n4_padded)

mu2_n5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu2_n5_padded[[i]],
        pop_mu2_n5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu2_n5_index <- c(mu2_n5_index, i)
  }
}

num_dupl_mu2_n5 <- length(mu2_n5_index)     # 1,173

rm(does_match, i, j, has_duplicate)
rm(pop_mu2_n5_padded)

mu2_n6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu2_n6_padded[[i]],
        pop_mu2_n6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu2_n6_index <- c(mu2_n6_index, i)
  }
}

num_dupl_mu2_n6 <- length(mu2_n6_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu2_n6_padded)

# set 3
mu3_n1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu3_n1_padded[[i]],
        pop_mu3_n1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu3_n1_index <- c(mu3_n1_index, i)
  }
}

num_dupl_mu3_n1 <- length(mu3_n1_index)     # 1,942

rm(does_match, i, j, has_duplicate)
rm(pop_mu3_n1_padded)

mu3_n2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu3_n2_padded[[i]],
        pop_mu3_n2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu3_n2_index <- c(mu2_n2_index, i)
  }
}

num_dupl_mu3_n2 <- length(mu3_n2_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu3_n2_padded)

mu3_n3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu3_n3_padded[[i]],
        pop_mu3_n3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu3_n3_index <- c(mu3_n3_index, i)
  }
}

num_dupl_mu3_n3 <- length(mu3_n3_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu3_n3_padded)

mu3_n4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu3_n4_padded[[i]],
        pop_mu3_n4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu3_n4_index <- c(mu3_n4_index, i)
  }
}

num_dupl_mu3_n4 <- length(mu3_n4_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu3_n4_padded)

mu3_n5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu3_n5_padded[[i]],
        pop_mu3_n5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu3_n5_index <- c(mu3_n5_index, i)
  }
}

num_dupl_mu3_n5 <- length(mu3_n5_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu3_n5_padded)

mu3_n6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_mu3_n6_padded[[i]],
        pop_mu3_n6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    mu3_n6_index <- c(mu3_n6_index, i)
  }
}

num_dupl_mu3_n6 <- length(mu3_n6_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_mu3_n6_padded)


#--------------- TIDY ALIGN DATA - FIXED MU --------------------

# set 1
fixed_mu1_vary_n <- tibble(
  mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8, 1.5e-8),
  pop_size = c(100, 316, 1000, 3160, 10000, 31600),
  total_dupl = c(num_dupl_mu1_n1, num_dupl_mu1_n2,
                 num_dupl_mu1_n3, num_dupl_mu1_n4,
                 num_dupl_mu1_n5, num_dupl_mu1_n6)
)

fixed_mu1_vary_n %>% 
  mutate(
    set = "fixed_mu",
    prop_dup = total_dupl/20000
  ) -> fixed_mu1_vary_n

# set 2
fixed_mu2_vary_n <- tibble(
  mut_rate = c(1.5e-9, 1.5e-9, 1.5e-9, 1.5e-9, 1.5e-9, 1.5e-9),
  pop_size = c(100, 316, 1000, 3160, 10000, 31600),
  total_dupl = c(num_dupl_mu2_n1, num_dupl_mu2_n2,
                 num_dupl_mu2_n3, num_dupl_mu2_n4,
                 num_dupl_mu2_n5, num_dupl_mu2_n6)
)

fixed_mu2_vary_n %>% 
  mutate(
    set = "fixed_mu",
    prop_dup = total_dupl/20000
  ) -> fixed_mu2_vary_n

# set 3
fixed_mu3_vary_n <- tibble(
  mut_rate = c(1.5e-7, 1.5e-7, 1.5e-7, 1.5e-7, 1.5e-7, 1.5e-7),
  pop_size = c(100, 316, 1000, 3160, 10000, 31600),
  total_dupl = c(num_dupl_mu3_n1, num_dupl_mu3_n2,
                 num_dupl_mu3_n3, num_dupl_mu3_n4,
                 num_dupl_mu3_n5, num_dupl_mu3_n6)
)

fixed_mu3_vary_n %>% 
  mutate(
    set = "fixed_mu",
    prop_dup = total_dupl/20000
  ) -> fixed_mu3_vary_n


#--------------- SAVE DATA SETS - FIXED MU --------------------

# save results data frame for duplicate comparison figures
save(
  fixed_mu1_vary_n, 
  file = file.path(
    path_to_results,
    'dup_analysis_fixed_mu1_align_results.RData'
  )
)

save(
  fixed_mu2_vary_n, 
  file = file.path(
    path_to_results,
    'dup_analysis_fixed_mu2_align_results.RData'
  )
)

save(
  fixed_mu3_vary_n, 
  file = file.path(
    path_to_results,
    'dup_analysis_fixed_mu3_align_results.RData'
  )
)

# save duplicate indices to compare parameter spaces
save(
  mu1_n1_index, mu1_n2_index, mu1_n3_index,
  mu1_n4_index, mu1_n5_index, mu1_n6_index,
  file = file.path(
    path_to_results,
    'fixed_mu1_align_indices.RData'
  )
)

save(
  mu2_n1_index, mu2_n2_index, mu2_n3_index,
  mu2_n4_index, mu2_n5_index, mu2_n6_index,
  file = file.path(
    path_to_results,
    'fixed_mu2_align_indices.RData'
  )
)

save(
  mu3_n1_index, mu3_n2_index, mu3_n3_index,
  mu3_n4_index, mu3_n5_index, mu3_n6_index,
  file = file.path(
    path_to_results,
    'fixed_mu3_align_indices.RData'
  )
)


#--------------- READ IN DATA - FIXED N --------------------

# load alignments 
load(
  file = file.path(
    path_to_data,
    'fixed_n',
    'fixed_n1_vary_mu_align_processed.RData'
  )
)

load(
  file = file.path(
    path_to_data,
    'fixed_n',
    'fixed_n2_vary_mu_align_processed.RData'
  )
)

load(
  file = file.path(
    path_to_data,
    'fixed_n',
    'fixed_n3_vary_mu_align_processed.RData'
  )
)

#--------------- FIND DUPLICATES - FIXED N --------------------

# compare each parameter set (vary mu for 3 fixed n values)
# set 1
n1_mu1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n1_mu1_padded[[i]],
        pop_n1_mu1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n1_mu1_index <- c(n1_mu1_index, i)
  }
}

num_dupl_n1_mu1 <- length(n1_mu1_index)     # 19,165

rm(does_match, i, j, has_duplicate)
rm(pop_n1_mu1_padded)

n1_mu2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n1_mu2_padded[[i]],
        pop_n1_mu2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n1_mu2_index <- c(n1_mu2_index, i)
  }
}

num_dupl_n1_mu2 <- length(n1_mu2_index)     # 15,588

rm(does_match, i, j, has_duplicate)
rm(pop_n1_mu2_padded)

n1_mu3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n1_mu3_padded[[i]],
        pop_n1_mu3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n1_mu3_index <- c(n1_mu3_index, i)
  }
}

num_dupl_n1_mu3 <- length(n1_mu3_index)     # 4, 547

rm(does_match, i, j, has_duplicate)
rm(pop_n1_mu3_padded)

n1_mu4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n1_mu4_padded[[i]],
        pop_n1_mu4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n1_mu4_index <- c(n1_mu4_index, i)
  }
}

num_dupl_n1_mu4 <- length(n1_mu4_index)     # 6

rm(does_match, i, j, has_duplicate)
rm(pop_n1_mu4_padded)

n1_mu5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n1_mu5_padded[[i]],
        pop_n1_mu5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n1_mu5_index <- c(n1_mu5_index, i)
  }
}

num_dupl_n1_mu5 <- length(n1_mu5_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n1_mu5_padded)

n1_mu6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n1_mu6_padded[[i]],
        pop_n1_mu6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n1_mu6_index <- c(n1_mu6_index, i)
  }
}

num_dupl_n1_mu6 <- length(n1_mu6_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n1_mu6_padded)

# set 2
n2_mu1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n2_mu1_padded[[i]],
        pop_n2_mu1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n2_mu1_index <- c(n2_mu1_index, i)
  }
}

num_dupl_n2_mu1 <- length(n2_mu1_index)     # 19,899

rm(does_match, i, j, has_duplicate)
rm(pop_n2_mu1_padded)

n2_mu2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n2_mu2_padded[[i]],
        pop_n2_mu2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n2_mu2_index <- c(n2_mu2_index, i)
  }
}

num_dupl_n2_mu2 <- length(n2_mu2_index)     # 19,789

rm(does_match, i, j, has_duplicate)
rm(pop_n2_mu2_padded)

n2_mu3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n2_mu3_padded[[i]],
        pop_n2_mu3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n2_mu3_index <- c(n2_mu3_index, i)
  }
}

num_dupl_n2_mu3 <- length(n2_mu3_index)     # 19,146

rm(does_match, i, j, has_duplicate)
rm(pop_n2_mu3_padded)

n2_mu4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n2_mu4_padded[[i]],
        pop_n2_mu4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n2_mu4_index <- c(n2_mu4_index, i)
  }
}

num_dupl_n2_mu4 <- length(n2_mu4_index)     # 15,668

rm(does_match, i, j, has_duplicate)
rm(pop_n2_mu4_padded)

n2_mu5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n2_mu5_padded[[i]],
        pop_n2_mu5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n2_mu5_index <- c(n2_mu5_index, i)
  }
}

num_dupl_n2_mu5 <- length(n2_mu5_index)     # 4,979

rm(does_match, i, j, has_duplicate)
rm(pop_n2_mu5_padded)

n2_mu6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n2_mu6_padded[[i]],
        pop_n2_mu6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n2_mu6_index <- c(n2_mu6_index, i)
  }
}

num_dupl_n2_mu6 <- length(n2_mu6_index)     # 14

rm(does_match, i, j, has_duplicate)
rm(pop_n2_mu6_padded)

# set 3
n3_mu1_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n3_mu1_padded[[i]],
        pop_n3_mu1_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n3_mu1_index <- c(n3_mu1_index, i)
  }
}

num_dupl_n3_mu1 <- length(n3_mu1_index)     # 4,319

rm(does_match, i, j, has_duplicate)
rm(pop_n3_mu1_padded)

n3_mu2_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n3_mu2_padded[[i]],
        pop_n3_mu2_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n3_mu2_index <- c(n3_mu2_index, i)
  }
}

num_dupl_n3_mu2 <- length(n3_mu2_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n3_mu2_padded)

n3_mu3_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n3_mu3_padded[[i]],
        pop_n3_mu3_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n3_mu3_index <- c(n3_mu3_index, i)
  }
}

num_dupl_n3_mu3 <- length(n3_mu3_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n3_mu3_padded)

n3_mu4_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n3_mu4_padded[[i]],
        pop_n3_mu4_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n3_mu4_index <- c(n3_mu4_index, i)
  }
}

num_dupl_n3_mu4 <- length(n3_mu4_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n3_mu4_padded)

n3_mu5_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n3_mu5_padded[[i]],
        pop_n3_mu5_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n3_mu5_index <- c(n3_mu5_index, i)
  }
}

num_dupl_n3_mu5 <- length(n3_mu5_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n3_mu5_padded)

n3_mu6_index <- c()

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i < j) {
      does_match <- identical(
        pop_n3_mu6_padded[[i]],
        pop_n3_mu6_padded[[j]]
      )
      if (does_match == TRUE){ 
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    n3_mu6_index <- c(n3_mu6_index, i)
  }
}

num_dupl_n3_mu6 <- length(n3_mu6_index)     # 0

rm(does_match, i, j, has_duplicate)
rm(pop_n3_mu6_padded)


#--------------- TIDY ALIGN DATA - FIXED N --------------------

# set 1
fixed_n1_vary_mu <- tibble(
  mut_rate = c(1.e-10, 3.16e-10, 1.0e-9, 3.16e-9, 1.0e-8, 3.16e-8),
  pop_size = c(10000, 10000, 10000, 10000, 10000, 10000),
  total_dupl = c(num_dupl_n1_mu1, num_dupl_n1_mu2,
                 num_dupl_n1_mu3, num_dupl_n1_mu4,
                 num_dupl_n1_mu5, num_dupl_n1_mu6)
)

fixed_n1_vary_mu %>% 
  mutate(
    set = "fixed_n",
    prop_dup = total_dupl/20000
  ) -> fixed_n1_vary_mu

# set 2
fixed_n2_vary_mu <- tibble(
  mut_rate = c(1.e-10, 3.16e-10, 1.0e-9, 3.16e-9, 1.0e-8, 3.16e-8),
  pop_size = c(1000, 1000, 1000, 1000, 1000, 1000),
  total_dupl = c(num_dupl_n2_mu1, num_dupl_n2_mu2,
                 num_dupl_n2_mu3, num_dupl_n2_mu4,
                 num_dupl_n2_mu5, num_dupl_n2_mu6)
)

fixed_n2_vary_mu %>% 
  mutate(
    set = "fixed_n",
    prop_dup = total_dupl/20000
  ) -> fixed_n2_vary_mu

# set 3
fixed_n3_vary_mu <- tibble(
  mut_rate = c(1.e-10, 3.16e-10, 1.0e-9, 3.16e-9, 1.0e-8, 3.16e-8),
  pop_size = c(100000, 100000, 100000, 100000, 100000, 100000),
  total_dupl = c(num_dupl_n3_mu1, num_dupl_n3_mu2,
                 num_dupl_n3_mu3, num_dupl_n3_mu4,
                 num_dupl_n3_mu5, num_dupl_n3_mu6)
)

fixed_n3_vary_mu %>% 
  mutate(
    set = "fixed_n",
    prop_dup = total_dupl/20000
  ) -> fixed_n3_vary_mu


#--------------- SAVE DATA SETS - FIXED N --------------------

# save results data frame for duplicate comparison figures
save(
  fixed_n1_vary_mu, 
  file = file.path(
    path_to_results,
    'dup_align',
    'dup_analysis_fixed_n1_align_results.RData'
  )
)

save(
  fixed_n2_vary_mu, 
  file = file.path(
    path_to_results,
    'dup_align',
    'dup_analysis_fixed_n2_align_results.RData'
  )
)

save(
  fixed_n3_vary_mu, 
  file = file.path(
    path_to_results,
    'dup_align',
    'dup_analysis_fixed_n3_align_results.RData'
  )
)

# save duplicate indices to compare parameter spaces
save(
  n1_mu1_index, n1_mu2_index, n1_mu3_index,
  n1_mu4_index, n1_mu5_index, n1_mu6_index,
  file = file.path(
    path_to_results,
    'indices',
    'fixed_n1_align_indices.RData'
  )
)

save(
  n2_mu1_index, n2_mu2_index, n2_mu3_index,
  n2_mu4_index, n2_mu5_index, n2_mu6_index,
  file = file.path(
    path_to_results,
    'indices',
    'fixed_n2_align_indices.RData'
  )
)

save(
  n3_mu1_index, n3_mu2_index, n3_mu3_index,
  n3_mu4_index, n3_mu5_index, n3_mu6_index,
  file = file.path(
    path_to_results,
    'indices',
    'fixed_n3_align_indices.RData'
  )
)

