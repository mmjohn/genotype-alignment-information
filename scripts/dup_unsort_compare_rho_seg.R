#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: compares rho and seg sites parameter distributions between 
# duplicates and unique alignments (after padding UNSORTED)
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

#--------------- READ IN RHO DATA --------------------

# load in rho data
# fixed mu
load(
  file = file.path(
    path_to_data, 'fixed_mu', 'fixed_mu1_vary_n_rho.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_mu', 'fixed_mu2_vary_n_rho.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_mu', 'fixed_mu3_vary_n_rho.RData'
  )
)

# fixed n
load(
  file = file.path(
    path_to_data, 'fixed_n', 'fixed_n1_vary_mu_rho.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_n', 'fixed_n2_vary_mu_rho.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_n', 'fixed_n3_vary_mu_rho.RData'
  )
)


#--------------- READ IN SITE DATA --------------------

# load in seg sites data
# fixed mu
load(
  file = file.path(
    path_to_data, 'fixed_mu', 'fixed_mu1_vary_n_sites.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_mu', 'fixed_mu2_vary_n_sites.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_mu', 'fixed_mu3_vary_n_sites.RData'
  )
)

# fixed n
load(
  file = file.path(
    path_to_data, 'fixed_n', 'fixed_n1_vary_mu_sites.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_n', 'fixed_n2_vary_mu_sites.RData'
  )
)
load(
  file = file.path(
    path_to_data, 'fixed_n', 'fixed_n3_vary_mu_sites.RData'
  )
)


#--------------- READ IN INDEX DATA --------------------

# load in indices of duplicated alignments
# fixed mu
load(
  file = file.path(
    path_to_results, 'fixed_mu1_align_unsort_indices.RData'
  )
)
load(
  file = file.path(
    path_to_results, 'fixed_mu2_align_unsort_indices.RData'
  )
)
load(
  file = file.path(
    path_to_results, 'fixed_mu3_align_unsort_indices.RData'
  )
)

# fixed n
load(
  file = file.path(
    path_to_results, 'fixed_n1_align_unsort_indices.RData'
  )
)
load(
  file = file.path(
    path_to_results, 'fixed_n2_align_unsort_indices.RData'
  )
)
load(
  file = file.path(
    path_to_results, 'fixed_n3_align_unsort_indices.RData'
  )
)


#--------------- GET RHO DATA - FIXED MU --------------------

# get duplicate and unique rho values
# set 1
tibble(
  pop_mu1_n1_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n1 = case_when(
      run %in% mu1_n1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n1_rho_df

rm(pop_mu1_n1_rho, mu1_n1_index)

tibble(
  pop_mu1_n2_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n2 = case_when(
      run %in% mu1_n2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n2_rho_df

rm(pop_mu1_n2_rho, mu1_n2_index)

tibble(
  pop_mu1_n3_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n3 = case_when(
      run %in% mu1_n3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n3_rho_df

rm(pop_mu1_n3_rho, mu1_n3_index)

tibble(
  pop_mu1_n4_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n4 = case_when(
      run %in% mu1_n4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n4_rho_df

rm(pop_mu1_n4_rho, mu1_n4_index)

tibble(
  pop_mu1_n5_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n5 = case_when(
      run %in% mu1_n5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n5_rho_df

rm(pop_mu1_n5_rho, mu1_n5_index)

tibble(
  pop_mu1_n6_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n6 = case_when(
      run %in% mu1_n6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n6_rho_df

rm(pop_mu1_n6_rho, mu1_n6_index)

# set 2
tibble(
  pop_mu2_n1_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n1 = case_when(
      run %in% mu2_n1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n1_rho_df

rm(pop_mu2_n1_rho, mu2_n1_index)

tibble(
  pop_mu2_n2_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n2 = case_when(
      run %in% mu2_n2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n2_rho_df

rm(pop_mu2_n2_rho, mu2_n2_index)

tibble(
  pop_mu2_n3_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n3 = case_when(
      run %in% mu2_n3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n3_rho_df

rm(pop_mu2_n3_rho, mu2_n3_index)

tibble(
  pop_mu2_n4_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n4 = case_when(
      run %in% mu2_n4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n4_rho_df

rm(pop_mu2_n4_rho, mu2_n4_index)

tibble(
  pop_mu2_n5_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n5 = case_when(
      run %in% mu2_n5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n5_rho_df

rm(pop_mu2_n5_rho, mu2_n5_index)

tibble(
  pop_mu2_n6_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n6 = case_when(
      run %in% mu2_n6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n6_rho_df

rm(pop_mu2_n6_rho, mu2_n6_index)

# set 3
tibble(
  pop_mu3_n1_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n1 = case_when(
      run %in% mu3_n1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n1_rho_df

rm(pop_mu3_n1_rho, mu3_n1_index)

tibble(
  pop_mu3_n2_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n2 = case_when(
      run %in% mu3_n2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n2_rho_df

rm(pop_mu3_n2_rho, mu3_n2_index)

tibble(
  pop_mu3_n3_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n3 = case_when(
      run %in% mu3_n3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n3_rho_df

rm(pop_mu3_n3_rho, mu3_n3_index)

tibble(
  pop_mu3_n4_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n4 = case_when(
      run %in% mu3_n4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n4_rho_df

rm(pop_mu3_n4_rho, mu3_n4_index)

tibble(
  pop_mu3_n5_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n5 = case_when(
      run %in% mu3_n5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n5_rho_df

rm(pop_mu3_n5_rho, mu3_n5_index)

tibble(
  pop_mu3_n6_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n6 = case_when(
      run %in% mu3_n6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n6_rho_df

rm(pop_mu3_n6_rho, mu3_n6_index)


# join data sets
# set 1
rho_df_mu1 <- full_join(pop_mu1_n1_rho_df, pop_mu1_n2_rho_df) %>% 
  full_join(., pop_mu1_n3_rho_df) %>% 
  full_join(., pop_mu1_n4_rho_df) %>% 
  full_join(., pop_mu1_n5_rho_df) %>% 
  full_join(., pop_mu1_n6_rho_df)

rm(pop_mu1_n1_rho_df, pop_mu1_n2_rho_df, pop_mu1_n3_rho_df,
   pop_mu1_n4_rho_df, pop_mu1_n5_rho_df, pop_mu1_n6_rho_df)

rho_df_mu1 %>% 
  pivot_longer(
    cols = c(pop_mu1_n1_rho, pop_mu1_n2_rho,
             pop_mu1_n3_rho, pop_mu1_n4_rho,
             pop_mu1_n5_rho, pop_mu1_n6_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp_mu1

rho_df_temp_mu1 %>% 
  mutate(
    status = case_when(
      param_set == "pop_mu1_n1_rho" ~ status_m1n1,
      param_set == "pop_mu1_n2_rho" ~ status_m1n2,
      param_set == "pop_mu1_n3_rho" ~ status_m1n3,
      param_set == "pop_mu1_n4_rho" ~ status_m1n4,
      param_set == "pop_mu1_n5_rho" ~ status_m1n5,
      param_set == "pop_mu1_n6_rho" ~ status_m1n6,
    )
  ) %>% 
  select(-run, -status_m1n1, -status_m1n2, 
         -status_m1n3, -status_m1n4, 
         -status_m1n5, -status_m1n6) -> rho_df_fixed_mu1

rm(rho_df_mu1, rho_df_temp_mu1)

# set 2
rho_df_mu2 <- full_join(pop_mu2_n1_rho_df, pop_mu2_n2_rho_df) %>% 
  full_join(., pop_mu2_n3_rho_df) %>% 
  full_join(., pop_mu2_n4_rho_df) %>% 
  full_join(., pop_mu2_n5_rho_df) %>% 
  full_join(., pop_mu2_n6_rho_df)

rm(pop_mu2_n1_rho_df, pop_mu2_n2_rho_df, pop_mu2_n3_rho_df,
   pop_mu2_n4_rho_df, pop_mu2_n5_rho_df, pop_mu2_n6_rho_df)

rho_df_mu2 %>% 
  pivot_longer(
    cols = c(pop_mu2_n1_rho, pop_mu2_n2_rho,
             pop_mu2_n3_rho, pop_mu2_n4_rho,
             pop_mu2_n5_rho, pop_mu2_n6_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp_mu2

rho_df_temp_mu2 %>% 
  mutate(
    status = case_when(
      param_set == "pop_mu2_n1_rho" ~ status_m2n1,
      param_set == "pop_mu2_n2_rho" ~ status_m2n2,
      param_set == "pop_mu2_n3_rho" ~ status_m2n3,
      param_set == "pop_mu2_n4_rho" ~ status_m2n4,
      param_set == "pop_mu2_n5_rho" ~ status_m2n5,
      param_set == "pop_mu2_n6_rho" ~ status_m2n6,
    )
  ) %>% 
  select(-run, -status_m2n1, -status_m2n2, 
         -status_m2n3, -status_m2n4, 
         -status_m2n5, -status_m2n6) -> rho_df_fixed_mu2

rm(rho_df_mu2, rho_df_temp_mu2)

# set 3
rho_df_mu3 <- full_join(pop_mu3_n1_rho_df, pop_mu3_n2_rho_df) %>% 
  full_join(., pop_mu3_n3_rho_df) %>% 
  full_join(., pop_mu3_n4_rho_df) %>% 
  full_join(., pop_mu3_n5_rho_df) %>% 
  full_join(., pop_mu3_n6_rho_df)

rm(pop_mu3_n1_rho_df, pop_mu3_n2_rho_df, pop_mu3_n3_rho_df,
   pop_mu3_n4_rho_df, pop_mu3_n5_rho_df, pop_mu3_n6_rho_df)

rho_df_mu3 %>% 
  pivot_longer(
    cols = c(pop_mu3_n1_rho, pop_mu3_n2_rho,
             pop_mu3_n3_rho, pop_mu3_n4_rho,
             pop_mu3_n5_rho, pop_mu3_n6_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp_mu3

rho_df_temp_mu3 %>% 
  mutate(
    status = case_when(
      param_set == "pop_mu3_n1_rho" ~ status_m3n1,
      param_set == "pop_mu3_n2_rho" ~ status_m3n2,
      param_set == "pop_mu3_n3_rho" ~ status_m3n3,
      param_set == "pop_mu3_n4_rho" ~ status_m3n4,
      param_set == "pop_mu3_n5_rho" ~ status_m3n5,
      param_set == "pop_mu3_n6_rho" ~ status_m3n6,
    )
  ) %>% 
  select(-run, -status_m3n1, -status_m3n2, 
         -status_m3n3, -status_m3n4, 
         -status_m3n5, -status_m3n6) -> rho_df_fixed_mu3

rm(rho_df_mu3, rho_df_temp_mu3)


#--------------- GET RHO DATA - FIXED N --------------------

# get duplicated and unique rho values 
# set 1
tibble(
  pop_n1_mu1_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m1 = case_when(
      run %in% n1_mu1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu1_rho_df

rm(pop_n1_mu1_rho, n1_mu1_index)

tibble(
  pop_n1_mu2_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m2 = case_when(
      run %in% n1_mu2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu2_rho_df

rm(pop_n1_mu2_rho, n1_mu2_index)

tibble(
  pop_n1_mu3_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m3 = case_when(
      run %in% n1_mu3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu3_rho_df

rm(pop_n1_mu3_rho, n1_mu3_index)

tibble(
  pop_n1_mu4_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m4 = case_when(
      run %in% n1_mu4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu4_rho_df

rm(pop_n1_mu4_rho, n1_mu4_index)

tibble(
  pop_n1_mu5_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m5 = case_when(
      run %in% n1_mu5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu5_rho_df

rm(pop_n1_mu5_rho, n1_mu5_index)

tibble(
  pop_n1_mu6_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m6 = case_when(
      run %in% n1_mu6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu6_rho_df

rm(pop_n1_mu6_rho, n1_mu6_index)

# set 2
tibble(
  pop_n2_mu1_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m1 = case_when(
      run %in% n2_mu1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu1_rho_df

rm(pop_n2_mu1_rho, n2_mu1_index)

tibble(
  pop_n2_mu2_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m2 = case_when(
      run %in% n2_mu2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu2_rho_df

rm(pop_n2_mu2_rho, n2_mu2_index)

tibble(
  pop_n2_mu3_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m3 = case_when(
      run %in% n2_mu3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu3_rho_df

rm(pop_n2_mu3_rho, n2_mu3_index)

tibble(
  pop_n2_mu4_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m4 = case_when(
      run %in% n2_mu4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu4_rho_df

rm(pop_n2_mu4_rho, n2_mu4_index)

tibble(
  pop_n2_mu5_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m5 = case_when(
      run %in% n2_mu5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu5_rho_df

rm(pop_n2_mu5_rho, n2_mu5_index)

tibble(
  pop_n2_mu6_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m6 = case_when(
      run %in% n2_mu6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu6_rho_df

rm(pop_n2_mu6_rho, n2_mu6_index)

# set 3
tibble(
  pop_n3_mu1_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m1 = case_when(
      run %in% n3_mu1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu1_rho_df

rm(pop_n3_mu1_rho, n3_mu1_index)

tibble(
  pop_n3_mu2_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m2 = case_when(
      run %in% n3_mu2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu2_rho_df

rm(pop_n3_mu2_rho, n3_mu2_index)

tibble(
  pop_n3_mu3_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m3 = case_when(
      run %in% n3_mu3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu3_rho_df

rm(pop_n3_mu3_rho, n3_mu3_index)

tibble(
  pop_n3_mu4_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m4 = case_when(
      run %in% n3_mu4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu4_rho_df

rm(pop_n3_mu4_rho, n3_mu4_index)

tibble(
  pop_n3_mu5_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m5 = case_when(
      run %in% n3_mu5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu5_rho_df

rm(pop_n3_mu5_rho, n3_mu5_index)

tibble(
  pop_n3_mu6_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m6 = case_when(
      run %in% n3_mu6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu6_rho_df

rm(pop_n3_mu6_rho, n3_mu6_index)


# join data sets
# set 1
rho_df_n1 <- full_join(pop_n1_mu1_rho_df, pop_n1_mu2_rho_df) %>% 
  full_join(., pop_n1_mu3_rho_df) %>% 
  full_join(., pop_n1_mu4_rho_df) %>% 
  full_join(., pop_n1_mu5_rho_df) %>% 
  full_join(., pop_n1_mu6_rho_df)

rm(pop_n1_mu1_rho_df, pop_n1_mu2_rho_df, pop_n1_mu3_rho_df,
   pop_n1_mu4_rho_df, pop_n1_mu5_rho_df, pop_n1_mu6_rho_df)

rho_df_n1 %>% 
  pivot_longer(
    cols = c(pop_n1_mu1_rho, pop_n1_mu2_rho,
             pop_n1_mu3_rho, pop_n1_mu4_rho,
             pop_n1_mu5_rho, pop_n1_mu6_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp_n1

rho_df_temp_n1 %>% 
  mutate(
    status = case_when(
      param_set == "pop_n1_mu1_rho" ~ status_n1m1,
      param_set == "pop_n1_mu2_rho" ~ status_n1m2,
      param_set == "pop_n1_mu3_rho" ~ status_n1m3,
      param_set == "pop_n1_mu4_rho" ~ status_n1m4,
      param_set == "pop_n1_mu5_rho" ~ status_n1m5,
      param_set == "pop_n1_mu6_rho" ~ status_n1m6,
    )
  ) %>% 
  select(-run, -status_n1m1, -status_n1m2,
         -status_n1m3, -status_n1m4, 
         -status_n1m5, -status_n1m6) -> rho_df_fixed_n1

rm(rho_df_n1, rho_df_temp_n1)

# set 2
rho_df_n2 <- full_join(pop_n2_mu1_rho_df, pop_n2_mu2_rho_df) %>% 
  full_join(., pop_n2_mu3_rho_df) %>% 
  full_join(., pop_n2_mu4_rho_df) %>% 
  full_join(., pop_n2_mu5_rho_df) %>% 
  full_join(., pop_n2_mu6_rho_df)

rm(pop_n2_mu1_rho_df, pop_n2_mu2_rho_df, pop_n2_mu3_rho_df,
   pop_n2_mu4_rho_df, pop_n2_mu5_rho_df, pop_n2_mu6_rho_df)

rho_df_n2 %>% 
  pivot_longer(
    cols = c(pop_n2_mu1_rho, pop_n2_mu2_rho,
             pop_n2_mu3_rho, pop_n2_mu4_rho,
             pop_n2_mu5_rho, pop_n2_mu6_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp_n2

rho_df_temp_n2 %>% 
  mutate(
    status = case_when(
      param_set == "pop_n2_mu1_rho" ~ status_n2m1,
      param_set == "pop_n2_mu2_rho" ~ status_n2m2,
      param_set == "pop_n2_mu3_rho" ~ status_n2m3,
      param_set == "pop_n2_mu4_rho" ~ status_n2m4,
      param_set == "pop_n2_mu5_rho" ~ status_n2m5,
      param_set == "pop_n2_mu6_rho" ~ status_n2m6,
    )
  ) %>% 
  select(-run, -status_n2m1, -status_n2m2,
         -status_n2m3, -status_n2m4, 
         -status_n2m5, -status_n2m6) -> rho_df_fixed_n2

rm(rho_df_n2, rho_df_temp_n2)

# set 3
rho_df_n3 <- full_join(pop_n3_mu1_rho_df, pop_n3_mu2_rho_df) %>% 
  full_join(., pop_n3_mu3_rho_df) %>% 
  full_join(., pop_n3_mu4_rho_df) %>% 
  full_join(., pop_n3_mu5_rho_df) %>% 
  full_join(., pop_n3_mu6_rho_df)

rm(pop_n3_mu1_rho_df, pop_n3_mu2_rho_df, pop_n3_mu3_rho_df,
   pop_n3_mu4_rho_df, pop_n3_mu5_rho_df, pop_n3_mu6_rho_df)

rho_df_n3 %>% 
  pivot_longer(
    cols = c(pop_n3_mu1_rho, pop_n3_mu2_rho,
             pop_n3_mu3_rho, pop_n3_mu4_rho,
             pop_n3_mu5_rho, pop_n3_mu6_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp_n3

rho_df_temp_n3 %>% 
  mutate(
    status = case_when(
      param_set == "pop_n3_mu1_rho" ~ status_n3m1,
      param_set == "pop_n3_mu2_rho" ~ status_n3m2,
      param_set == "pop_n3_mu3_rho" ~ status_n3m3,
      param_set == "pop_n3_mu4_rho" ~ status_n3m4,
      param_set == "pop_n3_mu5_rho" ~ status_n3m5,
      param_set == "pop_n3_mu6_rho" ~ status_n3m6,
    )
  ) %>% 
  select(-run, -status_n3m1, -status_n3m2,
         -status_n3m3, -status_n3m4, 
         -status_n3m5, -status_n3m6) -> rho_df_fixed_n3

rm(rho_df_n3, rho_df_temp_n3)


#--------------- TIDY RHO DATA  --------------------

# join data sets
rho_df_fixed_mu <- full_join(rho_df_fixed_mu1, rho_df_fixed_mu2) %>% 
  full_join(., rho_df_fixed_mu3)

rm(rho_df_fixed_mu1, rho_df_fixed_mu2, rho_df_fixed_mu3)

rho_df_fixed_n <- full_join(rho_df_fixed_n1, rho_df_fixed_n2) %>% 
  full_join(., rho_df_fixed_n3)

rm(rho_df_fixed_n1, rho_df_fixed_n2, rho_df_fixed_n3)

# add parameter info
rho_df_fixed_mu %>% 
  mutate(
    pop_size = case_when(
      param_set == "pop_mu1_n1_rho" |
        param_set == "pop_mu2_n1_rho" |
        param_set == "pop_mu3_n1_rho" ~ 100,
      param_set == "pop_mu1_n2_rho" |
        param_set == "pop_mu2_n2_rho" |
        param_set == "pop_mu3_n2_rho" ~ 316,
      param_set == "pop_mu1_n3_rho" |
        param_set == "pop_mu2_n3_rho" |
        param_set == "pop_mu3_n3_rho" ~ 1000,
      param_set == "pop_mu1_n4_rho" |
        param_set == "pop_mu2_n4_rho" |
        param_set == "pop_mu3_n4_rho" ~ 3160,
      param_set == "pop_mu1_n5_rho" |
        param_set == "pop_mu2_n5_rho" |
        param_set == "pop_mu3_n5_rho" ~ 10000,
      param_set == "pop_mu1_n6_rho" |
        param_set == "pop_mu2_n6_rho" |
        param_set == "pop_mu3_n6_rho" ~ 31600
    ),
    mut_rate = case_when(
      param_set == "pop_mu1_n1_rho" |
        param_set == "pop_mu1_n2_rho" |
        param_set == "pop_mu1_n3_rho" |
        param_set == "pop_mu1_n4_rho" |
        param_set == "pop_mu1_n5_rho" |
        param_set == "pop_mu1_n6_rho" ~ 1.5e-8,
      param_set == "pop_mu2_n1_rho" |
        param_set == "pop_mu2_n2_rho" |
        param_set == "pop_mu2_n3_rho" |
        param_set == "pop_mu2_n4_rho" |
        param_set == "pop_mu2_n5_rho" |
        param_set == "pop_mu2_n6_rho" ~ 1.5e-9,
      param_set == "pop_mu3_n1_rho" |
        param_set == "pop_mu3_n2_rho" |
        param_set == "pop_mu3_n3_rho" |
        param_set == "pop_mu3_n4_rho" |
        param_set == "pop_mu3_n5_rho" |
        param_set == "pop_mu3_n6_rho" ~ 1.5e-7,
    ),
    set = "fixed_mu"
  ) -> rho_mu_unsort_df

rho_mu_unsort_df$param_set <- as.factor(rho_mu_unsort_df$param_set)
rho_mu_unsort_df$set <- as.factor(rho_mu_unsort_df$set)
rho_mu_unsort_df$status <- as.factor(rho_mu_unsort_df$status)

rm(rho_df_fixed_mu)

rho_df_fixed_n %>% 
  mutate(
    pop_size = case_when(
      param_set == "pop_n1_mu1_rho" |
        param_set == "pop_n1_mu2_rho" |
        param_set == "pop_n1_mu3_rho" |
        param_set == "pop_n1_mu4_rho" |
        param_set == "pop_n1_mu5_rho" |
        param_set == "pop_n1_mu6_rho" ~ 10000,
      param_set == "pop_n2_mu1_rho" |
        param_set == "pop_n2_mu2_rho" |
        param_set == "pop_n2_mu3_rho" |
        param_set == "pop_n2_mu4_rho" |
        param_set == "pop_n2_mu5_rho" |
        param_set == "pop_n2_mu6_rho" ~ 1000,
      param_set == "pop_n3_mu1_rho" |
        param_set == "pop_n3_mu2_rho" |
        param_set == "pop_n3_mu3_rho" |
        param_set == "pop_n3_mu4_rho" |
        param_set == "pop_n3_mu5_rho" |
        param_set == "pop_n3_mu6_rho" ~ 100000
    ),
    mut_rate = case_when(
      param_set == "pop_n1_mu1_rho" |
        param_set == "pop_n2_mu1_rho" |
        param_set == "pop_n3_mu1_rho" ~ 1.0e-10,
      param_set == "pop_n1_mu2_rho" |
        param_set == "pop_n2_mu2_rho" |
        param_set == "pop_n3_mu2_rho" ~ 3.16e-10,
      param_set == "pop_n1_mu3_rho" |
        param_set == "pop_n2_mu3_rho" |
        param_set == "pop_n3_mu3_rho" ~ 1.0e-9,
      param_set == "pop_n1_mu4_rho" |
        param_set == "pop_n2_mu4_rho" |
        param_set == "pop_n3_mu4_rho" ~ 3.16e-9,
      param_set == "pop_n1_mu5_rho" |
        param_set == "pop_n2_mu5_rho" |
        param_set == "pop_n3_mu5_rho" ~ 1.0e-8,
      param_set == "pop_n1_mu6_rho" |
        param_set == "pop_n2_mu6_rho" |
        param_set == "pop_n3_mu6_rho" ~ 3.16e-8
    ),
    set = "fixed_n"
  ) -> rho_n_unsort_df

rho_n_unsort_df$param_set <- as.factor(rho_n_unsort_df$param_set)
rho_n_unsort_df$set <- as.factor(rho_n_unsort_df$set)
rho_n_unsort_df$status <- as.factor(rho_n_unsort_df$status)

rm(rho_df_fixed_n)


#--------------- SAVE RHO DATA --------------------

save(
  rho_mu_unsort_df, 
  file = file.path(
    path_to_results,
    'dup_analysis_rho_fixed_mu_unsort_results.RData'
  )
)

save(
  rho_n_unsort_df, 
  file = file.path(
    path_to_results,
    'dup_analysis_rho_fixed_n_unsort_results.RData'
  )
)

#--------------- GET SEGSITE DATA - FIXED MU --------------------

# get duplicate and unique rho values
# set 1
tibble(
  pop_mu1_n1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n1 = case_when(
      run %in% mu1_n1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n1_sites_df

rm(pop_mu1_n1_segsites, mu1_n1_index)

tibble(
  pop_mu1_n2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n2 = case_when(
      run %in% mu1_n2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n2_sites_df

rm(pop_mu1_n2_segsites, mu1_n2_index)

tibble(
  pop_mu1_n3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n3 = case_when(
      run %in% mu1_n3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n3_sites_df

rm(pop_mu1_n3_segsites, mu1_n3_index)

tibble(
  pop_mu1_n4_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n4 = case_when(
      run %in% mu1_n4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n4_sites_df

rm(pop_mu1_n4_segsites, mu1_n4_index)

tibble(
  pop_mu1_n5_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n5 = case_when(
      run %in% mu1_n5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n5_sites_df

rm(pop_mu1_n5_segsites, mu1_n5_index)

tibble(
  pop_mu1_n6_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1n6 = case_when(
      run %in% mu1_n6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu1_n6_sites_df

rm(pop_mu1_n6_segsites, mu1_n6_index)

# set 2
tibble(
  pop_mu2_n1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n1 = case_when(
      run %in% mu2_n1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n1_sites_df

rm(pop_mu2_n1_segsites, mu2_n1_index)

tibble(
  pop_mu2_n2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n2 = case_when(
      run %in% mu2_n2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n2_sites_df

rm(pop_mu2_n2_segsites, mu2_n2_index)

tibble(
  pop_mu2_n3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n3 = case_when(
      run %in% mu2_n3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n3_sites_df

rm(pop_mu2_n3_segsites, mu2_n3_index)

tibble(
  pop_mu2_n4_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n4 = case_when(
      run %in% mu2_n4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n4_sites_df

rm(pop_mu2_n4_segsites, mu2_n4_index)

tibble(
  pop_mu2_n5_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n5 = case_when(
      run %in% mu2_n5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n5_sites_df

rm(pop_mu2_n5_segsites, mu2_n5_index)

tibble(
  pop_mu2_n6_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2n6 = case_when(
      run %in% mu2_n6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu2_n6_sites_df

rm(pop_mu2_n6_segsites, mu2_n6_index)

# set 3
tibble(
  pop_mu3_n1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n1 = case_when(
      run %in% mu3_n1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n1_sites_df

rm(pop_mu3_n1_segsites, mu3_n1_index)

tibble(
  pop_mu3_n2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n2 = case_when(
      run %in% mu3_n2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n2_sites_df

rm(pop_mu3_n2_segsites, mu3_n2_index)

tibble(
  pop_mu3_n3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n3 = case_when(
      run %in% mu3_n3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n3_sites_df

rm(pop_mu3_n3_segsites, mu3_n3_index)

tibble(
  pop_mu3_n4_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n4 = case_when(
      run %in% mu3_n4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n4_sites_df

rm(pop_mu3_n4_segsites, mu3_n4_index)

tibble(
  pop_mu3_n5_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n5 = case_when(
      run %in% mu3_n5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n5_sites_df

rm(pop_mu3_n5_segsites, mu3_n5_index)

tibble(
  pop_mu3_n6_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m3n6 = case_when(
      run %in% mu3_n6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_mu3_n6_sites_df

rm(pop_mu3_n6_segsites, mu3_n6_index)


# join data sets
# set 1
sites_df_mu1 <- full_join(pop_mu1_n1_sites_df, pop_mu1_n2_sites_df) %>% 
  full_join(., pop_mu1_n3_sites_df) %>% 
  full_join(., pop_mu1_n4_sites_df) %>% 
  full_join(., pop_mu1_n5_sites_df) %>% 
  full_join(., pop_mu1_n6_sites_df)

rm(pop_mu1_n1_sites_df, pop_mu1_n2_sites_df, pop_mu1_n3_sites_df,
   pop_mu1_n4_sites_df, pop_mu1_n5_sites_df, pop_mu1_n6_sites_df)

sites_df_mu1 %>% 
  pivot_longer(
    cols = c(pop_mu1_n1_segsites, pop_mu1_n2_segsites,
             pop_mu1_n3_segsites, pop_mu1_n4_segsites,
             pop_mu1_n5_segsites, pop_mu1_n6_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp_mu1

sites_df_temp_mu1 %>% 
  mutate(
    status = case_when(
      param_set == "pop_mu1_n1_segsites" ~ status_m1n1,
      param_set == "pop_mu1_n2_segsites" ~ status_m1n2,
      param_set == "pop_mu1_n3_segsites" ~ status_m1n3,
      param_set == "pop_mu1_n4_segsites" ~ status_m1n4,
      param_set == "pop_mu1_n5_segsites" ~ status_m1n5,
      param_set == "pop_mu1_n6_segsites" ~ status_m1n6
    )
  ) %>% 
  select(-run, -status_m1n1, -status_m1n2, 
         -status_m1n3, -status_m1n4, 
         -status_m1n5, -status_m1n6) -> sites_df_fixed_mu1

rm(sites_df_mu1, sites_df_temp_mu1)

# set 2
sites_df_mu2 <- full_join(pop_mu2_n1_sites_df, pop_mu2_n2_sites_df) %>% 
  full_join(., pop_mu2_n3_sites_df) %>% 
  full_join(., pop_mu2_n4_sites_df) %>% 
  full_join(., pop_mu2_n5_sites_df) %>% 
  full_join(., pop_mu2_n6_sites_df)

rm(pop_mu2_n1_sites_df, pop_mu2_n2_sites_df, pop_mu2_n3_sites_df,
   pop_mu2_n4_sites_df, pop_mu2_n5_sites_df, pop_mu2_n6_sites_df)

sites_df_mu2 %>% 
  pivot_longer(
    cols = c(pop_mu2_n1_segsites, pop_mu2_n2_segsites,
             pop_mu2_n3_segsites, pop_mu2_n4_segsites,
             pop_mu2_n5_segsites, pop_mu2_n6_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp_mu2

sites_df_temp_mu2 %>% 
  mutate(
    status = case_when(
      param_set == "pop_mu2_n1_segsites" ~ status_m2n1,
      param_set == "pop_mu2_n2_segsites" ~ status_m2n2,
      param_set == "pop_mu2_n3_segsites" ~ status_m2n3,
      param_set == "pop_mu2_n4_segsites" ~ status_m2n4,
      param_set == "pop_mu2_n5_segsites" ~ status_m2n5,
      param_set == "pop_mu2_n6_segsites" ~ status_m2n6
    )
  ) %>% 
  select(-run, -status_m2n1, -status_m2n2, 
         -status_m2n3, -status_m2n4, 
         -status_m2n5, -status_m2n6) -> sites_df_fixed_mu2

rm(sites_df_mu2, sites_df_temp_mu2)

# set 3
sites_df_mu3 <- full_join(pop_mu3_n1_sites_df, pop_mu3_n2_sites_df) %>% 
  full_join(., pop_mu3_n3_sites_df) %>% 
  full_join(., pop_mu3_n4_sites_df) %>% 
  full_join(., pop_mu3_n5_sites_df) %>% 
  full_join(., pop_mu3_n6_sites_df)

rm(pop_mu3_n1_sites_df, pop_mu3_n2_sites_df, pop_mu3_n3_sites_df,
   pop_mu3_n4_sites_df, pop_mu3_n5_sites_df, pop_mu3_n6_sites_df)

sites_df_mu3 %>% 
  pivot_longer(
    cols = c(pop_mu3_n1_segsites, pop_mu3_n2_segsites,
             pop_mu3_n3_segsites, pop_mu3_n4_segsites,
             pop_mu3_n5_segsites, pop_mu3_n6_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp_mu3

sites_df_temp_mu3 %>% 
  mutate(
    status = case_when(
      param_set == "pop_mu3_n1_segsites" ~ status_m3n1,
      param_set == "pop_mu3_n2_segsites" ~ status_m3n2,
      param_set == "pop_mu3_n3_segsites" ~ status_m3n3,
      param_set == "pop_mu3_n4_segsites" ~ status_m3n4,
      param_set == "pop_mu3_n5_segsites" ~ status_m3n5,
      param_set == "pop_mu3_n6_segsites" ~ status_m3n6
    )
  ) %>% 
  select(-run, -status_m3n1, -status_m3n2, 
         -status_m3n3, -status_m3n4, 
         -status_m3n5, -status_m3n6) -> sites_df_fixed_mu3

rm(sites_df_mu3, sites_df_temp_mu3)


#--------------- GET SEGSITE DATA - FIXED N --------------------

# get duplicated and unique rho values 
# set 1
tibble(
  pop_n1_mu1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m1 = case_when(
      run %in% n1_mu1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu1_sites_df

rm(pop_n1_mu1_segsites, n1_mu1_index)

tibble(
  pop_n1_mu2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m2 = case_when(
      run %in% n1_mu2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu2_sites_df

rm(pop_n1_mu2_segsites, n1_mu2_index)

tibble(
  pop_n1_mu3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m3 = case_when(
      run %in% n1_mu3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu3_sites_df

rm(pop_n1_mu3_segsites, n1_mu3_index)

tibble(
  pop_n1_mu4_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m4 = case_when(
      run %in% n1_mu4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu4_sites_df

rm(pop_n1_mu4_segsites, n1_mu4_index)

tibble(
  pop_n1_mu5_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m5 = case_when(
      run %in% n1_mu5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu5_sites_df

rm(pop_n1_mu5_segsites, n1_mu5_index)

tibble(
  pop_n1_mu6_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n1m6 = case_when(
      run %in% n1_mu6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n1_mu6_sites_df

rm(pop_n1_mu6_segsites, n1_mu6_index)

# set 2
tibble(
  pop_n2_mu1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m1 = case_when(
      run %in% n2_mu1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu1_sites_df

rm(pop_n2_mu1_segsites, n2_mu1_index)

tibble(
  pop_n2_mu2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m2 = case_when(
      run %in% n2_mu2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu2_sites_df

rm(pop_n2_mu2_segsites, n2_mu2_index)

tibble(
  pop_n2_mu3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m3 = case_when(
      run %in% n2_mu3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu3_sites_df

rm(pop_n2_mu3_segsites, n2_mu3_index)

tibble(
  pop_n2_mu4_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m4 = case_when(
      run %in% n2_mu4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu4_sites_df

rm(pop_n2_mu4_segsites, n2_mu4_index)

tibble(
  pop_n2_mu5_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m5 = case_when(
      run %in% n2_mu5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu5_sites_df

rm(pop_n2_mu5_segsites, n2_mu5_index)

tibble(
  pop_n2_mu6_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n2m6 = case_when(
      run %in% n2_mu6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n2_mu6_sites_df

rm(pop_n2_mu6_segsites, n2_mu6_index)

# set 3
tibble(
  pop_n3_mu1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m1 = case_when(
      run %in% n3_mu1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu1_sites_df

rm(pop_n3_mu1_segsites, n3_mu1_index)

tibble(
  pop_n3_mu2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m2 = case_when(
      run %in% n3_mu2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu2_sites_df

rm(pop_n3_mu2_segsites, n3_mu2_index)

tibble(
  pop_n3_mu3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m3 = case_when(
      run %in% n3_mu3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu3_sites_df

rm(pop_n3_mu3_segsites, n3_mu3_index)

tibble(
  pop_n3_mu4_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m4 = case_when(
      run %in% n3_mu4_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu4_sites_df

rm(pop_n3_mu4_segsites, n3_mu4_index)

tibble(
  pop_n3_mu5_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m5 = case_when(
      run %in% n3_mu5_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu5_sites_df

rm(pop_n3_mu5_segsites, n3_mu5_index)

tibble(
  pop_n3_mu6_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_n3m6 = case_when(
      run %in% n3_mu6_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> pop_n3_mu6_sites_df

rm(pop_n3_mu6_segsites, n3_mu6_index)


# join data sets
# set 1
sites_df_n1 <- full_join(pop_n1_mu1_sites_df, pop_n1_mu2_sites_df) %>% 
  full_join(., pop_n1_mu3_sites_df) %>% 
  full_join(., pop_n1_mu4_sites_df) %>% 
  full_join(., pop_n1_mu5_sites_df) %>% 
  full_join(., pop_n1_mu6_sites_df)

rm(pop_n1_mu1_sites_df, pop_n1_mu2_sites_df, pop_n1_mu3_sites_df,
   pop_n1_mu4_sites_df, pop_n1_mu5_sites_df, pop_n1_mu6_sites_df)

sites_df_n1 %>% 
  pivot_longer(
    cols = c(pop_n1_mu1_segsites, pop_n1_mu2_segsites,
             pop_n1_mu3_segsites, pop_n1_mu4_segsites,
             pop_n1_mu5_segsites, pop_n1_mu6_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp_n1

sites_df_temp_n1 %>% 
  mutate(
    status = case_when(
      param_set == "pop_n1_mu1_segsites" ~ status_n1m1,
      param_set == "pop_n1_mu2_segsites" ~ status_n1m2,
      param_set == "pop_n1_mu3_segsites" ~ status_n1m3,
      param_set == "pop_n1_mu4_segsites" ~ status_n1m4,
      param_set == "pop_n1_mu5_segsites" ~ status_n1m5,
      param_set == "pop_n1_mu6_segsites" ~ status_n1m6,
    )
  ) %>% 
  select(-run, -status_n1m1, -status_n1m2,
         -status_n1m3, -status_n1m4,
         -status_n1m5, -status_n1m6) -> sites_df_fixed_n1

rm(sites_df_n1, sites_df_temp_n1)

# set 2
sites_df_n2 <- full_join(pop_n2_mu1_sites_df, pop_n2_mu2_sites_df) %>% 
  full_join(., pop_n2_mu3_sites_df) %>% 
  full_join(., pop_n2_mu4_sites_df) %>% 
  full_join(., pop_n2_mu5_sites_df) %>% 
  full_join(., pop_n2_mu6_sites_df)

rm(pop_n2_mu1_sites_df, pop_n2_mu2_sites_df, pop_n2_mu3_sites_df,
   pop_n2_mu4_sites_df, pop_n2_mu5_sites_df, pop_n2_mu6_sites_df)

sites_df_n2 %>% 
  pivot_longer(
    cols = c(pop_n2_mu1_segsites, pop_n2_mu2_segsites,
             pop_n2_mu3_segsites, pop_n2_mu4_segsites,
             pop_n2_mu5_segsites, pop_n2_mu6_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp_n2

sites_df_temp_n2 %>% 
  mutate(
    status = case_when(
      param_set == "pop_n2_mu1_segsites" ~ status_n2m1,
      param_set == "pop_n2_mu2_segsites" ~ status_n2m2,
      param_set == "pop_n2_mu3_segsites" ~ status_n2m3,
      param_set == "pop_n2_mu4_segsites" ~ status_n2m4,
      param_set == "pop_n2_mu5_segsites" ~ status_n2m5,
      param_set == "pop_n2_mu6_segsites" ~ status_n2m6,
    )
  ) %>% 
  select(-run, -status_n2m1, -status_n2m2,
         -status_n2m3, -status_n2m4,
         -status_n2m5, -status_n2m6) -> sites_df_fixed_n2

rm(sites_df_n2, sites_df_temp_n2)

# set 3
sites_df_n3 <- full_join(pop_n3_mu1_sites_df, pop_n3_mu2_sites_df) %>% 
  full_join(., pop_n3_mu3_sites_df) %>% 
  full_join(., pop_n3_mu4_sites_df) %>% 
  full_join(., pop_n3_mu5_sites_df) %>% 
  full_join(., pop_n3_mu6_sites_df)

rm(pop_n3_mu1_sites_df, pop_n3_mu2_sites_df, pop_n3_mu3_sites_df,
   pop_n3_mu4_sites_df, pop_n3_mu5_sites_df, pop_n3_mu6_sites_df)

sites_df_n3 %>% 
  pivot_longer(
    cols = c(pop_n3_mu1_segsites, pop_n3_mu2_segsites,
             pop_n3_mu3_segsites, pop_n3_mu4_segsites,
             pop_n3_mu5_segsites, pop_n3_mu6_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp_n3

sites_df_temp_n3 %>% 
  mutate(
    status = case_when(
      param_set == "pop_n3_mu1_segsites" ~ status_n3m1,
      param_set == "pop_n3_mu2_segsites" ~ status_n3m2,
      param_set == "pop_n3_mu3_segsites" ~ status_n3m3,
      param_set == "pop_n3_mu4_segsites" ~ status_n3m4,
      param_set == "pop_n3_mu5_segsites" ~ status_n3m5,
      param_set == "pop_n3_mu6_segsites" ~ status_n3m6,
    )
  ) %>% 
  select(-run, -status_n3m1, -status_n3m2,
         -status_n3m3, -status_n3m4,
         -status_n3m5, -status_n3m6) -> sites_df_fixed_n3

rm(sites_df_n3, sites_df_temp_n3)


#--------------- TIDY SEGSITE DATA  --------------------

# join data sets
sites_df_fixed_mu <- full_join(sites_df_fixed_mu1, sites_df_fixed_mu2) %>% 
  full_join(., sites_df_fixed_mu3)

rm(sites_df_fixed_mu1, sites_df_fixed_mu2, sites_df_fixed_mu3)

sites_df_fixed_n <- full_join(sites_df_fixed_n1, sites_df_fixed_n2) %>% 
  full_join(., sites_df_fixed_n3)

rm(sites_df_fixed_n1, sites_df_fixed_n2, sites_df_fixed_n3)

# add parameter info
sites_df_fixed_mu %>% 
  mutate(
    pop_size = case_when(
      param_set == "pop_mu1_n1_segsites" |
        param_set == "pop_mu2_n1_segsites" |
        param_set == "pop_mu3_n1_segsites" ~ 100,
      param_set == "pop_mu1_n2_segsites" |
        param_set == "pop_mu2_n2_segsites" |
        param_set == "pop_mu3_n2_segsites" ~ 316,
      param_set == "pop_mu1_n3_segsites" |
        param_set == "pop_mu2_n3_segsites" |
        param_set == "pop_mu3_n3_segsites" ~ 1000,
      param_set == "pop_mu1_n4_segsites" |
        param_set == "pop_mu2_n4_segsites" |
        param_set == "pop_mu3_n4_segsites" ~ 3160,
      param_set == "pop_mu1_n5_segsites" |
        param_set == "pop_mu2_n5_segsites" |
        param_set == "pop_mu3_n5_segsites" ~ 10000,
      param_set == "pop_mu1_n6_segsites" |
        param_set == "pop_mu2_n6_segsites" |
        param_set == "pop_mu3_n6_segsites" ~ 31600
    ),
    mut_rate = case_when(
      param_set == "pop_mu1_n1_segsites" |
        param_set == "pop_mu1_n2_segsites" |
        param_set == "pop_mu1_n3_segsites" |
        param_set == "pop_mu1_n4_segsites" |
        param_set == "pop_mu1_n5_segsites" |
        param_set == "pop_mu1_n6_segsites" ~ 1.5e-8,
      param_set == "pop_mu2_n1_segsites" |
        param_set == "pop_mu2_n2_segsites" |
        param_set == "pop_mu2_n3_segsites" |
        param_set == "pop_mu2_n4_segsites" |
        param_set == "pop_mu2_n5_segsites" |
        param_set == "pop_mu2_n6_segsites" ~ 1.5e-9,
      param_set == "pop_mu3_n1_segsites" |
        param_set == "pop_mu3_n2_segsites" |
        param_set == "pop_mu3_n3_segsites" |
        param_set == "pop_mu3_n4_segsites" |
        param_set == "pop_mu3_n5_segsites" |
        param_set == "pop_mu3_n6_segsites" ~ 1.5e-7
    ),
    set = "fixed_mu"
  ) -> sites_mu_unsort_df

sites_mu_unsort_df$param_set <- as.factor(sites_mu_unsort_df$param_set)
sites_mu_unsort_df$set <- as.factor(sites_mu_unsort_df$set)
sites_mu_unsort_df$status <- as.factor(sites_mu_unsort_df$status)

rm(sites_df_fixed_mu)

sites_df_fixed_n %>% 
  mutate(
    pop_size = case_when(
      param_set == "pop_n1_mu1_segsites" |
        param_set == "pop_n1_mu2_segsites" |
        param_set == "pop_n1_mu3_segsites" |
        param_set == "pop_n1_mu4_segsites" |
        param_set == "pop_n1_mu5_segsites" |
        param_set == "pop_n1_mu6_segsites" ~ 10000,
      param_set == "pop_n2_mu1_segsites" |
        param_set == "pop_n2_mu2_segsites" |
        param_set == "pop_n2_mu3_segsites" |
        param_set == "pop_n2_mu4_segsites" |
        param_set == "pop_n2_mu5_segsites" |
        param_set == "pop_n2_mu6_segsites" ~ 1000,
      param_set == "pop_n3_mu1_segsites" |
        param_set == "pop_n3_mu2_segsites" |
        param_set == "pop_n3_mu3_segsites" |
        param_set == "pop_n3_mu4_segsites" |
        param_set == "pop_n3_mu5_segsites" |
        param_set == "pop_n3_mu6_segsites" ~ 100000
    ),
    mut_rate = case_when(
      param_set == "pop_n1_mu1_segsites" |
        param_set == "pop_n2_mu1_segsites" |
        param_set == "pop_n3_mu1_segsites" ~ 1.0e-10,
      param_set == "pop_n1_mu2_segsites" |
        param_set == "pop_n2_mu2_segsites" |
        param_set == "pop_n3_mu2_segsites" ~ 3.16e-10,
      param_set == "pop_n1_mu3_segsites" |
        param_set == "pop_n2_mu3_segsites" |
        param_set == "pop_n3_mu3_segsites" ~ 1.0e-9,
      param_set == "pop_n1_mu4_segsites" |
        param_set == "pop_n2_mu4_segsites" |
        param_set == "pop_n3_mu4_segsites" ~ 3.16e-9,
      param_set == "pop_n1_mu5_segsites" |
        param_set == "pop_n2_mu5_segsites" |
        param_set == "pop_n3_mu5_segsites" ~ 1.0e-8,
      param_set == "pop_n1_mu6_segsites" |
        param_set == "pop_n2_mu6_segsites" |
        param_set == "pop_n3_mu6_segsites" ~ 3.16e-8
    ),
    set = "fixed_n"
  ) -> sites_n_unsort_df

sites_n_unsort_df$param_set <- as.factor(sites_n_unsort_df$param_set)
sites_n_unsort_df$set <- as.factor(sites_n_unsort_df$set)
sites_n_unsort_df$status <- as.factor(sites_n_unsort_df$status)

rm(sites_df_fixed_n)

#--------------- SAVE SITES DATA --------------------

save(
  sites_mu_unsort_df, 
  file = file.path(
    path_to_results,
    'dup_analysis_sites_fixed_mu_unsort_results.RData'
  )
)

save(
  sites_n_unsort_df, 
  file = file.path(
    path_to_results,
    'dup_analysis_sites_fixed_n_unsort_results.RData'
  )
)


