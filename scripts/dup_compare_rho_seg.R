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
load(glue('{path_to_data}fixed_mu_vary_n_sites.RData'))
load(glue('{path_to_data}fixed_n_vary_mu_sites.RData'))

# load in indices of duplicated alignments
load(glue('{path_to_results}fixed_mu_align_indices.RData'))
load(glue('{path_to_results}fixed_n_align_indices.RData'))


#--------------- GET RHO DATA - FIXED MU --------------------

# get duplicate and unique rho values

tibble(
  sm1_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s1 = case_when(
      run %in% sm1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm1_pop_rho_df

rm(sm1_pop_rho, sm1_index)

tibble(
  sm2_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s2 = case_when(
      run %in% sm2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm2_pop_rho_df

rm(sm2_pop_rho, sm2_index)

tibble(
  sm3_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s3 = case_when(
      run %in% sm3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm3_pop_rho_df

rm(sm3_pop_rho, sm3_index)

tibble(
  small_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s = case_when(
      run %in% sm_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm_pop_rho_df

rm(small_pop_rho, sm_index)

tibble(
  med1_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1 = case_when(
      run %in% md1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med1_pop_rho_df

rm(med1_pop_rho, md1_index)

tibble(
  med2_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2 = case_when(
      run %in% md2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med2_pop_rho_df

rm(med2_pop_rho, md2_index)

tibble(
  med_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m = case_when(
      run %in% md_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med_pop_rho_df

rm(med_pop_rho, md_index)

tibble(
  large_pop_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_l = case_when(
      run %in% lg_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> lg_pop_rho_df

rm(large_pop_rho, lg_index)

# join data sets
rho_df_1 <- full_join(sm1_pop_rho_df, sm2_pop_rho_df) %>% 
  full_join(., sm3_pop_rho_df) %>% 
  full_join(., sm_pop_rho_df) %>% 
  full_join(., med1_pop_rho_df) %>% 
  full_join(., med2_pop_rho_df) %>% 
  full_join(., med_pop_rho_df) %>% 
  full_join(., lg_pop_rho_df)

rm(sm1_pop_rho_df, sm2_pop_rho_df, sm3_pop_rho_df,
   sm_pop_rho_df, med1_pop_rho_df, med2_pop_rho_df,
   med_pop_rho_df, lg_pop_rho_df)

rho_df_1 %>% 
  pivot_longer(
    cols = c(sm1_pop_rho, sm2_pop_rho, sm3_pop_rho,
             small_pop_rho, med1_pop_rho, med2_pop_rho,
             med_pop_rho, large_pop_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp

rho_df_temp %>% 
  mutate(
    status = case_when(
      param_set == "sm1_pop_rho" ~ status_s1,
      param_set == "sm2_pop_rho" ~ status_s2,
      param_set == "sm3_pop_rho" ~ status_s3,
      param_set == "small_pop_rho" ~ status_s,
      param_set == "med1_pop_rho" ~ status_m1,
      param_set == "med2_pop_rho" ~ status_m2,
      param_set == "med_pop_rho" ~ status_m,
      param_set == "large_pop_rho" ~ status_l,
    )
  ) %>% 
  select(-run, -status_s1, -status_s2, -status_s3,
         -status_s, -status_m1, -status_m2, 
         -status_m, -status_l) -> rho_df_fixed_mu

rm(rho_df_1, rho_df_temp)


#--------------- GET RHO DATA - FIXED N --------------------

# get duplicated and unique rho values 

tibble(
  lo1_mu_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_l1 = case_when(
      run %in% lw1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> lo1_mu_rho_df

rm(lo1_mu_rho, lw1_index)

tibble(
  lo2_mu_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_l2 = case_when(
      run %in% lw2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> lo2_mu_rho_df

rm(lo2_mu_rho, lw2_index)

tibble(
  low_mu_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_lw = case_when(
      run %in% lw_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> low_mu_rho_df

rm(low_mu_rho, lw_index)

tibble(
  me1_mu_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1 = case_when(
      run %in% md1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med1_mu_rho_df

rm(me1_mu_rho, md1_index)

tibble(
  med_mu_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m = case_when(
      run %in% md_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med_mu_rho_df

rm(med_mu_rho, md_index)

tibble(
  high_mu_rho,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_h = case_when(
      run %in% hg_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> high_mu_rho_df

rm(high_mu_rho, hg_index)


# join data sets
rho_df_2 <- full_join(lo1_mu_rho_df, lo2_mu_rho_df) %>% 
  full_join(., low_mu_rho_df) %>% 
  full_join(., med1_mu_rho_df) %>% 
  full_join(., med_mu_rho_df) %>% 
  full_join(., high_mu_rho_df) 

rm(lo1_mu_rho_df, lo2_mu_rho_df, low_mu_rho_df,
   med1_mu_rho_df, med_mu_rho_df, high_mu_rho_df)

rho_df_2 %>% 
  pivot_longer(
    cols = c(lo1_mu_rho, lo2_mu_rho, low_mu_rho,
             me1_mu_rho, med_mu_rho, high_mu_rho),
    names_to = "param_set",
    values_to = "rho"
  ) -> rho_df_temp

rho_df_temp %>% 
  mutate(
    status = case_when(
      param_set == "lo1_mu_rho" ~ status_l1,
      param_set == "lo2_mu_rho" ~ status_l2,
      param_set == "low_mu_rho" ~ status_lw,
      param_set == "me1_mu_rho" ~ status_m1,
      param_set == "med_mu_rho" ~ status_m,
      param_set == "high_mu_rho" ~ status_h,
    )
  ) %>% 
  select(-run, -status_l1, -status_l2, -status_lw, 
         -status_m1, -status_m, -status_h) -> rho_df_fixed_n

rm(rho_df_2, rho_df_temp)


#--------------- TIDY RHO DATA  --------------------

# join data sets
rho_df_all <- full_join(rho_df_fixed_mu, rho_df_fixed_n)

# add parameter info
rho_df_all %>% 
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
rho_df$status <- as.factor(rho_df$status)

rm(rho_df_all, rho_df_fixed_mu, rho_df_fixed_n)

#--------------- SAVE RHO DATA --------------------

save(
  rho_df, 
  file = glue('{path_to_results}dup_analysis_rho_results.RData')
)


#--------------- GET SEGSITE DATA - FIXED MU --------------------

# get duplicate and unique rho values

tibble(
  sm1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s1 = case_when(
      run %in% sm1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm1_pop_sites_df

rm(sm1_segsites, sm1_index)

tibble(
  sm2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s2 = case_when(
      run %in% sm2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm2_pop_sites_df

rm(sm2_segsites, sm2_index)


tibble(
  sm3_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s3 = case_when(
      run %in% sm3_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm3_pop_sites_df

rm(sm3_segsites, sm3_index)


tibble(
  sm_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_s = case_when(
      run %in% sm_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> sm_pop_sites_df

rm(sm_segsites, sm_index)


tibble(
  med1_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1 = case_when(
      run %in% md1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med1_pop_sites_df

rm(med1_segsites, md1_index)

tibble(
  med2_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m2 = case_when(
      run %in% md2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med2_pop_sites_df

rm(med2_segsites, md2_index)

tibble(
  med_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m = case_when(
      run %in% md_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med_pop_sites_df

rm(med_segsites, md_index)

tibble(
  lg_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_l = case_when(
      run %in% lg_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> lg_pop_sites_df

rm(lg_segsites, lg_index)

# join data sets
sites_df_1 <- full_join(sm1_pop_sites_df, sm2_pop_sites_df) %>% 
  full_join(., sm3_pop_sites_df) %>% 
  full_join(., sm_pop_sites_df) %>% 
  full_join(., med1_pop_sites_df) %>% 
  full_join(., med2_pop_sites_df) %>% 
  full_join(., med_pop_sites_df) %>% 
  full_join(., lg_pop_sites_df)

rm(sm1_pop_sites_df, sm2_pop_sites_df, sm3_pop_sites_df,
   sm_pop_sites_df, med1_pop_sites_df, med2_pop_sites_df,
   med_pop_sites_df, lg_pop_sites_df)

sites_df_1 %>% 
  pivot_longer(
    cols = c(sm1_segsites, sm2_segsites, sm3_segsites,
             sm_segsites, med1_segsites, med2_segsites,
             med_segsites, lg_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp

sites_df_temp %>% 
  mutate(
    status = case_when(
      param_set == "sm1_segsites" ~ status_s1,
      param_set == "sm2_segsites" ~ status_s2,
      param_set == "sm3_segsites" ~ status_s3,
      param_set == "sm_segsites" ~ status_s,
      param_set == "med1_segsites" ~ status_m1,
      param_set == "med2_segsites" ~ status_m2,
      param_set == "med_segsites" ~ status_m,
      param_set == "lg_segsites" ~ status_l,
    )
  ) %>% 
  select(-run, -status_s1, -status_s2, -status_s3,
         -status_s, -status_m1, -status_m2, 
         -status_m, -status_l) -> sites_df_fixed_mu

rm(sites_df_1, sites_df_temp)


#--------------- GET SEGSITE DATA - FIXED N --------------------

# get duplicated and unique rho values 

tibble(
  lo1_mu_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_l1 = case_when(
      run %in% lw1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> lo1_mu_sites_df

rm(lo1_mu_segsites, lw1_index)

tibble(
  lo2_mu_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_l2 = case_when(
      run %in% lw2_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> lo2_mu_sites_df

rm(lo2_mu_segsites, lw2_index)

tibble(
  low_mu_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_lw = case_when(
      run %in% lw_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> low_mu_sites_df

rm(low_mu_segsites, lw_index)

tibble(
  md1_mu_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m1 = case_when(
      run %in% md1_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> md1_mu_sites_df

rm(md1_mu_segsites, md1_index)

tibble(
  med_mu_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_m = case_when(
      run %in% md_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> med_mu_sites_df

rm(med_mu_segsites, md_index)

tibble(
  high_mu_segsites,
  run = seq(1:num_sims)
) %>% 
  mutate(
    status_h = case_when(
      run %in% hg_index ~ "duplicate",
      TRUE ~ "unique"
    )
  ) -> high_mu_sites_df

rm(high_mu_segsites, hg_index)

# join data sets
sites_df_2 <- full_join(lo1_mu_sites_df, lo2_mu_sites_df) %>% 
  full_join(., low_mu_sites_df) %>% 
  full_join(., md1_mu_sites_df) %>% 
  full_join(., med_mu_sites_df) %>% 
  full_join(., high_mu_sites_df) 

rm(lo1_mu_sites_df, lo2_mu_sites_df, low_mu_sites_df,
   md1_mu_sites_df, med_mu_sites_df, high_mu_sites_df)

sites_df_2 %>% 
  pivot_longer(
    cols = c(lo1_mu_segsites, lo2_mu_segsites, low_mu_segsites,
             md1_mu_segsites, med_mu_segsites, high_mu_segsites),
    names_to = "param_set",
    values_to = "segsites"
  ) -> sites_df_temp

sites_df_temp %>% 
  mutate(
    status = case_when(
      param_set == "lo1_mu_segsites" ~ status_l1,
      param_set == "lo2_mu_segsites" ~ status_l2,
      param_set == "low_mu_segsites" ~ status_lw,
      param_set == "md1_mu_segsites" ~ status_m1,
      param_set == "med_mu_segsites" ~ status_m,
      param_set == "high_mu_segsites" ~ status_h,
    )
  ) %>% 
  select(-run, -status_l1, -status_l2, -status_lw, 
         -status_m1, -status_m, -status_h) -> sites_df_fixed_n

rm(sites_df_2, sites_df_temp)

#--------------- TIDY RHO DATA  --------------------

# join data sets
sites_df_all <- full_join(sites_df_fixed_mu, sites_df_fixed_n)

# add parameter info
sites_df_all %>% 
  mutate(
    pop_size = case_when(
      param_set == "sm1_segsites" ~ 50,
      param_set == "sm2_segsites" ~ 100,
      param_set == "sm3_segsites" ~ 500,
      param_set == "sm_segsites" ~ 1000,
      param_set == "med1_segsites" ~ 2000,
      param_set == "med2_segsites" ~ 5000,
      param_set == "med_segsites" ~ 10000,
      param_set == "lg_segsites" ~ 50000,
      param_set == "low_mu_segsites" |
        param_set == "lo1_mu_segsites" |
        param_set == "lo2_mu_segsites" |
        param_set == "md1_mu_segsites" |
        param_set == "med_mu_segsites" | 
        param_set == "high_mu_segsites" ~ 10000
    ),
    mut_rate = case_when(
      param_set == "lo1_mu_segsites" ~ 1.5e-11,
      param_set == "lo2_mu_segsites" ~ 1.5e-10,
      param_set == "low_mu_segsites" ~ 1.5e-9,
      param_set == "md1_mu_segsites" ~ 0.5e-8,
      param_set == "med_mu_segsites" ~ 1.5e-8,
      param_set == "high_mu_segsites" ~ 1.5e-7,
      param_set == "sm_segsites" |
        param_set == "sm1_segsites" |
        param_set == "sm2_segsites" |
        param_set == "sm3_segsites" |
        param_set == "med1_segsites" |
        param_set == "med2_segsites" |
        param_set == "med_segsites" |
        param_set == "lg_segsites" ~ 1.5e-8,
    ),
    set = case_when(
      param_set == "low_mu_segsites" | 
        param_set == "lo1_mu_segsites" | 
        param_set == "lo2_mu_segsites" | 
        param_set == "md1_mu_segsites" |
        param_set == "med_mu_segsites" | 
        param_set == "high_mu_segsites" ~ "fixed_n",
      param_set == "sm_segsites" | 
        param_set == "sm1_segsites" | 
        param_set == "sm2_segsites" | 
        param_set == "sm3_segsites" | 
        param_set == "med1_segsites" |
        param_set == "med2_segsites" |
        param_set == "med_segsites" |
        param_set == "lg_segsites" ~ "fixed_mu"
    )
  ) -> sites_df

sites_df$param_set <- as.factor(sites_df$param_set)
sites_df$set <- as.factor(sites_df$set)
sites_df$status <- as.factor(sites_df$status)

rm(sites_df_all, sites_df_fixed_mu, sites_df_fixed_n)


#--------------- SAVE SITES DATA --------------------

save(
  sites_df, 
  file = glue('{path_to_results}dup_analysis_sites_results.RData')
)


