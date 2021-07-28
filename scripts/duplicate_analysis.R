#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: parses simulation data and compares all alignments to find duplicates
# Mackenzie M. Johnson
# July 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------
Sys.time()
cat("\nConfiguring environment.....\n")

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggtext)
library(glue)
library(reticulate)
# call the conda environment that has keras and tensorflow installed
#use_condaenv(condaenv = "r-reticulate", conda = "/home1/05072/mmjohn/.local/share/r-miniconda/conda.exe")
use_condaenv("r-reticulate", conda = "/stor/home/mmj2238/.local/share/r-miniconda/bin/conda")
library(keras) 

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
path_to_results <- "/stor/home/mmj2238/bio-cnns/code/recombination/results/"

# size of alignments
num_chrom <- 50

# max_size - need to use one standard size for consistency across training and test sets
max_size <- 400 

#--------------- READ IN DATA --------------------
Sys.time()
cat("\nReading in data.....\n")

# read in data for set a
small_pop <- readLines(glue('{path_to_raw}fixed_mu_n_1000_sims_all.txt'))
medium_pop <- readLines(glue('{path_to_raw}fixed_mu_n_10000_sims_all.txt'))
large_pop <- readLines(glue('{path_to_raw}fixed_mu_n_50000_sims_all.txt'))

# read in data for set b
low_mu <- readLines(glue('{path_to_raw}fixed_n_low_mu_sims_all.txt'))
medium_mu <- readLines(glue('{path_to_raw}fixed_n_med_mu_sims_all.txt'))
high_mu <- readLines(glue('{path_to_raw}fixed_n_high_mu_sims_all.txt'))


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


#--------------- SORT ALIGNMENTS --------------------
Sys.time()
cat("\nSorting alignments.....\n")

sm_pop_data <- lapply(sm_pop_unsorted, sort_align)
rm(sm_pop_unsorted)

md_pop_data <- lapply(md_pop_unsorted, sort_align)
rm(md_pop_unsorted)

lg_pop_data <- lapply(lg_pop_unsorted, sort_align)
rm(lg_pop_unsorted)

lw_mu_data <- lapply(lw_mu_unsorted, sort_align)
rm(lw_mu_unsorted)

md_mu_data <- lapply(md_mu_unsorted, sort_align)
rm(md_mu_unsorted)

hg_mu_data <- lapply(hg_mu_unsorted, sort_align)
rm(hg_mu_unsorted)


#--------------- PAD ALIGNMENTS --------------------
Sys.time()
cat("\nPadding alignments.....\n")

# use keras built-in function to pad alignments
sm_pop_padded <- lapply(
  sm_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(sm_pop_data)

md_pop_padded <- lapply(
  md_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md_pop_data)

lg_pop_padded <- lapply(
  lg_pop_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(lg_pop_data)

lw_mu_padded <- lapply(
  lw_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(lw_mu_data)

md_mu_padded <- lapply(
  md_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(md_mu_data)

hg_mu_padded <- lapply(
  hg_mu_data,
  function(x, ...) {pad_sequences(x, ...)}, # adding ... maintains matrix when segsites = 1
  #maxlen = 406,
  maxlen = max_size,   
  dtype = "float32",
  padding = "post"
)

rm(hg_mu_data)


#--------------- FIND DUPLICATES --------------------

Sys.time()
cat("\nIdentifying duplicates in parameter sets.....\n")

# compare each parameter set
true_cntr_1 <- 0
num_dupl_1 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        sm_pop_padded[[i]],
        sm_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_1 <- true_cntr_1 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_1 <- num_dupl_1 + 1 
  }
}

true_cntr_1     # 31,921
num_dupl_1      # 1,505
# total_cntr_1    # 199,990,000

rm(does_match, i, j, has_duplicate)

true_cntr_2 <- 0
num_dupl_2 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md_pop_padded[[i]],
        md_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_2 <- true_cntr_2 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_2 <- num_dupl_2 + 1 
  }
}

true_cntr_2     # 0
num_dupl_2      # 0

rm(does_match, i, j, has_duplicate)

true_cntr_3 <- 0
num_dupl_3 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lg_pop_padded[[i]],
        lg_pop_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_3 <- true_cntr_3 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_3 <- num_dupl_3 + 1 
  }
}

true_cntr_3     # 0
num_dupl_3      # 
 
rm(does_match, i, j, has_duplicate)

fixed_mu_vary_n <- tibble(
  mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8),
  pop_size = c(1000, 10000, 50000),
  num_match = c(true_cntr_1, true_cntr_2, true_cntr_3),
  total_dupl = c(num_dupl_1, num_dupl_2, num_dupl_3)
)

rm(sm_pop_padded, md_pop_padded, lg_pop_padded)

true_cntr_4 <- 0
num_dupl_4 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        lw_mu_padded[[i]],
        lw_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_4 <- true_cntr_4 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_4 <- num_dupl_4 + 1 
  }
}

true_cntr_4     # 21,897
num_dupl_4      # 1,170
 
rm(does_match, i, j, has_duplicate)

true_cntr_5 <- 0
num_dupl_5 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        md_mu_padded[[i]],
        md_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_5 <- true_cntr_5 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_5 <- num_dupl_5 + 1 
  }
}

true_cntr_5     # 0
num_dupl_5

rm(does_match, i, j, has_duplicate)

true_cntr_6 <- 0
num_dupl_6 <- 0

for (i in 1:20000) {
  has_duplicate <- FALSE
  for (j in 1:20000) {
    if (i > j) {
      does_match <- identical(
        hg_mu_padded[[i]],
        hg_mu_padded[[j]]
      )
      if (does_match == TRUE){ 
        true_cntr_6 <- true_cntr_6 + 1
        has_duplicate <- TRUE
      }
    }
  }
  if (has_duplicate == TRUE) {
    num_dupl_6 <- num_dupl_6 + 1 
  }
}

true_cntr_6     # 0
num_dupl_6      # 0

rm(does_match, i, j, has_duplicate)

fixed_n_vary_mu <- tibble(
  mut_rate = c(1.5e-9, 1.5e-8, 1.5e-7),
  pop_size = c(10000, 10000, 10000),
  num_match = c(true_cntr_4, true_cntr_5, true_cntr_6),
  total_dupl = c(num_dupl_4, num_dupl_5, num_dupl_6)
)

rm(lw_mu_padded, md_mu_padded, hg_mu_padded)

#--------------- TIDY DATA SETS --------------------

# fixed_mu_vary_n <- tibble(
#   mut_rate = c(1.5e-8, 1.5e-8, 1.5e-8),
#   pop_size = c(1000, 10000, 50000),
#   num_match = c(31921, 0, 0),
#   total_dupl = c(1505, 0, 0)
# )
# 
# fixed_n_vary_mu <- tibble(
#   mut_rate = c(1.5e-9, 1.5e-8, 1.5e-7),
#   pop_size = c(10000, 10000, 10000),
#   num_match = c(21897, 0, 0),
#   total_dupl = c(1170, 0, 0)
# )

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

dup_df <- full_join(fixed_mu_vary_n, fixed_n_vary_mu)

rho_df <- tibble(
  small_pop_rho, 
  med_pop_rho, 
  large_pop_rho,
  low_mu_rho,
  med_mu_rho,
  high_mu_rho
) %>% pivot_longer(
  cols = small_pop_rho:high_mu_rho,
  names_to = "param_set",
  values_to = "rho"
)

rho_df %>% 
  mutate(
    pop_size = case_when(
      param_set == "small_pop_rho" ~ 1000,
      param_set == "med_pop_rho" ~ 10000,
      param_set == "large_pop_rho" ~ 50000,
      param_set == "low_mu_rho" | 
        param_set == "med_mu_rho" | 
        param_set == "high_mu_rho" ~ 10000
    ),
    mut_rate = case_when(
      param_set == "low_mu_rho" ~ 1.5e-9,
      param_set == "med_mu_rho" ~ 1.5e-8,
      param_set == "high_mu_rho" ~ 1.5e-7,
      param_set == "small_pop_rho" | 
        param_set == "med_pop_rho" |
        param_set == "large_pop_rho" ~ 1.5e-8,
    ),
    set = case_when(
      param_set == "low_mu_rho" | 
        param_set == "med_mu_rho" | 
        param_set == "high_mu_rho" ~ "fixed_n",
      param_set == "small_pop_rho" | 
        param_set == "med_pop_rho" |
        param_set == "large_pop_rho" ~ "fixed_mu"
    )
  ) -> rho_df


#--------------- SAVE DATA SETS --------------------

save(
  dup_df, rho_df, 
  file = glue('{path_to_results}dup_analysis_results.RData')
)

#--------------- FIGURES --------------------

# figures for duplicates on n vs mu parameter sets
dup_df %>% 
  filter(set == "fixed_mu") %>% 
  ggplot(aes(x = pop_size, y = prop_dup*100)) +
  geom_vline(xintercept = 1000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 5000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 10000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 15000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 20000, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 50000, linetype = "dashed", color = "grey") +
  geom_point() +
  geom_path() +
  scale_x_log10() +
  labs( 
    x = "*N*",
    y = "Percent duplicated (%)"
  ) +
  theme_half_open() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) -> fig_dup_n

fig_dup_n

dup_df %>% 
  filter(set == "fixed_n") %>% 
  ggplot(aes(x = mut_rate, y = prop_dup*100)) +
  geom_vline(xintercept = 1.5e-8, linetype = "dashed", color = "grey") +
  geom_point() +
  geom_path() +
  scale_x_log10() +
  labs(
    x = "&mu;", 
    y = "Percent duplicated (%)"
  ) +
  theme_half_open() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) -> fig_dup_mu

fig_dup_mu

plot_grid(
  fig_dup_n, fig_dup_mu,
  nrow = 2,
  align = "v"
) -> fig_dup_n_mu

fig_dup_n_mu

# figure for rho values of parameter sets
rho_df %>% 
  filter(set == "fixed_mu") %>% 
  ggplot(aes(x = as.factor(pop_size), y = rho)) +
  geom_violin() +
  #facet_grid(vars(set)) +
  coord_flip() +
  labs(
    x = "*N*",
    y = "&rho;"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) -> fig_rho_n

fig_rho_n

rho_df %>% 
  filter(set == "fixed_n") %>% 
  ggplot(aes(x = as.factor(mut_rate), y = rho)) +
  geom_violin() +
  #facet_grid(vars(set)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 4000)) +
  labs(
    x = "&mu;",
    y = "&rho;"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) -> fig_rho_mu

fig_rho_mu

plot_grid(
  fig_rho_n, fig_rho_mu,
  nrow = 2,
  align = "v"
) -> fig_rho_n_mu

fig_rho_n_mu

#--------------- SAVE FIGURES --------------------

save_plot(
  glue('{path_to_results}tmpfigs/fig_dup_by_param.png'), 
  fig_dup_n_mu, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  glue('{path_to_results}tmpfigs/fig_rho_by_param.png'), 
  fig_rho_n_mu, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)


