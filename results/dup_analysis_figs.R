#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: creates figures for alignment comparisons and duplicate parameter ranges
# Mackenzie M. Johnson
# July 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggtext)


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50

#--------------- LOAD DATA SETS --------------------

# alignment data - sorted
# fixed mu
load(file.path(path_to_results, 'dup_analysis_fixed_mu1_align_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_mu2_align_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_mu3_align_results.RData'))

# fixed n
load(file.path(path_to_results, 'dup_analysis_fixed_n1_align_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_n2_align_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_n3_align_results.RData'))

# join alignment data sets
dup_mu_df <- full_join(fixed_mu1_vary_n, fixed_mu2_vary_n) %>% 
  full_join(., fixed_mu3_vary_n)
rm(fixed_mu1_vary_n, fixed_mu2_vary_n, fixed_mu3_vary_n)

dup_n_df <- full_join(fixed_n1_vary_mu, fixed_n2_vary_mu) %>% 
  full_join(., fixed_n3_vary_mu)
rm(fixed_n1_vary_mu, fixed_n2_vary_mu, fixed_n3_vary_mu)

dup_df <- full_join(dup_mu_df, dup_n_df)
rm(dup_mu_df, dup_n_df)

# alignment data - unsorted
# fixed mu
load(file.path(path_to_results, 'dup_analysis_fixed_mu1_align_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_mu2_align_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_mu3_align_unsort_results.RData'))

# fixed n
load(file.path(path_to_results, 'dup_analysis_fixed_n1_align_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_n2_align_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_fixed_n3_align_unsort_results.RData'))

# join alignment data sets
dup_mu_unsort_df <- full_join(fixed_mu1_vary_n_unsort, fixed_mu2_vary_n_unsort) %>% 
  full_join(., fixed_mu3_vary_n_unsort)
rm(fixed_mu1_vary_n_unsort, fixed_mu2_vary_n_unsort, fixed_mu3_vary_n_unsort)

dup_n_unsort_df <- full_join(fixed_n1_vary_mu_unsort, fixed_n2_vary_mu_unsort) %>% 
  full_join(., fixed_n3_vary_mu_unsort)
rm(fixed_n1_vary_mu_unsort, fixed_n2_vary_mu_unsort, fixed_n3_vary_mu_unsort)

dup_unsort_df <- full_join(dup_mu_unsort_df, dup_n_unsort_df)
rm(dup_mu_unsort_df, dup_n_unsort_df)

#--------------- PERCENT DUPLICATE FIGURES --------------------

# figures for duplicates on n vs mu parameter sets
dup_df %>% 
  filter(set == "fixed_mu") %>% 
  ggplot(aes(x = pop_size, y = prop_dup*100)) +
  # geom_vline(xintercept = 1000, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 2000, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 5000, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 10000, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 15000, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 20000, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 50000, linetype = "dashed", color = "grey") +
  geom_point() +
  geom_path(aes(linetype = as.factor(mut_rate))) +
  scale_x_log10() +
  labs( 
    x = "*N*",
    y = "Percent duplicated (%)",
    linetype = "&mu;"
  ) +
  theme_half_open() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown()
  ) -> fig_dup_n

fig_dup_n

dup_df %>% 
  filter(set == "fixed_n") %>% 
  ggplot(aes(x = mut_rate, y = prop_dup*100)) +
  # geom_vline(xintercept = 1.5e-8, linetype = "dashed", color = "grey") +
  geom_point() +
  geom_path(aes(linetype = as.factor(pop_size))) +
  scale_x_log10() +
  labs(
    x = "&mu;", 
    y = "Percent duplicated (%)",
    linetype = "*N*"
  ) +
  theme_half_open() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown()
  ) -> fig_dup_mu

fig_dup_mu

plot_grid(
  fig_dup_n, fig_dup_mu,
  nrow = 2,
  align = "v"
) -> fig_dup_n_mu

fig_dup_n_mu


dup_unsort_df %>% 
  filter(set == "fixed_mu") %>% 
  ggplot(aes(x = pop_size, y = prop_dup*100)) +
  geom_point() +
  geom_path(aes(linetype = as.factor(mut_rate))) +
  scale_x_log10() +
  labs( 
    x = "*N*",
    y = "Percent duplicated (%)",
    linetype = "&mu;"
  ) +
  theme_half_open() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown()
  ) -> fig_dup_n_unsort

fig_dup_n_unsort

dup_unsort_df %>% 
  filter(set == "fixed_n") %>% 
  ggplot(aes(x = mut_rate, y = prop_dup*100)) +
  # geom_vline(xintercept = 1.5e-8, linetype = "dashed", color = "grey") +
  geom_point() +
  geom_path(aes(linetype = as.factor(pop_size))) +
  scale_x_log10() +
  labs(
    x = "&mu;", 
    y = "Percent duplicated (%)",
    linetype = "*N*"
  ) +
  theme_half_open() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown()
  ) -> fig_dup_mu_unsort

fig_dup_mu_unsort

plot_grid(
  fig_dup_n_unsort, fig_dup_mu_unsort,
  nrow = 2,
  align = "v"
) -> fig_dup_n_mu_unsort

fig_dup_n_mu_unsort


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_dup_by_param.png'),
  fig_dup_n_mu, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_dup_by_param_unsort.png'),
  fig_dup_n_mu_unsort, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

