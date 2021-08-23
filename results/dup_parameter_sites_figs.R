#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: creates figures for rho parameter range of all and duplicated alignments
# Mackenzie M. Johnson
# August 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggtext)
library(colorspace)


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50


#--------------- LOAD DATA SETS --------------------

# segregating sites data
# sorted, padded alignments
load(file.path(path_to_results, 'dup_analysis_sites_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_analysis_sites_fixed_n_results.RData'))

# unsorted, padded alignments
load(file.path(path_to_results, 'dup_analysis_sites_fixed_mu_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_sites_fixed_n_unsort_results.RData'))


#--------------- FIXED MU SITES DISTRIBUTION FIGURES --------------------

# combine data sets for faceted figure
sites_mu_df %>% 
  mutate(processing = "Sorted") -> mu_df1
sites_mu_unsort_df %>% 
  mutate(processing = "Unsorted") -> mu_df2
full_join(mu_df1, mu_df2) -> sites_mu_full_df

# manually set labels
labels_n <- c(
  "100" = "N = 100", 
  "316" = "N = 316",
  "1000" = "N = 1,000"
)

# figure showing seg. sites of duplicates compared to whole data set
sites_mu_full_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  filter(pop_size == 100 | pop_size == 316 | pop_size == 1000) %>% 
  ggplot(aes(x = segsites, fill = status)) +
  geom_bar(position = "stack") +
  facet_grid(
    rows = vars(processing),
    cols = vars(pop_size),
    labeller = labeller(pop_size = labels_n)
  ) +
  scale_fill_discrete_qualitative(
    palette = "Cold",
    name = "Identity",
    labels = c("Duplicate", "Unique")
  ) +
  coord_cartesian(xlim = c(-0.2,10)) +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10),
    name = "Number of segregating sites"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05)), 
    name = "Count"
  ) +
  theme_bw(12) +
  theme(
    strip.text = element_markdown()
  ) -> fig_sites_sort_v_unsort_fixed_mu

fig_sites_sort_v_unsort_fixed_mu


#--------------- FIXED N SITES DISTRIBUTION FIGURES --------------------

# combine data sets for faceted figure
sites_n_df %>% 
  mutate(processing = "Sorted") -> n1_df
sites_n_unsort_df %>% 
  mutate(processing = "Unsorted") -> n2_df
full_join(n1_df, n2_df) -> sites_n_full_df

# manually set labels
labels_mu <- c(
  "1e-10" = "&mu; = 1.0e-10", 
  "3.16e-10" = "&mu; = 3.16e-10",
  "1e-09" = "&mu; = 1.0e-9"
)

sites_n_full_df %>% 
  filter(pop_size == 10000) %>% 
  filter(mut_rate == 1e-10 | mut_rate == 3.16e-10 | mut_rate == 1e-9) %>% 
  ggplot(aes(x = segsites, fill = status)) +
  geom_bar(position = "stack") +
  facet_grid(
    rows = vars(processing),
    cols = vars(mut_rate),
    labeller = labeller(mut_rate = labels_mu)
  ) +
  scale_fill_discrete_qualitative(
    palette = "Cold",
    name = "Identity",
    labels = c("Duplicate", "Unique")
  ) +
  coord_cartesian(xlim = c(-0.2,10)) +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10),
    name = "Number of segregating sites"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05)), 
    name = "Count"
  ) +
  theme_bw(12) +
  theme(
    strip.text = element_markdown()
  ) -> fig_sites_sort_v_unsort_fixed_n

fig_sites_sort_v_unsort_fixed_n


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_sort_v_unsort_fixed_mu.png'),
  fig_sites_sort_v_unsort_fixed_mu, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_sort_v_unsort_fixed_n.png'),
  fig_sites_sort_v_unsort_fixed_n, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

