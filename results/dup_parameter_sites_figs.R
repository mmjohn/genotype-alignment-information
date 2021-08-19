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

# figure showing seg. sites of duplicates compared to whole data set
sites_mu_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  ggplot(aes(x = segsites, fill = status, color = status)) +
  geom_bar(position = "stack") +
  #geom_density_ridges(alpha = 0.5) +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Count"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  #theme_ridges() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_dup_v_unq_fixed_mu

fig_sites_dup_v_unq_fixed_mu

sites_mu_unsort_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  ggplot(aes(x = segsites, fill = status, color = status)) +
  geom_bar(position = "stack") +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Count"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_dup_v_unq_fixed_mu_unsort

fig_sites_dup_v_unq_fixed_mu_unsort


sites_mu_df %>% 
  mutate(processing = "sorted") -> mu_df1
sites_mu_unsort_df %>% 
  mutate(processing = "unsorted") -> mu_df2
full_join(mu_df1, mu_df2) -> sites_mu_full_df

sites_mu_full_df %>% 
  filter(mut_rate == 1.5e-9 & status == "duplicate") %>% 
  ggplot(aes(x = segsites, fill = processing)) +
  geom_bar(position = "dodge") +
  #geom_density_ridges(alpha = 0.5) +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Count"
  ) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_sort_v_unsort_fixed_mu

fig_sites_sort_v_unsort_fixed_mu


#--------------- FIXED N SITES DISTRIBUTION FIGURES --------------------

sites_n_df %>% 
  filter(pop_size == 10000) %>% 
  ggplot(aes(x = segsites, fill = status, color = status)) +
  #geom_histogram(alpha = 0.5, position = "dodge") +
  geom_bar(position = "stack") +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Count"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_dup_v_unq_fixed_n

fig_sites_dup_v_unq_fixed_n

sites_n_unsort_df %>% 
  filter(pop_size == 10000) %>% 
  ggplot(aes(x = segsites, fill = status, color = status)) +
  #geom_histogram(alpha = 0.5, position = "dodge") +
  geom_bar(position = "stack") +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Count"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_dup_v_unq_fixed_n_unsort

fig_sites_dup_v_unq_fixed_n_unsort


sites_n_df %>% 
  mutate(processing = "sorted") -> n1_df
sites_n_unsort_df %>% 
  mutate(processing = "unsorted") -> n2_df
full_join(n1_df, n2_df) -> sites_n_full_df

sites_n_full_df %>% 
  filter(pop_size == 1000 & status == "duplicate") %>% 
  ggplot(aes(x = segsites, fill = processing)) +
  geom_bar(position = "dodge") +
  #geom_density_ridges(alpha = 0.5) +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Count"
  ) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_sort_v_unsort_fixed_n

fig_sites_sort_v_unsort_fixed_n


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_dup_fixed_mu.png'),
  fig_sites_dup_v_unq_fixed_mu, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_dup_fixed_mu_unsort.png'),
  fig_sites_dup_v_unq_fixed_mu_unsort, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_dup_fixed_n.png'),
  fig_sites_dup_v_unq_fixed_n, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_dup_fixed_n_unsort.png'),
  fig_sites_dup_v_unq_fixed_n_unsort, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

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

