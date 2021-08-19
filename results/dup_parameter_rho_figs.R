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

# rho data - sorted alignments
load(file.path(path_to_results, 'dup_analysis_rho_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_analysis_rho_fixed_n_results.RData'))

# rho data - unsorted alignments
load(file.path(path_to_results, 'dup_analysis_rho_fixed_mu_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_rho_fixed_n_unsort_results.RData'))


#--------------- RHO DISTRIBUTION FIGURES --------------------

# figure for rho values of parameter sets
rho_mu_df %>% 
  ggplot(aes(x = as.factor(pop_size), y = rho, fill = factor(mut_rate))) +
  geom_violin() +
  labs(
    x = "*N*",
    y = "&rho;",
    fill = "&mu;"
  ) +
  scale_y_log10() +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown()
  ) -> fig_rho_n

fig_rho_n



#--------------- DUPLICATE RHO DISTRIBUTION FIGURES --------------------

# figure showing rho values of duplicates compared to whole data set
rho_mu_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  geom_density(alpha = 0.5, position = "fill") +
  #geom_histogram() +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "&rho;",
    y = "Density"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    legend.title = element_blank()
  ) -> fig_rho_dup_v_unq_fixed_mu

fig_rho_dup_v_unq_fixed_mu



rho_n_df %>% 
  filter(pop_size == 10000) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  #geom_density(alpha = 0.5, position = "dodge") +
  geom_bar(position = "stack") +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "&rho;",
    y = "Density"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    legend.title = element_blank()
  ) -> fig_rho_dup_v_unq_fixed_n1
 
fig_rho_dup_v_unq_fixed_n1



fig_rho_dup_v_unq_fixed_n


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_rho_by_param.png'),
  fig_rho_n, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

save_plot( 
  file.path(path_to_results, 'figures', 'fig_rho_dup_fixed_mu.png'),
  fig_rho_dup_v_unq_fixed_mu, 
  ncol = 1, nrow = 1, base_height = 6.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_rho_dup_fixed_n.png'),
  fig_rho_dup_v_unq_fixed_n, 
  ncol = 1, nrow = 1, base_height = 6.71,
  base_asp = 1.618, base_width = NULL
)

