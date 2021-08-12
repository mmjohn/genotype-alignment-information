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

# alignment data
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

# rho data
load(file.path(path_to_results, 'dup_analysis_rho_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_analysis_rho_fixed_n_results.RData'))

# segregating sites data
load(file.path(path_to_results, 'dup_analysis_sites_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_analysis_sites_fixed_n_results.RData'))

#--------------- FIGURES --------------------

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

rho_n_df %>% 
  ggplot(aes(x = as.factor(mut_rate), y = rho, fill = factor(pop_size))) +
  geom_violin() +
  labs(
    x = "&mu;",
    y = "&rho;",
    fill = "*N*"
  ) +
  scale_y_log10() +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown()
  ) -> fig_rho_mu

fig_rho_mu

plot_grid(
  fig_rho_n, fig_rho_mu,
  nrow = 2,
  align = "v"
) -> fig_rho_n_mu

fig_rho_n_mu


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
  ) -> fig_rho_dup_v_unq_fixed_mu1

fig_rho_dup_v_unq_fixed_mu1

rho_mu_df %>% 
  filter(mut_rate == 1.5e-9) %>% 
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
  ) -> fig_rho_dup_v_unq_fixed_mu2

fig_rho_dup_v_unq_fixed_mu2

rho_mu_df %>% 
  filter(mut_rate == 1.5e-7) %>% 
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
  ) -> fig_rho_dup_v_unq_fixed_mu3

fig_rho_dup_v_unq_fixed_mu3

plot_grid(
  fig_rho_dup_v_unq_fixed_mu1, 
  fig_rho_dup_v_unq_fixed_mu2,
  fig_rho_dup_v_unq_fixed_mu3,
  ncol = 1
) -> fig_rho_dup_v_unq_fixed_mu

fig_rho_dup_v_unq_fixed_mu

rho_n_df %>% 
  filter(pop_size == 10000) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  geom_density(alpha = 0.5, position = "fill") +
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

rho_n_df %>% 
  filter(pop_size == 1000) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  geom_density(alpha = 0.5, position = "fill") +
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
  ) -> fig_rho_dup_v_unq_fixed_n2

fig_rho_dup_v_unq_fixed_n2

rho_n_df %>% 
  filter(pop_size == 100000) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  geom_density(alpha = 0.5, position = "fill") +
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
  ) -> fig_rho_dup_v_unq_fixed_n3

fig_rho_dup_v_unq_fixed_n3

plot_grid(
  fig_rho_dup_v_unq_fixed_n1,
  fig_rho_dup_v_unq_fixed_n2,
  fig_rho_dup_v_unq_fixed_n3,
  ncol = 1
) -> fig_rho_dup_v_unq_fixed_n

fig_rho_dup_v_unq_fixed_n


# figure showing seg. sites of duplicates compared to whole data set
sites_mu_df %>% 
  ggplot(aes(x = segsites, fill = status, color = status)) +
  #geom_histogram(alpha = 0.5, position = "dodge") +
  geom_density(alpha = 0.5) +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Density"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_dup_v_unq_fixed_mu

fig_sites_dup_v_unq_fixed_mu

sites_n_df %>% 
  ggplot(aes(x = segsites, fill = status, color = status)) +
  #geom_histogram(alpha = 0.5, position = "dodge") +
  geom_density(alpha = 0.5) +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "Number of segregating sites",
    y = "Density"
  ) +
  scale_color_manual(values = c("#1b9e77", "#7570b3")) +
  scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  theme_bw() +
  theme(
    legend.title = element_blank()
  ) -> fig_sites_dup_v_unq_fixed_n

fig_sites_dup_v_unq_fixed_n


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_dup_by_param.png'),
  fig_dup_n_mu, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_rho_by_param.png'),
  fig_rho_n_mu, ncol = 1, nrow = 1, base_height = 3.71,
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

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_dup_fixed_mu.png'),
  fig_sites_dup_v_unq_fixed_mu, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_sites_dup_fixed_n.png'),
  fig_sites_dup_v_unq_fixed_n, 
  ncol = 1, nrow = 1, base_height = 4.71,
  base_asp = 1.618, base_width = NULL
)
