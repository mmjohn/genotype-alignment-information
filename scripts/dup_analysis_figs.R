#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of msprime simulation under different parameter conditions
# This script: creates figures for alignment comparisons and duplicate parameter ranges
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

# record session info
sessionInfo()

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50

# max_size - need to use one standard size for consistency across training and test sets
max_size <- 400 


#--------------- LOAD DATA SETS --------------------

# alignment data
load(glue('{path_to_results}dup_analysis_fixed_mu_align_results.RData'))
load(glue('{path_to_results}dup_analysis_fixed_n_align_results.RData'))

# join alignment data sets
dup_df <- full_join(fixed_mu_vary_n, fixed_n_vary_mu)

# rho data
load(glue('{path_to_results}dup_analysis_rho_results.RData'))
load(file.path(path_to_results, "dup_analysis_rho_results.RData"))

# segregating sites data
load(glue('{path_to_results}dup_analysis_sites_results.RData'))

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


# figure showing rho values of duplicates compared to whole data set
rho_df %>% 
  filter(set == "fixed_mu") %>%
  ggplot(aes(x = rho, fill = status, color = status)) +
  geom_density(alpha = 0.5, position = "fill") +
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

rho_df %>% 
  filter(set == "fixed_n") %>%
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
  ) -> fig_rho_dup_v_unq_fixed_n
 
fig_rho_dup_v_unq_fixed_n

# figure showing seg. sites of duplicates compared to whole data set
sites_df %>% 
  filter(set == "fixed_mu") %>%
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

sites_df %>% 
  filter(set == "fixed_n") %>%
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
  glue('{path_to_results}tmpfigs/fig_dup_by_param.png'), 
  fig_dup_n_mu, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  glue('{path_to_results}tmpfigs/fig_rho_by_param.png'), 
  fig_rho_n_mu, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  glue('{path_to_results}tmpfigs/fig_rho_dup_fixed_mu.png'), 
  fig_rho_dup_v_unq_fixed_mu, 
  ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  glue('{path_to_results}tmpfigs/fig_rho_dup_fixed_n.png'), 
  fig_rho_dup_v_unq_fixed_n, 
  ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  glue('{path_to_results}tmpfigs/fig_sites_dup_fixed_mu.png'), 
  fig_sites_dup_v_unq_fixed_mu, 
  ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  glue('{path_to_results}tmpfigs/fig_sites_dup_fixed_n.png'), 
  fig_sites_dup_v_unq_fixed_n, 
  ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)
