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

# rho data - sorted alignments
load(file.path(path_to_results, 'dup_analysis_rho_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_analysis_rho_fixed_n_results.RData'))

# rho data - unsorted alignments
load(file.path(path_to_results, 'dup_analysis_rho_fixed_mu_unsort_results.RData'))
load(file.path(path_to_results, 'dup_analysis_rho_fixed_n_unsort_results.RData'))


#--------------- RHO DISTRIBUTION FIGURES --------------------

# figure for rho values of parameter sets
rho_mu_df %>% 
  ggplot(
    aes(x = factor(pop_size), y = rho, fill = factor(mut_rate))
  ) +
  geom_boxplot() +
  scale_x_discrete(
    #breaks = c(100, 1000, 10000),
    labels = scales::math_format(
      expr = 10^.x,
      format = function(x) round(log10(as.numeric(x)), digits = 3)
    ),
    name = "*N*"
  ) +
  scale_y_log10(
    labels = scales::math_format(
      format = log10
    ),
    name = "&rho;"
  ) +
  scale_fill_discrete_sequential(
    palette = "Light Grays",
    labels = scales::math_format(
      expr = 1.5%*%10^.x, 
      format = function(x) log10(as.numeric(x)/1.5)
    ),
    name = "&mu;"
  ) +
  theme_bw(12) +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown(),
    legend.text = element_text(vjust = 1)
  ) -> fig_rho_n

fig_rho_n



#--------------- DUPLICATE RHO DISTRIBUTION FIGURES --------------------

# figure showing rho values of duplicates compared to whole data set
rho_mu_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  filter(pop_size == 100 | pop_size == 316 | pop_size == 1000) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  geom_density(aes(y = after_stat(count)), alpha = 0.5, position = "fill") +
  #geom_area(alpha = 0.5, stat = "bin") +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "&rho;",
    y = "Count"
  ) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    legend.title = element_blank()
  ) -> fig_rho_dup_v_unq_fixed_mu_sort

fig_rho_dup_v_unq_fixed_mu_sort



rho_n_df %>% 
  filter(pop_size == 10000) %>% 
  filter(mut_rate == 1.0e-10 | mut_rate == 3.16e-10 | mut_rate == 1.0e-9) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  #geom_density(alpha = 0.5, position = "dodge") +
  geom_area(alpha = 0.5, stat = "bin", binwidth = 20) +
  #geom_histogram(alpha = 0.5) +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "&rho;",
    y = "Count"
  ) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    legend.title = element_blank()
  ) -> fig_rho_dup_v_unq_fixed_n_sort
 
fig_rho_dup_v_unq_fixed_n_sort


rho_mu_unsort_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  filter(pop_size == 100 | pop_size == 316 | pop_size == 1000) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  #geom_density(alpha = 0.5, position = "fill") +
  geom_area(alpha = 0.5, stat = "bin") +
  facet_wrap(vars(pop_size), scales = "free") +
  labs(
    x = "&rho;",
    y = "Count"
  ) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    legend.title = element_blank()
  ) -> fig_rho_dup_v_unq_fixed_mu_unsort

fig_rho_dup_v_unq_fixed_mu_unsort

rho_n_unsort_df %>% 
  filter(pop_size == 10000) %>% 
  filter(mut_rate == 1.0e-10 | mut_rate == 3.16e-10 | mut_rate == 1.0e-9) %>% 
  ggplot(aes(x = rho, fill = status, color = status)) +
  #geom_density(alpha = 0.5, position = "dodge") +
  geom_area(alpha = 0.5, stat = "bin", binwidth = 20) +
  facet_wrap(vars(mut_rate), scales = "free") +
  labs(
    x = "&rho;",
    y = "Count"
  ) +
  scale_color_manual(values = c("#7570b3", "#1b9e77")) +
  scale_fill_manual(values = c("#7570b3", "#1b9e77")) +
  theme_bw() +
  theme(
    axis.title.x = element_markdown(),
    legend.title = element_blank()
  ) -> fig_rho_dup_v_unq_fixed_n_unsort

fig_rho_dup_v_unq_fixed_n_unsort

plot_grid(
  fig_rho_dup_v_unq_fixed_mu_sort,
  fig_rho_dup_v_unq_fixed_mu_unsort,
  ncol = 1,
  align = "v"
) -> fig_rho_dup_v_unq_fixed_mu

fig_rho_dup_v_unq_fixed_mu

plot_grid(
  fig_rho_dup_v_unq_fixed_n_sort,
  fig_rho_dup_v_unq_fixed_n_unsort,
  ncol = 1,
  align = "v"
) -> fig_rho_dup_v_unq_fixed_n

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

