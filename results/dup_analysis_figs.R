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
library(colorspace)


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

dup_df %>% 
  mutate(processing = "Sorted") -> dup_df
dup_unsort_df %>% 
  mutate(processing = "Unsorted") -> dup_unsort_df

full_join(dup_df, dup_unsort_df) -> dup_full_df


#--------------- PERCENT DUPLICATE FIGURES --------------------

# figures for duplicates for sorted vs unsorted alignments in fixed n and mu parameter sets

# unsorted
dup_full_df %>% 
  filter(processing == "Unsorted" & set == "fixed_mu") %>% 
  ggplot(aes(x = pop_size, y = prop_dup, color = factor(mut_rate))) +
  geom_point(size = 2) +
  geom_path(size = 1.25) +
  scale_x_log10(
    labels = scales::math_format(format = log10),
    name = "*N*"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    name = "Percent duplicated"
  ) +
  scale_color_discrete_sequential(
    palette = "TealGrn",
    labels = scales::math_format(
      expr = 1.5%*%10^.x, 
      format = function(x) log10(as.numeric(x)/1.5)
    ),
    name = "&mu;"
  ) +
  theme_half_open() +
  background_grid(minor = 'none')  +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown(),
    legend.text = element_text(vjust = 1),
    legend.position = c(1,1),
    legend.justification = c(1,1)
  ) -> dup_unsort_mu

dup_full_df %>% 
  filter(processing == "Unsorted" & set == "fixed_n") %>% 
  ggplot(aes(x = mut_rate, y = prop_dup, color = factor(pop_size))) +
  geom_point(size = 2) +
  geom_path(size = 1.25) +
  scale_x_log10(
    labels = scales::math_format(
      format = log10
    ),
    name = "&mu;"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    name = "Percent duplicated"
  ) +
  scale_color_discrete_sequential(
    palette = "OrYel",
    labels = scales::math_format(
      format = function(x) log10(as.numeric(x))
    ),
    name = "*N*"
  ) +
  theme_half_open() +
  background_grid(minor = 'none')  +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.title = element_markdown(),
    legend.text = element_text(vjust = 1),
    legend.position = c(1,1),
    legend.justification = c(1,1)
  ) -> dup_unsort_n

# sorted

dup_full_df %>% 
  filter(processing == "Sorted" & set == "fixed_mu") %>% 
  ggplot(aes(x = pop_size, y = prop_dup, color = factor(mut_rate))) +
  geom_point(size = 2) +
  geom_path(size = 1.25) +
  scale_x_log10(
    labels = scales::math_format(format = log10),
    name = "*N*"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    name = "Percent duplicated"
  ) +
  scale_color_discrete_sequential(
    palette = "TealGrn",
    labels = scales::math_format(
      expr = 1.5%*%10^.x, 
      format = function(x) log10(as.numeric(x)/1.5)
    ),
    name = "&mu;"
  ) +
  theme_half_open() +
  background_grid(minor = 'none')  +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.position = "none"
  ) -> dup_sort_mu

dup_full_df %>% 
  filter(processing == "Sorted" & set == "fixed_n") %>% 
  ggplot(aes(x = mut_rate, y = prop_dup, color = factor(pop_size))) +
  geom_point(size = 2) +
  geom_path(size = 1.25) +
  scale_x_log10(
    labels = scales::math_format(
      format = log10
    ),
    name = "&mu;"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(),
    name = "Percent duplicated"
  ) +
  scale_color_discrete_sequential(
    palette = "OrYel",
    labels = scales::math_format(
      format = function(x) log10(as.numeric(x))
    ),
    name = "*N*"
  ) +
  theme_half_open() +
  background_grid(minor = 'none')  +
  theme(
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown(),
    legend.position = "none"
  ) -> dup_sort_n

plot_grid(
  dup_unsort_mu, dup_unsort_n, dup_sort_mu, dup_sort_n,
  ncol = 2,
  labels = c("A", "", "B", "")
) + theme(
    plot.background = element_rect(fill = "white", color = NA)
  ) -> fig_dup_param

fig_dup_param


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_dup_by_param_full.png'),
  fig_dup_param, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

