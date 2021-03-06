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
library(patchwork)

#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set
num_sims <- 20000     

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'

# size of alignments
num_chrom <- 50


#--------------- LOAD DATA SETS --------------------

# rho data - sorted alignments
load(file.path(path_to_results, 'dup_align', 'dup_analysis_rho_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_align', 'dup_analysis_rho_fixed_n_results.RData'))

# segregating sites data
# sorted, padded alignments
load(file.path(path_to_results, 'dup_align', 'dup_analysis_sites_fixed_mu_results.RData'))
load(file.path(path_to_results, 'dup_align', 'dup_analysis_sites_fixed_n_results.RData'))


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
    palette = "TealGrn", #"Light Grays",
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
    legend.text = element_text(vjust = 1),
    axis.text = element_text(color = "black")
  ) -> fig_rho_n

fig_rho_n


#--------------- FIXED MU SITE & RHO DISTRIBUTION FIGURES --------------------

# manually set labels
labels_n <- c(
  "100" = "N = 100", 
  "316" = "N = 316",
  "1000" = "N = 1,000"
)

# figure showing rho values of duplicates compared to whole data set
rho_mu_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  filter(pop_size == 100 | pop_size == 316 | pop_size == 1000) %>% 
  ggplot(
    aes(x = rho, fill = status)
  ) +
  geom_density(
    aes(y = after_stat(count)), 
    alpha = 0.7,
    position = "fill"
  ) +
  facet_grid(
    #rows = vars(processing),
    cols = vars(pop_size),
    scales = "free_x",
    labeller = labeller(pop_size = labels_n)
  ) +
  scale_x_continuous(
    expand = c(0 ,0),
    name = "&rho;"
  ) +
  scale_y_continuous(
    breaks = c(0.25, 0.5, 0.75, 1),
    expand = c(0, 0),
    name = "Density"
  ) +
  scale_fill_discrete_sequential(
    palette = "Burg",
    name = "Identity",
    labels = c("Duplicate", "Unique")
  ) +
  theme_linedraw(16) + #12
  theme(
    axis.title.x = element_markdown(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_markdown(color = "black"),
    legend.position = "top"
  ) -> fig_rho_dup_v_unq_fixed_mu

fig_rho_dup_v_unq_fixed_mu

# figure showing seg. sites of duplicates compared to whole data set
sites_mu_df %>% 
  filter(mut_rate == 1.5e-8) %>% 
  filter(pop_size == 100 | pop_size == 316 | pop_size == 1000) %>% 
  ggplot(aes(x = segsites, fill = status)) +
  geom_bar(position = "stack") +
  facet_grid(
    #rows = vars(processing),
    cols = vars(pop_size),
    labeller = labeller(pop_size = labels_n)
  ) +
  scale_fill_discrete_sequential(
    palette = "Burg" #"Purp"
  ) +
  # scale_fill_discrete_qualitative(
  #   palette = "Dark 2"
  # ) +
  coord_cartesian(xlim = c(-0.2,10)) +
  scale_x_continuous(
    breaks = c(0, 2, 4, 6, 8, 10),
    name = "Number of segregating sites"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, .05)),
    name = "Count"
  ) +
  theme_bw(16) + # 12
  theme(
    strip.text = element_markdown(),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) -> fig_sites_sort_v_unsort_fixed_mu

fig_sites_sort_v_unsort_fixed_mu

fig_rho_site <- fig_rho_dup_v_unq_fixed_mu / fig_sites_sort_v_unsort_fixed_mu &
  plot_annotation(tag_levels = "A")

fig_rho_site


#--------------- FIXED N SITE & RHO DISTRIBUTION FIGURES --------------------

# manually set labels
labels_mu <- c(
  "1e-10" = "&mu; = 1.0e-10",
  "3.16e-10" = "&mu; = 3.16e-10",
  "1e-09" = "&mu; = 1.0e-9"
)

# figure showing rho values of duplicates compared to whole data set
rho_n_df %>%
  filter(pop_size == 10000) %>%
  filter(mut_rate == 1.00e-10 | mut_rate == 3.16e-10 | mut_rate == 1.00e-09) %>%
  ggplot(
    aes(x = rho, fill = status)
  ) +
  geom_density(
    aes(y = after_stat(count)),
    alpha = 0.7,
    position = "fill"
  ) +
  facet_grid(
    cols = vars(mut_rate),
    scales = "free_x",
    labeller = labeller(mut_rate = labels_mu)
  ) +
  scale_x_continuous(
    expand = c(0 ,0),
    name = "&rho;"
  ) +
  scale_y_continuous(
    breaks = c(0.25, 0.5, 0.75, 1),
    expand = c(0, 0),
    name = "Density"
  ) +
  scale_fill_discrete_sequential(
    palette = "Burg",
    name = "Identity",
    labels = c("Duplicate", "Unique")
  ) +
  theme_linedraw(16) +
  theme(
    axis.title.x = element_markdown(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey92"),
    strip.text = element_markdown(color = "black")
  ) -> fig_rho_dup_v_unq_fixed_n

fig_rho_dup_v_unq_fixed_n

sites_n_df %>% 
  filter(pop_size == 10000) %>% 
  filter(mut_rate == 1e-10 | mut_rate == 3.16e-10 | mut_rate == 1e-9) %>% 
  ggplot(aes(x = segsites, fill = status)) +
  geom_bar(position = "stack") +
  facet_grid(
    cols = vars(mut_rate),
    labeller = labeller(mut_rate = labels_mu)
  ) +
  scale_fill_discrete_sequential(
    palette = "Burg",
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
  theme_bw(16) +
  theme(
    strip.text = element_markdown(),
    legend.position = "none",
    axis.text = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) -> fig_sites_sort_v_unsort_fixed_n

fig_sites_sort_v_unsort_fixed_n

fig_rho_site_n <- fig_rho_dup_v_unq_fixed_n / fig_sites_sort_v_unsort_fixed_n &
  plot_annotation(tag_levels = "A")

fig_rho_site_n


#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'fig_rho_by_param.png'),
  fig_rho_n, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_rho_site.png'),
  fig_rho_site, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)

save_plot(
  file.path(path_to_results, 'figures', 'fig_rho_site_n.png'),
  fig_rho_site_n, ncol = 1, nrow = 1, base_height = 5.71,
  base_asp = 1.618, base_width = NULL
)


