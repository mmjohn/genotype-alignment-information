#!/stor/system/opt/R/R-3.6.1/bin/Rscript

# Comparisons of CNN performance when duplicates are included or not included; using 
# simulation sets with a high and low amount of duplicates
# This script: creates figures for CNN performance comparison
# Mackenzie M. Johnson
# September 2021 


#--------------- CONFIGURE ENVIRONMENT --------------------

# load libraries
# library(purrr)
library(dplyr)
library(caret)
# library(tidyr)
library(ggplot2)
library(cowplot)
library(ggtext)
library(colorspace)


#--------------- GLOBAL PARAMETERS --------------------

# set the number of simulations in each parameter set

# num_sims <- 20000     

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/models/'

# size of alignments
num_chrom <- 50


#--------------- LOAD DATA SETS --------------------

# low duplicate set
load(file.path(path_to_results, 'keras_results_low_dup_all_60.RData'))
low_all_hist <- history
low_all_predict <- predictions
rm(history, predictions)

load(file.path(path_to_results, 'keras_results_low_dup_unq_60.RData'))
low_unq_hist <- history
low_unq_predict <- predictions
rm(history, predictions)

# high duplicate set


#--------------- TIDY DATA --------------------

caret::postResample(
  pred = low_all_predict, 
  obs = low_rho_all_test_centered
)

# RMSE  Rsquared       MAE 
# 0.8694387 0.7244904 0.6661903 

low_all_df <- tibble(
  num_obs = seq(1:length(low_all_predict)),
  estimate = low_all_predict,
  actual = low_rho_all_test_centered,
  set = "low_all"
)

caret::postResample(
  pred = low_unq_predict, 
  obs = low_rho_unq_test_centered
)

# RMSE  Rsquared       MAE 
# 0.8605685 0.7290385 0.6587781 

low_unq_df <- tibble(
  num_obs = seq(1:length(low_unq_predict)),
  estimate = low_unq_predict,
  actual = low_rho_unq_test_centered,
  set = "low_unq"
)

low_df <- full_join(low_all_df, low_unq_df)

rm(low_all_predict, low_unq_predict, low_rho_all_test_centered, 
   low_rho_unq_test_centered, low_all_df, low_unq_df)


#--------------- CNN PERFORMANCE FIGURES --------------------

# figures for 

r2_text <- data.frame(
  label = c("*R*<sup>2</sup> = 0.724", 
            "*R*<sup>2</sup> = 0.729"), 
  set = c("low_all", "low_unq"),
  x = c(-2.5, -2.5),
  y = c(2.5, 2.5)
)

low_df %>% 
  ggplot(aes(x = actual, y = estimate)) +
  geom_point(alpha = 0.1) + 
  geom_abline(color = "blue") +
  geom_richtext(
    data = r2_text, 
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(name = "Actual") +
  scale_y_continuous(name = "Estimate") +
  facet_grid(
    vars(set),
    labeller = labeller(set = c("low_all" = "All", "low_unq" = "Unique"))
  ) +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90")
  )



#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'l.png'),
  fig_d, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

