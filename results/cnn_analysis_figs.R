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
library(tidyr)
library(ggplot2)
library(cowplot)
library(ggtext)
library(colorspace)

# paths to data
path_to_results <- '/stor/home/mmj2238/genotype-alignment-information/results/'


#--------------- LOAD DATA SETS --------------------

# low duplicate set
load(
  file.path(
    path_to_results, 'models', 
    'torch_model_hist_low_dup_all_38_epoch_1e-4_lr.RData'
  )
)



#--------------- TIDY DATA --------------------

torch_38_hist %>% 
  pivot_longer(
    train_losses:valid_losses,
    names_to = "set",
    values_to = "loss"
  ) -> torch_38_hist


#--------------- CNN TRAINING FIGURES --------------------

torch_38_hist %>% 
  ggplot(aes(x = epochs, y = sqrt(loss), color = set)) +
  geom_point(size = 2) +
  geom_path(size = 1.75) +
  scale_color_viridis_d(
    begin = 0.25, 
    name = "Set",
    labels = c("Train", "Validation")
  ) +
  scale_x_continuous(name = "Epoch") +
  scale_y_continuous(name = "RMSE") +
  theme_half_open() +
  background_grid() -> fig_torch_38_hist



#--------------- CNN PERFORMANCE FIGURES --------------------

# figures for cnn performance on test set
r2_text_t38 <- tibble(label = "*R*<sup>2</sup> = 0.358", x= -2.75, y = 2)

torch_38_perform %>% 
  ggplot(aes(x = rho_actual, y = rho_train_prediction)) +
  geom_point(alpha = 0.1) +
  geom_abline(color = "blue") +
  geom_richtext(
    data = r2_text_t38,
    aes(x = x, y = y,label = label),
    label.color = NA,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(name = "Actual") +
  scale_y_continuous(name = "Estimate") +
  theme_half_open(12) +
  background_grid() +
  panel_border() +
  theme(
    strip.background = element_rect(fill = "gray90")
  ) -> fig_torch_38_perform

fig_torch_38_perform


# r2_text <- data.frame(
#   label = c("*R*<sup>2</sup> = 0.724", 
#             "*R*<sup>2</sup> = 0.729"), 
#   set = c("low_all", "low_unq"),
#   x = c(-2.5, -2.5),
#   y = c(2.5, 2.5)
# )
# 
# low_df %>% 
#   ggplot(aes(x = actual, y = estimate)) +
#   geom_point(alpha = 0.1) + 
#   geom_abline(color = "blue") +
#   geom_richtext(
#     data = r2_text, 
#     aes(x = x, y = y,label = label),
#     label.color = NA,
#     inherit.aes = FALSE
#   ) +
#   scale_x_continuous(name = "Actual") +
#   scale_y_continuous(name = "Estimate") +
#   facet_grid(
#     vars(set),
#     labeller = labeller(set = c("low_all" = "All", "low_unq" = "Unique"))
#   ) +
#   theme_half_open(12) +
#   background_grid() +
#   panel_border() +
#   theme(
#     strip.background = element_rect(fill = "gray90")
#   )



#--------------- SAVE FIGURES --------------------

save_plot(
  file.path(path_to_results, 'figures', 'torch_38_train.png'),
  fig_torch_38_hist, ncol = 1, nrow = 1, base_height = 3.71,
  base_asp = 1.618, base_width = NULL
)

