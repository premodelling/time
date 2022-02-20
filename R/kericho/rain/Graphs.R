# Title     : Rainfall.R
# Objective : Natural logarithm of cases vs. Lagged rainfall series
# Created by: greyhypotheses
# Created on: 16/02/2022


#' Draws the graphs of ln(cases) vs. Lagged rainfall series
#'
#' @param data: a Kericho data set that includes cases, and lagged rain fields
#' @param lagfields: the lagged rain fields
#'
Rainfall <- function (data, lagfields) {

  data %>%
    dplyr::select(c(dplyr::all_of(lagfields), CasesLN)) %>%
    gather(key = 'variable', value = 'value', -CasesLN) %>%
    na.omit() %>%
    ggplot(mapping = aes(x = value, y = CasesLN)) +
    geom_point(alpha = 0.35) +
    geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
    facet_wrap(~variable, ncol = 3, nrow = 2) +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          plot.title = element_text(hjust = 0.5),
          strip.text.x = element_text(size = 11, face = 'bold', margin = margin(t = 5, b = 5, unit = "pt")),
          plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13)) +
    xlab(label = '\nmm\n')


}