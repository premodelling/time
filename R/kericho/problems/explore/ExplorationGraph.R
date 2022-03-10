# Title     : ExplorationGraph.R
# Objective : Relationships
# Created by: greyhypotheses
# Created on: 16/02/2022


#'
#' Explores the relationship between ln(cases) and {minimum temperature,
#' maximum temperature, rainfall}
#'
#' @param instances: a data frame that includes the ln(cases), minimum temperature,
#' maximum temperature, and rainfall fields
#'
ExplorationGraphNL <- function (instances) {

  instances %>%
    dplyr::select(maxT, minT, Rain, CasesLN) %>%
    na.omit() %>%
    gather(key = 'variable', value = 'value', -CasesLN) %>%
    ggplot(mapping = aes(x = value, y = CasesLN)) +
    geom_point(alpha = 0.35) +
    geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
    facet_wrap(~variable, scales='free', ncol = 2, nrow = 2, strip.position = 'bottom') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          plot.title = element_text(hjust = 0.5),
          strip.placement = 'outside',
          strip.text.x = element_text(size = 11, face = 'bold', margin = margin(t = 15, unit = "pt")),
          plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13)) +
    xlab(label = '') +
    ylab(label = '\nln(Cases)\n')

}



#'
#' Explores the relationship between cases and {minimum temperature,
#' maximum temperature, rainfall}
#'
#' @param instances: a data frame that includes the cases, minimum temperature,
#' maximum temperature, and rainfall fields
#'
ExplorationGraphBaseline <- function (instances) {

  instances %>%
    dplyr::select(maxT, minT, Rain, Cases) %>%
    na.omit() %>%
    gather(key = 'variable', value = 'value', -Cases) %>%
    ggplot(mapping = aes(x = value, y = Cases)) +
    geom_point(alpha = 0.35) +
    geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
    facet_wrap(~variable, scales='free', ncol = 2, nrow = 2, strip.position = 'bottom') +
    theme_minimal() +
    theme(panel.spacing = unit(x = 3, units = 'lines'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          plot.title = element_text(hjust = 0.5),
          strip.placement = 'outside',
          strip.text.x = element_text(size = 11, face = 'bold', margin = margin(t = 15, unit = "pt")),
          plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
          axis.text.x = element_text(size = 11), axis.text.y = element_text(size = 11),
          axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13)) +
    xlab(label = '')

}

