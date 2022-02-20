# Title     : Curves.R
# Objective : TODO
# Created by: greyhypotheses
# Created on: 17/02/2022

Curves <- function (estimates) {

  estimates %>%
    gather(key = 'Type', value = 'value', -time) %>%
    na.omit() %>%
    ggplot(mapping = aes(x = time, y = value)) +
    geom_line(alpha = 0.35, mapping = aes(colour = factor(Type))) +
    theme_minimal()

}