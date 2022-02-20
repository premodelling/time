# Title     : CurvesOfPredictions.R
# Objective : Curves Of Predictions
# Created by: greyhypotheses
# Created on: 17/02/2022

CurvesOfPredictions <- function (estimates) {

  estimates %>%
    gather(key = 'Variable', value = 'value', -time) %>%
    na.omit() %>%
    ggplot(mapping = aes(x = time, y = value)) +
    geom_line(alpha = 0.35, mapping = aes(colour = factor(Type))) +
    theme_minimal()

}