# Title     : PredictionsGraph.R
# Objective : TODO
# Created by: greyhypotheses
# Created on: 24/02/2022



#'
#' @param predictor: a time.predict() object ... ref. auxiliary_function.R
#'
PredictionsGraph <- function (predictor) {

  predictions <- data.frame(time = predictor$time.pred, ln_predicted = log(predictor$predictions))
  predictions[, c('ln_predicted_lci', 'ln_predicted_uci')] <- log(predictor$quantiles)


  ggplot(data = predictions, mapping = aes(x = time, y = ln_predicted)) +
    geom_line() +
    geom_ribbon(mapping = aes(ymin = ln_predicted_lci, ymax = ln_predicted_uci), alpha = 0.3, linetype = 0) +
    geom_point(alpha = 0.65, size = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.05),
          plot.caption = element_text(hjust = 0, size = 11, colour = 'darkgrey'),
          axis.title.x = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 11),
          axis.title.y = element_text(size = 13, face = 'bold'), axis.text.y = element_text(size = 11) ) +
    xlab(label = '\ntime (months)\n') +
    ylab(label = '\npredicted ln(cases)\n and 95% confidence interval\n')

}