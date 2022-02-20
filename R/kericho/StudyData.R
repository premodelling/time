# Title     : StudyData.R
# Objective : Data
# Created by: greyhypotheses
# Created on: 16/02/2022

StudyData <- function () {

  # data
  instances <- read.csv(file = 'docs/day/01/Kericho.csv')
  instances$Month <- factor(x = instances$Month,
                            levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                       'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)


  # a field for the natural logarithm of Cases
  instances$CasesLN <- log(instances$Cases)


  # using the Year & Month fields to create a <date string> field, and a <date object>
  # the lubridate date fields simplify time based calculations
  instances <- instances %>%
    dplyr::mutate(datestr = paste(instances$Year, sprintf( fmt = '%02d', instances$Month ), sep = '-')) %>%
    dplyr::mutate(date = lubridate::ym(datestr)) %>%
    dplyr::arrange(date)


  # creating field <time> - unit of measure months - such that the time per instance is
  #     months(instance date - minimum date)
  #
  T <- interval(start = min(instances$date), end = instances$date) %/%
    lubridate::period(num = 1, units = 'months')
  instances$time <- T


  # preview
  str(instances)


  # the # of missing values per field  ... remember MAR,MCAR, MNAR
  print('The number of missing fields per field:', quote = FALSE)
  print(colSums(is.na(instances)))


  # observations that have missing values  ... remember MAR,MCAR, MNAR
  if (sum(complete.cases(instances)) != nrow(instances)) {
    condition <- complete.cases(instances)
    print('Instances that have missing values:', quote = FALSE)
    instances[!condition, ] %>%
      tibble() %>%
      print()
  }

  return(instances)

}