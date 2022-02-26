# Title     : StudyData.R
# Objective : Study Data
# Created by: greyhypotheses
# Created on: 25/02/2022


StudyData <- function () {

  # the data
  instances <- read.csv(file = 'docs/programme/mauna/mauna_loa.csv')
  str(instances)


  # using the Year & Month fields to create a <date string> field, and a <date object>
  # the lubridate date fields simplify time based calculations
  instances <- instances %>%
    dplyr::mutate(datestr = paste(instances$year, sprintf( fmt = '%02d', instances$month ), sep = '-')) %>%
    dplyr::mutate(date = lubridate::ym(datestr)) %>%
    dplyr::arrange(date)


  # creating field <time> - unit of measure months - such that the time per instance is
  #     months(instance date - minimum date)
  #
  T <- interval(start = min(instances$date), end = instances$date) %/%
    lubridate::period(num = 1, units = 'months')
  instances$time <- T

  instances <- instances %>%
    dplyr::select(-c('index', 'year_month'))

  return(instances)

}