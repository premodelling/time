# Title     : TimeDependentLag.R
# Objective : Time depedent lag calculation
# Created by: greyhypotheses
# Created on: 16/02/2022


TimeDependentLag <- function (frame, frame.date, frame.focus, frame.date.granularity, lags) {

  # foremost, a time series without missing time points
  times <- frame[[frame.date]]
  points <- seq(from = min(times), to = max(times), by = frame.date.granularity) %>% data.frame()
  names(points) <- frame.date

  
  # hence, build a reference data frame ...
  reference <- frame[, c(frame.date, frame.focus)]
  reference <- dplyr::left_join(x = points, y = reference, by = frame.date)


  # addressing missed time points ... a field expert should be consulted ...
  # meanwhile, it is assumed that when a measure is not recorded the default value is 0
  reference[is.na(reference[frame.focus]), frame.focus] <- 0


  # lags
  N <- nrow(reference)
  lagfunction <- function (lag) {
    series <- data.frame(  c(rep(NaN, lag), reference[1:(N - lag), frame.focus]) )
    names(series) <- paste0(tolower(frame.focus), '_lag_', lag)
    return(series)
  }
  vectors <- dplyr::bind_cols(lapply(X = lags, FUN = lagfunction))
  lagfields <- names(vectors)


  # append the lagged fields to the reference data frame
  reference <- cbind( reference[!(names(reference) %in% frame.focus)], vectors)


  # finally, focus on the original set of dates
  frame <- dplyr::left_join(x = frame, y = reference, by = frame.date)

  
  return(list(frame = frame, lagfields = lagfields))

}