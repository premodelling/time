# Title     : TimeDependentLag.R
# Objective : Time depedent lag calculation
# Created by: greyhypotheses
# Created on: 16/02/2022


TimeDependentLag <- function (frame, frame.date, frame.date.granularity, variables, lags) {

  # foremost, a time series without missing time points
  times <- frame[[frame.date]]
  points <- seq(from = min(times), to = max(times), by = frame.date.granularity) %>% data.frame()
  names(points) <- frame.date


  temporary <- function (frame.focus) {

    # hence, build a reference data frame ...
    reference <- dplyr::left_join(x = points, y = frame[, c(frame.date, frame.focus)], by = frame.date)


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


    return(vectors)

  }
  T <- dplyr::bind_cols( lapply(FUN = temporary, X = variables) )


  # append the lagged fields to the points data frame
  timeseries <- cbind( points, T)


  # finally, focus on the original set of dates
  frame <- dplyr::left_join(x = frame, y = timeseries, by = frame.date)


  return(list(frame = frame, laggedfields = names(x = T)))
}