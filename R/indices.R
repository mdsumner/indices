# Index Calculation Functions ---------------------------------------------

#' Calculate growing degree days
#'
#' Calculates the daily GDD from daily maximum (T_max) and daily minimum (T_min) temperatures,
#' with respect to a base temperature T_base.
#'
#' The formula for GDD is the averages of max and min temperatures minus the base temperature.
#' Results with negative values are set to zero (rather than having a 'negative' GDD).
#'
#' The T_upper parameter allows an upper limit to be specified. If T_max is greater than T_upper, then the GDD for the day will be zero (i.e. no growth happens on that day)
#'
#' @param T_max Daily Maximum Temperature
#' @param T_min Daily Minimum Temperature
#' @param T_base Base Temperature
#' @param T_upper If T_max is above the T_upper limit, ignore the entire day (i.e. GDD equals zero, no growth happens that day)
#'
#' @return The GDD for the given data per the formula: \code{GDD = (T_max - T_min) / 2 - T_base}
#' @export
#'
#' @examples
#' calc_GDD(T_max = 24.12, T_min = 15.94, T_base = 10)
calc_GDD <- function(T_max, T_min, T_base, T_upper = NULL) {

  GDD <- ((T_max + T_min) / 2) - T_base

  # GDD can't be negative
  GDD[GDD < 0] <- 0

  # if T_max is above the T_upper limit, ignore the entire day
  if (!is.null(T_upper)) {
    GDD[T_max > T_upper] <- 0
  }

  GDD
}


#' Calculate Weighted Growing Degree Days
#'
#' Calculates the cumulative weighted GDD where:
#' \itemize{
#'   \item{\code{GDD_base_budb} is used when cumulative GDD (x) meets: \code{0 < x < params[["GDD_budb_threshold"]]}}
#'   \item{\code{GDD_base_leaf} is used when cumulative GDD (x) meets: \code{params[["GDD_budb_threshold"]] < x < params[["GDD_leaf_threshold"]]}}
#'   \item{\code{GDD_base_10} is used when cumulative GDD (x) meets: \code{x > params[["GDD_leaf_threshold"]]}}
#' }
#'
#' @inheritParams calc_GDD
#' @param T_budb Budbreak temperature
#' @param T_leaf Leaf appearance temperature
#' @param T_vera Veraison temperature
#' @param GDD_budb_threshold GDD threshold
#' @param GDD_leaf_threshold GDD threshold
#' @param GDD_harvest_threshold GDD threshold
#'
#' @return Vector of weighted GDD values
#' @export
calc_GDD_weighted <- function(T_max, T_min,
                              T_budb = 4, T_leaf = 7, T_vera = 10,
                              GDD_budb_threshold = 350, GDD_leaf_threshold = 1000, GDD_harvest_threshold = 1300,
                              T_upper = 1000) {

  # Vector of GDD values for each stage as constant values
  GDD_base_budb <- calc_GDD(T_max, T_min, T_base = T_budb, T_upper = T_upper)
  GDD_base_leaf <- calc_GDD(T_max, T_min, T_base = T_leaf, T_upper = T_upper)
  GDD_base_vera <- calc_GDD(T_max, T_min, T_base = T_vera, T_upper = T_upper)

  # calculated each of the three periods p1, p2, p3 separately
  # and create output vector by combining those

  # period 1: from pre bud-break until cumulative GDD threshold
  # determine index at which threshold is crossed
  # result will use from beginning to this index
  # if threshold is not crossed set p1 to entire vector length

  p1 <- cumsum(GDD_base_budb)
  p1_end_ind <- which(p1 > GDD_budb_threshold)[1]
  if (is.na(p1_end_ind)) p1_end_ind <- length(GDD_base_budb)
  p1_end <- p1[p1_end_ind]

  # if threshold was not crossed, then skip next periods
  if (p1_end_ind == length(GDD_base_budb)) {
    # TODO: this case not tested yet
    p2 <- c()
    p3 <- c()
    p2_end_ind <- 0  # so that p2[1:p2_end_ind] later is NULL
  } else {
    # period 2:
    # calculate cumsum from the next element after the end of the first period
    # use the p1_end value as the fixed value to add to the cumsum (which starts at zero)
    p2 <- p1_end + cumsum(GDD_base_leaf[(p1_end_ind + 1):length(GDD_base_leaf)])
    p2_end_ind <- which(p2 > GDD_leaf_threshold)[1]
    if (is.na(p2_end_ind)) p2_end_ind <- length(GDD_base_leaf)
    p2_end <- p2[p2_end_ind]

    # if threshold not crossed, then skip next period
    if (p2_end_ind == length(GDD_base_leaf)) {
      p3 <- c()
    } else {
      p3 <- p2_end + cumsum(GDD_base_vera[(p1_end_ind + p2_end_ind + 1):length(GDD_base_vera)])
    }
  }

  # create vector using the calculated bounds for the partical vectors
  GDD_weighted <- c(
    p1[1:p1_end_ind],
    p2[1:p2_end_ind],
    p3)

  # because we are creating the GDD_weighted vector by manually joining vectors
  # we have to ensure it is the same length as corresponding time series
  # it may be created too long
  if(length(GDD_weighted) > length(GDD_base_vera)) {
    GDD_weighted <- GDD_weighted[1:length(GDD_base_vera)]
  }

  return(GDD_weighted)
}



#' Calculate significant event Excess Heat Index
#'
#' \code{EHI_sig = (Avg DMT over spread of days) - DMT_threshold} as per Nairn 2013.
#'
#' @param temps Vector of daily mean temperatures to process
#' @param DMT_threshold Threshold of daily temperature (in degC or K, i.e. matching input) for the desired climate reference period (calculated using all days of the year). Typically the 95th percentile.
#' @param sig_spread Length of period to average for significance value
#'
#' @return Vector with EHI_sig for given temperatures, padded with NA values at the start to maintain the same length as the input vector
#' @export
#'
#' @examples
#' t <- c(20.48, 17.81, 19.06, 16.40, 19.34, 20.79, 19.82, 16.28, 18.25, 19.56)
#' DMT_95_pctile <- 19.2
#' calc_EHI_sig(t, DMT_95_pctile)
calc_EHI_sig <- function(temps, DMT_threshold, sig_spread = 3) {
  zoo::rollapply(temps, width = sig_spread, FUN = function(x) { sum(x)/sig_spread }, align = 'right', fill = NA) - DMT_threshold
}



#' Calculate acclimatisation Excess Heat Index
#'
#' @param accl_length Length of acclimatisation period
#' @inheritParams calc_EHI_sig
#'
#' @return Vector with EHI_accl for given temperatures, padded with NA values at the start to maintain the same length as the input vector
#' @export
#'
#' @examples
#' t <- rnorm(n = 80, mean = 20)
#' calc_EHI_accl(t, 30, 3)
calc_EHI_accl <- function(temps, accl_length = 30, sig_spread = 3) {
  # figures to average for acclimatisation period
  width_ <- list( -(accl_length+sig_spread-1) : -sig_spread )

  # (first 3 days)
  # days: i, i-1, i-2
  part1 <- zoo::rollapply(temps, width = sig_spread, FUN = function(x) { sum(x)/sig_spread }, align = 'right', fill = NA)

  # (next 30 days after that)
  #: i-3, i-4, ..., i-32
  part2 <- zoo::rollapply(temps, width = width_, FUN = function(x) { sum(x)/accl_length }, align = 'right', fill = NA)

  part1 - part2
}



#' Calculate Excess Heat Factor from indicators for Excess Heat and Heat Stress
#'
#' @param EHI_sig Significant event EHI from \code{calc_EHI_sig}
#' @param EHI_accl Acclimatisation EHI from \code{calc_EHI_accl}
#'
#' @return Vector with EHF values for given temperatures, padded with NA values at the start to maintain the same length as the input vector
#' @export
#'
#' @examples
#' t <- rnorm(n = 80, mean = 20)
#' DMT_95_pctile <- 19.2
#' EHI_sig <- calc_EHI_sig(t, DMT_95_pctile)
#' EHI_accl <- calc_EHI_accl(t, 30, 3)
#' calc_EHF(EHI_sig, EHI_accl)
calc_EHF <- function(EHI_sig, EHI_accl) {
  # EHF = EHI_sign * max(1, EHI_accl)
  # note: can't use "max" function as it aggregates over the group
  EHI_sig * ifelse(EHI_accl < 1, 1, EHI_accl)
}


## from cfdtk
Austral_season_month <- function (x)
{
  as.integer(ifelse(lubridate::month(x) <= 6, lubridate::month(x) +
                      6, lubridate::month(x) - 6))
}

#' Calculate accumulated heat
#'
#' For vector of GDD values and a data frame containing associated date information,
#' calculate the accumulated heat (cumulative sum of GDD values) for the
#'
#' @param dates Vector of dates
#' @param GDD_values Vector of GDD values corresponding to each date
#' @param from_Austral_month Number of the month in the Austral season (1 = Jul ... 12 = Jun) to start calculating from
#' @param to_Austral_month Number of the month in the Austral season (1 = Jul ... 12 = Jun) to stop calculating from, defaults to 12
#'
#' @return Vector of calculated results, padded with NA values for dates that are not in the range,
#' so that length of vector is the same as the length of the vector of GDD values
#' @export
calc_accumulated_heat <- function(dates, GDD_values, from_Austral_month, to_Austral_month = 12) {
  # which days are within the from/to range?
  x_inds <- which(Austral_season_month(dates) >= from_Austral_month & Austral_season_month(dates) <= to_Austral_month)

  if(length(x_inds) == 0) {
    stop("No dates in date range")
  }

  # insert NA for days prior the start of 'from month'
  # cumsum of days of interest
  # insert NA for days after the end of 'to month'

  na_prior_start <- x_inds[1]-1
  na_after_end <- length(GDD_values) - x_inds[length(x_inds)]

  c(
    rep(NA, na_prior_start),
    cumsum(GDD_values[x_inds]),
    rep(NA, na_after_end)
  )
}


#' Calculate Downy Mildew Pressure Index
#'
#' Calculates the Downy Mildew Pressure index for a set of daily
#' rainfall and temperature min/max data. This is true if:
#' there is a daily rainfall of > 10mm and the
#' temperature is > 10 degrees C for more than 10 hours.
#'
#' Approximates temperature curve to be from \code{T_min} at start of day,
#' rising linearly to linear rising to \code{T_max} at middle of day,
#' and then falling linearly to \code{T_min} againt at the end of the day.
#'
#' The threshold value defaults to 10, but can be changed if necessary.
#'
#' @param rain Daily rainfall
#' @param T_max Maximum daily temperature
#' @param T_min Minimum daily temperature
#' @param T_threshold Tempature above which must be sustained during the day for 10 hours
#'
#' @return A logical vector specifying if the index was calculated to be TRUE of FALSE for the given data points
#' @export
#'
#' @examples
#' rain <- rnorm(n = 30, mean = 9, sd = 4)
#' tmax <- rnorm(n = 30, mean = 12, sd = 4)
#' tmin <- rnorm(n = 30, mean = 8, sd = 4)
#' downy_mildew_pressure_index(rain, tmax, tmin)
downy_mildew_pressure_index <- function(rain, T_max, T_min, T_threshold = 10) {
  # "high-school trigonometry!"
  hours_gt_threshold <- (24 * (T_max - T_threshold)) / (T_max - T_min)

  ifelse(rain >= 10 & hours_gt_threshold >= 10, TRUE, FALSE)
}



# Index Analysis Functions ------------------------------------------------

#' Detect Heatwaves from EHF Method Calculations
#'
#' @param EHF_values Vector EHF values as output from the \code{calc_EHF} function
#' @param min_duration Minimum number of days in a row with EHF > 0 that gets classified as a heatwave
#'
#' @return A data frame with two columns, detect and duration.
#' The detect column is a logical vector, specifying if the observation is a heatwave or not.
#' The duration column is an integer vector, specifying the duration of a heatwave in the first corresponding TRUE value in the detect column.
#' @export
heatwave_detect_EHF_with_lengths <- function(EHF_values, min_duration) {

  if (min_duration < 1) {
    stop("Parameter min_duration must be greater than 0")
  }

  # set NA values to false and values > 0 to TRUE
  v <- ifelse(is.na(EHF_values), FALSE, EHF_values > 0)

  # reduce vector to groups and init working vectors
  EHF_rle <- rle(v)
  EHF_rle$detect <- rep(FALSE, length(EHF_rle$values))
  EHF_rle$duration <- rep(0, length(EHF_rle$values))
  EHF_rle$cum_length <- cumsum(EHF_rle$lengths)

  # filter out values and lengths less than the min_duration
  for (i in 1:length(EHF_rle$values)) {
    # set detect flag for EHF > 0 for consecutive values 3 or more values
    if (EHF_rle$values[i] == TRUE && EHF_rle$lengths[i] >= min_duration) {
      EHF_rle$detect[i] = TRUE
      EHF_rle$duration[i] = EHF_rle$lengths[i]
    }

    # set cumulative_index
    if (i == 1) {
      EHF_rle$cum_ind[i] <- 1
    } else {
      EHF_rle$cum_ind[i] <- EHF_rle$cum_length[i-1] + 1
    }
  }

  # create detect vector by overwriting rle values with true detected instances and applying inverse
  EHF_rle$values <- EHF_rle$detect
  detect_vector <- inverse.rle(EHF_rle)

  # create duration vector by using the cumulative index of the start of detected periods
  # and writing the duration of the period in the first corresponding element
  # e.g. F F F T T T T T F F
  #      0 0 0 5 0 0 0 0 0 0
  duration_vector <- rep(0, length(detect_vector))

  for (i in 1:length(EHF_rle$cum_ind)) {
    if (EHF_rle$detect[i]) {
      start <- EHF_rle$cum_ind[i]
      length <- EHF_rle$duration[i]

      duration_vector[start] <- length
    }

  }

  data.frame(detect = detect_vector,
             duration = duration_vector)
}


#' @rdname heatwave_detect_EHF_with_lengths
heatwave_detect_EHF <- function(EHF_values, min_duration) {
  heatwave_detect_EHF_with_lengths(EHF_values, min_duration)$detect
}


#' Calculate EHF severity
#'
#' @param EHF Vector of EHF values
#' @param baseline_EHF_85th Baseline EHF value (85th percentile)
#' @param is_heatwave Vector of logical values defining if corresponding EHF value is a heatwave
#'
#' @export
calc_heatwave_EHF_severity <- function(EHF, baseline_EHF_85th, is_heatwave) {
  severity_levels = list(HIGH = 'High', SEVERE = 'Severe', EXTREME = 'Extreme')
  # calculate EHF severity
  EHF_severe = EHF >= baseline_EHF_85th
  EHF_extreme = EHF >= 3 * baseline_EHF_85th
  EHF_severity = dplyr::case_when(
    is_heatwave & !EHF_severe & !EHF_extreme ~ severity_levels$HIGH,
    is_heatwave &  EHF_severe & !EHF_extreme ~ severity_levels$SEVERE,
    is_heatwave &  EHF_severe &  EHF_extreme ~ severity_levels$EXTREME
  ) #close case_when
  EHF_severity <- factor(EHF_severity, levels = c(severity_levels$HIGH, severity_levels$SEVERE, severity_levels$EXTREME), ordered = TRUE)

  EHF_severity
}
