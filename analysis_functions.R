#' Rank channels by activity at each time step
#'
#' This function ranks the channels at every time step based on the volume of their activity.
#'
#' @param data A cleaned data frame with columns channel, date, and activity
#' @return A data frame with an additional column 'rank' representing the rank of each channel at every time step
#' @export
rank_channels_by_activity <- function(data) {
  # Check if the required columns are present in the input data
  if (!all(c("channel", "date", "activity") %in% colnames(data))) {
    stop("The input data must have columns 'channel', 'date', and 'activity'.")
  }
  
  # Rank the channels by activity at each time step
  ranked_data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(rank = rank(-activity, ties.method = "first")) %>%
    dplyr::ungroup()
  
  return(ranked_data)
}

#' Estimate power law parameters for each time step
#'
#' This function fits a power law to the data at each time step using the methods from
#' Voitalov et al. (2018) and returns a data frame with the estimated power law parameters for each date.
#'
#' @param ranked_data A data frame with columns channel, date, activity, and rank
#' @return A data frame with columns date, power_law_exponent, and power_law_intercept
#' @export
#' @references I. Voitalov, Tail Index Estimation for Degree Sequences of Complex Networks, https://github.com/ivanvoitalov/tail-estimation, 2018.
estimate_power_law_parameters <- function(ranked_data) {
  # Check if the required columns are present in the input data
  if (!all(c("channel", "date", "activity", "rank") %in% colnames(ranked_data))) {
    stop("The input data must have columns 'channel', 'date', 'activity', and 'rank'.")
  }
  
  # Load the tailDepFun package
  library(tailDepFun)
  
  # Group the data by date
  data_by_date <- split(ranked_data, ranked_data$date)
  
  # Initialize an empty data frame to store the power law parameters for each date
  power_law_params <- data.frame(
    date = as.Date(character()),
    power_law_exponent = numeric(),
    power_law_intercept = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through the data for each date and estimate the power law parameters
  for (date_data in data_by_date) {
    # Extract the date
    date <- unique(date_data$date)
    
    # Fit the power law using the tailDepFun package
    power_law_fit <- tailDepFun::fit_power_law(date_data$activity)
    
    # Extract the power law exponent and intercept
    exponent <- power_law_fit$alpha
    intercept <- power_law_fit$A
    
    # Append the power law parameters for the current date to the data frame
    power_law_params <- rbind(power_law_params, data.frame(date = date, power_law_exponent = exponent, power_law_intercept = intercept))
  }
  
  return(power_law_params)
}
