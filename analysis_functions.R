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
#' This function fits a power law to the data at each time step 
#' and returns a data frame with the estimated power law parameters for each date.
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

#' Analyze activity changes between time steps for each rank
#'
#' This function calculates the change in activity for each rank position between
#' consecutive time steps.
#'
#' @param ranked_data A data frame with columns channel, date, activity, and rank
#' @return A data frame with the mean change in activity for each rank position and
#'         the Shapiro-Wilk test results
#' @export
analyze_rank_changes <- function(ranked_data) {
  # Check if the required columns are present in the input data
  if (!all(c("channel", "date", "activity", "rank") %in% colnames(ranked_data))) {
    stop("The input data must have columns 'channel', 'date', 'activity', and 'rank'.")
  }
  
  # Compute the activity differences between consecutive time steps for each rank
  ranked_data <- ranked_data %>%
    dplyr::arrange(date, rank) %>%
    dplyr::group_by(rank) %>%
    dplyr::mutate(activity_change = dplyr::lead(activity) - activity) %>%
    dplyr::ungroup()
  
  # Calculate the mean activity change for each rank position
  mean_activity_changes <- ranked_data %>%
    dplyr::group_by(rank) %>%
    dplyr::summarize(mean_activity_change = mean(activity_change, na.rm = TRUE))
  
  # Compute the logarithm of the activity changes (ignoring NAs)
  log_activity_changes <- log(ranked_data$activity_change[!is.na(ranked_data$activity_change)])
  
  # Perform the Shapiro-Wilk test for lognormal distribution
  shapiro_test_result <- shapiro.test(log_activity_changes)
  
  # Combine the mean activity changes and Shapiro-Wilk test result into a single data frame
  result <- list(
    mean_activity_changes = mean_activity_changes,
    shapiro_test_result = shapiro_test_result
  )
  
  return(result)
}

#' Voitalov power-law fit
#'
#' This function fits a power-law distribution to the data using the Voitalov method.
#' Note: you need to install the reticulate package in R.  
#' You also need to install the 'powerlaw' package in your python environment ("pip install powerlaw")
#'
#' @param data A numeric vector of the data to fit the power-law distribution
#' @return A list with the estimated parameters and goodness of fit
#' @export
voitalov_power_law_fit <- function(data) {
  # Load the reticulate package
  library(reticulate)
  
  # Import the required Python modules
  powerlaw <- import("powerlaw")
  
  # Fit the power-law distribution to the data
  fit <- powerlaw$Fit(data, discrete = TRUE)
  
  # Extract the estimated parameters and goodness of fit
  alpha <- fit$alpha
  xmin <- fit$xmin
  ks_distance <- fit$D
  ks_p_value <- fit$p
  
  # Return the estimated parameters and goodness of fit
  result <- list(
    alpha = alpha,
    xmin = xmin,
    ks_distance = ks_distance,
    ks_p_value = ks_p_value
  )
  
  return(result)
}

#' Fit lognormal distribution
#'
#' This function fits a lognormal distribution to the given data.
#'
#' @param data A numeric vector of the data to fit the lognormal distribution
#' @return A list with the estimated parameters (meanlog and sdlog) and the fitting result
#' @export
fit_lognormal_distribution <- function(data) {
  # Load the MASS package
  library(MASS)
  
  # Fit the lognormal distribution to the data
  fit <- fitdistr(data, "lognormal")
  
  # Extract the estimated parameters (meanlog and sdlog)
  meanlog <- fit$estimate["meanlog"]
  sdlog <- fit$estimate["sdlog"]
  
  # Return the estimated parameters and the fitting result
  result <- list(
    meanlog = meanlog,
    sdlog = sdlog,
    fit = fit
  )
  
  return(result)
}
#' Fit generalized Pareto distribution
#'
#' This function fits a generalized Pareto distribution to the given data.
#'
#' @param data A numeric vector of the data to fit the generalized Pareto distribution
#' @param threshold A numeric value specifying the threshold for the generalized Pareto distribution
#' @return A list with the estimated parameters (location, scale, and shape) and the fitting result
#' @export
fit_generalized_pareto_distribution <- function(data, threshold) {
  # Load the evd package
  library(evd)
  
  # Fit the generalized Pareto distribution to the data
  fit <- gpd(data, threshold)
  
  # Extract the estimated parameters (location, scale, and shape)
  location <- fit$par.ests["location"]
  scale <- fit$par.ests["scale"]
  shape <- fit$par.ests["shape"]
  
  # Return the estimated parameters and the fitting result
  result <- list(
    location = location,
    scale = scale,
    shape = shape,
    fit = fit
  )
  
  return(result)
}
#' Extrapolate unobserved activity
#'
#' This function estimates the activity in unobserved channels by integrating the
#' fitted distribution beyond the observed data's tail.
#'
#' @param fit_result A list containing the fitted distribution parameters
#' @param observed_activity A vector of observed channel activity values
#' @return The estimated unobserved activity
extrapolate_unobserved_activity <- function(fit_result, observed_activity) {
  library(pracma)
  
  # Define a function to calculate the probability density based on the distribution type
  pdf <- function(x) {
    if (fit_result$distribution_type == "taildepfun") {
      return(dtaildepfun(x, fit_result$params$alpha, fit_result$params$beta))
    } else if (fit_result$distribution_type == "voitalov_power_law") {
      return(dpower_law(x, fit_result$params$alpha, fit_result$params$beta))
    } else if (fit_result$distribution_type == "lognormal") {
      return(dlnorm(x, fit_result$params$meanlog, fit_result$params$sdlog))
    } else if (fit_result$distribution_type == "generalized_pareto") {
      return(dgpd(x, fit_result$params$shape, fit_result$params$scale, fit_result$params$threshold))
    } else {
      stop("Invalid distribution type.")
    }
  }
  
  # Find the maximum observed activity
  max_activity <- max(observed_activity)
  
  # Integrate the probability density function beyond the observed data's tail
  unobserved_activity <- integral(function(x) x * pdf(x), max_activity, Inf)
  
  return(unobserved_activity)
}
#' Estimate total platform activity
#'
#' This function estimates the total amount of activity on the platform at a given time step
#' by fitting a specified distribution to the observed top channels and extrapolating the
#' unobserved activity.
#'
#' @param observed_data A data frame containing channel activity for the top channels
#' @param distribution_type A character string specifying the distribution type to fit and
#'        extrapolate (default: "taildepfun", alternatives: "voitalov_power_law", "lognormal", "generalized_pareto")
#' @return A list with the estimated total amount of activity, observed total in the list of top channels,
#'         and the estimated unobserved activity
#' @export
estimate_total_platform_activity <- function(observed_data, distribution_type = "taildepfun") {
  # Load required libraries
  library(reticulate)
  
  # Fit the specified distribution to the observed data
  if (distribution_type == "taildepfun") {
    fit_result <- fit_taildepfun(observed_data$activity)
  } else if (distribution_type == "voitalov_power_law") {
    fit_result <- voitalov_power_law_fit(observed_data$activity)
  } else if (distribution_type == "lognormal") {
    fit_result <- fit_lognormal_distribution(observed_data$activity)
  } else if (distribution_type == "generalized_pareto") {
    fit_result <- fit_generalized_pareto_distribution(observed_data$activity, threshold = 0)
  } else {
    stop("Invalid distribution type. Choose from 'taildepfun', 'voitalov_power_law', 'lognormal', or 'generalized_pareto'.")
  }
  
  # Calculate the observed total activity in the top channels
  observed_total <- sum(observed_data$activity)
  
  # Estimate the activity in unobserved channels
  unobserved_total <- extrapolate_unobserved_activity(fit_result, observed_data$activity)
  
  # Estimate the total amount of activity on the platform
  estimated_total <- observed_total + unobserved_total
  
  # Return the results
  result <- list(
    estimated_total_activity = estimated_total,
    observed_total_activity = observed_total,
    estimated_unobserved_activity = unobserved_total
  )
  
  return(result)
}


