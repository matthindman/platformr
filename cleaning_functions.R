#' Clean platform data
#'
#' This function cleans the raw platform data, handling missing values, standardizing
#' the date format, and ensuring the activity variable is numeric.
#'
#' @param data A data frame containing the raw platform data
#' @param date_format A string specifying the format of the date variable (default: "%Y-%m-%d")
#' @return A cleaned data frame
#'
#' @examples
#' # Example data frame
#' raw_data <- data.frame(
#'   channel = c("A", "B", "C"),
#'   date = c("2021-01-01", "2021-01-02", "2021-01-03"),
#'   activity = c(10, 20, 30)
#' )
#' cleaned_data <- clean_platform_data(raw_data)
clean_platform_data <- function(data, date_format = "%Y-%m-%d") {
  # Remove rows with missing values
  data <- data[complete.cases(data), ]
  
  # Convert date column to a standardized format
  data$date <- as.Date(data$date, format = date_format)
  
  # Ensure the activity column is numeric
  data$activity <- as.numeric(data$activity)
  
  return(data)
}
