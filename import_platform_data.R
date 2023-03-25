#' Import platform data
#'
#' This function imports platform data from a CSV file, specifying the names of
#' the channel and activity variables if they have non-standard names.
#'
#' @param file_path The path to the CSV file containing the platform data.
#' @param channel_var The name of the channel variable (default is "channel").
#' @param activity_var The name of the activity variable (default is "activity").
#' @param date_var The name of the date variable (default is "date").
#' @return A data frame containing the imported platform data.
#' @examples
#' # Use default variable names
#' data <- import_platform_data("data.csv")
#'
#' # Specify custom variable names
#' data <- import_platform_data("data.csv", channel_var = "page", activity_var = "comments")
#' @export
import_platform_data <- function(file_path,
                                 channel_var = "channel",
                                 activity_var = "activity",
                                 date_var = "date") {
  data <- read.csv(file_path)
  
  # Check if the specified variable names exist in the data frame
  if (!channel_var %in% colnames(data) ||
      !activity_var %in% colnames(data) ||
      !date_var %in% colnames(data)) {
    stop("One or more of the specified variable names do not exist in the data frame.")
  }
  
  # Convert the date variable to a Date object
  data[[date_var]] <- as.Date(data[[date_var]])
  
  # Rename columns to standard names
  colnames(data)[colnames(data) == channel_var] <- "channel"
  colnames(data)[colnames(data) == activity_var] <- "activity"
  colnames(data)[colnames(data) == date_var] <- "date"
  
  return(data)
}
