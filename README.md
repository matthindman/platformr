# platformr

`platformr` is an R package designed for analyzing digital platform data, with the goal of maximizing efficient data collection and providing valid estimates of coverage through a focus on the most popular channels, pages, groups, subreddits, or other places where users cluster their activity. The package provides functions for data import, cleaning, preprocessing, analysis, and visualization.

## Installation

You can install the development version of `platformr` from GitHub with:

```R
# Install devtools if you don't have it
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install platformr from GitHub
devtools::install_github("matthindman/platformr")

```

# Usage

Import your data with import_platform_data().

Clean and preprocess the data with clean_platform_data().

Rank channels by activity levels with rank_channels().

Analyze the changes in activity for each rank position with analyze_rank_changes().

Fit a power law, lognormal, or generalized Pareto distribution to the data with fit_power_law(), fit_lognormal(), or fit_generalized_pareto().

Estimate the total activity on the platform at a given time step with estimate_total_activity().

Extrapolate unobserved activity with extrapolate_unobserved_activity().

For more detailed information on the functions and their usage, please refer to the package documentation.

# License

This project is licensed under the MIT License. See the LICENSE file for details.





