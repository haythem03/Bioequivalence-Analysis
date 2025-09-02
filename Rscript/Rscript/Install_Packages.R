# This script installs all R dependencies required for the Integrated Drug Development Platform.

# List of packages to install
packages_to_install <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "DT",
  "plotly",
  "rxode2",
  "dplyr",
  "ggplot2",
  "tidyr",
  "pracma",
  "fresh",
  "reticulate"
)

# Function to install packages
install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    message(paste("Installing", p, "..."))
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}

# Loop through the list and install each package
invisible(sapply(packages_to_install, install_if_missing))

message("All required R packages have been installed and loaded.")
