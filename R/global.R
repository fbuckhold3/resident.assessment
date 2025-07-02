# global.R - Clean version

# ============================================================================
# LIBRARIES
# ============================================================================
library(shiny)
library(dplyr)
library(config)
library(bslib)
library(httr)
library(jsonlite)

# ============================================================================
# ENVIRONMENT SETUP
# ============================================================================

# Identify whether we are hosted
is_hosted <- Sys.getenv("FAC_TOKEN") != ""

# Load tokens from environment or config
if (is_hosted) {
  fac_token <- Sys.getenv("FAC_TOKEN")
  rdm_token <- Sys.getenv("RDM_TOKEN")
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
} else {
  conf <- config::get(file = "config.yml")
  fac_token <- conf$fac_token
  rdm_token <- conf$rdm_token
}

# RedCap URL
url <- "https://redcapsurvey.slu.edu/api/"

# ============================================================================
# SOURCE HELPER FILES (IN CORRECT ORDER)
# ============================================================================

# Source data functions first
source("R/data_functions.R")

# Source helper functions (non-evaluation)
source("R/helpers.R")

# Source evaluation-specific functions
source("R/evaluation_helpers.R")

# Source form builder functions
source("R/evaluation_form_builder.R")

# ============================================================================
# DATA LOADING
# ============================================================================

faculty_data <- get_faculty_data()
resident_data <- get_resident_data()
rdm_dict <- get_evaluation_dictionary()

# Debug output
if (!is.null(faculty_data)) {
  cat("Faculty data loaded successfully\n")
}

if (!is.null(resident_data)) {
  cat("Final resident data loaded. Total available:", nrow(resident_data), "\n")
  if ("Level" %in% names(resident_data)) {
    cat("Level distribution:\n")
    print(table(resident_data$Level, useNA = "always"))
  }
}

if (!is.null(rdm_dict)) {
  cat("Evaluation dictionary loaded successfully\n")
}