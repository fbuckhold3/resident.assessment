# global.R - Using working pattern from your other app

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
# CONFIGURATION INITIALIZATION (using working pattern)
# ============================================================================

initialize_app_config <- function() {
  # Set up REDCap API URL
  url <- "https://redcapsurvey.slu.edu/api/"
  
  # Debug information about environment variables
  cat("Available environment variables (first 10):\n")
  env_vars <- names(Sys.getenv())
  print(head(env_vars, 10))
  cat("FAC_TOKEN exists:", "FAC_TOKEN" %in% names(Sys.getenv()), "\n")
  cat("RDM_TOKEN exists:", "RDM_TOKEN" %in% names(Sys.getenv()), "\n")
  
  # Identify whether we are in a hosted environment
  is_hosted <- Sys.getenv("FAC_TOKEN") != ""
  
  # Load tokens from environment variables or config file
  if (is_hosted) {
    fac_token <- Sys.getenv("FAC_TOKEN")
    rdm_token <- Sys.getenv("RDM_TOKEN")
    
    # Check if tokens are empty strings even though they exist
    if (nchar(fac_token) == 0 || nchar(rdm_token) == 0) {
      cat("WARNING: One or more required tokens are empty in environment!\n")
      cat("Using config file as fallback.\n")
      
      # Load from config file as fallback
      conf <- tryCatch({
        config::get(file = "config.yml")
      }, error = function(e) {
        message("Error loading config file: ", e$message)
        list(
          fac_token = "",
          rdm_token = ""
        )
      })
      
      # Use config values if environment variables are empty
      if (nchar(fac_token) == 0) fac_token <- conf$fac_token
      if (nchar(rdm_token) == 0) rdm_token <- conf$rdm_token
    }
    
    # Disable SSL verification in the hosted environment (NOT recommended for production)
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
    
  } else {
    # Use config file for local development
    conf <- tryCatch({
      config::get(file = "config.yml")
    }, error = function(e) {
      message("Error loading config file: ", e$message)
      list(
        fac_token = "",
        rdm_token = ""
      )
    })
    
    fac_token <- conf$fac_token
    rdm_token <- conf$rdm_token
  }
  
  # Print token values (length only for security)
  cat("FAC_TOKEN length:", nchar(fac_token), "\n")
  cat("RDM_TOKEN length:", nchar(rdm_token), "\n")
  
  # Return the environment with the tokens and URL
  list(
    url = url,
    fac_token = fac_token,
    rdm_token = rdm_token
  )
}

# Initialize configuration
cat("=== INITIALIZING APP CONFIGURATION ===\n")
app_config <- initialize_app_config()

# Extract configuration values
url <- app_config$url
fac_token <- app_config$fac_token
rdm_token <- app_config$rdm_token

cat("Configuration initialized successfully\n")
cat("URL:", url, "\n")

# ============================================================================
# SOURCE HELPER FILES (IN CORRECT ORDER)
# ============================================================================

cat("Loading helper files...\n")

# Source data functions first
source("R/data_functions.R")

# Source helper functions (non-evaluation)
source("R/helpers.R")

# Source evaluation-specific functions
source("R/evaluation_helpers.R")

# Source form builder functions
source("R/evaluation_form_builder.R")

cat("Helper files loaded successfully\n")

# ============================================================================
# DATA LOADING
# ============================================================================

cat("Loading data from REDCap...\n")

# Load data with error handling
tryCatch({
  faculty_data <- get_faculty_data()
  if (!is.null(faculty_data)) {
    cat("✅ Faculty data loaded successfully:", nrow(faculty_data), "records\n")
  } else {
    cat("⚠️ Faculty data is NULL\n")
  }
}, error = function(e) {
  cat("❌ Error loading faculty data:", e$message, "\n")
  faculty_data <<- NULL
})

tryCatch({
  resident_data <- get_resident_data()
  if (!is.null(resident_data)) {
    cat("✅ Resident data loaded successfully:", nrow(resident_data), "records\n")
    if ("Level" %in% names(resident_data)) {
      cat("Level distribution:\n")
      print(table(resident_data$Level, useNA = "always"))
    }
  } else {
    cat("⚠️ Resident data is NULL\n")
  }
}, error = function(e) {
  cat("❌ Error loading resident data:", e$message, "\n")
  resident_data <<- NULL
})

tryCatch({
  rdm_dict <- get_evaluation_dictionary()
  if (!is.null(rdm_dict)) {
    cat("✅ Evaluation dictionary loaded successfully:", nrow(rdm_dict), "fields\n")
  } else {
    cat("⚠️ Evaluation dictionary is NULL\n")
  }
}, error = function(e) {
  cat("❌ Error loading evaluation dictionary:", e$message, "\n")
  rdm_dict <<- NULL
})

cat("=== GLOBAL.R INITIALIZATION COMPLETE ===\n")
cat("Faculty data: ", if(!is.null(faculty_data)) "✅ LOADED" else "❌ FAILED", "\n")
cat("Resident data: ", if(!is.null(resident_data)) "✅ LOADED" else "❌ FAILED", "\n")
cat("Dictionary: ", if(!is.null(rdm_dict)) "✅ LOADED" else "❌ FAILED", "\n")