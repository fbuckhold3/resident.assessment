# global.R - Faculty Evaluation App (Simple Start)

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

# 1) Identify whether we are hosted
is_hosted <- Sys.getenv("FAC_TOKEN") != ""

# 2) Load tokens from environment or config
if (is_hosted) {
  fac_token <- Sys.getenv("FAC_TOKEN")    # Faculty database token
  rdm_token <- Sys.getenv("RDM_TOKEN")    # Resident database token (for later)
  
  # Disable SSL verification in the hosted environment
  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  
} else {
  conf <- config::get(file = "config.yml")
  fac_token <- conf$fac_token
  rdm_token <- conf$rdm_token
}

# The RedCap URL
url <- "https://redcapsurvey.slu.edu/api/"

# Source helper functions
source("R/helpers.R")

# ============================================================================
# FUNCTIONS
# ============================================================================

# Get faculty data from faculty database
get_faculty_data <- function() {
  tryCatch({
    cat("Pulling faculty database...\n")
    
    formData <- list(
      "token" = fac_token,
      content = 'record',
      action = 'export',
      format = 'json',
      type = 'flat',
      csvDelimiter = '',
      rawOrLabel = 'label',
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'false',
      exportDataAccessGroups = 'false',
      returnFormat = 'json'
    )
    
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (httr::status_code(response) != 200) {
      stop("Faculty REDCap API call failed with status: ", httr::status_code(response))
    }
    
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    faculty_data <- jsonlite::fromJSON(response_text)
    
    cat("Faculty data loaded. Total rows:", nrow(faculty_data), "\n")
    cat("Faculty data columns:", paste(names(faculty_data), collapse = ", "), "\n")
    
    return(faculty_data)
    
  }, error = function(e) {
    cat("Error in Faculty API pull:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# DATA LOADING
# ============================================================================

source('R/helpers.R')
# Load faculty data on startup
faculty_data <- get_faculty_data()

# Debug output
if (!is.null(faculty_data)) {
  cat("Sample faculty data (first 3 rows):\n")
  if (nrow(faculty_data) > 0) {
    print(head(faculty_data, 3))
  }
}