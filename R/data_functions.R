# ============================================================================
# DATA FUNCTIONS
# ============================================================================

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
    return(faculty_data)
    
  }, error = function(e) {
    cat("Error in Faculty API pull:", e$message, "\n")
    return(NULL)
  })
}

get_resident_data <- function() {
  tryCatch({
    cat("Pulling resident database (resident_data and rotator_data forms)...\n")
    
    formData <- list(
      "token" = rdm_token,
      content = 'record',
      action = 'export',
      format = 'json',
      type = 'flat',
      csvDelimiter = '',
      forms = 'resident_data,rotator_data',
      rawOrLabel = 'label',
      rawOrLabelHeaders = 'raw',
      exportCheckboxLabel = 'false',
      exportSurveyFields = 'false',
      exportDataAccessGroups = 'false',
      returnFormat = 'json'
    )
    
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (httr::status_code(response) != 200) {
      stop("Resident REDCap API call failed with status: ", httr::status_code(response))
    }
    
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    resident_data <- jsonlite::fromJSON(response_text)
    
    cat("Raw resident data loaded. Total rows:", nrow(resident_data), "\n")
    
    # Map standard resident columns
    if ("Graduation year" %in% names(resident_data)) {
      resident_data$grad_yr <- resident_data$`Graduation year`
    }
    if ("Resident type" %in% names(resident_data)) {
      resident_data$type <- resident_data$`Resident type`
    }
    if ("Name from evaluation instrument (for database linking)" %in% names(resident_data)) {
      resident_data$name <- resident_data$`Name from evaluation instrument (for database linking)`
    }
    if ("Resident Last Name" %in% names(resident_data)) {
      resident_data$last_name <- resident_data$`Resident Last Name`
    }
    if ("Resident First Name" %in% names(resident_data)) {
      resident_data$first_name <- resident_data$`Resident First Name`
    }
    if ("Archived? " %in% names(resident_data)) {
      resident_data$res_archive <- resident_data$`Archived? `
    }
    if ("Record ID" %in% names(resident_data)) {
      resident_data$record_id <- as.character(resident_data$`Record ID`)
    }
    
    # Handle rotator fields - check both possible field names
    if ("rot_name" %in% names(resident_data)) {
      # Field already exists with correct name
    } else if ("Rotator Name" %in% names(resident_data)) {
      resident_data$rot_name <- resident_data$`Rotator Name`
    }
    
    if ("rot_grad_yr" %in% names(resident_data)) {
      # Field already exists with correct name
    } else if ("Rotator Graduation Year" %in% names(resident_data)) {
      resident_data$rot_grad_yr <- resident_data$`Rotator Graduation Year`
    }
    
    if ("rot_program1" %in% names(resident_data)) {
      resident_data$rot_program <- resident_data$rot_program1
    } else if ("Rotator Program" %in% names(resident_data)) {
      resident_data$rot_program <- resident_data$`Rotator Program`
    }
    
    # Process rotator records (record_id 157)
    rotator_rows <- which(resident_data$record_id == "157")
    if (length(rotator_rows) > 0) {
      cat("Found", length(rotator_rows), "rotator records (ID 157)...\n")
      
      for (i in rotator_rows) {
        if (!is.na(resident_data$rot_name[i]) && resident_data$rot_name[i] != "") {
          cat("Processing rotator:", resident_data$rot_name[i], "\n")
          
          resident_data$name[i] <- resident_data$rot_name[i]
          if (!is.na(resident_data$rot_grad_yr[i])) {
            resident_data$grad_yr[i] <- resident_data$rot_grad_yr[i]
          }
          resident_data$type[i] <- "Rotator"
        }
      }
    }
    
    # Process the data through helpers.R functions
    processed_data <- process_resident_data(resident_data)
    return(processed_data)
    
  }, error = function(e) {
    cat("Error in Resident API pull:", e$message, "\n")
    return(NULL)
  })
}

# Function to get evaluation data dictionary
get_evaluation_dictionary <- function() {
  tryCatch({
    cat("Pulling evaluation data dictionary...\n")
    
    formData <- list(
      "token" = rdm_token,
      content = 'metadata',
      format = 'json',
      returnFormat = 'json'
    )
    
    response <- httr::POST(url, body = formData, encode = "form")
    
    if (httr::status_code(response) != 200) {
      stop("Dictionary REDCap API call failed with status: ", httr::status_code(response))
    }
    
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    dict_data <- jsonlite::fromJSON(response_text)
    
    cat("Evaluation dictionary loaded. Total fields:", nrow(dict_data), "\n")
    
    # Filter for assessment fields only
    assessment_fields <- dict_data[dict_data$form_name == "assessment", ]
    cat("Assessment fields found:", nrow(assessment_fields), "\n")
    
    return(dict_data)  # Return full dictionary, filtering will be done in functions
    
  }, error = function(e) {
    cat("Error in Dictionary API pull:", e$message, "\n")
    return(NULL)
  })
}