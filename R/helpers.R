# helpers.R - Complete file with all functions

# Define %||% operator if rlang is not loaded (backup)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ============================================================================
# RESIDENT DATA PROCESSING FUNCTIONS
# ============================================================================

# Archive function - get list of archived residents
archive <- function(data) {
  archived_residents <- data %>%
    filter(res_archive == "Yes") %>%
    pull(name)
  return(archived_residents)
}

# Remove archived residents from dataset
remove_archived_residents <- function(data) {
  # Get list of archived residents
  archived_residents <- archive(data)
  # Remove rows where name is in the list of archived residents
  data <- data %>% filter(!name %in% archived_residents)
  return(data)
}

# Calculate resident level based on graduation year and type - CORRECTED FOR PRELIMS WITH DEBUGGING
calculate_resident_level <- function(coach_data) {
  cat("Inside calculate_resident_level\n")
  
  if (!inherits(coach_data, "data.frame")) {
    stop("Input to calculate_resident_level must be a data frame or tibble.")
  }
  
  # Get today's date and current academic year
  current_date <- Sys.Date()
  current_calendar_year <- as.numeric(format(current_date, "%Y"))
  
  # Academic year starts July 1
  # If today is before July 1, we're still in the previous academic year
  current_academic_year <- ifelse(format(current_date, "%m-%d") >= "07-01",
                                  current_calendar_year,
                                  current_calendar_year - 1)
  
  cat("Current date:", as.character(current_date), "\n")
  cat("Current calendar year:", current_calendar_year, "\n")
  cat("Current academic year:", current_academic_year, "\n")
  
  # Debug: Show resident types and graduation years
  cat("\nResident type distribution before calculation:\n")
  type_table <- table(coach_data$type, useNA = "always")
  print(type_table)
  
  cat("\nGraduation year distribution:\n")
  grad_table <- table(coach_data$grad_yr, useNA = "always")
  print(grad_table)
  
  coach_data <- coach_data %>%
    mutate(
      grad_yr = as.numeric(grad_yr),  # Ensure grad_yr is numeric
      Level = case_when(
        # Preliminary residents - they are 1-year residents
        # Current academic year 2024-2025 (July 1, 2024 - June 30, 2025)
        # Before July 1, 2025: Current prelims have grad_yr = 2025 (graduating June 2025)
        # After July 1, 2025: New prelims have grad_yr = 2026 (will graduate June 2026)
        
        type == "Preliminary" & format(current_date, "%m-%d") < "07-01" & grad_yr == current_calendar_year ~ "Intern",
        type == "Preliminary" & format(current_date, "%m-%d") >= "07-01" & grad_yr == current_calendar_year + 1 ~ "Intern",
        
        # Rotators - handle like preliminary residents (1-year program)
        type == "Rotator" & format(current_date, "%m-%d") < "07-01" & grad_yr == current_calendar_year ~ "Intern",
        type == "Rotator" & format(current_date, "%m-%d") >= "07-01" & grad_yr == current_calendar_year + 1 ~ "Intern",
        
        # Handle preliminary residents and rotators from previous/future years
        type %in% c("Preliminary", "Rotator") & grad_yr < current_calendar_year ~ "Graduated",
        type %in% c("Preliminary", "Rotator") & grad_yr > current_calendar_year + 1 ~ "Future",
        
        # Categorical residents - 3-year program based on academic year
        # Academic year 2024-2025 (July 1, 2024 - June 30, 2025)
        type == "Categorical" & grad_yr == current_academic_year + 1 ~ "PGY3",    # Graduate June 2025
        type == "Categorical" & grad_yr == current_academic_year + 2 ~ "PGY2",    # Graduate June 2026
        type == "Categorical" & grad_yr == current_academic_year + 3 ~ "Intern",  # Graduate June 2027 (current interns)
        type == "Categorical" & grad_yr == current_academic_year + 4 ~ "Incoming", # Graduate June 2028 (incoming July 2025)
        
        TRUE ~ NA_character_
      )
    )
  
  # Debug output
  cat("\nLevel distribution after calculation:\n")
  level_table <- table(coach_data$Level, useNA = "always")
  print(level_table)
  
  # Show examples by type and graduation year
  cat("\nBreakdown by type and grad_yr:\n")
  breakdown <- coach_data %>%
    filter(!is.na(Level)) %>%
    count(type, grad_yr, Level) %>%
    arrange(type, grad_yr)
  print(breakdown)
  
  # Specifically look for preliminary residents and rotators
  prelim_count <- sum(coach_data$type == "Preliminary", na.rm = TRUE)
  rotator_count <- sum(coach_data$type == "Rotator", na.rm = TRUE)
  cat("\nTotal preliminary residents in data:", prelim_count, "\n")
  cat("Total rotators in data:", rotator_count, "\n")
  
  if (prelim_count > 0) {
    cat("Preliminary resident details:\n")
    prelim_details <- coach_data %>%
      filter(type == "Preliminary") %>%
      select(name, type, grad_yr, Level) %>%
      head(10)
    print(prelim_details)
  }
  
  if (rotator_count > 0) {
    cat("Rotator details:\n")
    rotator_details <- coach_data %>%
      filter(type == "Rotator") %>%
      select(name, type, grad_yr, Level) %>%
      head(10)
    print(rotator_details)
  }
  
  return(coach_data)
}

# Filter residents for availability (exclude incoming interns before July 1) - CORRECTED FOR PRELIMS WITH DEBUGGING
filter_available_residents <- function(data) {
  current_date <- Sys.Date()
  current_calendar_year <- as.numeric(format(current_date, "%Y"))
  
  cat("Filtering available residents...\n")
  cat("Current date:", as.character(current_date), "\n")
  cat("Current calendar year:", current_calendar_year, "\n")
  
  # Debug: Show what we have before filtering
  cat("Before filtering - Level distribution:\n")
  before_table <- table(data$Level, useNA = "always")
  print(before_table)
  
  # Show preliminary residents and rotators before filtering
  if (any(data$type %in% c("Preliminary", "Rotator"), na.rm = TRUE)) {
    cat("Preliminary residents and rotators before filtering:\n")
    prelim_rot_before <- data %>%
      filter(type %in% c("Preliminary", "Rotator")) %>%
      select(name, type, grad_yr, Level) %>%
      head(15)
    print(prelim_rot_before)
  }
  
  # Before July 1, exclude incoming interns but keep current preliminary and categorical interns
  if (format(current_date, "%m-%d") < "07-01") {
    cat("Before July 1 - filtering out incoming categorical interns only\n")
    
    data <- data %>%
      filter(
        # Remove incoming categorical interns (Level == "Incoming")
        Level != "Incoming" | is.na(Level),
        
        # Keep all residents with valid levels (Intern, PGY2, PGY3)
        # Remove only those explicitly marked as "Graduated" or "Future"
        !(Level %in% c("Graduated", "Future"))
      )
  } else {
    cat("After July 1 - including new interns, excluding graduated residents\n")
    
    data <- data %>%
      # Convert "Incoming" to "Intern" after July 1
      mutate(Level = ifelse(Level == "Incoming", "Intern", Level)) %>%
      # Remove residents who graduated or are future
      filter(
        !(Level %in% c("Graduated", "Future"))
      )
  }
  
  # Debug: Show what we have after filtering
  cat("After filtering - Level distribution:\n")
  after_table <- table(data$Level, useNA = "always")
  print(after_table)
  
  # Show preliminary residents and rotators after filtering
  if (any(data$type %in% c("Preliminary", "Rotator"), na.rm = TRUE)) {
    cat("Preliminary residents and rotators after filtering:\n")
    prelim_rot_after <- data %>%
      filter(type %in% c("Preliminary", "Rotator")) %>%
      select(name, type, grad_yr, Level) %>%
      head(15)
    print(prelim_rot_after)
  }
  
  return(data)
}

# Process resident data - complete pipeline
process_resident_data <- function(raw_data) {
  cat("Processing resident data...\n")
  cat("Initial rows:", nrow(raw_data), "\n")
  
  # Step 1: Remove archived residents
  data <- remove_archived_residents(raw_data)
  cat("After removing archived:", nrow(data), "\n")
  
  # Step 2: Calculate resident levels
  data <- calculate_resident_level(data)
  
  # Step 3: Filter for available residents (based on date)
  data <- filter_available_residents(data)
  cat("After filtering available:", nrow(data), "\n")
  
  # Step 4: Remove residents without a Level (shouldn't be selectable)
  data <- data %>% filter(!is.na(Level))
  cat("After removing residents without Level:", nrow(data), "\n")
  
  # Note: No longer excluding Rotator record (157) - it's now processed as a rotator
  
  return(data)
}

# ============================================================================
# FACULTY SUBMISSION FUNCTIONS
# ============================================================================

# Function to get next faculty record ID
get_next_faculty_record_id <- function(token, url) {
  tryCatch({
    cat("Getting next faculty record ID...\n")
    
    # Query for all records to find the highest record_id
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        fieldNames = "record_id",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    cat("Faculty record query response status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) == 200) {
      response_text <- httr::content(response, "text", encoding = "UTF-8")
      all_data <- jsonlite::fromJSON(response_text)
      
      if (is.data.frame(all_data) && nrow(all_data) > 0) {
        # Get all record IDs and find the maximum
        record_ids <- as.numeric(all_data$record_id)
        record_ids <- record_ids[!is.na(record_ids)]
        
        if (length(record_ids) > 0) {
          max_id <- max(record_ids)
          next_id <- max_id + 1
          
          cat("Found existing record IDs. Max:", max_id, "Next:", next_id, "\n")
          return(next_id)
        }
      }
    } else {
      cat("❌ Faculty record query failed with status:", httr::status_code(response), "\n")
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      cat("Error details:", error_text, "\n")
    }
    
    # Fallback to record ID 1 if no existing records found
    cat("⚠️ No existing faculty records found, starting with record ID 1\n")
    return(1)
    
  }, error = function(e) {
    cat("❌ Error in get_next_faculty_record_id:", e$message, "\n")
    return(1)
  })
}

# Function to submit new faculty to REDCap - CORRECTED VERSION
submit_new_faculty_to_redcap <- function(faculty_data, token, url) {
  # Get the next available record ID
  next_record_id <- get_next_faculty_record_id(token, url)
  
  cat("Next faculty record ID:", next_record_id, "\n")
  
  # Split the full name into first and last names
  full_name <- trimws(faculty_data$fac_name)
  name_parts <- strsplit(full_name, "\\s+")[[1]]
  
  # Simple name splitting logic
  if (length(name_parts) >= 2) {
    first_name <- name_parts[1]
    last_name <- paste(name_parts[2:length(name_parts)], collapse = " ")
  } else {
    first_name <- full_name
    last_name <- ""
  }
  
  # Start with basic data frame
  redcap_data <- data.frame(
    record_id = as.character(next_record_id),
    fac_name = faculty_data$fac_name %||% "",
    fac_f_name = first_name,  # Add first name field
    fac_l_name = last_name,   # Add last name field
    fac_email = faculty_data$fac_email %||% "",
    fac_clin = faculty_data$fac_clin %||% "",
    fac_div = faculty_data$fac_div %||% "",
    fac_fell = faculty_data$fac_fell %||% "",
    other_div = faculty_data$other_div %||% "",
    stringsAsFactors = FALSE
  )
  
  # Handle checkbox field (fac_med_ed) - each choice gets its own column
  # Choices: 1=Core Faculty IM, 2=Core Faculty Fellowship, 3=APD IM, 4=APD Fellowship, 
  #          5=Fellowship PD, 6=Learning Community, 7=Clerkship, 8=Core IM PD, 9=MS Course, 10=Other
  med_ed_choices <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  
  # Parse the selected choices (comma-separated string)
  selected_choices <- character(0)
  if (!is.null(faculty_data$fac_med_ed) && faculty_data$fac_med_ed != "") {
    selected_choices <- trimws(strsplit(faculty_data$fac_med_ed, ",")[[1]])
  }
  
  # Add checkbox columns - each choice gets a separate column
  for (choice in med_ed_choices) {
    col_name <- paste0("fac_med_ed___", choice)
    redcap_data[[col_name]] <- if (choice %in% selected_choices) "1" else "0"
  }
  
  cat("Submitting faculty data to REDCap:\n")
  print(redcap_data)
  
  # Submit to REDCap
  result <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      data = jsonlite::toJSON(redcap_data, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  
  response_text <- httr::content(result, "text")
  cat("REDCap faculty submission response status:", httr::status_code(result), "\n")
  cat("REDCap faculty submission response:", response_text, "\n")
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit faculty to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
  }
  
  return(response_text)
}

# ============================================================================
# RESIDENT EVALUATION FUNCTIONS
# ============================================================================

# Submit evaluation to REDCap
submit_evaluation_to_redcap <- function(eval_data, token, url) {
  # Add the required fields that were missing
  
  # 1. Add fac_fell_name from selected faculty
  if (exists("values") && !is.null(values$selected_faculty)) {
    eval_data$fac_fell_name <- values$selected_faculty$fac_name
  }
  
  # 2. Add current system date in Y-M-D format (REDCap requirement)
  eval_data$fac_eval_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Convert evaluation data to REDCap format
  redcap_data <- data.frame(eval_data, stringsAsFactors = FALSE)
  
  cat("Final REDCap submission data:\n")
  print(redcap_data)
  
  # Submit to REDCap
  result <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      data = jsonlite::toJSON(redcap_data, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  
  response_text <- httr::content(result, "text")
  cat("REDCap response status:", httr::status_code(result), "\n")
  cat("REDCap response:", response_text, "\n")
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit evaluation to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
  }
  
  return(response_text)
}

# Get next faculty evaluation instance number for a resident
get_next_faculty_eval_instance <- function(resident_id, token, url) {
  tryCatch({
    cat("=== GETTING NEXT INSTANCE FOR RESIDENT ID:", resident_id, "===\n")
    
    # Query for specific record with only needed fields
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = resident_id,
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    cat("REDCap query response status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) == 200) {
      response_text <- httr::content(response, "text", encoding = "UTF-8")
      all_data <- jsonlite::fromJSON(response_text)
      
      if (is.data.frame(all_data) && nrow(all_data) > 0) {
        # Filter for faculty_evaluation instances
        faculty_evals <- all_data[
          !is.na(all_data$redcap_repeat_instrument) & 
            all_data$redcap_repeat_instrument == "faculty_evaluation", 
        ]
        
        cat("Faculty evaluation records found:", nrow(faculty_evals), "\n")
        
        if (nrow(faculty_evals) > 0) {
          # Get all instance numbers
          instances <- as.numeric(faculty_evals$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          
          if (length(instances) > 0) {
            instances <- sort(instances)
            max_instance <- max(instances)
            next_instance <- max_instance + 1
            
            cat("✅ Found existing instances:", paste(instances, collapse = ", "), "\n")
            cat("✅ Max instance:", max_instance, "Next instance:", next_instance, "\n")
            
            return(next_instance)
          }
        }
      }
    } else {
      cat("❌ REDCap query failed with status:", httr::status_code(response), "\n")
      error_text <- httr::content(response, "text", encoding = "UTF-8")
      cat("Error details:", error_text, "\n")
    }
    
    # Fallback to instance 1 if no existing instances found
    cat("⚠️ No existing faculty evaluation instances found, starting with instance 1\n")
    return(1)
    
  }, error = function(e) {
    cat("❌ Error in get_next_faculty_eval_instance:", e$message, "\n")
    return(1)
  })
}

# Function to submit pending faculty to REDCap faculty database as repeating instrument
submit_pending_faculty_to_redcap <- function(pending_data, token, url) {
  # Get the next available instance number for record_id = 1, pending_queue instrument
  next_instance <- tryCatch({
    result <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        action = "export",
        format = "json",
        records = "1",
        fieldNames = "record_id,redcap_repeat_instrument,redcap_repeat_instance",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    if (httr::status_code(result) == 200) {
      records <- jsonlite::fromJSON(httr::content(result, "text"))
      if (is.data.frame(records) && nrow(records) > 0) {
        # Find max instance number for pending_queue
        existing_instances <- records[!is.na(records$redcap_repeat_instrument) & 
                                        records$redcap_repeat_instrument == "pending_queue", ]
        if (nrow(existing_instances) > 0) {
          instances <- as.numeric(existing_instances$redcap_repeat_instance)
          instances <- instances[!is.na(instances)]
          if (length(instances) > 0) {
            max_instance <- max(instances)
            cat("Found existing pending_queue instances:", paste(sort(instances), collapse = ", "), "\n")
            cat("Next pending_queue instance:", max_instance + 1, "\n")
            max_instance + 1
          } else {
            1
          }
        } else {
          1
        }
      } else {
        1
      }
    } else {
      cat("No existing pending_queue instances found, starting with instance 1\n")
      1
    }
  }, error = function(e) {
    cat("Warning: Could not get existing pending instances, using instance = 1\n")
    1
  })
  
  redcap_data <- data.frame(
    record_id = "1",
    redcap_repeat_instrument = "pending_queue",
    redcap_repeat_instance = as.character(next_instance),
    pend_name = pending_data$pend_name,
    pend_fac_fell = pending_data$pend_fac_fell,
    pend_rot = pending_data$pend_rot,
    pending_queue_complete = 2,
    stringsAsFactors = FALSE
  )
  
  cat("Submitting pending faculty data as repeating instrument instance", next_instance, ":\n")
  print(redcap_data)
  
  # Submit to REDCap faculty database
  result <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      data = jsonlite::toJSON(redcap_data, auto_unbox = TRUE)
    ),
    encode = "form"
  )
  
  response_text <- httr::content(result, "text")
  cat("REDCap pending queue response status:", httr::status_code(result), "\n")
  cat("REDCap pending queue response:", response_text, "\n")
  
  if (httr::status_code(result) != 200) {
    stop("Failed to submit pending faculty to REDCap. Status: ", httr::status_code(result), " Response: ", response_text)
  }
  
  return(response_text)
}

# Count monthly faculty evaluations for a resident
count_monthly_faculty_evals <- function(faculty_eval_data, resident_data, resident_name) {
  tryCatch({
    # Get resident record
    resident_record <- resident_data %>%
      filter(name == resident_name) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      return(0)
    }
    
    # Calculate date 30 days ago
    thirty_days_ago <- Sys.Date() - 30
    
    # Filter evaluations for this resident in the last 30 days
    if (!is.null(faculty_eval_data) && nrow(faculty_eval_data) > 0) {
      monthly_evals <- faculty_eval_data %>%
        filter(
          record_id == resident_record$record_id,
          !is.na(fac_eval_date),
          as.Date(fac_eval_date) >= thirty_days_ago
        )
      
      return(nrow(monthly_evals))
    }
    
    return(0)
  }, error = function(e) {
    cat("Error counting monthly evaluations:", e$message, "\n")
    return(0)
  })
}