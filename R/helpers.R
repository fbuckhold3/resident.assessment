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


# Add this function to your server.R (or helpers.R)
get_cc_completion_status <- function(resident_name, resident_level) {
  tryCatch({
    cat("=== Getting CC completion status with academic year check ===\n")
    cat("Resident:", resident_name, "Level:", resident_level, "\n")
    
    # Check if we have the necessary REDCap credentials
    if (!exists("rdm_token") || !exists("url")) {
      cat("❌ REDCap credentials not available\n")
      return(create_empty_completion_status())
    }
    
    if (!exists("resident_data") || is.null(resident_data)) {
      cat("❌ Resident data not available\n")
      return(create_empty_completion_status())
    }
    
    # Get the resident's record ID
    resident_record <- resident_data %>%
      filter(name == resident_name) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      cat("❌ Resident not found in resident_data:", resident_name, "\n")
      return(create_empty_completion_status())
    }
    
    resident_id <- resident_record$record_id
    cat("✅ Found resident ID:", resident_id, "\n")
    
    # Get current academic year (July 1 - June 30)
    current_date <- Sys.Date()
    current_calendar_year <- as.numeric(format(current_date, "%Y"))
    academic_year_start <- if (format(current_date, "%m-%d") >= "07-01") {
      current_calendar_year
    } else {
      current_calendar_year - 1
    }
    academic_year_end <- academic_year_start + 1
    
    cat("📅 Current academic year:", academic_year_start, "-", academic_year_end, "\n")
    
    # Query REDCap for assessment records for this resident in current academic year
    cat("📡 Querying REDCap for assessment data...\n")
    
    response <- httr::POST(
      url = url,
      body = list(
        token = rdm_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(resident_id),
        forms = "assessment",
        rawOrLabel = "raw",
        rawOrLabelHeaders = "raw",
        exportCheckboxLabel = "false",
        exportSurveyFields = "false",
        exportDataAccessGroups = "false",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    cat("📡 REDCap response status:", httr::status_code(response), "\n")
    
    if (httr::status_code(response) != 200) {
      cat("❌ REDCap query failed with status:", httr::status_code(response), "\n")
      return(create_empty_completion_status())
    }
    
    # Parse the response
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    assessment_data <- jsonlite::fromJSON(response_text)
    
    cat("📊 Retrieved", nrow(assessment_data), "assessment records\n")
    
    if (!is.data.frame(assessment_data) || nrow(assessment_data) == 0) {
      cat("ℹ️ No assessment data found for resident\n")
      return(create_empty_completion_status())
    }
    
    # Filter for CC evaluations in current academic year
    has_cc_quarter <- !is.na(assessment_data$ass_cc_quart) & 
      assessment_data$ass_cc_quart != "" & 
      assessment_data$ass_cc_quart %in% c("1", "2", "3", "4")
    
    # Filter by academic year
    assessment_data$ass_date_parsed <- as.Date(assessment_data$ass_date)
    academic_year_start_date <- as.Date(paste0(academic_year_start, "-07-01"))
    academic_year_end_date <- as.Date(paste0(academic_year_end, "-06-30"))
    
    in_current_academic_year <- !is.na(assessment_data$ass_date_parsed) &
      assessment_data$ass_date_parsed >= academic_year_start_date &
      assessment_data$ass_date_parsed <= academic_year_end_date
    
    cc_evaluations_this_year <- assessment_data[has_cc_quarter & in_current_academic_year, ]
    
    cat("🎯 Found", nrow(cc_evaluations_this_year), "CC evaluations in current academic year\n")
    
    if (nrow(cc_evaluations_this_year) > 0) {
      cat("📝 CC evaluation quarters found this year:", paste(cc_evaluations_this_year$ass_cc_quart, collapse = ", "), "\n")
    }
    
    # Create completion status for all 4 quarters
    quarters <- c("1", "2", "3", "4")
    quarter_labels <- c(
      "1" = "Q1 - In-Basket (Fall)",
      "2" = "Q2 - Semi-Annual (Winter)", 
      "3" = "Q3 - Documentation (Spring)",
      "4" = "Q4 - Semi-Annual (Summer)"
    )
    
    completion_data <- data.frame(
      quarter = quarters,
      quarter_label = quarter_labels[quarters],
      completed = sapply(quarters, function(q) {
        completed <- any(cc_evaluations_this_year$ass_cc_quart == q, na.rm = TRUE)
        cat("📊 Quarter", q, "completed this academic year:", completed, "\n")
        return(completed)
      }),
      evaluator = sapply(quarters, function(q) {
        matching_evals <- cc_evaluations_this_year[cc_evaluations_this_year$ass_cc_quart == q & !is.na(cc_evaluations_this_year$ass_cc_quart), ]
        if (nrow(matching_evals) > 0) {
          evaluator <- matching_evals$ass_faculty[1]
          if (!is.na(evaluator) && evaluator != "") {
            return(evaluator)
          }
        }
        return(NA_character_)
      }),
      evaluation_date = sapply(quarters, function(q) {
        matching_evals <- cc_evaluations_this_year[cc_evaluations_this_year$ass_cc_quart == q & !is.na(cc_evaluations_this_year$ass_cc_quart), ]
        if (nrow(matching_evals) > 0) {
          eval_date <- matching_evals$ass_date[1]
          if (!is.na(eval_date) && eval_date != "") {
            return(eval_date)
          }
        }
        return(NA_character_)
      }),
      stringsAsFactors = FALSE
    )
    
    cat("✅ Completion status created successfully\n")
    cat("📊 Summary: ", sum(completion_data$completed), "out of 4 quarters completed this academic year\n")
    
    return(completion_data)
    
  }, error = function(e) {
    cat("❌ Error in get_cc_completion_status:", e$message, "\n")
    return(create_empty_completion_status())
  })
}

# Build clickable quarter selection table
build_clickable_quarter_table <- function(completion_data) {
  if (is.null(completion_data) || nrow(completion_data) == 0) {
    return(div(
      style = "padding: 1rem; background: #f8f9fa; border-radius: 8px; text-align: center; color: #666; border: 1px dashed #dee2e6;",
      "📊 No completion data available"
    ))
  }
  
  div(
    class = "cc-completion-status",
    style = "margin-bottom: 1.5rem; padding: 1rem; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #17a2b8;",
    h6("📊 Select Quarter to Evaluate", style = "margin-bottom: 0.5rem; color: #17a2b8; font-weight: 600;"),
    p("Click on any quarter to start that evaluation:", style = "margin-bottom: 1rem; font-size: 0.9rem; color: #666;"),
    div(class = "completion-grid", style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); gap: 0.75rem;",
        lapply(1:nrow(completion_data), function(i) {
          row <- completion_data[i, ]
          is_completed <- row$completed
          status_icon <- if (is_completed) "✅" else "⭕"
          
          # Build evaluator and date text
          detail_text <- ""
          if (is_completed && !is.na(row$evaluator)) {
            detail_text <- paste("by", row$evaluator)
            if (!is.na(row$evaluation_date)) {
              formatted_date <- tryCatch({
                date_obj <- as.Date(row$evaluation_date)
                format(date_obj, "%m/%d/%Y")
              }, error = function(e) row$evaluation_date)
              detail_text <- paste0(detail_text, " (", formatted_date, ")")
            }
          }
          
          # Create the clickable quarter card
          if (is_completed) {
            # Completed quarter - show as completed but allow override
            div(
              class = "completion-item completed clickable-quarter",
              style = paste0(
                "padding: 0.75rem; border-radius: 8px; text-align: center; cursor: pointer; ",
                "background: #d4edda; border: 2px solid #c3e6cb; color: #155724; ",
                "transition: all 0.2s ease; position: relative;"
              ),
              onclick = paste0("selectQuarter('", row$quarter, "', '", row$quarter_label, "', true)"),
              div(style = "font-weight: 600; margin-bottom: 0.5rem; font-size: 1rem;", paste(status_icon, row$quarter_label)),
              div(style = "font-size: 0.85rem; font-weight: 500; margin-bottom: 0.5rem;", "Completed This Year"),
              if (detail_text != "") {
                div(style = "font-size: 0.75rem; margin-bottom: 0.75rem; opacity: 0.8; font-style: italic;", detail_text)
              },
              div(style = "font-size: 0.7rem; opacity: 0.7; background: rgba(255,255,255,0.3); padding: 0.25rem; border-radius: 4px;", 
                  "⚠️ Click to re-evaluate")
            )
          } else {
            # Pending quarter - ready to start
            div(
              class = "completion-item pending clickable-quarter",
              style = paste0(
                "padding: 0.75rem; border-radius: 8px; text-align: center; cursor: pointer; ",
                "background: #fff3cd; border: 2px solid #ffeaa7; color: #856404; ",
                "transition: all 0.2s ease; position: relative;"
              ),
              onclick = paste0("selectQuarter('", row$quarter, "', '", row$quarter_label, "', false)"),
              div(style = "font-weight: 600; margin-bottom: 0.5rem; font-size: 1rem;", paste(status_icon, row$quarter_label)),
              div(style = "font-size: 0.85rem; font-weight: 500; margin-bottom: 0.75rem;", "Ready to Start"),
              div(style = "font-size: 0.8rem; background: rgba(255,255,255,0.5); padding: 0.5rem; border-radius: 4px; font-weight: 600;", 
                  "▶️ Click to Begin")
            )
          }
        })
    ),
    
    # JavaScript for quarter selection
    tags$script(HTML("
      function selectQuarter(quarter, quarterLabel, isCompleted) {
        console.log('Selecting quarter:', quarter, quarterLabel, 'Completed:', isCompleted);
        
        // Show confirmation if already completed
        if (isCompleted) {
          var confirmMsg = 'This quarter (' + quarterLabel + ') has already been completed this academic year. Are you sure you want to create another evaluation?';
          if (!confirm(confirmMsg)) {
            return;
          }
        }
        
        // Set the quarter value (this will trigger conditional panels to show)
        Shiny.setInputValue('ass_cc_quart', quarter, {priority: 'event'});
        
        // Also set a flag for the server to know this was manually selected
        Shiny.setInputValue('cc_quarter_manually_selected', {
          quarter: quarter,
          label: quarterLabel,
          isCompleted: isCompleted,
          timestamp: new Date().getTime()
        }, {priority: 'event'});
        
        // Scroll to the next section after a brief delay
        setTimeout(function() {
          var nextSection = document.querySelector('.eval-section:nth-child(2)');
          if (nextSection) {
            nextSection.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }
        }, 200);
        
        // Visual feedback
        document.querySelectorAll('.clickable-quarter').forEach(function(el) {
          el.style.transform = '';
          el.style.boxShadow = '';
        });
        
        // Highlight selected quarter
        event.currentTarget.style.transform = 'scale(1.02)';
        event.currentTarget.style.boxShadow = '0 4px 20px rgba(0,0,0,0.15)';
      }
    "))
  )
}


create_empty_completion_status <- function() {
  quarters <- c("1", "2", "3", "4")
  quarter_labels <- c(
    "1" = "Q1 - In-Basket (Fall)",
    "2" = "Q2 - Semi-Annual (Winter)", 
    "3" = "Q3 - Documentation (Spring)",
    "4" = "Q4 - Semi-Annual (Summer)"
  )
  
  data.frame(
    quarter = quarters,
    quarter_label = quarter_labels[quarters],
    completed = rep(FALSE, 4),  # All quarters pending
    evaluator = rep(NA_character_, 4),
    evaluation_date = rep(NA_character_, 4),
    stringsAsFactors = FALSE
  )
}

build_completion_status_display <- function(completion_data) {
  if (is.null(completion_data) || nrow(completion_data) == 0) {
    return(div(
      style = "padding: 1rem; background: #f8f9fa; border-radius: 8px; text-align: center; color: #666; border: 1px dashed #dee2e6;",
      "📊 No completion data available"
    ))
  }
  
  div(
    class = "cc-completion-status",
    style = "margin-bottom: 1.5rem; padding: 1rem; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #17a2b8;",
    h6("📊 Current Completion Status", style = "margin-bottom: 0.5rem; color: #17a2b8; font-weight: 600;"),
    p("Click on any quarter to select it:", style = "margin-bottom: 1rem; font-size: 0.9rem; color: #666; font-style: italic;"),
    div(class = "completion-grid", style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 0.75rem;",
        lapply(1:nrow(completion_data), function(i) {
          row <- completion_data[i, ]
          status_class <- if (row$completed) "completed" else "pending"
          status_icon <- if (row$completed) "✅" else "⭕"
          status_text <- if (row$completed) "Completed" else "Pending"
          
          # Build evaluator and date text
          detail_text <- ""
          if (row$completed && !is.na(row$evaluator)) {
            detail_text <- paste("by", row$evaluator)
            if (!is.na(row$evaluation_date)) {
              formatted_date <- tryCatch({
                date_obj <- as.Date(row$evaluation_date)
                format(date_obj, "%m/%d/%Y")
              }, error = function(e) row$evaluation_date)
              detail_text <- paste0(detail_text, " (", formatted_date, ")")
            }
          }
          
          # Create action button instead of div with onclick
          if (row$completed) {
            # For completed quarters - show info modal
            div(
              class = paste("completion-item", status_class),
              style = paste0(
                "padding: 0.75rem; border-radius: 6px; text-align: center; ",
                "background: #d4edda; border: 1px solid #c3e6cb; color: #155724;"
              ),
              div(style = "font-weight: 600; margin-bottom: 0.25rem;", paste(status_icon, row$quarter_label)),
              div(style = "font-size: 0.85rem;", status_text),
              if (detail_text != "") {
                div(style = "font-size: 0.75rem; margin-top: 0.25rem; opacity: 0.8;", detail_text)
              },
              div(style = "font-size: 0.7rem; margin-top: 0.5rem; opacity: 0.6;", "Already completed"),
              br(),
              actionButton(
                inputId = paste0("cc_select_quarter_", row$quarter),
                label = "Select Anyway",
                class = "btn btn-sm btn-outline-success",
                style = "font-size: 0.75rem; padding: 0.25rem 0.5rem;"
              )
            )
          } else {
            # For pending quarters - direct selection
            div(
              class = paste("completion-item", status_class),
              style = paste0(
                "padding: 0.75rem; border-radius: 6px; text-align: center; ",
                "background: #fff3cd; border: 1px solid #ffeaa7; color: #856404;"
              ),
              div(style = "font-weight: 600; margin-bottom: 0.25rem;", paste(status_icon, row$quarter_label)),
              div(style = "font-size: 0.85rem;", status_text),
              div(style = "font-size: 0.7rem; margin-top: 0.5rem; opacity: 0.6;", "Click to start"),
              br(),
              actionButton(
                inputId = paste0("cc_select_quarter_", row$quarter),
                label = "Start Evaluation",
                class = "btn btn-sm btn-warning",
                style = "font-size: 0.75rem; padding: 0.25rem 0.5rem;"
              )
            )
          }
        })
    )
  )
}







