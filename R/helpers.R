# evaluation_helpers.R - Functions for evaluation type selection and logic

# ============================================================================
# EVALUATION TYPE MAPPING AND LOGIC
# ============================================================================

# Define evaluation types with metadata
get_evaluation_types <- function() {
  list(
    cc = list(
      id = "cc",
      name = "Continuity Clinic",
      icon = "🏥",
      description = "Primary care continuity clinic evaluation",
      field_prefix = "ass_cc"
    ),
    obs = list(
      id = "obs", 
      name = "Observations",
      icon = "👁️",
      description = "Direct observation of clinical skills",
      field_prefix = "ass_obs"
    ),
    int_ip = list(
      id = "int_ip",
      name = "Intern Inpatient", 
      icon = "🏨",
      description = "Inpatient ward evaluation for interns",
      field_prefix = "ass_int_ip"
    ),
    res_ip = list(
      id = "res_ip",
      name = "Senior Inpatient",
      icon = "🏥", 
      description = "Inpatient ward evaluation for senior residents",
      field_prefix = "ass_res_ip"
    ),
    bridge = list(
      id = "bridge",
      name = "Bridge Clinic",
      icon = "🌉",
      description = "Transitional clinic experience evaluation", 
      field_prefix = "ass_bridge"
    ),
    cons = list(
      id = "cons",
      name = "Consults",
      icon = "💬",
      description = "Consultation service evaluation",
      field_prefix = "ass_cons"
    ),
    day = list(
      id = "day",
      name = "Single Day Clinic", 
      icon = "📅",
      description = "Single day outpatient clinic evaluation",
      field_prefix = "ass_day"
    )
  )
}

# Get available evaluation types based on faculty division
get_available_eval_types_by_division <- function(fac_div) {
  cat("Input fac_div:", fac_div, "Type:", class(fac_div), "\n")
  
  # Create mapping from text labels to numeric codes
  division_text_to_num <- list(
    "Addiction Medicine" = "1",
    "Allergy" = "2", 
    "Cardiology" = "3",
    "Endocrinology" = "4",
    "Gastroenterology" = "5",
    "Geriatrics" = "6",
    "GIM - Hospitalist" = "7",
    "GIM - Primary Care" = "8",
    "Hematology / Oncology" = "9",
    "Infectious Disease" = "10",
    "Nephrology" = "11",
    "Palliative Care" = "12",
    "Pulmonary / Critical Care" = "13",
    "Rheumatology" = "14",
    "Other" = "15"
  )
  
  # Convert division to numeric code
  div_num <- NULL
  
  if (is.character(fac_div)) {
    # First try direct text lookup
    if (fac_div %in% names(division_text_to_num)) {
      div_num <- division_text_to_num[[fac_div]]
      cat("Found text match:", fac_div, "-> code", div_num, "\n")
    } else {
      # Try converting to numeric in case it's already a number as text
      div_num <- suppressWarnings(as.numeric(fac_div))
      if (is.na(div_num)) {
        cat("Could not convert fac_div to number:", fac_div, "\n")
        div_num <- "15"  # Default to "Other"
      } else {
        div_num <- as.character(div_num)
      }
    }
  } else if (is.numeric(fac_div)) {
    div_num <- as.character(fac_div)
  } else {
    cat("Unknown fac_div type:", class(fac_div), "\n")
    div_num <- "15"  # Default to "Other"
  }
  
  cat("Final div_num:", div_num, "\n")
  
  # Define evaluation types by division code
  division_evals <- list(
    "1" = c("cons", "day", "obs"),  # Addiction Medicine
    "2" = c("day", "obs"),          # Allergy
    "3" = c("int_ip", "res_ip", "cons", "day", "obs"),  # Cardiology
    "4" = c("cons", "day", "obs"),  # Endocrinology
    "5" = c("res_ip", "cons", "day", "obs"),  # Gastroenterology
    "6" = c("cons", "day", "obs"),  # Geriatrics
    "7" = c("int_ip", "res_ip", "bridge", "day", "obs"),  # GIM - Hospitalist
    "8" = c("cc", "int_ip", "res_ip", "bridge", "day", "obs"),  # GIM - Primary Care
    "9" = c("cons", "day", "obs"),  # Hematology / Oncology
    "10" = c("cons", "day", "obs"), # Infectious Disease
    "11" = c("cons", "day", "obs"), # Nephrology
    "12" = c("cons", "day", "obs"), # Palliative Care
    "13" = c("int_ip", "res_ip", "cons", "day", "obs"),  # Pulmonary / Critical Care
    "14" = c("cons", "day", "obs"), # Rheumatology
    "15" = c("day", "obs")          # Other
  )
  
  # Get evaluation types for this division
  eval_codes <- division_evals[[div_num]]
  
  if (is.null(eval_codes)) {
    cat("No eval codes found for div_num:", div_num, "using default\n")
    # Default for unknown divisions
    eval_codes <- c("day", "obs")
  }
  
  cat("Returning eval_codes:", paste(eval_codes, collapse = ", "), "\n")
  return(eval_codes)
}

# Filter evaluation types based on resident level
filter_eval_types_by_resident_level <- function(eval_types, resident_level) {
  filtered_types <- eval_types
  
  cat("Filtering eval types for resident level:", resident_level, "\n")
  cat("Input eval types:", paste(eval_types, collapse = ", "), "\n")
  
  # Remove level-specific evaluations based on resident level
  if (resident_level == "Intern") {
    # Interns cannot do senior inpatient evaluations
    filtered_types <- filtered_types[filtered_types != "res_ip"]
    cat("Intern: Removed res_ip\n")
  } else if (resident_level %in% c("PGY2", "PGY3")) {
    # Senior residents (PGY2/PGY3) cannot do intern-specific evaluations
    # They should do senior inpatient evaluations instead
    filtered_types <- filtered_types[filtered_types != "int_ip"]
    cat("Senior resident: Removed int_ip\n")
  } else if (resident_level == "Rotator") {
    # Rotators can do most evaluations except continuity clinic
    # They also typically don't do intern-specific evaluations
    filtered_types <- filtered_types[filtered_types != "cc"]
    filtered_types <- filtered_types[filtered_types != "int_ip"]
    cat("Rotator: Removed cc and int_ip\n")
  }
  
  cat("Final filtered types:", paste(filtered_types, collapse = ", "), "\n")
  return(filtered_types)
}

# Get evaluation type metadata for display
get_eval_type_display_info <- function(eval_type_id, resident_level = NULL) {
  all_types <- get_evaluation_types()
  
  if (!eval_type_id %in% names(all_types)) {
    return(NULL)
  }
  
  eval_info <- all_types[[eval_type_id]]
  
  # Add level-specific tags
  tags <- c()
  
  if (eval_type_id == "int_ip") {
    tags <- c(tags, "Intern Level")
  } else if (eval_type_id == "res_ip") {
    tags <- c(tags, "Senior Level")
  } else if (eval_type_id == "cc") {
    tags <- c(tags, "Longitudinal")
  }
  
  # Add resident level if provided
  if (!is.null(resident_level)) {
    if (eval_type_id == "int_ip" && resident_level != "Intern") {
      tags <- c(tags, "Supervising")
    }
  }
  
  eval_info$tags <- tags
  
  return(eval_info)
}

# Check if evaluation type is appropriate for resident level
is_eval_appropriate_for_level <- function(eval_type_id, resident_level) {
  # Most evaluations are appropriate for all levels
  appropriate <- TRUE
  
  # Special cases
  if (eval_type_id == "cc" && resident_level == "Rotator") {
    appropriate <- FALSE  # Rotators typically don't do continuity clinic
  }
  
  return(appropriate)
}

# Get the next evaluation instance for a specific evaluation type
get_next_eval_instance <- function(resident_id, eval_type, token, url) {
  tryCatch({
    cat("=== GETTING NEXT INSTANCE FOR RESIDENT:", resident_id, "EVAL TYPE:", eval_type, "===\n")
    
    # Map evaluation type to REDCap instrument name
    instrument_map <- list(
      "cc" = "continuity_clinic_evaluation",
      "obs" = "observation_evaluation", 
      "int_ip" = "intern_inpatient_evaluation",
      "res_ip" = "senior_inpatient_evaluation",
      "bridge" = "bridge_clinic_evaluation",
      "cons" = "consultation_evaluation",
      "day" = "single_day_clinic_evaluation"
    )
    
    instrument_name <- instrument_map[[eval_type]]
    
    if (is.null(instrument_name)) {
      cat("Unknown evaluation type:", eval_type, "\n")
      return(1)
    }
    
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
        # Filter for this specific evaluation instrument
        eval_instances <- all_data[
          !is.na(all_data$redcap_repeat_instrument) & 
            all_data$redcap_repeat_instrument == instrument_name, 
        ]
        
        cat("Evaluation instances found for", instrument_name, ":", nrow(eval_instances), "\n")
        
        if (nrow(eval_instances) > 0) {
          # Get all instance numbers
          instances <- as.numeric(eval_instances$redcap_repeat_instance)
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
    cat("⚠️ No existing", instrument_name, "instances found, starting with instance 1\n")
    return(1)
    
  }, error = function(e) {
    cat("❌ Error in get_next_eval_instance:", e$message, "\n")
    return(1)
  })
}
