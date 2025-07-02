# evaluation_form_builder.R - Functions to build evaluation forms dynamically from rdm_dict

# ============================================================================
# CORE EVALUATION FORM FUNCTIONS
# ============================================================================

# Build the universal plus/delta section (required for all evaluations)
build_plus_delta_section <- function() {
  tagList(
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Required Fields", class = "eval-section-title"),
            p("These fields must be completed before proceeding to the evaluation questions.", 
              class = "eval-section-description")
        ),
        
        div(class = "eval-form-grid",
            # Plus comments
            div(class = "eval-field-group",
                tags$label("Plus - What did the resident do well?", class = "eval-field-label required"),
                div(class = "textarea-container",
                    tags$textarea(
                      id = "ass_plus",
                      class = "form-control eval-textarea",
                      placeholder = "Describe specific strengths and positive observations...",
                      rows = "4"
                    ),
                    # Speech-to-text button
                    tags$button(
                      type = "button",
                      class = "btn btn-outline-secondary speech-btn",
                      onclick = "startSpeechRecognition('ass_plus')",
                      title = "Click to use voice input",
                      style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                      "🎤"
                    )
                ),
                div(class = "eval-field-help", 
                    "Provide specific, actionable feedback on what the resident excelled at. Click 🎤 for voice input.")
            ),
            
            # Delta comments  
            div(class = "eval-field-group",
                tags$label("Delta - Areas for improvement", class = "eval-field-label required"),
                div(class = "textarea-container",
                    tags$textarea(
                      id = "ass_delta", 
                      class = "form-control eval-textarea",
                      placeholder = "Describe specific opportunities for growth...",
                      rows = "4"
                    ),
                    # Speech-to-text button
                    tags$button(
                      type = "button",
                      class = "btn btn-outline-secondary speech-btn",
                      onclick = "startSpeechRecognition('ass_delta')",
                      title = "Click to use voice input",
                      style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                      "🎤"
                    )
                ),
                div(class = "eval-field-help",
                    "Provide constructive feedback on areas where the resident can improve. Click 🎤 for voice input.")
            )
        )
    )
  )
}

# Get assessment fields from rdm_dict for a specific evaluation type
get_assessment_fields <- function(eval_type) {
  if (!exists("rdm_dict") || is.null(rdm_dict)) {
    stop("rdm_dict not found. Please ensure the data dictionary is loaded.")
  }
  
  # Filter for assessment form and specific evaluation type
  pattern <- paste0("^ass_", eval_type, "_")
  
  assessment_fields <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(grepl(pattern, field_name)) %>%
    arrange(field_name)
  
  cat("Found", nrow(assessment_fields), "fields for evaluation type:", eval_type, "\n")
  if (nrow(assessment_fields) > 0) {
    cat("Fields found:", paste(assessment_fields$field_name, collapse = ", "), "\n")
  }
  
  return(assessment_fields)
}

# Build radio button field from data dictionary row
build_radio_field_from_dict <- function(field_row, required = FALSE) {
  field_id <- field_row$field_name
  field_label <- field_row$field_label
  choices_text <- field_row$select_choices_or_calculations
  
  cat("Building radio field:", field_id, "with choices:", choices_text, "\n")
  
  # Parse choices (format: "1, Choice 1 | 2, Choice 2 | 3, Choice 3")
  choices_list <- strsplit(choices_text, " \\| ")[[1]]
  choices <- list()
  
  for (choice in choices_list) {
    # Split on first comma to separate value from label
    parts <- strsplit(choice, ", ", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      value <- trimws(parts[1])
      label <- trimws(paste(parts[2:length(parts)], collapse = ", "))
      # Format: "1 - Choice text"
      display_label <- paste(value, "-", label)
      choices[[display_label]] <- value
    }
  }
  
  # Create label class
  label_class <- if (required) "eval-field-label required" else "eval-field-label"
  
  div(class = "eval-field-group",
      tags$label(field_label, class = label_class),
      radioButtons(
        inputId = field_id,
        label = NULL,
        choices = choices,
        selected = character(0),
        width = "100%",
        choiceNames = names(choices),
        choiceValues = unname(choices)
      )
  )
}

# Build dropdown field from data dictionary row
build_dropdown_field_from_dict <- function(field_row, required = FALSE) {
  field_id <- field_row$field_name
  field_label <- field_row$field_label
  choices_text <- field_row$select_choices_or_calculations
  
  cat("Building dropdown field:", field_id, "with choices:", choices_text, "\n")
  
  # Parse choices (format: "1, Choice 1 | 2, Choice 2")
  choices_list <- strsplit(choices_text, " \\| ")[[1]]
  choices <- list("Choose..." = "")
  
  for (choice in choices_list) {
    # Split on first comma to separate value from label
    parts <- strsplit(choice, ", ", fixed = TRUE)[[1]]
    if (length(parts) >= 2) {
      value <- trimws(parts[1])
      label <- trimws(paste(parts[2:length(parts)], collapse = ", "))
      choices[[label]] <- value
    }
  }
  
  # Create label class
  label_class <- if (required) "eval-field-label required" else "eval-field-label"
  
  div(class = "eval-field-group",
      tags$label(field_label, class = label_class),
      selectInput(
        field_id,
        label = NULL,
        choices = choices,
        selected = "",
        width = "100%"
      )
  )
}

# Build field based on field type from data dictionary
build_field_from_dict <- function(field_row, required = FALSE) {
  field_type <- field_row$field_type
  
  if (field_type == "radio") {
    return(build_radio_field_from_dict(field_row, required))
  } else if (field_type == "dropdown") {
    return(build_dropdown_field_from_dict(field_row, required))
  } else {
    # For other field types, create a basic text input
    div(class = "eval-field-group",
        tags$label(field_row$field_label, class = if (required) "eval-field-label required" else "eval-field-label"),
        textInput(
          field_row$field_name,
          label = NULL,
          placeholder = paste("Enter", tolower(field_row$field_label), "...")
        )
    )
  }
}

# ============================================================================
# SINGLE DAY CLINIC EVALUATION FORM (Two-Step with Plus/Delta First)
# ============================================================================

build_single_day_clinic_form <- function() {
  # Get single day clinic fields from data dictionary
  day_fields <- get_assessment_fields("day")
  
  # Also get the professionalism field (ass_cons_prof)
  prof_field <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name == "ass_cons_prof")
  
  if (nrow(day_fields) == 0 && nrow(prof_field) == 0) {
    return(div(
      class = "text-center",
      style = "padding: 2rem;",
      h5("Configuration Error", style = "color: #dc3545;"),
      p("No single day clinic fields found in data dictionary.")
    ))
  }
  
  # Combine fields
  all_fields <- rbind(day_fields, prof_field)
  
  cat("Building single day clinic form with", nrow(all_fields), "fields\n")
  
  # Build form fields dynamically for assessment questions
  assessment_fields <- lapply(1:nrow(all_fields), function(i) {
    field_row <- all_fields[i, ]
    cat("Building field", i, ":", field_row$field_name, "\n")
    build_field_from_dict(field_row, required = TRUE)
  })
  
  tagList(
    # Step 1: Plus/Delta section (always visible)
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Step 1: Required Feedback", class = "eval-section-title"),
            p("Please complete the Plus and Delta comments before proceeding to the evaluation questions.", 
              class = "eval-section-description")
        ),
        
        div(class = "eval-form-grid",
            # Plus comments
            div(class = "eval-field-group",
                tags$label("Plus - What did the resident do well?", class = "eval-field-label required"),
                div(class = "textarea-container",
                    tags$textarea(
                      id = "ass_plus",
                      class = "form-control eval-textarea",
                      placeholder = "Describe specific strengths and positive observations...",
                      rows = "4",
                      onchange = "checkPlusDeltaComplete()"
                    ),
                    # Speech-to-text button (only show if HTTPS)
                    conditionalPanel(
                      condition = "window.location.protocol === 'https:'",
                      tags$button(
                        type = "button",
                        class = "btn btn-outline-secondary speech-btn",
                        onclick = "startSpeechRecognition('ass_plus')",
                        title = "Click to use voice input",
                        style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                        "🎤"
                      )
                    )
                ),
                div(class = "eval-field-help", 
                    "Provide specific, actionable feedback on what the resident excelled at.",
                    if (TRUE) " Voice input available on HTTPS sites with supported browsers." else "")
            ),
            
            # Delta comments  
            div(class = "eval-field-group",
                tags$label("Delta - Areas for improvement", class = "eval-field-label required"),
                div(class = "textarea-container",
                    tags$textarea(
                      id = "ass_delta", 
                      class = "form-control eval-textarea",
                      placeholder = "Describe specific opportunities for growth...",
                      rows = "4",
                      onchange = "checkPlusDeltaComplete()"
                    ),
                    # Speech-to-text button (only show if HTTPS)
                    conditionalPanel(
                      condition = "window.location.protocol === 'https:'",
                      tags$button(
                        type = "button",
                        class = "btn btn-outline-secondary speech-btn",
                        onclick = "startSpeechRecognition('ass_delta')",
                        title = "Click to use voice input",
                        style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                        "🎤"
                      )
                    )
                ),
                div(class = "eval-field-help",
                    "Provide constructive feedback on areas where the resident can improve.",
                    if (TRUE) " Voice input available on HTTPS sites with supported browsers." else "")
            )
        ),
        
        # Continue button
        div(class = "text-center mt-3",
            actionButton("continue_to_assessment", "Continue to Assessment Questions →", 
                         class = "btn btn-primary btn-lg", style = "display: none;")
        )
    ),
    
    # Step 2: Assessment questions (initially hidden)
    div(id = "assessment_questions_section", style = "display: none;",
        div(class = "eval-section",
            div(class = "eval-section-header",
                h4("Step 2: Single Day Clinic Assessment", class = "eval-section-title"),
                p("Rate the resident's performance in the following areas:", 
                  class = "eval-section-description")
            ),
            
            div(class = "eval-questions-container",
                assessment_fields
            )
        )
    ),
    
    # Form controls (initially hidden, shown after plus/delta complete)
    div(id = "form_controls_section", class = "eval-form-controls", style = "display: none;",
        div(class = "text-center mt-4",
            actionButton("back_to_eval_selection", "← Back to Evaluation Types", 
                         class = "btn btn-secondary me-2"),
            actionButton("submit_single_day_evaluation", "Submit Evaluation", 
                         class = "btn btn-success btn-lg")
        )
    ),
    
    # JavaScript for progressive disclosure
    tags$script(HTML("
      // Check if Plus and Delta are both completed
      function checkPlusDeltaComplete() {
        var plusValue = $('#ass_plus').val();
        var deltaValue = $('#ass_delta').val();
        
        if (plusValue && plusValue.trim() && deltaValue && deltaValue.trim()) {
          $('#continue_to_assessment').show();
          // Auto-scroll to continue button
          setTimeout(function() {
            $('#continue_to_assessment')[0].scrollIntoView({ 
              behavior: 'smooth', 
              block: 'center' 
            });
          }, 100);
        } else {
          $('#continue_to_assessment').hide();
          $('#assessment_questions_section').hide();
          $('#form_controls_section').hide();
        }
      }
      
      // Handle continue button click
      $(document).on('click', '#continue_to_assessment', function() {
        $('#assessment_questions_section').slideDown(500);
        $('#form_controls_section').slideDown(500);
        $('#continue_to_assessment').hide();
        
        // Scroll to assessment questions
        setTimeout(function() {
          $('#assessment_questions_section')[0].scrollIntoView({ 
            behavior: 'smooth', 
            block: 'start' 
          });
        }, 600);
      });
      
      // Check on page load and input events
      $(document).ready(function() {
        $('#ass_plus, #ass_delta').on('input paste keyup', function() {
          setTimeout(checkPlusDeltaComplete, 100);
        });
        
        checkPlusDeltaComplete();
      });
    "))
  )
}

# ============================================================================
# FORM VALIDATION FUNCTIONS (Dynamic)
# ============================================================================

# Validate plus/delta section
validate_plus_delta <- function(input) {
  missing_fields <- character(0)
  
  if (is.null(input$ass_plus) || trimws(input$ass_plus) == "") {
    missing_fields <- c(missing_fields, "Plus comments")
  }
  
  if (is.null(input$ass_delta) || trimws(input$ass_delta) == "") {
    missing_fields <- c(missing_fields, "Delta comments")
  }
  
  return(missing_fields)
}

# Validate single day clinic form (dynamic)
validate_single_day_clinic_form <- function(input) {
  missing_fields <- validate_plus_delta(input)
  
  # Get single day clinic fields from data dictionary
  day_fields <- get_assessment_fields("day")
  
  # Also check professionalism field
  prof_field <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name == "ass_cons_prof")
  
  all_fields <- rbind(day_fields, prof_field)
  
  cat("Validating", nrow(all_fields), "assessment fields\n")
  
  # Check each field dynamically
  for (i in 1:nrow(all_fields)) {
    field_name <- all_fields$field_name[i]
    field_label <- all_fields$field_label[i]
    
    # Debug what we're checking
    field_value <- input[[field_name]]
    cat("Checking field:", field_name, "Value:", 
        ifelse(is.null(field_value), "NULL", 
               ifelse(field_value == "", "EMPTY", field_value)), "\n")
    
    # Check if field is missing or empty
    if (is.null(field_value) || length(field_value) == 0 || field_value == "" || is.na(field_value)) {
      missing_fields <- c(missing_fields, field_label)
      cat("Field", field_name, "is missing or empty\n")
    } else {
      cat("Field", field_name, "has value:", field_value, "\n")
    }
  }
  
  cat("Total missing fields:", length(missing_fields), "\n")
  if (length(missing_fields) > 0) {
    cat("Missing fields:", paste(missing_fields, collapse = ", "), "\n")
  }
  
  return(missing_fields)
}

# ============================================================================
# DATA COLLECTION FUNCTIONS (Dynamic)
# ============================================================================

# ============================================================================
# DATA COLLECTION FUNCTIONS (Dynamic)
# ============================================================================

# Collect single day clinic evaluation data (dynamic)
collect_single_day_clinic_data <- function(input, faculty, resident) {
  # Start with universal fields
  eval_data <- list(
    # Universal fields (auto-populated)
    ass_date = format(Sys.Date(), "%Y-%m-%d"),
    ass_faculty = faculty$fac_name,
    ass_specialty = faculty$fac_div,
    
    # Plus/Delta (user input)
    ass_plus = trimws(input$ass_plus),
    ass_delta = trimws(input$ass_delta)
  )
  
  # Get single day clinic fields from data dictionary
  day_fields <- get_assessment_fields("day")
  
  # Also get professionalism field
  prof_field <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name == "ass_cons_prof")
  
  all_fields <- rbind(day_fields, prof_field)
  
  # Add dynamic fields
  for (i in 1:nrow(all_fields)) {
    field_name <- all_fields$field_name[i]
    eval_data[[field_name]] <- input[[field_name]]
    cat("Added field", field_name, "with value:", input[[field_name]], "\n")
  }
  
  return(eval_data)
}

# ============================================================================
# SUBMISSION FUNCTIONS
# ============================================================================

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
