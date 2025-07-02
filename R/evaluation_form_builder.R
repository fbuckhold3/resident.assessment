# evaluation_form_builder.R - Complete version with all evaluation types

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
        width = "100%"
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
  
  build_progressive_disclosure_form(
    form_title = "Single Day Clinic Assessment",
    assessment_fields = assessment_fields,
    submit_button_id = "submit_single_day_evaluation"
  )
}

# ============================================================================
# CONSULTATION EVALUATION FORM
# ============================================================================

build_consultation_form <- function() {
  # Get consultation fields: ass_cons_prof, ass_cons_care, plan_pc3_r1, ass_cons_testing_mk3, ass_cons_comm_ics2_r1, ass_cons_sdh_sbp2_r2
  cons_fields <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name %in% c("ass_cons_prof", "ass_cons_care", "plan_pc3_r1", 
                             "ass_cons_testing_mk3", "ass_cons_comm_ics2_r1", "ass_cons_sdh_sbp2_r2")) %>%
    arrange(field_name)
  
  if (nrow(cons_fields) == 0) {
    return(div(
      class = "text-center",
      style = "padding: 2rem;",
      h5("Configuration Error", style = "color: #dc3545;"),
      p("No consultation fields found in data dictionary.")
    ))
  }
  
  cat("Building consultation form with", nrow(cons_fields), "fields\n")
  
  # Build form fields dynamically
  assessment_fields <- lapply(1:nrow(cons_fields), function(i) {
    field_row <- cons_fields[i, ]
    cat("Building field", i, ":", field_row$field_name, "\n")
    build_field_from_dict(field_row, required = TRUE)
  })
  
  build_progressive_disclosure_form(
    form_title = "Consultation Assessment",
    assessment_fields = assessment_fields,
    submit_button_id = "submit_consultation_evaluation"
  )
}

# ============================================================================
# BRIDGE CLINIC EVALUATION FORM
# ============================================================================

build_bridge_clinic_form <- function() {
  # Get bridge fields: ass_bridge_sbp2_r2, ass_bridge_pc5_r3, ass_bridge_sbp3_r2
  bridge_fields <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name %in% c("ass_bridge_sbp2_r2", "ass_bridge_pc5_r3", "ass_bridge_sbp3_r2")) %>%
    arrange(field_name)
  
  if (nrow(bridge_fields) == 0) {
    return(div(
      class = "text-center",
      style = "padding: 2rem;",
      h5("Configuration Error", style = "color: #dc3545;"),
      p("No bridge clinic fields found in data dictionary.")
    ))
  }
  
  cat("Building bridge clinic form with", nrow(bridge_fields), "fields\n")
  
  # Build form fields dynamically
  assessment_fields <- lapply(1:nrow(bridge_fields), function(i) {
    field_row <- bridge_fields[i, ]
    cat("Building field", i, ":", field_row$field_name, "\n")
    build_field_from_dict(field_row, required = TRUE)
  })
  
  build_progressive_disclosure_form(
    form_title = "Bridge Clinic Assessment",
    assessment_fields = assessment_fields,
    submit_button_id = "submit_bridge_evaluation"
  )
}

# ============================================================================
# INTERN INPATIENT EVALUATION FORM
# ============================================================================

build_intern_inpatient_form <- function() {
  # Get intern inpatient fields: ass_int_ip_pc4_r1, ass_int_ip_pc4_r2, ass_int_ip_pc1_r1, ass_int_ip_pc3_r1, 
  # ass_int_ip_mk1, ass_int_ip_sbp2_r1, ass_int_ip_ics3_r2, ass_int_ip_pbl2_r2, ass_int_ip_beside
  int_ip_fields <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name %in% c("ass_int_ip_pc4_r1", "ass_int_ip_pc4_r2", "ass_int_ip_pc1_r1", 
                             "ass_int_ip_pc3_r1", "ass_int_ip_mk1", "ass_int_ip_sbp2_r1", 
                             "ass_int_ip_ics3_r2", "ass_int_ip_pbl2_r2", "ass_int_ip_beside")) %>%
    arrange(field_name)
  
  if (nrow(int_ip_fields) == 0) {
    return(div(
      class = "text-center",
      style = "padding: 2rem;",
      h5("Configuration Error", style = "color: #dc3545;"),
      p("No intern inpatient fields found in data dictionary.")
    ))
  }
  
  cat("Building intern inpatient form with", nrow(int_ip_fields), "fields\n")
  
  # Build form fields dynamically
  assessment_fields <- lapply(1:nrow(int_ip_fields), function(i) {
    field_row <- int_ip_fields[i, ]
    cat("Building field", i, ":", field_row$field_name, "\n")
    build_field_from_dict(field_row, required = TRUE)
  })
  
  build_progressive_disclosure_form(
    form_title = "Intern Inpatient Assessment",
    assessment_fields = assessment_fields,
    submit_button_id = "submit_intern_inpatient_evaluation"
  )
}

# ============================================================================
# SENIOR INPATIENT EVALUATION FORM
# ============================================================================

build_senior_inpatient_form <- function() {
  # Get senior inpatient fields: ass_res_ip_pc4_r2, ass_res_ip_ics2_r1, ass_res_ip_mk1, 
  # ass_res_ip_sbp3_r1, ass_res_ip_pc4_r1, ass_res_ip_sbp3_r2
  res_ip_fields <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name %in% c("ass_res_ip_pc4_r2", "ass_res_ip_ics2_r1", "ass_res_ip_mk1", 
                             "ass_res_ip_sbp3_r1", "ass_res_ip_pc4_r1", "ass_res_ip_sbp3_r2")) %>%
    arrange(field_name)
  
  if (nrow(res_ip_fields) == 0) {
    return(div(
      class = "text-center",
      style = "padding: 2rem;",
      h5("Configuration Error", style = "color: #dc3545;"),
      p("No senior inpatient fields found in data dictionary.")
    ))
  }
  
  cat("Building senior inpatient form with", nrow(res_ip_fields), "fields\n")
  
  # Build form fields dynamically
  assessment_fields <- lapply(1:nrow(res_ip_fields), function(i) {
    field_row <- res_ip_fields[i, ]
    cat("Building field", i, ":", field_row$field_name, "\n")
    build_field_from_dict(field_row, required = TRUE)
  })
  
  build_progressive_disclosure_form(
    form_title = "Senior Inpatient Assessment",
    assessment_fields = assessment_fields,
    submit_button_id = "submit_senior_inpatient_evaluation"
  )
}

# ============================================================================
# GENERIC PROGRESSIVE DISCLOSURE FORM BUILDER
# ============================================================================

build_progressive_disclosure_form <- function(form_title, assessment_fields, submit_button_id) {
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
                h4(paste("Step 2:", form_title), class = "eval-section-title"),
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
            actionButton(submit_button_id, "Submit Evaluation", 
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

# Generic validation function for any evaluation type
validate_evaluation_form <- function(input, eval_type, field_names) {
  missing_fields <- validate_plus_delta(input)
  
  cat("Validating", length(field_names), "assessment fields for", eval_type, "\n")
  
  # Check each field dynamically
  for (field_name in field_names) {
    # Debug what we're checking
    field_value <- input[[field_name]]
    cat("Checking field:", field_name, "Value:", 
        ifelse(is.null(field_value), "NULL", 
               ifelse(field_value == "", "EMPTY", field_value)), "\n")
    
    # Check if field is missing or empty
    if (is.null(field_value) || length(field_value) == 0 || field_value == "" || is.na(field_value)) {
      # Get field label from data dictionary for better error message
      field_info <- rdm_dict %>%
        filter(field_name == !!field_name) %>%
        slice(1)
      
      field_label <- if (nrow(field_info) > 0) field_info$field_label else field_name
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

# Specific validation functions for each evaluation type
validate_single_day_clinic_form <- function(input) {
  field_names <- c("ass_cons_prof")  # Add day-specific fields when they exist
  return(validate_evaluation_form(input, "day", field_names))
}

validate_consultation_form <- function(input) {
  field_names <- c("ass_cons_prof", "ass_cons_care", "plan_pc3_r1", 
                   "ass_cons_testing_mk3", "ass_cons_comm_ics2_r1", "ass_cons_sdh_sbp2_r2")
  return(validate_evaluation_form(input, "cons", field_names))
}

validate_bridge_clinic_form <- function(input) {
  field_names <- c("ass_bridge_sbp2_r2", "ass_bridge_pc5_r3", "ass_bridge_sbp3_r2")
  return(validate_evaluation_form(input, "bridge", field_names))
}

validate_intern_inpatient_form <- function(input) {
  field_names <- c("ass_int_ip_pc4_r1", "ass_int_ip_pc4_r2", "ass_int_ip_pc1_r1", 
                   "ass_int_ip_pc3_r1", "ass_int_ip_mk1", "ass_int_ip_sbp2_r1", 
                   "ass_int_ip_ics3_r2", "ass_int_ip_pbl2_r2", "ass_int_ip_beside")
  return(validate_evaluation_form(input, "int_ip", field_names))
}

validate_senior_inpatient_form <- function(input) {
  field_names <- c("ass_res_ip_pc4_r2", "ass_res_ip_ics2_r1", "ass_res_ip_mk1", 
                   "ass_res_ip_sbp3_r1", "ass_res_ip_pc4_r1", "ass_res_ip_sbp3_r2")
  return(validate_evaluation_form(input, "res_ip", field_names))
}

# ============================================================================
# DATA COLLECTION FUNCTIONS (Dynamic)
# ============================================================================

# Generic data collection function
collect_evaluation_data <- function(input, faculty, resident, field_names) {
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
  
  # Add dynamic fields
  for (field_name in field_names) {
    eval_data[[field_name]] <- input[[field_name]]
    cat("Added field", field_name, "with value:", input[[field_name]], "\n")
  }
  
  return(eval_data)
}

# Specific data collection functions for each evaluation type
collect_single_day_clinic_data <- function(input, faculty, resident) {
  field_names <- c("ass_cons_prof")  # Add day-specific fields when they exist
  return(collect_evaluation_data(input, faculty, resident, field_names))
}

collect_consultation_data <- function(input, faculty, resident) {
  field_names <- c("ass_cons_prof", "ass_cons_care", "plan_pc3_r1", 
                   "ass_cons_testing_mk3", "ass_cons_comm_ics2_r1", "ass_cons_sdh_sbp2_r2")
  return(collect_evaluation_data(input, faculty, resident, field_names))
}

collect_bridge_clinic_data <- function(input, faculty, resident) {
  field_names <- c("ass_bridge_sbp2_r2", "ass_bridge_pc5_r3", "ass_bridge_sbp3_r2")
  return(collect_evaluation_data(input, faculty, resident, field_names))
}

collect_intern_inpatient_data <- function(input, faculty, resident) {
  field_names <- c("ass_int_ip_pc4_r1", "ass_int_ip_pc4_r2", "ass_int_ip_pc1_r1", 
                   "ass_int_ip_pc3_r1", "ass_int_ip_mk1", "ass_int_ip_sbp2_r1", 
                   "ass_int_ip_ics3_r2", "ass_int_ip_pbl2_r2", "ass_int_ip_beside")
  return(collect_evaluation_data(input, faculty, resident, field_names))
}

collect_senior_inpatient_data <- function(input, faculty, resident) {
  field_names <- c("ass_res_ip_pc4_r2", "ass_res_ip_ics2_r1", "ass_res_ip_mk1", 
                   "ass_res_ip_sbp3_r1", "ass_res_ip_pc4_r1", "ass_res_ip_sbp3_r2")
  return(collect_evaluation_data(input, faculty, resident, field_names))
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

# Continuity Clinic Evaluation Form Builder
# This handles the complex logic for quarter and resident level-specific evaluations

# ============================================================================
# CONTINUITY CLINIC FORM BUILDER
# ============================================================================

build_continuity_clinic_form <- function() {
  tagList(
    # Step 1: Quarter Selection via Clickable Table (always visible)
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Step 1: Select Quarter", class = "eval-section-title"),
            p("Click on any quarter below to start that evaluation:", 
              class = "eval-section-description")
        ),
        
        # Completion status table (clickable)
        uiOutput("quarter_selection_table")
    ),
    
    # Step 2: Plus/Delta section (visible after quarter selection)
    conditionalPanel(
      condition = "input.ass_cc_quart != null && input.ass_cc_quart != ''",
      div(class = "eval-section",
          div(class = "eval-section-header",
              h4("Step 2: Required Feedback", class = "eval-section-title"),
              uiOutput("cc_quarter_subtitle")  # Dynamic subtitle based on selected quarter
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
                      "Provide specific, actionable feedback on what the resident excelled at.")
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
                      "Provide constructive feedback on areas where the resident can improve.")
              )
          )
      )
    ),
    
    # Step 3: Dynamic Assessment Questions (visible after quarter selection)
    conditionalPanel(
      condition = "input.ass_cc_quart != null && input.ass_cc_quart != ''",
      div(class = "eval-section",
          div(class = "eval-section-header",
              h4("Step 3: Assessment Questions", class = "eval-section-title"),
              p("Rate the resident's performance in the following areas:", 
                class = "eval-section-description")
          ),
          
          div(class = "eval-questions-container",
              uiOutput("cc_dynamic_questions")
          )
      )
    ),
    
    # Form controls (visible after quarter selection)
    conditionalPanel(
      condition = "input.ass_cc_quart != null && input.ass_cc_quart != ''",
      div(class = "eval-form-controls",
          div(class = "text-center mt-4",
              actionButton("back_to_eval_selection", "← Back to Evaluation Types", 
                           class = "btn btn-secondary me-2"),
              actionButton("submit_continuity_clinic_evaluation", "Submit Evaluation", 
                           class = "btn btn-success btn-lg")
          )
      )
    ),
    
    # Hidden input to track selected quarter
    shinyjs::hidden(
      textInput("ass_cc_quart", "", value = "")
    )
  )
}


# ============================================================================
# DYNAMIC QUESTION BUILDERS BASED ON QUARTER AND LEVEL
# ============================================================================

get_cc_fields_for_quarter_and_level <- function(quarter, resident_level) {
  cat("Getting CC fields for quarter:", quarter, "resident level:", resident_level, "\n")
  
  field_names <- character(0)
  
  if (quarter == "1") {
    # 1st quarter - inbasket coverage (same for all levels)
    field_names <- c("ass_cc_inb_resp", "ass_cc_inb_resu", "ass_cc_inb_mych", 
                     "ass_cc_inb_comp", "ass_cc_inb_comm")
  } else if (quarter == "3") {
    # 3rd quarter - documentation (same for all levels)
    field_names <- c("ass_cc_doc_update", "ass_cc_doc_review", "ass_cc_doc_medall", 
                     "ass_cc_doc_remind", "ass_cc_doc_diag", "ass_cc_doc_notes", "ass_cc_doc_comp")
  } else if (quarter %in% c("2", "4")) {
    # 2nd and 4th quarter - semi-annual (different by level)
    if (resident_level == "Intern") {
      field_names <- c("ass_cc_int_semi_pc1", "ass_cc_int_semi_pc3", "ass_cc_int_semi_pc5",
                       "ass_cc_int_semi_ics1_r2", "ass_cc_int_semi_ics1_r1", "ass_cc_int_semi_prof1",
                       "ass_cc_int_semi_pbl2_r1", "ass_cc_int_semi_pc3_r2")
    } else if (resident_level == "PGY2") {
      field_names <- c("ass_cc_pgy2_semi_pc1", "ass_cc_pgy2_semi_ics3", "ass_cc_pgy2_semi_pc3_r2",
                       "ass_cc_pgy2_semi_sbp2", "ass_cc_pgy2_semi_pbl1", "ass_cc_pgy2_semi_ics1_r1",
                       "ass_cc_pgy2_semi_ics2", "ass_cc_pgy2_semi_pbl2_r1", "ass_cc_pgy2_semi_ics1_r2")
    } else if (resident_level == "PGY3") {
      field_names <- c("ass_cc_pgy3_semi_pc1", "ass_cc_pgy3_semi_ics2", "ass_cc_pgy3_semi_ics1_1",
                       "ass_cc_pgy3_semi_ics1_2", "ass_cc_pgy3_semi_sbp2", "ass_cc_pgy3_semi_pbl1",
                       "ass_cc_pgy3_semi_pbl2_r1", "ass_cc_pgy3_semi_pc3_r2")
    }
  }
  
  cat("Returning", length(field_names), "fields:", paste(field_names, collapse = ", "), "\n")
  return(field_names)
}

get_quarter_description <- function(quarter, resident_level) {
  descriptions <- list(
    "1" = "1st Quarter - In-Basket Coverage (Fall evaluation)",
    "2" = paste("2nd Quarter - Semi-Annual Evaluation (Winter) -", resident_level, "Level"),
    "3" = "3rd Quarter - Documentation Evaluation (Spring)", 
    "4" = paste("4th Quarter - Semi-Annual Evaluation (Summer) -", resident_level, "Level")
  )
  
  return(descriptions[[quarter]] %||% "Unknown Quarter")
}

# ============================================================================
# VALIDATION AND DATA COLLECTION FOR CONTINUITY CLINIC
# ============================================================================

validate_continuity_clinic_form <- function(input) {
  missing_fields <- validate_plus_delta(input)
  
  # Check quarter selection
  if (is.null(input$ass_cc_quart) || input$ass_cc_quart == "") {
    missing_fields <- c(missing_fields, "Quarter Selection")
  }
  
  # Get the resident level from values (this will need to be passed or accessed)
  # For now, we'll get it from the selected resident in the calling context
  
  # Check quarter-specific fields
  if (!is.null(input$ass_cc_quart) && input$ass_cc_quart != "") {
    # We need to determine resident level - this will be handled in the server
    # For now, we'll return the basic validation
  }
  
  return(missing_fields)
}

validate_continuity_clinic_with_level <- function(input, resident_level) {
  missing_fields <- validate_plus_delta(input)
  
  # Check quarter selection
  if (is.null(input$ass_cc_quart) || input$ass_cc_quart == "") {
    missing_fields <- c(missing_fields, "Quarter Selection")
    return(missing_fields)
  }
  
  # Get field names for this quarter and level
  field_names <- get_cc_fields_for_quarter_and_level(input$ass_cc_quart, resident_level)
  
  cat("Validating CC form for quarter", input$ass_cc_quart, "level", resident_level, "\n")
  cat("Checking fields:", paste(field_names, collapse = ", "), "\n")
  
  # Check each field
  for (field_name in field_names) {
    field_value <- input[[field_name]]
    cat("Checking field:", field_name, "Value:", 
        ifelse(is.null(field_value), "NULL", 
               ifelse(field_value == "", "EMPTY", field_value)), "\n")
    
    if (is.null(field_value) || length(field_value) == 0 || field_value == "" || is.na(field_value)) {
      # Get field label from data dictionary for better error message
      field_info <- rdm_dict %>%
        filter(field_name == !!field_name) %>%
        slice(1)
      
      field_label <- if (nrow(field_info) > 0) field_info$field_label else field_name
      missing_fields <- c(missing_fields, field_label)
      cat("Field", field_name, "is missing or empty\n")
    }
  }
  
  cat("Total missing fields:", length(missing_fields), "\n")
  return(missing_fields)
}


collect_continuity_clinic_data <- function(input, faculty, resident) {
  # Start with universal fields
  eval_data <- list(
    # Universal fields (auto-populated)
    ass_date = format(Sys.Date(), "%Y-%m-%d"),
    ass_faculty = faculty$fac_name,
    ass_specialty = faculty$fac_div,
    
    # Plus/Delta (user input)
    ass_plus = trimws(input$ass_plus),
    ass_delta = trimws(input$ass_delta),
    
    # Quarter selection
    ass_cc_quart = input$ass_cc_quart
  )
  
  # Add metadata about the selection if available
  if (exists("values") && !is.null(values$cc_quarter_selection_info)) {
    selection_info <- values$cc_quarter_selection_info
    if (selection_info$isCompleted) {
      cat("NOTE: This is a re-evaluation of a completed quarter\n")
      # You could add a field to track this if needed
      # eval_data$ass_cc_re_evaluation <- "1"
    }
  }
  
  # Get field names for this quarter and level
  field_names <- get_cc_fields_for_quarter_and_level(input$ass_cc_quart, resident$Level)
  
  # Add dynamic fields
  for (field_name in field_names) {
    eval_data[[field_name]] <- input[[field_name]]
    cat("Added CC field", field_name, "with value:", input[[field_name]], "\n")
  }
  
  return(eval_data)
}

# Add this to your evaluation_form_builder.R file

# ============================================================================
# OBSERVATION EVALUATION FORM BUILDER
# ============================================================================

build_observation_form <- function() {
  tagList(
    # Step 1: Observation Type Selection (always visible)
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Step 1: Select Observation Type", class = "eval-section-title"),
            p("Click on the type of observation you are evaluating:", 
              class = "eval-section-description")
        ),
        
        # Observation type buttons
        uiOutput("observation_type_selection")
    ),
    
    # Step 2: Plus/Delta section (visible after observation type selection)
    # REMOVED conditionalPanel - using JavaScript control instead
    div(id = "obs_plus_delta_section", style = "display: none;",
        div(class = "eval-section",
            div(class = "eval-section-header",
                h4("Step 2: Required Feedback", class = "eval-section-title"),
                uiOutput("obs_type_subtitle")
            ),
            
            div(class = "eval-form-grid",
                # Plus comments
                div(class = "eval-field-group",
                    tags$label("Plus - What did the resident do well?", class = "eval-field-label required"),
                    div(class = "textarea-container",
                        tags$textarea(
                          id = "ass_obs_plus",
                          class = "form-control eval-textarea",
                          placeholder = "Describe specific strengths and positive observations...",
                          rows = "4",
                          onchange = "checkObsPlusDeltaComplete()",
                          oninput = "checkObsPlusDeltaComplete()"
                        ),
                        # Speech-to-text button
                        conditionalPanel(
                          condition = "window.location.protocol === 'https:'",
                          tags$button(
                            type = "button",
                            class = "btn btn-outline-secondary speech-btn",
                            onclick = "startSpeechRecognition('ass_obs_plus')",
                            title = "Click to use voice input",
                            style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                            "🎤"
                          )
                        )
                    ),
                    div(class = "eval-field-help", 
                        "Provide specific, actionable feedback on what the resident excelled at.")
                ),
                
                # Delta comments  
                div(class = "eval-field-group",
                    tags$label("Delta - Areas for improvement", class = "eval-field-label required"),
                    div(class = "textarea-container",
                        tags$textarea(
                          id = "ass_obs_delta",
                          class = "form-control eval-textarea",
                          placeholder = "Describe specific opportunities for growth...",
                          rows = "4",
                          onchange = "checkObsPlusDeltaComplete()",
                          oninput = "checkObsPlusDeltaComplete()"
                        ),
                        # Speech-to-text button
                        conditionalPanel(
                          condition = "window.location.protocol === 'https:'",
                          tags$button(
                            type = "button",
                            class = "btn btn-outline-secondary speech-btn",
                            onclick = "startSpeechRecognition('ass_obs_delta')",
                            title = "Click to use voice input",
                            style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                            "🎤"
                          )
                        )
                    ),
                    div(class = "eval-field-help",
                        "Provide constructive feedback on areas where the resident can improve.")
                )
            ),
            
            # Continue button (initially hidden)
            div(class = "text-center mt-3",
                actionButton("continue_to_obs_assessment", "Continue to Assessment Questions →", 
                             class = "btn btn-primary btn-lg", style = "display: none;")
            )
        )
    ),
    
    # Step 3: Dynamic Assessment Questions (initially hidden)
    div(id = "obs_assessment_questions_section", style = "display: none;",
        div(class = "eval-section",
            div(class = "eval-section-header",
                h4("Step 3: Assessment Questions", class = "eval-section-title"),
                p("Rate the resident's performance for this observation type:", 
                  class = "eval-section-description")
            ),
            
            div(class = "eval-questions-container",
                uiOutput("obs_dynamic_questions")
            )
        )
    ),
    
    # Form controls (initially hidden, shown after plus/delta complete)
    div(id = "obs_form_controls_section", class = "eval-form-controls", style = "display: none;",
        div(class = "text-center mt-4",
            actionButton("back_to_eval_selection", "← Back to Evaluation Types", 
                         class = "btn btn-secondary me-2"),
            actionButton("submit_observation_evaluation", "Submit Evaluation", 
                         class = "btn btn-success btn-lg")
        )
    ),
    
    # Hidden input to track selected observation type
    shinyjs::hidden(
      textInput("ass_obs_type", "", value = "")
    ),
    
    # Updated JavaScript for progressive disclosure
    tags$script(HTML("
      // Show plus/delta section when observation type is selected
      function showObsPlusDeltaSection() {
        $('#obs_plus_delta_section').slideDown(500);
        // Scroll to the section
        setTimeout(function() {
          $('#obs_plus_delta_section')[0].scrollIntoView({ 
            behavior: 'smooth', 
            block: 'start' 
          });
        }, 600);
      }
      
      // Check if Plus and Delta are both completed for observations
      function checkObsPlusDeltaComplete() {
        var plusValue = $('#ass_obs_plus').val();
        var deltaValue = $('#ass_obs_delta').val();
        
        console.log('Checking obs plus/delta:', plusValue ? 'has plus' : 'no plus', deltaValue ? 'has delta' : 'no delta');
        
        if (plusValue && plusValue.trim() && deltaValue && deltaValue.trim()) {
          $('#continue_to_obs_assessment').show();
          console.log('Showing continue button');
          // Auto-scroll to continue button
          setTimeout(function() {
            $('#continue_to_obs_assessment')[0].scrollIntoView({ 
              behavior: 'smooth', 
              block: 'center' 
            });
          }, 100);
        } else {
          $('#continue_to_obs_assessment').hide();
          $('#obs_assessment_questions_section').hide();
          $('#obs_form_controls_section').hide();
          console.log('Hiding continue button and questions');
        }
      }
      
      // Handle continue button click for observations
      $(document).on('click', '#continue_to_obs_assessment', function(e) {
        console.log('Continue button clicked for observations');
        e.preventDefault();
        
        $('#obs_assessment_questions_section').slideDown(500);
        $('#obs_form_controls_section').slideDown(500);
        $('#continue_to_obs_assessment').hide();
        
        console.log('Showing obs assessment questions and form controls');
        
        // Scroll to assessment questions
        setTimeout(function() {
          var questionsSection = $('#obs_assessment_questions_section')[0];
          if (questionsSection) {
            questionsSection.scrollIntoView({ 
              behavior: 'smooth', 
              block: 'start' 
            });
            console.log('Scrolled to questions section');
          }
        }, 600);
      });
      
      // Function called when observation type is selected
      function selectObservationType(typeId, typeName) {
        console.log('Selecting observation type:', typeId, typeName);
        
        // Set the observation type value
        Shiny.setInputValue('ass_obs_type', typeId, {priority: 'event'});
        
        // Store selection info for the server
        Shiny.setInputValue('obs_type_manually_selected', {
          type: typeId,
          name: typeName,
          timestamp: new Date().getTime()
        }, {priority: 'event'});
        
        // Visual feedback
        document.querySelectorAll('.obs-type-button').forEach(function(el) {
          el.classList.remove('selected');
        });
        
        // Highlight selected type
        event.currentTarget.classList.add('selected');
        
        // Show the plus/delta section
        showObsPlusDeltaSection();
      }
      
      // Bind events to observation textareas when they're created
      function bindObsEvents() {
        $('#ass_obs_plus, #ass_obs_delta').off('input paste keyup change').on('input paste keyup change', function() {
          console.log('Obs Plus/Delta changed:', $(this).attr('id'), $(this).val().length);
          setTimeout(checkObsPlusDeltaComplete, 100);
        });
      }
      
      // Initialize when page loads and when content updates
      $(document).ready(function() {
        console.log('Document ready - setting up obs events');
        setTimeout(function() {
          bindObsEvents();
          checkObsPlusDeltaComplete();
        }, 500);
      });
      
      // Re-bind events when Shiny updates content
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'ass_obs_type') {
          console.log('Observation type changed, re-binding events');
          setTimeout(function() {
            bindObsEvents();
            checkObsPlusDeltaComplete();
          }, 1000);
        }
      });
    "))
  )
}

# ============================================================================
# OBSERVATION TYPE SELECTION BUILDER
# ============================================================================

# Simple, reliable observation form - replace your build_observation_form() function:

build_observation_form <- function() {
  tagList(
    # Step 1: Observation Type Selection (always visible)
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Step 1: Select Observation Type", class = "eval-section-title"),
            p("Click on the type of observation you are evaluating:", 
              class = "eval-section-description")
        ),
        
        # Observation type buttons
        uiOutput("observation_type_selection")
    ),
    
    # Step 2: Plus/Delta section (shows when observation type is selected)
    conditionalPanel(
      condition = "input.ass_obs_type != null && input.ass_obs_type != ''",
      div(class = "eval-section",
          div(class = "eval-section-header",
              h4("Step 2: Required Feedback", class = "eval-section-title"),
              uiOutput("obs_type_subtitle")
          ),
          
          div(class = "eval-form-grid",
              # Plus comments
              div(class = "eval-field-group",
                  tags$label("Plus - What did the resident do well?", class = "eval-field-label required"),
                  tags$textarea(
                    id = "ass_obs_plus",
                    class = "form-control eval-textarea",
                    placeholder = "Describe specific strengths and positive observations...",
                    rows = "4"
                  ),
                  div(class = "eval-field-help", 
                      "Provide specific, actionable feedback on what the resident excelled at.")
              ),
              
              # Delta comments  
              div(class = "eval-field-group",
                  tags$label("Delta - Areas for improvement", class = "eval-field-label required"),
                  tags$textarea(
                    id = "ass_obs_delta",
                    class = "form-control eval-textarea",
                    placeholder = "Describe specific opportunities for growth...",
                    rows = "4"
                  ),
                  div(class = "eval-field-help",
                      "Provide constructive feedback on areas where the resident can improve.")
              )
          )
      )
    ),
    
    # Step 3: Assessment Questions (shows when observation type is selected)
    conditionalPanel(
      condition = "input.ass_obs_type != null && input.ass_obs_type != ''",
      div(class = "eval-section",
          div(class = "eval-section-header",
              h4("Step 3: Assessment Questions", class = "eval-section-title"),
              p("Rate the resident's performance for this observation type:", 
                class = "eval-section-description")
          ),
          
          div(class = "eval-questions-container",
              uiOutput("obs_dynamic_questions")
          )
      )
    ),
    
    # Form controls (shows when observation type is selected)
    conditionalPanel(
      condition = "input.ass_obs_type != null && input.ass_obs_type != ''",
      div(class = "eval-form-controls",
          div(class = "text-center mt-4",
              actionButton("back_to_eval_selection", "← Back to Evaluation Types", 
                           class = "btn btn-secondary me-2"),
              actionButton("submit_observation_evaluation", "Submit Evaluation", 
                           class = "btn btn-success btn-lg")
          )
      )
    ),
    
    # Hidden input to track selected observation type
    shinyjs::hidden(
      textInput("ass_obs_type", "", value = "")
    ),
    
    # Simple JavaScript for observation type selection
    tags$script(HTML("
      function selectObservationType(typeId, typeName) {
        console.log('Selecting observation type:', typeId, typeName);
        
        // Set the observation type value
        Shiny.setInputValue('ass_obs_type', typeId, {priority: 'event'});
        
        // Store selection info for the server
        Shiny.setInputValue('obs_type_manually_selected', {
          type: typeId,
          name: typeName,
          timestamp: new Date().getTime()
        }, {priority: 'event'});
        
        // Visual feedback
        document.querySelectorAll('.obs-type-button').forEach(function(el) {
          el.classList.remove('selected');
        });
        
        // Highlight selected type
        if (event && event.currentTarget) {
          event.currentTarget.classList.add('selected');
        }
        
        // Scroll to next section after a brief delay
        setTimeout(function() {
          var nextSection = document.querySelector('.eval-section:nth-child(2)');
          if (nextSection) {
            nextSection.scrollIntoView({ behavior: 'smooth', block: 'start' });
          }
        }, 200);
      }
    "))
  )
}

# Also, let's update the observation type buttons to prevent multiple clicks:

build_observation_type_buttons <- function(resident_level) {
  # Define observation types with metadata
  observation_types <- list(
    "1" = list(
      id = "1",
      name = "Clinical Decision Making",
      icon = "🧠",
      description = "Observation of clinical reasoning and decision-making process",
      restricted = FALSE
    ),
    "2" = list(
      id = "2", 
      name = "Advance Care Planning",
      icon = "📋",
      description = "Discussion of goals of care and advance directives",
      restricted = FALSE
    ),
    "3" = list(
      id = "3",
      name = "Educational Session", 
      icon = "📚",
      description = "Teaching or learning activity observation",
      restricted = FALSE
    ),
    "4" = list(
      id = "4",
      name = "Physical Exam",
      icon = "🩺", 
      description = "Observation of physical examination skills",
      restricted = FALSE
    ),
    "5" = list(
      id = "5",
      name = "Presentation",
      icon = "🗣️",
      description = "Oral presentation of patient case",
      restricted = FALSE
    ),
    "6" = list(
      id = "6",
      name = "Written H&P",
      icon = "📝",
      description = "History and physical documentation",
      restricted = FALSE
    ),
    "7" = list(
      id = "7", 
      name = "Daily Notes",
      icon = "📄",
      description = "Progress note documentation",
      restricted = FALSE
    ),
    "8" = list(
      id = "8",
      name = "Patient Discharge",
      icon = "🏠",
      description = "Discharge planning and execution", 
      restricted = FALSE
    ),
    "9" = list(
      id = "9",
      name = "Patient/Family Counseling",
      icon = "👥",
      description = "Communication with patients and families",
      restricted = FALSE
    ),
    "10" = list(
      id = "10",
      name = "Supervision of Intern",
      icon = "👨‍🏫", 
      description = "Senior resident supervising intern or acting intern",
      restricted = TRUE  # Senior residents only
    ),
    "11" = list(
      id = "11",
      name = "Procedure",
      icon = "⚕️",
      description = "Performance of medical procedures",
      restricted = FALSE
    ),
    "12" = list(
      id = "12",
      name = "Multi-D Rounds", 
      icon = "🏥",
      description = "Multidisciplinary team rounds participation",
      restricted = FALSE
    ),
    "13" = list(
      id = "13",
      name = "Emergency Condition",
      icon = "🚨",
      description = "Rapid response or code blue situation",
      restricted = FALSE
    )
  )
  
  # Filter based on resident level (remove supervision for interns)
  if (resident_level == "Intern") {
    observation_types <- observation_types[names(observation_types) != "10"]
  }
  
  div(
    class = "obs-type-selection",
    style = "margin: 1.5rem 0;",
    div(class = "obs-type-container",
        lapply(observation_types, function(obs_type) {
          is_available <- !obs_type$restricted || resident_level != "Intern"
          
          button_class <- if (is_available) {
            "obs-type-button"
          } else {
            "obs-type-button disabled"
          }
          
          div(
            class = button_class,
            onclick = if (is_available) paste0("selectObservationType('", obs_type$id, "', '", obs_type$name, "');") else NULL,
            style = if (is_available) "cursor: pointer;" else NULL,
            div(class = "obs-type-icon", obs_type$icon),
            div(class = "obs-type-title", obs_type$name),
            div(class = "obs-type-description", obs_type$description),
            if (obs_type$restricted && resident_level == "Intern") {
              div(class = "obs-type-restriction", "Senior Residents Only")
            }
          )
        })
    )
  )
}

# ============================================================================
# DYNAMIC QUESTIONS BASED ON OBSERVATION TYPE
# ============================================================================

# Fix the get_obs_fields_for_type function in evaluation_form_builder.R

get_obs_fields_for_type <- function(obs_type) {
  cat("Getting observation fields for type:", obs_type, "\n")
  
  field_mapping <- list(
    "1" = c("ass_obs_cdm"),  # Clinical Decision Making - text field
    "2" = c("ass_obs_acp"),  # Advance Care Planning
    "3" = c("ass_educat"),   # Educational Session - FIXED: removed "obs_" prefix
    "4" = c("ass_obs_pe1", "ass_obs_pe2", "ass_obs_pe_3", "ass_obs_pe_4"),  # Physical Exam
    "5" = c("ass_obs_pres_1", "ass_obs_pres_2", "ass_obs_pres_3"),  # Presentation
    "6" = c("ass_obs_writehp_1", "ass_obs_writehp_2", "ass_obs_writehp_3", "ass_obs_writehp_4"),  # Written H&P
    "7" = c("ass_obs_daily_1", "ass_obs_daily_2", "ass_obs_daily_3", "ass_obs_daily_4"),  # Daily notes
    "8" = c("ass_obs_dc_1", "ass_obs_dc_2", "ass_obs_dc_3", "ass_obs_dc_4"),  # Patient Discharge
    "9" = c("ass_obs_meet_1", "ass_obs_meet_2", "ass_obs_meet_3"),  # Patient/Family Counseling
    "10" = c("ass_obs_senior_1", "ass_obs_senior_2", "ass_obs_senior_3", "ass_obs_senior_4"),  # Supervision
    "11" = c("ass_obs_proc_type", "ass_obs_proc_prim", "ass_obs_proc_up", "ass_obs_proc_ass", "ass_obs_proc_pt_comf"),  # Procedure
    "12" = c("ass_obs_mdr_1", "ass_obs_mdr_2", "ass_obs_mdr_3"),  # Multi-D Rounds
    "13" = c("ass_obs_emer_sit", "ass_obs_emer_1", "ass_obs_emer_2", "ass_obs_emer_3", "ass_obs_emer_4", "ass_obs_emer5")  # Emergency
  )
  
  fields <- field_mapping[[obs_type]]
  if (is.null(fields)) {
    cat("No fields found for observation type:", obs_type, "\n")
    fields <- character(0)
  }
  
  cat("Returning", length(fields), "fields:", paste(fields, collapse = ", "), "\n")
  return(fields)
}

get_obs_type_description <- function(obs_type) {
  descriptions <- list(
    "1" = "Clinical Decision Making Observation",
    "2" = "Advance Care Planning Discussion", 
    "3" = "Educational Session Participation",
    "4" = "Physical Examination Skills",
    "5" = "Oral Presentation Performance",
    "6" = "Written History & Physical Documentation",
    "7" = "Daily Progress Note Documentation",
    "8" = "Patient Discharge Process",
    "9" = "Patient/Family Communication",
    "10" = "Supervision of Junior Resident",
    "11" = "Procedure Performance",
    "12" = "Multidisciplinary Rounds Participation", 
    "13" = "Emergency Situation Management"
  )
  
  return(descriptions[[obs_type]] %||% "Unknown Observation Type")
}

# ============================================================================
# VALIDATION AND DATA COLLECTION FOR OBSERVATIONS
# ============================================================================

validate_observation_form <- function(input) {
  missing_fields <- character(0)
  
  # Check plus/delta with observation-specific field names
  if (is.null(input$ass_obs_plus) || trimws(input$ass_obs_plus) == "") {
    missing_fields <- c(missing_fields, "Plus comments")
  }
  
  if (is.null(input$ass_obs_delta) || trimws(input$ass_obs_delta) == "") {
    missing_fields <- c(missing_fields, "Delta comments")
  }
  
  # Check observation type selection
  if (is.null(input$ass_obs_type) || input$ass_obs_type == "") {
    missing_fields <- c(missing_fields, "Observation Type Selection")
    return(missing_fields)
  }
  
  # Get field names for this observation type
  field_names <- get_obs_fields_for_type(input$ass_obs_type)
  
  cat("Validating observation form for type", input$ass_obs_type, "\n")
  cat("Checking fields:", paste(field_names, collapse = ", "), "\n")
  
  # Check each field
  for (field_name in field_names) {
    field_value <- input[[field_name]]
    cat("Checking field:", field_name, "Value:", 
        ifelse(is.null(field_value), "NULL", 
               ifelse(field_value == "", "EMPTY", field_value)), "\n")
    
    if (is.null(field_value) || length(field_value) == 0 || field_value == "" || is.na(field_value)) {
      # Get field label from data dictionary for better error message
      field_info <- rdm_dict %>%
        filter(field_name == !!field_name) %>%
        slice(1)
      
      field_label <- if (nrow(field_info) > 0) field_info$field_label else field_name
      missing_fields <- c(missing_fields, field_label)
      cat("Field", field_name, "is missing or empty\n")
    }
  }
  
  cat("Total missing fields:", length(missing_fields), "\n")
  return(missing_fields)
}

collect_observation_data <- function(input, faculty, resident) {
  # Start with universal fields
  eval_data <- list(
    # Universal fields (auto-populated)
    ass_date = format(Sys.Date(), "%Y-%m-%d"),
    ass_faculty = faculty$fac_name,
    ass_specialty = faculty$fac_div,
    
    # Plus/Delta (user input) - Use observation-specific field names
    ass_plus = trimws(input$ass_obs_plus),
    ass_delta = trimws(input$ass_obs_delta),
    
    # Observation type selection
    ass_obs_type = input$ass_obs_type
  )
  
  # Get field names for this observation type
  field_names <- get_obs_fields_for_type(input$ass_obs_type)
  
  # Add dynamic fields
  for (field_name in field_names) {
    eval_data[[field_name]] <- input[[field_name]]
    cat("Added observation field", field_name, "with value:", input[[field_name]], "\n")
  }
  
  return(eval_data)
}