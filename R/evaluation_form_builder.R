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
                tags$textarea(
                  id = "ass_plus",
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
                  id = "ass_delta", 
                  class = "form-control eval-textarea",
                  placeholder = "Describe specific opportunities for growth...",
                  rows = "4"
                ),
                div(class = "eval-field-help",
                    "Provide constructive feedback on areas where the resident can improve.")
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
  
  return(assessment_fields)
}

# Build radio button field from data dictionary row
build_radio_field_from_dict <- function(field_row, required = FALSE) {
  field_id <- field_row$field_name
  field_label <- field_row$field_label
  choices_text <- field_row$select_choices_or_calculations
  
  # Parse choices (format: "1, Choice 1 | 2, Choice 2 | 3, Choice 3")
  choices_list <- strsplit(choices_text, " \\| ")[[1]]
  choices <- list()
  
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
      div(class = "eval-radio-group",
          lapply(names(choices), function(label) {
            value <- choices[[label]]
            div(class = "eval-radio-option",
                tags$input(
                  type = "radio",
                  name = field_id,
                  value = value,
                  id = paste0(field_id, "_", value),
                  class = "eval-radio-input"
                ),
                tags$label(
                  `for` = paste0(field_id, "_", value),
                  class = "eval-radio-label",
                  span(class = "eval-radio-number", value),
                  span(class = "eval-radio-text", label)
                )
            )
          })
      )
  )
}

# Build dropdown field from data dictionary row
build_dropdown_field_from_dict <- function(field_row, required = FALSE) {
  field_id <- field_row$field_name
  field_label <- field_row$field_label
  choices_text <- field_row$select_choices_or_calculations
  
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
# SINGLE DAY CLINIC EVALUATION FORM (Dynamic)
# ============================================================================

build_single_day_clinic_form <- function() {
  # Get single day clinic fields from data dictionary
  day_fields <- get_assessment_fields("day")
  
  if (nrow(day_fields) == 0) {
    return(div(
      class = "text-center",
      style = "padding: 2rem;",
      h5("Configuration Error", style = "color: #dc3545;"),
      p("No single day clinic fields found in data dictionary.")
    ))
  }
  
  # Also get the professionalism field (ass_cons_prof)
  prof_field <- rdm_dict %>%
    filter(form_name == "assessment") %>%
    filter(field_name == "ass_cons_prof")
  
  # Combine fields
  all_fields <- rbind(day_fields, prof_field)
  
  # Build form fields dynamically
  form_fields <- lapply(1:nrow(all_fields), function(i) {
    field_row <- all_fields[i, ]
    build_field_from_dict(field_row, required = TRUE)
  })
  
  tagList(
    # Plus/Delta section (universal)
    build_plus_delta_section(),
    
    # Single Day Clinic specific questions
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Single Day Clinic Evaluation", class = "eval-section-title"),
            p("Rate the resident's performance in the following areas:", 
              class = "eval-section-description")
        ),
        
        div(class = "eval-questions-container",
            form_fields
        )
    ),
    
    # Form controls
    div(class = "eval-form-controls",
        div(class = "text-center mt-4",
            actionButton("back_to_eval_selection", "← Back to Evaluation Types", 
                         class = "btn btn-secondary me-2"),
            actionButton("submit_single_day_evaluation", "Submit Evaluation", 
                         class = "btn btn-success btn-lg")
        )
    )
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
  
  # Check each field dynamically
  for (i in 1:nrow(all_fields)) {
    field_name <- all_fields$field_name[i]
    field_label <- all_fields$field_label[i]
    
    if (is.null(input[[field_name]]) || input[[field_name]] == "") {
      missing_fields <- c(missing_fields, field_label)
    }
  }
  
  return(missing_fields)
}

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
  }
  
  return(eval_data)
}# evaluation_form_builder.R - Functions to build evaluation forms dynamically

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
                tags$textarea(
                  id = "ass_plus",
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
                  id = "ass_delta", 
                  class = "form-control eval-textarea",
                  placeholder = "Describe specific opportunities for growth...",
                  rows = "4"
                ),
                div(class = "eval-field-help",
                    "Provide constructive feedback on areas where the resident can improve.")
            )
        )
    )
  )
}

# Build radio button field
build_radio_field <- function(field_id, field_label, choices_text, required = FALSE) {
  # Parse choices (format: "1, Choice 1 | 2, Choice 2 | 3, Choice 3")
  choices_list <- strsplit(choices_text, " \\| ")[[1]]
  choices <- list()
  
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
      div(class = "eval-radio-group",
          lapply(names(choices), function(label) {
            value <- choices[[label]]
            div(class = "eval-radio-option",
                tags$input(
                  type = "radio",
                  name = field_id,
                  value = value,
                  id = paste0(field_id, "_", value),
                  class = "eval-radio-input"
                ),
                tags$label(
                  `for` = paste0(field_id, "_", value),
                  class = "eval-radio-label",
                  span(class = "eval-radio-number", value),
                  span(class = "eval-radio-text", label)
                )
            )
          })
      )
  )
}

# Build dropdown field
build_dropdown_field <- function(field_id, field_label, choices_text, required = FALSE) {
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

# ============================================================================
# SINGLE DAY CLINIC EVALUATION FORM
# ============================================================================

build_single_day_clinic_form <- function() {
  tagList(
    # Plus/Delta section (universal)
    build_plus_delta_section(),
    
    # Single Day Clinic specific questions
    div(class = "eval-section",
        div(class = "eval-section-header",
            h4("Single Day Clinic Evaluation", class = "eval-section-title"),
            p("Rate the resident's performance in the following areas:", 
              class = "eval-section-description")
        ),
        
        div(class = "eval-questions-container",
            # Question 1: History Taking
            build_radio_field(
              "ass_day_pc1_r1",
              "The resident elicited hypothesis driven history",
              "1, Needed guidance | 2, Effectively elicited for a patient with a common problem | 3, Effectively elicited for a patient with a complex problem | 4, Efficiently elicited a history and incorporated social determinants of health",
              required = TRUE
            ),
            
            # Question 2: Clinical Reasoning  
            build_radio_field(
              "ass_day_pc3_r1",
              "Describe the clinical reasoning of the resident", 
              "1, Organized information to make a clinical impression | 2, Made a basic differential for a common condition | 3, Made a thorough and prioritized differential for a common condition | 4, Made a thorough and prioritized differential for a complex condition",
              required = TRUE
            ),
            
            # Question 3: Management Plan
            build_radio_field(
              "ass_day_pc5_r3",
              "Please describe how the resident did in creating a plan for the patient",
              "1, Formulates management plans for acute common conditions, with guidance | 2, Develops and implements management plans for common acute conditions | 3, Develops and implements an initial management plan for patients with urgent or emergent conditions in the setting of chronic comorbidities | 4, Develops and implements value-based (high value) management plans for patients with acute conditions",
              required = TRUE
            ),
            
            # Question 4: Professionalism
            build_dropdown_field(
              "ass_cons_prof",
              "Did the resident demonstrate professional behavior (show up on time, dressed appropriately, prepared to see patients, etc)?",
              "1, Yes | 2, No",
              required = TRUE
            )
        )
    ),
    
    # Form controls
    div(class = "eval-form-controls",
        div(class = "text-center mt-4",
            actionButton("back_to_eval_selection", "← Back to Evaluation Types", 
                         class = "btn btn-secondary me-2"),
            actionButton("submit_single_day_evaluation", "Submit Evaluation", 
                         class = "btn btn-success btn-lg")
        )
    )
  )
}

# ============================================================================
# FORM VALIDATION FUNCTIONS
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

# Validate single day clinic form
validate_single_day_clinic_form <- function(input) {
  missing_fields <- validate_plus_delta(input)
  
  # Check required radio buttons
  if (is.null(input$ass_day_pc1_r1) || input$ass_day_pc1_r1 == "") {
    missing_fields <- c(missing_fields, "History taking assessment")
  }
  
  if (is.null(input$ass_day_pc3_r1) || input$ass_day_pc3_r1 == "") {
    missing_fields <- c(missing_fields, "Clinical reasoning assessment")
  }
  
  if (is.null(input$ass_day_pc5_r3) || input$ass_day_pc5_r3 == "") {
    missing_fields <- c(missing_fields, "Management plan assessment")
  }
  
  if (is.null(input$ass_cons_prof) || input$ass_cons_prof == "") {
    missing_fields <- c(missing_fields, "Professional behavior assessment")
  }
  
  return(missing_fields)
}

# ============================================================================
# DATA COLLECTION FUNCTIONS
# ============================================================================

# Collect single day clinic evaluation data
collect_single_day_clinic_data <- function(input, faculty, resident) {
  list(
    # Universal fields (auto-populated)
    ass_date = format(Sys.Date(), "%Y-%m-%d"),
    ass_faculty = faculty$fac_name,
    ass_specialty = faculty$fac_div,
    
    # Plus/Delta (user input)
    ass_plus = trimws(input$ass_plus),
    ass_delta = trimws(input$ass_delta),
    
    # Single day clinic specific fields
    ass_day_pc1_r1 = input$ass_day_pc1_r1,
    ass_day_pc3_r1 = input$ass_day_pc3_r1, 
    ass_day_pc5_r3 = input$ass_day_pc5_r3,
    ass_cons_prof = input$ass_cons_prof
  )
}