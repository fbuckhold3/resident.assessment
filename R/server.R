# server.R - Faculty Evaluation App (Complete Rewrite with Observations)

server <- function(input, output, session) {
  
  # Reactive values to track app state
  values <- reactiveValues(
    selected_faculty = NULL,
    selected_resident = NULL,
    selected_eval_type = NULL,
    current_search_results = NULL,
    current_resident_results = NULL,
    current_step = "intro"
  )
  
  # Helper function to get field name safely
  get_field_name <- function(data, preferred_fields) {
    for (field in preferred_fields) {
      if (field %in% names(data)) {
        return(field)
      }
    }
    return(names(data)[1])  # fallback
  }
  
  # Manage current step display
  output$current_step <- reactive({
    values$current_step
  })
  outputOptions(output, "current_step", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # INTRO PAGE HANDLER
  # ============================================================================
  
  observeEvent(input$begin_evaluation, {
    values$current_step <- "faculty"
    showNotification("Welcome! Please search for your name to begin.", type = "default", duration = 3)
  })
  
  # ============================================================================
  # NAVIGATION BETWEEN STEPS
  # ============================================================================
  
  observeEvent(input$back_to_faculty, {
    values$current_step <- "faculty"
    values$selected_resident <- NULL
    values$selected_eval_type <- NULL
    updateTextInput(session, "resident_search", value = "")
  })
  
  observeEvent(input$back_to_resident, {
    values$current_step <- "resident"
    values$selected_eval_type <- NULL
  })
  
  observeEvent(input$start_over, {
    values$selected_faculty <- NULL
    values$selected_resident <- NULL
    values$selected_eval_type <- NULL
    values$current_search_results <- NULL
    values$current_resident_results <- NULL
    values$current_step <- "intro"
    
    updateTextInput(session, "faculty_search", value = "")
    updateTextInput(session, "resident_search", value = "")
  })
  
  observeEvent(input$back_to_eval_selection, {
    values$current_step <- "evaluation_type"
  })
  
  # ============================================================================
  # FACULTY SEARCH AND SELECTION
  # ============================================================================
  
  filtered_faculty <- reactive({
    req(input$faculty_search)
    req(faculty_data)
    
    search_term <- tolower(trimws(input$faculty_search))
    if (nchar(search_term) < 2) return(NULL)
    
    name_field <- get_field_name(faculty_data, c("fac_name", "name", "faculty_name"))
    
    cat("Using name field:", name_field, "\n")
    
    filtered <- faculty_data %>%
      filter(!is.na(.data[[name_field]]) & 
               grepl(search_term, tolower(.data[[name_field]]), fixed = TRUE)) %>%
      head(10)
    
    cat("Faculty search for '", search_term, "' returned ", nrow(filtered), " results\n")
    return(filtered)
  })
  
  # ============================================================================
  # FACULTY SELECTION - Remove notifications
  # ============================================================================
  
  # Handle faculty selection clicks (UPDATED - no notifications)
  observe({
    faculty_results <- values$current_search_results
    if (!is.null(faculty_results)) {
      for (i in 1:nrow(faculty_results)) {
        local({
          faculty_index <- i
          button_id <- paste0("select_faculty_", faculty_index)
          
          observeEvent(input[[button_id]], {
            if (!is.null(faculty_results) && faculty_index <= nrow(faculty_results)) {
              values$selected_faculty <- faculty_results[faculty_index, ]
              
              name_field <- get_field_name(values$selected_faculty, c("fac_name", "name", "faculty_name"))
              cat("Selected faculty:", values$selected_faculty[[name_field]], "\n")
              
              values$current_step <- "resident"
              
              # REMOVED: showNotification line
              # showNotification(paste("Selected:", values$selected_faculty[[name_field]]), type = "default")
            }
          })
        })
      }
    }
  })
  
  output$faculty_search_results <- renderUI({
    faculty_results <- filtered_faculty()
    
    if (is.null(faculty_results) || nrow(faculty_results) == 0) {
      message <- if (!is.null(input$faculty_search) && nchar(trimws(input$faculty_search)) >= 2) {
        "No faculty found. Please check spelling or add yourself below."
      } else {
        "Start typing to search for your name..."
      }
      return(div(style = "padding: 10px; color: #666;", message))
    }
    
    values$current_search_results <- faculty_results
    name_field <- get_field_name(faculty_results, c("fac_name", "name", "faculty_name"))
    
    result_list <- lapply(1:nrow(faculty_results), function(i) {
      faculty <- faculty_results[i, ]
      button_id <- paste0("select_faculty_", i)
      
      # Build display text
      faculty_text <- faculty[[name_field]]
      
      # Add department/division if available
      dept_field <- get_field_name(faculty, c("fac_div", "department", "division"))
      if (!is.null(dept_field) && !is.na(faculty[[dept_field]])) {
        faculty_text <- paste0(faculty_text, " - ", faculty[[dept_field]])
      }
      
      # Add role if available
      role_field <- get_field_name(faculty, c("fac_fell", "role", "position"))
      if (!is.null(role_field) && !is.na(faculty[[role_field]])) {
        faculty_text <- paste0(faculty_text, " (", faculty[[role_field]], ")")
      }
      
      div(class = "search-result",
          actionButton(button_id, 
                       label = faculty_text,
                       style = "background: #f8f9fa; border: 1px solid #ddd; text-align: left; width: 100%; padding: 10px;",
                       class = "btn btn-light")
      )
    })
    
    do.call(tagList, result_list)
  })
  
  output$selected_faculty_info <- renderUI({
    if (!is.null(values$selected_faculty)) {
      faculty <- values$selected_faculty
      
      name_field <- get_field_name(faculty, c("fac_name", "name", "faculty_name"))
      dept_field <- get_field_name(faculty, c("fac_div", "department", "division"))
      role_field <- get_field_name(faculty, c("fac_fell", "role", "position"))
      
      div(
        strong(faculty[[name_field]]),
        br(),
        if (!is.null(dept_field) && !is.na(faculty[[dept_field]])) {
          paste("Department:", faculty[[dept_field]])
        } else {
          "Faculty member"
        },
        br(),
        if (!is.null(role_field) && !is.na(faculty[[role_field]])) {
          paste("Role:", faculty[[role_field]])
        },
        br(), br(),
        div(class = "faculty-selected", "✅ Faculty selected successfully!")
      )
    } else {
      div("Select your name from the search results above.")
    }
  })
  
  # ============================================================================
  # RESIDENT SEARCH AND SELECTION
  # ============================================================================
  
  filtered_residents <- reactive({
    req(input$resident_search)
    req(resident_data)
    
    search_term <- tolower(trimws(input$resident_search))
    if (nchar(search_term) < 2) return(NULL)
    
    filtered <- resident_data %>%
      filter(!is.na(name) & 
               grepl(search_term, tolower(name), fixed = TRUE)) %>%
      arrange(Level, name) %>%
      head(15)
    
    cat("Resident search for '", search_term, "' returned ", nrow(filtered), " results\n")
    return(filtered)
  })
  
  output$resident_search_results <- renderUI({
    resident_results <- filtered_residents()
    
    if (is.null(resident_results) || nrow(resident_results) == 0) {
      message <- if (!is.null(input$resident_search) && nchar(trimws(input$resident_search)) >= 2) {
        "No residents found. Please check spelling."
      } else {
        "Start typing to search for a resident..."
      }
      return(div(style = "padding: 10px; color: #666;", message))
    }
    
    values$current_resident_results <- resident_results
    
    result_list <- lapply(1:nrow(resident_results), function(i) {
      resident <- resident_results[i, ]
      button_id <- paste0("select_resident_", i)
      
      resident_text <- paste0(resident$name, " (", resident$Level, ")")
      if (!is.na(resident$grad_yr)) {
        resident_text <- paste0(resident_text, " - Class of ", resident$grad_yr)
      }
      
      div(class = "search-result resident-result",
          actionButton(button_id, 
                       label = resident_text,
                       style = "background: #f8f9fa; border: 1px solid #ddd; text-align: left; width: 100%; padding: 10px;",
                       class = "btn btn-light")
      )
    })
    
    do.call(tagList, result_list)
  })
  
  # Handle resident selection clicks
  # Handle resident selection clicks (UPDATED - no notifications)
  observe({
    resident_results <- values$current_resident_results
    if (!is.null(resident_results)) {
      for (i in 1:nrow(resident_results)) {
        local({
          resident_index <- i
          button_id <- paste0("select_resident_", resident_index)
          
          observeEvent(input[[button_id]], {
            if (!is.null(resident_results) && resident_index <= nrow(resident_results)) {
              values$selected_resident <- resident_results[resident_index, ]
              
              cat("Selected resident:", values$selected_resident$name, "\n")
              values$current_step <- "evaluation_type"
              
              # REMOVED: showNotification line
              # showNotification(paste("Selected resident:", values$selected_resident$name), type = "default")
            }
          })
        })
      }
    }
  })
  
  
  output$selected_resident_info <- renderUI({
    if (!is.null(values$selected_resident)) {
      resident <- values$selected_resident
      
      if (resident$Level == "Rotator" || resident$type == "Rotator") {
        div(
          strong(resident$name),
          br(),
          "External/Visiting Resident",
          br(),
          if (!is.na(resident$grad_yr)) paste("Graduation Year:", resident$grad_yr),
          br(), br(),
          div(class = "resident-selected", "✅ Rotator selected!")
        )
      } else {
        div(
          strong(resident$name),
          br(),
          paste("Level:", resident$Level),
          br(),
          if (!is.na(resident$grad_yr)) paste("Graduation Year:", resident$grad_yr),
          br(),
          if (!is.na(resident$type)) paste("Program Type:", resident$type),
          br(), br(),
          div(class = "resident-selected", "✅ Resident selected successfully!")
        )
      }
    } else {
      div("Select a resident from the search results above.")
    }
  })
  
  # ============================================================================
  # EVALUATION TYPE SELECTION
  # ============================================================================
  
  output$evaluation_type_buttons <- renderUI({
    req(values$selected_faculty)
    req(values$selected_resident)
    
    # Check if helper functions exist
    if (!exists("get_available_eval_types_by_division")) {
      return(div(
        class = "text-center",
        style = "padding: 2rem;",
        h5("Configuration Error", style = "color: #dc3545;"),
        p("Evaluation configuration functions are not loaded. Please check your helper functions.")
      ))
    }
    
    # Get faculty division
    faculty_div <- values$selected_faculty$fac_div %||% 
      values$selected_faculty$department %||% 
      values$selected_faculty$division %||% 
      "15"  # Default to "Other"
    
    resident_level <- values$selected_resident$Level
    
    cat("=== EVALUATION TYPE SELECTION DEBUG ===\n")
    cat("Faculty fields available:", paste(names(values$selected_faculty), collapse = ", "), "\n")
    cat("Faculty division field value:", faculty_div, "\n")
    cat("Resident level:", resident_level, "\n")
    
    tryCatch({
      # Get available evaluation types
      available_types <- get_available_eval_types_by_division(faculty_div)
      filtered_types <- filter_eval_types_by_resident_level(available_types, resident_level)
      
      cat("Available eval types:", paste(available_types, collapse = ", "), "\n")
      cat("Filtered eval types:", paste(filtered_types, collapse = ", "), "\n")
      
      if (length(filtered_types) == 0) {
        return(div(
          class = "text-center",
          style = "padding: 2rem;",
          h5("No evaluations available", style = "color: #666;"),
          p("No evaluation types are configured for this faculty-resident combination.")
        ))
      }
      
      # Create buttons for each evaluation type
      button_list <- lapply(filtered_types, function(eval_id) {
        eval_info <- get_eval_type_display_info(eval_id, resident_level)
        
        if (is.null(eval_info)) return(NULL)
        
        is_appropriate <- is_eval_appropriate_for_level(eval_id, resident_level)
        
        button_class <- if (is_appropriate) {
          "eval-type-button"
        } else {
          "eval-type-button disabled"
        }
        
        # Create tags
        tag_elements <- if (length(eval_info$tags) > 0) {
          lapply(eval_info$tags, function(tag) {
            tag_class <- if (tag %in% c("Intern Level", "Senior Level", "Longitudinal")) {
              "eval-type-tag resident-level"
            } else {
              "eval-type-tag"
            }
            span(class = tag_class, tag)
          })
        } else {
          list()
        }
        
        div(
          class = button_class,
          onclick = if (is_appropriate) paste0("Shiny.setInputValue('select_eval_type', '", eval_id, "', {priority: 'event'});") else NULL,
          div(class = "eval-type-icon", eval_info$icon),
          div(class = "eval-type-title", eval_info$name),
          div(class = "eval-type-description", eval_info$description),
          if (length(tag_elements) > 0) {
            div(class = "eval-type-tags", tag_elements)
          }
        )
      })
      
      # Remove NULL elements
      button_list <- button_list[!sapply(button_list, is.null)]
      
      if (length(button_list) == 0) {
        return(div(
          class = "text-center",
          style = "padding: 2rem;",
          h5("No evaluations available", style = "color: #666;"),
          p("No appropriate evaluation types found for this combination.")
        ))
      }
      
      # Get faculty name for display
      faculty_name <- values$selected_faculty$fac_name %||% 
        values$selected_faculty$name %||% 
        "Selected Faculty"
      
      tagList(
        div(class = "eval-selection-info",
            h5("Available Evaluations"),
            p(paste("Based on", faculty_name, "'s specialty and", 
                    values$selected_resident$name, "'s level, the following evaluations are available:"))
        ),
        div(class = "eval-type-container", button_list)
      )
      
    }, error = function(e) {
      cat("Error in evaluation type selection:", e$message, "\n")
      return(div(
        class = "text-center",
        style = "padding: 2rem;",
        h5("Error loading evaluations", style = "color: #dc3545;"),
        p(paste("Error:", e$message))
      ))
    })
  })
  
  # Handle evaluation type selection
  observeEvent(input$select_eval_type, {
    eval_type <- input$select_eval_type
    
    if (!is.null(eval_type) && eval_type != "") {
      values$selected_eval_type <- eval_type
      
      if (exists("get_eval_type_display_info")) {
        eval_info <- get_eval_type_display_info(eval_type)
        if (!is.null(eval_info)) {
          cat("Selected evaluation type:", eval_info$name, "\n")
          
          # REMOVED: showNotification line
          # showNotification(paste("Selected evaluation:", eval_info$name), type = "default")
        }
      }
      
      values$current_step <- "evaluation_form"
    }
  })
  
  # ============================================================================
  # EVALUATION FORM
  # ============================================================================
  
  output$evaluation_form_header <- renderUI({
    req(values$selected_eval_type)
    
    if (exists("get_eval_type_display_info")) {
      eval_info <- get_eval_type_display_info(values$selected_eval_type)
      
      if (!is.null(eval_info)) {
        p(paste("Complete", eval_info$name, "evaluation"), style = "margin: 0; opacity: 0.9;")
      } else {
        p("Complete evaluation", style = "margin: 0; opacity: 0.9;")
      }
    } else {
      p("Complete evaluation", style = "margin: 0; opacity: 0.9;")
    }
  })
  
  output$evaluation_form_content <- renderUI({
    req(values$selected_eval_type)
    
    # Build form based on evaluation type
    form_content <- switch(values$selected_eval_type,
                           "day" = {
                             if (exists("build_single_day_clinic_form")) {
                               build_single_day_clinic_form()
                             } else {
                               build_form_error("Single Day Clinic form builder function not found.")
                             }
                           },
                           "cons" = {
                             if (exists("build_consultation_form")) {
                               build_consultation_form()
                             } else {
                               build_form_error("Consultation form builder function not found.")
                             }
                           },
                           "bridge" = {
                             if (exists("build_bridge_clinic_form")) {
                               build_bridge_clinic_form()
                             } else {
                               build_form_error("Bridge Clinic form builder function not found.")
                             }
                           },
                           "int_ip" = {
                             if (exists("build_intern_inpatient_form")) {
                               build_intern_inpatient_form()
                             } else {
                               build_form_error("Intern Inpatient form builder function not found.")
                             }
                           },
                           "res_ip" = {
                             if (exists("build_senior_inpatient_form")) {
                               build_senior_inpatient_form()
                             } else {
                               build_form_error("Senior Inpatient form builder function not found.")
                             }
                           },
                           "cc" = {
                             if (exists("build_continuity_clinic_form")) {
                               session$sendCustomMessage("setResidentLevel", values$selected_resident$Level)
                               build_continuity_clinic_form()
                             } else {
                               build_form_error("Continuity Clinic form builder function not found.")
                             }
                           },
                           "obs" = {
                             if (exists("build_observation_form")) {
                               build_observation_form()
                             } else {
                               build_form_error("Observation form builder function not found.")
                             }
                           },
                           {
                             # Default case for unimplemented evaluation types
                             eval_info <- NULL
                             if (exists("get_eval_type_display_info")) {
                               eval_info <- get_eval_type_display_info(values$selected_eval_type)
                             }
                             
                             faculty_name <- values$selected_faculty$fac_name %||% 
                               values$selected_faculty$name %||% 
                               "Unknown Faculty"
                             
                             div(style = "text-align: center; padding: 2rem;",
                                 div(class = "eval-type-icon", style = "font-size: 4rem; margin-bottom: 1rem;", 
                                     if (!is.null(eval_info)) eval_info$icon else "📝"),
                                 h4(paste("Evaluation Form:", if (!is.null(eval_info)) eval_info$name else values$selected_eval_type)),
                                 p("This evaluation form is not yet implemented."),
                                 p(paste("Faculty:", faculty_name)),
                                 p(paste("Resident:", values$selected_resident$name)),
                                 p(paste("Level:", values$selected_resident$Level)),
                                 br(),
                                 div(
                                   actionButton("back_to_eval_selection", "← Back to Evaluation Types", class = "btn btn-secondary me-2"),
                                   actionButton("submit_evaluation", "Submit Evaluation", class = "btn btn-success"),
                                   br(), br(),
                                   actionButton("start_over", "Start Over", class = "btn btn-outline-secondary")
                                 )
                             )
                           }
    )
    
    return(form_content)
  })
  
  # Helper function to build error messages
  build_form_error <- function(message) {
    div(style = "text-align: center; padding: 2rem;",
        h5("Form Builder Error", style = "color: #dc3545;"),
        p(message),
        br(),
        actionButton("back_to_eval_selection", "← Back to Evaluation Types", class = "btn btn-secondary"))
  }
  
  # ============================================================================
  # CONTINUITY CLINIC EVALUATION SERVER CODE
  # ============================================================================
  
  # Render the quarter selection table
  output$quarter_selection_table <- renderUI({
    cat("=== RENDERING QUARTER SELECTION TABLE ===\n")
    
    if (is.null(values$selected_resident)) {
      return(div("No resident selected"))
    }
    
    # Get completion status with academic year filtering
    completion_data <- get_cc_completion_status(values$selected_resident$name, values$selected_resident$Level)
    
    cat("📊 Completion data received, rows:", ifelse(is.null(completion_data), 0, nrow(completion_data)), "\n")
    
    if (is.null(completion_data)) {
      return(div("Error loading completion data"))
    }
    
    # Build the clickable table
    build_clickable_quarter_table(completion_data)
  })
  
  # Add new output for dynamic quarter subtitle
  output$cc_quarter_subtitle <- renderUI({
    req(input$ass_cc_quart)
    req(values$selected_resident)
    
    quarter_desc <- get_quarter_description(input$ass_cc_quart, values$selected_resident$Level)
    p(paste("Selected evaluation:", quarter_desc), 
      style = "margin: 0; opacity: 0.9; font-weight: 500;")
  })
  
  # Handle manual quarter selection
  observeEvent(input$cc_quarter_manually_selected, {
    selection_data <- input$cc_quarter_manually_selected
    
    cat("=== QUARTER MANUALLY SELECTED ===\n")
    cat("Quarter:", selection_data$quarter, "\n")
    cat("Label:", selection_data$label, "\n")
    cat("Was completed:", selection_data$isCompleted, "\n")
    
    # Store selection info for validation/submission
    values$cc_quarter_selection_info <- selection_data
    
    # REMOVED: Both showNotification lines
    # if (selection_data$isCompleted) {
    #   showNotification(paste("⚠️ Re-evaluating completed quarter:", selection_data$label), type = "warning", duration = 4)
    # } else {
    #   showNotification(paste("✅ Starting evaluation for:", selection_data$label), type = "default", duration = 3)
    # }
  })
  
  output$cc_dynamic_questions <- renderUI({
    req(input$ass_cc_quart)
    req(values$selected_resident)
    
    cat("=== BUILDING CC DYNAMIC QUESTIONS ===\n")
    cat("Quarter:", input$ass_cc_quart, "Resident Level:", values$selected_resident$Level, "\n")
    
    # Get field names for this quarter and level
    field_names <- get_cc_fields_for_quarter_and_level(input$ass_cc_quart, values$selected_resident$Level)
    
    cat("📋 Found", length(field_names), "fields:", paste(field_names, collapse = ", "), "\n")
    
    if (length(field_names) == 0) {
      return(div(
        style = "padding: 1rem; background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 8px; text-align: center;",
        h6("No Questions Available", style = "color: #856404;"),
        p("No evaluation questions found for this quarter and resident level combination.")
      ))
    }
    
    # Get the relevant fields from the data dictionary
    if (!exists("rdm_dict") || is.null(rdm_dict)) {
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Configuration Error", style = "color: #721c24;"),
        p("Data dictionary not available.")
      ))
    }
    
    # Build questions from data dictionary
    question_fields <- rdm_dict %>%
      filter(form_name == "assessment") %>%
      filter(field_name %in% field_names) %>%
      arrange(match(field_name, field_names))  # Preserve order
    
    cat("📝 Found", nrow(question_fields), "questions in data dictionary\n")
    
    if (nrow(question_fields) == 0) {
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Questions Not Found", style = "color: #721c24;"),
        p("The expected question fields were not found in the data dictionary.")
      ))
    }
    
    # Build form fields
    question_elements <- lapply(1:nrow(question_fields), function(i) {
      field_row <- question_fields[i, ]
      cat("Building question", i, ":", field_row$field_name, "\n")
      
      # Build field based on type
      if (field_row$field_type == "radio") {
        build_radio_field_from_dict(field_row, required = TRUE)
      } else if (field_row$field_type == "dropdown") {
        build_dropdown_field_from_dict(field_row, required = TRUE)
      } else {
        # Default to text input for other types
        div(class = "eval-field-group",
            tags$label(field_row$field_label, class = "eval-field-label required"),
            textInput(
              field_row$field_name,
              label = NULL,
              placeholder = paste("Enter", tolower(field_row$field_label), "...")
            )
        )
      }
    })
    
    cat("✅ Built", length(question_elements), "question elements\n")
    
    # Return the question elements
    tagList(question_elements)
  })
  
  # Handle quarter selection change
  observeEvent(input$ass_cc_quart, {
    cat("📋 Quarter dropdown changed to:", input$ass_cc_quart, "\n")
  })
  
  # Handle continuity clinic submission
  observeEvent(input$submit_continuity_clinic_evaluation, {
    cat("=== SUBMITTING CONTINUITY CLINIC EVALUATION ===\n")
    
    if (!exists("validate_continuity_clinic_with_level")) {
      showNotification("Validation function not found. Please check your form builder functions.", 
                       type = "error", duration = 5)
      return()
    }
    
    missing_fields <- validate_continuity_clinic_with_level(input, values$selected_resident$Level)
    
    if (length(missing_fields) > 0) {
      cat("Validation failed. Missing fields:", paste(missing_fields, collapse = ", "), "\n")
      showNotification(
        paste("Please complete the following required fields:", paste(missing_fields, collapse = ", ")),
        type = "error",
        duration = 5
      )
      return()
    }
    
    cat("Form validation passed\n")
    
    if (!exists("collect_continuity_clinic_data")) {
      showNotification("Data collection function not found. Please check your form builder functions.", 
                       type = "error", duration = 5)
      return()
    }
    
    eval_data <- collect_continuity_clinic_data(input, values$selected_faculty, values$selected_resident)
    
    cat("Continuity clinic evaluation data collected:\n")
    print(eval_data)
    
    tryCatch({
      if (!exists("submit_evaluation_data")) {
        showNotification("Submission function not found. Please check your evaluation helper functions.", 
                         type = "error", duration = 5)
        return()
      }
      
      result <- submit_evaluation_data(
        eval_data = eval_data,
        eval_type = "cc",
        resident_id = values$selected_resident$record_id,
        token = if(exists("rdm_token")) rdm_token else NULL,
        url = if(exists("url")) url else NULL
      )
      
      cat("Submission successful:", result, "\n")
      
      # Get quarter description for notification
      quarter_desc <- get_quarter_description(input$ass_cc_quart, values$selected_resident$Level)
      
      showNotification(
        paste0("✅ Continuity Clinic evaluation submitted successfully! (", quarter_desc, ")"),
        type = "default",
        duration = 5
      )
      
      # Reset to start
      values$selected_faculty <- NULL
      values$selected_resident <- NULL
      values$selected_eval_type <- NULL
      values$current_step <- "faculty"
      
      updateTextInput(session, "faculty_search", value = "")
      updateTextInput(session, "resident_search", value = "")
      
    }, error = function(e) {
      cat("Error submitting continuity clinic evaluation:", e$message, "\n")
      showNotification(
        paste("❌ Error submitting evaluation:", e$message),
        type = "error",
        duration = 8
      )
    })
  })
  
  # ============================================================================
  # OBSERVATION EVALUATION SERVER CODE
  # ============================================================================
  
  # Render observation type selection buttons
  output$observation_type_selection <- renderUI({
    req(values$selected_resident)
    
    cat("=== RENDERING OBSERVATION TYPE SELECTION ===\n")
    cat("Resident level:", values$selected_resident$Level, "\n")
    
    if (!exists("build_observation_type_buttons")) {
      return(div(
        style = "padding: 2rem; background: #f8d7da; border-radius: 8px; text-align: center;",
        h5("Function Missing", style = "color: #721c24;"),
        p("build_observation_type_buttons function not found in evaluation_form_builder.R")
      ))
    }
    
    build_observation_type_buttons(values$selected_resident$Level)
  })
  
  # Add observation type subtitle
  output$obs_type_subtitle <- renderUI({
    req(input$ass_obs_type)
    
    if (!exists("get_obs_type_description")) {
      return(p("Observation evaluation selected", style = "margin: 0; opacity: 0.9; font-weight: 500;"))
    }
    
    obs_desc <- get_obs_type_description(input$ass_obs_type)
    p(paste("Selected observation:", obs_desc), 
      style = "margin: 0; opacity: 0.9; font-weight: 500;")
  })
  
  # Handle manual observation type selection
  observeEvent(input$obs_type_manually_selected, {
    if (is.null(input$obs_type_manually_selected)) return()
    
    selection_data <- input$obs_type_manually_selected
    
    cat("=== OBSERVATION TYPE MANUALLY SELECTED ===\n")
    cat("Type:", selection_data$type, "\n")
    cat("Name:", selection_data$name, "\n")
    
    # Store selection info for validation/submission
    values$obs_type_selection_info <- selection_data
    
    # REMOVED: showNotification line
    # showNotification(paste("✅ Starting observation evaluation for:", selection_data$name), type = "default", duration = 3)
  })
  
  observeEvent(input$submit_observation_evaluation, {
    cat("=== DEBUGGING OBSERVATION SUBMISSION ===\n")
    
    # Print all input values that start with "ass"
    all_inputs <- reactiveValuesToList(input)
    ass_inputs <- all_inputs[grepl("^ass", names(all_inputs))]
    
    cat("All fields starting with 'ass':\n")
    for (name in names(ass_inputs)) {
      value <- ass_inputs[[name]]
      cat(sprintf("  %s: %s\n", name, ifelse(is.null(value), "NULL", 
                                             ifelse(value == "", "EMPTY", as.character(value)))))
    }
    
    cat("\nSpecifically looking for Plus/Delta fields:\n")
    cat("input$ass_plus:", ifelse(is.null(input$ass_plus), "NULL", input$ass_plus), "\n")
    cat("input$ass_delta:", ifelse(is.null(input$ass_delta), "NULL", input$ass_delta), "\n")
    cat("input$ass_obs_plus:", ifelse(is.null(input$ass_obs_plus), "NULL", input$ass_obs_plus), "\n")
    cat("input$ass_obs_delta:", ifelse(is.null(input$ass_obs_delta), "NULL", input$ass_obs_delta), "\n")
    
    cat("Observation type:", input$ass_obs_type, "\n")
    
    # Continue with existing validation...
    # [rest of your existing submission code]
  })
  
  # Build dynamic questions based on observation type
  output$obs_dynamic_questions <- renderUI({
    req(input$ass_obs_type)
    
    cat("=== FORM BUILDING DEBUG ===\n")
    cat("Building form for observation type:", input$ass_obs_type, "\n")
    
    # Check what fields will be created
    field_names <- get_obs_fields_for_type(input$ass_obs_type)
    cat("Expected assessment fields:", paste(field_names, collapse = ", "), "\n")
    
    cat("=== BUILDING OBSERVATION DYNAMIC QUESTIONS ===\n")
    cat("Observation type:", input$ass_obs_type, "\n")
    
    # Check if helper functions exist
    if (!exists("get_obs_fields_for_type")) {
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Function Missing", style = "color: #721c24;"),
        p("get_obs_fields_for_type function not found in evaluation_form_builder.R")
      ))
    }
    
    # Get field names for this observation type
    field_names <- get_obs_fields_for_type(input$ass_obs_type)
    
    cat("📋 Found", length(field_names), "fields:", paste(field_names, collapse = ", "), "\n")
    
    if (length(field_names) == 0) {
      return(div(
        style = "padding: 1rem; background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 8px; text-align: center;",
        h6("No Questions Available", style = "color: #856404;"),
        p("No evaluation questions found for this observation type.")
      ))
    }
    
    # Get the relevant fields from the data dictionary
    if (!exists("rdm_dict") || is.null(rdm_dict)) {
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Configuration Error", style = "color: #721c24;"),
        p("Data dictionary not available.")
      ))
    }
    
    # Build questions from data dictionary
    question_fields <- rdm_dict %>%
      filter(form_name == "assessment") %>%
      filter(field_name %in% field_names) %>%
      arrange(match(field_name, field_names))  # Preserve order
    
    cat("📝 Found", nrow(question_fields), "questions in data dictionary\n")
    
    if (nrow(question_fields) == 0) {
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Questions Not Found", style = "color: #721c24;"),
        p("The expected question fields were not found in the data dictionary."),
        p("Expected fields:", paste(field_names, collapse = ", "))
      ))
    }
    
    # Special handling for Clinical Decision Making (text field)
    if (input$ass_obs_type == "1") {
      # Clinical Decision Making uses a text area
      cdm_field <- question_fields[question_fields$field_name == "ass_obs_cdm", ]
      if (nrow(cdm_field) > 0) {
        return(
          div(class = "eval-field-group",
              tags$label(cdm_field$field_label, class = "eval-field-label required"),
              div(class = "textarea-container",
                  tags$textarea(
                    id = "ass_obs_cdm",
                    class = "form-control eval-textarea",
                    placeholder = "Describe the clinical decision making process observed...",
                    rows = "4"
                  ),
                  conditionalPanel(
                    condition = "window.location.protocol === 'https:'",
                    tags$button(
                      type = "button",
                      class = "btn btn-outline-secondary speech-btn",
                      onclick = "startSpeechRecognition('ass_obs_cdm')",
                      title = "Click to use voice input",
                      style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                      "🎤"
                    )
                  )
              ),
              div(class = "eval-field-help", "Provide detailed observations of the resident's clinical reasoning.")
          )
        )
      }
    }
    
    # For all other observation types, build standard fields
    if (!exists("build_radio_field_from_dict") || !exists("build_dropdown_field_from_dict")) {
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Function Missing", style = "color: #721c24;"),
        p("Form building functions not found in evaluation_form_builder.R")
      ))
    }
    
    question_elements <- lapply(1:nrow(question_fields), function(i) {
      field_row <- question_fields[i, ]
      cat("Building question", i, ":", field_row$field_name, "\n")
      
      # Special handling for Physical Exam yes/no fields
      if (field_row$field_name %in% c("ass_obs_pe_3", "ass_obs_pe_4")) {
        # These are yes/no fields, build as dropdown
        div(class = "eval-field-group",
            tags$label(field_row$field_label, class = "eval-field-label required"),
            selectInput(
              field_row$field_name,
              label = NULL,
              choices = list("Choose..." = "", "Yes" = "1", "No" = "0"),
              selected = "",
              width = "100%"
            )
        )
      } else if (field_row$field_type == "radio") {
        build_radio_field_from_dict(field_row, required = TRUE)
      } else if (field_row$field_type == "dropdown") {
        build_dropdown_field_from_dict(field_row, required = TRUE)
      } else if (field_row$field_type == "text") {
        # Text area for text fields
        div(class = "eval-field-group",
            tags$label(field_row$field_label, class = "eval-field-label required"),
            div(class = "textarea-container",
                tags$textarea(
                  id = field_row$field_name,
                  class = "form-control eval-textarea",
                  placeholder = paste("Enter", tolower(field_row$field_label), "..."),
                  rows = "3"
                ),
                conditionalPanel(
                  condition = "window.location.protocol === 'https:'",
                  tags$button(
                    type = "button",
                    class = "btn btn-outline-secondary speech-btn",
                    onclick = paste0("startSpeechRecognition('", field_row$field_name, "')"),
                    title = "Click to use voice input",
                    style = "position: absolute; right: 10px; top: 10px; padding: 5px 10px;",
                    "🎤"
                  )
                )
            )
        )
      } else {
        # Default to text input for other types
        div(class = "eval-field-group",
            tags$label(field_row$field_label, class = "eval-field-label required"),
            textInput(
              field_row$field_name,
              label = NULL,
              placeholder = paste("Enter", tolower(field_row$field_label), "...")
            )
        )
      }
    })
    
    cat("✅ Built", length(question_elements), "question elements\n")
    
    # Return the question elements
    tagList(question_elements)
  })
  
  output$obs_dynamic_questions <- renderUI({
    cat("=== OBS DYNAMIC QUESTIONS DEBUG START ===\n")
    
    # Check if input exists
    cat("input$ass_obs_type exists:", !is.null(input$ass_obs_type), "\n")
    cat("input$ass_obs_type value:", ifelse(is.null(input$ass_obs_type), "NULL", input$ass_obs_type), "\n")
    
    # Early return test
    if (is.null(input$ass_obs_type) || input$ass_obs_type == "") {
      cat("❌ Exiting early - no observation type selected\n")
      return(div("No observation type selected"))
    }
    
    cat("✅ Observation type is valid:", input$ass_obs_type, "\n")
    
    # Check if helper function exists
    if (!exists("get_obs_fields_for_type")) {
      cat("❌ Function get_obs_fields_for_type does not exist\n")
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Function Missing", style = "color: #721c24;"),
        p("get_obs_fields_for_type function not found in evaluation_form_builder.R")
      ))
    }
    
    cat("✅ get_obs_fields_for_type function exists\n")
    
    # Try to get field names
    tryCatch({
      field_names <- get_obs_fields_for_type(input$ass_obs_type)
      cat("✅ Got field names:", paste(field_names, collapse = ", "), "\n")
      cat("Field count:", length(field_names), "\n")
    }, error = function(e) {
      cat("❌ Error getting field names:", e$message, "\n")
      return(div("Error getting field names:", e$message))
    })
    
    if (length(field_names) == 0) {
      cat("❌ No fields found for observation type\n")
      return(div(
        style = "padding: 1rem; background: #fff3cd; border: 1px solid #ffeaa7; border-radius: 8px; text-align: center;",
        h6("No Questions Available", style = "color: #856404;"),
        p("No evaluation questions found for this observation type.")
      ))
    }
    
    # Check data dictionary
    if (!exists("rdm_dict") || is.null(rdm_dict)) {
      cat("❌ rdm_dict is missing or null\n")
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Configuration Error", style = "color: #721c24;"),
        p("Data dictionary not available.")
      ))
    }
    
    cat("✅ rdm_dict exists with", nrow(rdm_dict), "rows\n")
    
    # Try to filter data dictionary
    tryCatch({
      question_fields <- rdm_dict %>%
        filter(form_name == "assessment") %>%
        filter(field_name %in% field_names) %>%
        arrange(match(field_name, field_names))
      
      cat("✅ Found", nrow(question_fields), "questions in data dictionary\n")
      cat("Question field names:", paste(question_fields$field_name, collapse = ", "), "\n")
    }, error = function(e) {
      cat("❌ Error filtering data dictionary:", e$message, "\n")
      return(div("Error filtering data dictionary:", e$message))
    })
    
    if (nrow(question_fields) == 0) {
      cat("❌ No matching fields found in data dictionary\n")
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Questions Not Found", style = "color: #721c24;"),
        p("The expected question fields were not found in the data dictionary."),
        p("Expected fields:", paste(field_names, collapse = ", "))
      ))
    }
    
    # Special handling for Clinical Decision Making (text field)
    if (input$ass_obs_type == "1") {
      cat("✅ Building Clinical Decision Making text area\n")
      cdm_field <- question_fields[question_fields$field_name == "ass_obs_cdm", ]
      if (nrow(cdm_field) > 0) {
        return(
          div(class = "eval-field-group",
              tags$label(cdm_field$field_label, class = "eval-field-label required"),
              div(class = "textarea-container",
                  tags$textarea(
                    id = "ass_obs_cdm",
                    class = "form-control eval-textarea",
                    placeholder = "Describe the clinical decision making process observed...",
                    rows = "4"
                  )
              ),
              div(class = "eval-field-help", "Provide detailed observations of the resident's clinical reasoning.")
          )
        )
      }
    }
    
    # Check if form building functions exist
    if (!exists("build_radio_field_from_dict")) {
      cat("❌ build_radio_field_from_dict function missing\n")
      return(div(
        style = "padding: 1rem; background: #f8d7da; border: 1px solid #f5c6cb; border-radius: 8px; text-align: center;",
        h6("Function Missing", style = "color: #721c24;"),
        p("build_radio_field_from_dict function not found")
      ))
    }
    
    cat("✅ Form building functions exist\n")
    
    # Try to build questions
    tryCatch({
      question_elements <- lapply(1:nrow(question_fields), function(i) {
        field_row <- question_fields[i, ]
        cat("Building question", i, ":", field_row$field_name, "type:", field_row$field_type, "\n")
        
        # Special handling for Physical Exam yes/no fields
        if (field_row$field_name %in% c("ass_obs_pe_3", "ass_obs_pe_4")) {
          cat("  → Building as yes/no dropdown\n")
          return(div(class = "eval-field-group",
                     tags$label(field_row$field_label, class = "eval-field-label required"),
                     selectInput(
                       field_row$field_name,
                       label = NULL,
                       choices = list("Choose..." = "", "Yes" = "1", "No" = "0"),
                       selected = "",
                       width = "100%"
                     ),
                     div(class = "eval-field-help", "Select Yes or No")
          ))
        }
        
        # Build field based on type
        if (field_row$field_type == "radio") {
          cat("  → Building as radio buttons\n")
          build_radio_field_from_dict(field_row, required = TRUE)
        } else if (field_row$field_type == "dropdown") {
          cat("  → Building as dropdown\n")
          build_dropdown_field_from_dict(field_row, required = TRUE)
        } else {
          cat("  → Building as text input (default)\n")
          div(class = "eval-field-group",
              tags$label(field_row$field_label, class = "eval-field-label required"),
              textInput(
                field_row$field_name,
                label = NULL,
                placeholder = paste("Enter", tolower(field_row$field_label), "...")
              )
          )
        }
      })
      
      cat("✅ Built", length(question_elements), "question elements\n")
      cat("=== OBS DYNAMIC QUESTIONS DEBUG END ===\n")
      
      # Return the question elements
      return(tagList(question_elements))
      
    }, error = function(e) {
      cat("❌ Error building questions:", e$message, "\n")
      return(div("Error building questions:", e$message))
    })
  })
  
  # Also add this to check when the observation type changes:
  observeEvent(input$ass_obs_type, {
    cat("=== OBSERVATION TYPE CHANGED ===\n")
    cat("New value:", input$ass_obs_type, "\n")
    cat("Time:", Sys.time(), "\n")
  })
  
  # And check the manual selection:
  observeEvent(input$obs_type_manually_selected, {
    cat("=== MANUAL OBSERVATION SELECTION ===\n")
    if (!is.null(input$obs_type_manually_selected)) {
      selection_data <- input$obs_type_manually_selected
      cat("Selection data received:\n")
      print(selection_data)
    } else {
      cat("Selection data is NULL\n")
    }
  })
  
  # ============================================================================
  # UNIVERSAL SMART REFRESH FUNCTIONS
  # ============================================================================
  
  # Get all possible form field names that might exist
  get_all_form_field_names <- function() {
    c(
      # Universal fields
      "ass_plus", "ass_delta", "ass_obs_plus", "ass_obs_delta",
      
      # Continuity Clinic
      "ass_cc_quart",
      
      # Observation fields
      "ass_obs_type", "ass_obs_cdm", "ass_obs_acp", "ass_educat",
      
      # Physical Exam
      "ass_obs_pe1", "ass_obs_pe2", "ass_obs_pe_3", "ass_obs_pe_4",
      
      # Presentation
      "ass_obs_pres_1", "ass_obs_pres_2", "ass_obs_pres_3",
      
      # Written H&P
      "ass_obs_writehp_1", "ass_obs_writehp_2", "ass_obs_writehp_3", "ass_obs_writehp_4",
      
      # Daily Notes
      "ass_obs_daily_1", "ass_obs_daily_2", "ass_obs_daily_3", "ass_obs_daily_4",
      
      # Patient Discharge
      "ass_obs_dc_1", "ass_obs_dc_2", "ass_obs_dc_3", "ass_obs_dc_4",
      
      # Patient/Family Counseling
      "ass_obs_meet_1", "ass_obs_meet_2", "ass_obs_meet_3",
      
      # Supervision
      "ass_obs_senior_1", "ass_obs_senior_2", "ass_obs_senior_3", "ass_obs_senior_4",
      
      # Procedure
      "ass_obs_proc_type", "ass_obs_proc_prim", "ass_obs_proc_up", "ass_obs_proc_ass", "ass_obs_proc_pt_comf",
      
      # Multi-D Rounds
      "ass_obs_mdr_1", "ass_obs_mdr_2", "ass_obs_mdr_3",
      
      # Emergency
      "ass_obs_emer_sit", "ass_obs_emer_1", "ass_obs_emer_2", "ass_obs_emer_3", "ass_obs_emer_4", "ass_obs_emer5",
      
      # Single Day Clinic
      "ass_day_pc1_r1", "ass_day_pc3_r1", "ass_day_pc5_r3", "ass_cons_prof",
      
      # Consultation
      "ass_cons_care", "plan_pc3_r1", "ass_cons_testing_mk3", "ass_cons_comm_ics2_r1", "ass_cons_sdh_sbp2_r2",
      
      # Bridge Clinic
      "ass_bridge_sbp2_r2", "ass_bridge_pc5_r3", "ass_bridge_sbp3_r2",
      
      # Intern Inpatient
      "ass_int_ip_pc4_r1", "ass_int_ip_pc4_r2", "ass_int_ip_pc1_r1", "ass_int_ip_pc3_r1", 
      "ass_int_ip_mk1", "ass_int_ip_sbp2_r1", "ass_int_ip_ics3_r2", "ass_int_ip_pbl2_r2", "ass_int_ip_beside",
      
      # Senior Inpatient
      "ass_res_ip_pc4_r2", "ass_res_ip_ics2_r1", "ass_res_ip_mk1", 
      "ass_res_ip_sbp3_r1", "ass_res_ip_pc4_r1", "ass_res_ip_sbp3_r2"
    )
  }
  
  # Universal smart refresh function
  universal_smart_refresh <- function(preserve_faculty = TRUE, show_notification = TRUE) {
    cat("=== UNIVERSAL SMART REFRESH ===\n")
    
    # Preserve faculty info if requested
    preserved_faculty <- if (preserve_faculty && !is.null(values$selected_faculty)) {
      values$selected_faculty
    } else {
      NULL
    }
    
    # Clear all reactive values except faculty (if preserving)
    values$selected_resident <- NULL
    values$selected_eval_type <- NULL
    values$current_search_results <- NULL
    values$current_resident_results <- NULL
    values$cc_quarter_selection_info <- NULL
    values$obs_type_selection_info <- NULL
    
    # Clear all search fields
    updateTextInput(session, "resident_search", value = "")
    if (!preserve_faculty) {
      updateTextInput(session, "faculty_search", value = "")
    }
    
    # Clear ALL possible form fields universally
    all_fields <- get_all_form_field_names()
    
    for (field in all_fields) {
      try({
        # Try different input types
        updateTextInput(session, field, value = "")
        updateTextAreaInput(session, field, value = "")
        updateSelectInput(session, field, selected = "")
        updateRadioButtons(session, field, selected = character(0))
        updateCheckboxGroupInput(session, field, selected = character(0))
        updateNumericInput(session, field, value = NA)
      }, silent = TRUE)
    }
    
    # Hide any dynamic sections that might be visible
    session$sendCustomMessage("hideAllDynamicSections", list())
    
    # Restore faculty and set appropriate step
    if (preserve_faculty && !is.null(preserved_faculty)) {
      values$selected_faculty <- preserved_faculty
      values$current_step <- "resident"
      
      if (show_notification) {
        showNotification(
          paste0("🔄 Form cleared. Ready to evaluate another resident with ", preserved_faculty$fac_name),
          type = "default",
          duration = 3
        )
      }
      
      cat("Smart refresh completed. Faculty preserved:", preserved_faculty$fac_name, "\n")
    } else {
      values$selected_faculty <- NULL
      values$current_step <- "faculty"
      
      if (show_notification) {
        showNotification("🔄 Form completely cleared. Please select faculty to begin.", type = "default", duration = 3)
      }
      
      cat("Smart refresh completed. Everything cleared.\n")
    }
  }
  
  # Universal faculty data refresh
  universal_faculty_refresh <- function() {
    tryCatch({
      cat("=== UNIVERSAL FACULTY REFRESH ===\n")
      
      # Re-fetch faculty data from REDCap
      new_faculty_data <- get_faculty_data()
      
      if (!is.null(new_faculty_data)) {
        # Update global faculty data
        faculty_data <<- new_faculty_data
        cat("Faculty data refreshed. Total faculty:", nrow(faculty_data), "\n")
        
        # If a faculty member was selected, try to update their profile
        if (!is.null(values$selected_faculty)) {
          current_name <- values$selected_faculty$fac_name
          current_email <- values$selected_faculty$fac_email
          
          # Look for matching faculty in refreshed data
          name_field <- get_field_name(faculty_data, c("fac_name", "name", "faculty_name"))
          email_field <- get_field_name(faculty_data, c("fac_email", "email"))
          
          updated_faculty <- faculty_data %>%
            filter(
              (!is.na(.data[[name_field]]) & .data[[name_field]] == current_name) |
                (!is.na(.data[[email_field]]) & .data[[email_field]] == current_email)
            ) %>%
            slice(1)
          
          if (nrow(updated_faculty) > 0) {
            values$selected_faculty <- updated_faculty
            cat("Updated selected faculty with refreshed profile\n")
            
            showNotification(
              paste("✅", current_name, "'s profile updated with latest information!"),
              type = "default",
              duration = 4
            )
            
            return(TRUE)
          } else {
            cat("Could not find updated profile for current faculty\n")
            showNotification(
              paste("⚠️ Could not find updated profile for", current_name),
              type = "warning",
              duration = 4
            )
            return(FALSE)
          }
        } else {
          showNotification("✅ Faculty database refreshed!", type = "default", duration = 3)
          return(TRUE)
        }
        
      } else {
        cat("Failed to refresh faculty data\n")
        showNotification("❌ Could not refresh faculty database. Please try again.", type = "error", duration = 4)
        return(FALSE)
      }
      
    }, error = function(e) {
      cat("Error refreshing faculty data:", e$message, "\n")
      showNotification(
        paste("❌ Error refreshing faculty database:", e$message),
        type = "error",
        duration = 5
      )
      return(FALSE)
    })
  }
  
  # ============================================================================
  # UNIVERSAL SUBMISSION HANDLER
  # ============================================================================
  
  # Generic submission handler that works for all evaluation types
  universal_submission_handler <- function(eval_type, eval_type_name, validate_func, collect_func) {
    tryCatch({
      cat("=== UNIVERSAL SUBMISSION:", toupper(eval_type), "===\n")
      
      # Validate required objects exist
      if (is.null(values$selected_faculty)) {
        showNotification("Faculty selection was lost. Please select faculty again.", type = "error", duration = 4)
        values$current_step <- "faculty"
        return()
      }
      
      if (is.null(values$selected_resident)) {
        showNotification("Resident selection was lost. Please select resident again.", type = "error", duration = 4)
        values$current_step <- "resident" 
        return()
      }
      
      # Debug current state
      cat("Faculty:", values$selected_faculty$fac_name, "\n")
      cat("Resident:", values$selected_resident$name, "ID:", values$selected_resident$record_id, "\n")
      
      # Validate form
      missing_fields <- validate_func(input)
      
      if (length(missing_fields) > 0) {
        showNotification(
          paste("Please complete the following required fields:", paste(missing_fields, collapse = ", ")),
          type = "error",
          duration = 6
        )
        return()
      }
      
      cat("Form validation passed\n")
      
      # Collect data
      eval_data <- collect_func(input, values$selected_faculty, values$selected_resident)
      
      cat("Evaluation data collected for", eval_type, "\n")
      
      # Submit to REDCap
      result <- submit_evaluation_data(
        eval_data = eval_data,
        eval_type = eval_type,
        resident_id = values$selected_resident$record_id,
        token = rdm_token,
        url = url
      )
      
      cat("Submission successful for", eval_type, "\n")
      
      # Success notification
      showNotification(
        paste0("✅ ", eval_type_name, " evaluation submitted successfully!"),
        type = "default",
        duration = 5
      )
      
      # Universal smart refresh (preserve faculty)
      universal_smart_refresh(preserve_faculty = TRUE, show_notification = TRUE)
      
    }, error = function(e) {
      cat("Error submitting", eval_type, "evaluation:", e$message, "\n")
      showNotification(
        paste("❌ Error submitting", eval_type_name, "evaluation:", e$message),
        type = "error",
        duration = 8
      )
    })
  }
  
  # ============================================================================
  # UPDATE ALL SUBMISSION HANDLERS TO USE UNIVERSAL SYSTEM
  # ============================================================================
  
  # Replace all your existing submission handlers with these:
  
  observeEvent(input$submit_single_day_evaluation, {
    if (exists("validate_single_day_clinic_form") && exists("collect_single_day_clinic_data")) {
      universal_submission_handler("day", "Single Day Clinic", validate_single_day_clinic_form, collect_single_day_clinic_data)
    } else {
      showNotification("Single day clinic functions not found.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$submit_observation_evaluation, {
    if (exists("validate_observation_form") && exists("collect_observation_data")) {
      universal_submission_handler("obs", "Observation", validate_observation_form, collect_observation_data)
    } else {
      showNotification("Observation functions not found.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$submit_continuity_clinic_evaluation, {
    if (exists("validate_continuity_clinic_with_level") && exists("collect_continuity_clinic_data")) {
      # Special wrapper for CC validation (needs resident level)
      cc_validate <- function(input) validate_continuity_clinic_with_level(input, values$selected_resident$Level)
      universal_submission_handler("cc", "Continuity Clinic", cc_validate, collect_continuity_clinic_data)
    } else {
      showNotification("Continuity clinic functions not found.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$submit_consultation_evaluation, {
    if (exists("validate_consultation_form") && exists("collect_consultation_data")) {
      universal_submission_handler("cons", "Consultation", validate_consultation_form, collect_consultation_data)
    } else {
      showNotification("Consultation functions not found.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$submit_bridge_evaluation, {
    if (exists("validate_bridge_clinic_form") && exists("collect_bridge_clinic_data")) {
      universal_submission_handler("bridge", "Bridge Clinic", validate_bridge_clinic_form, collect_bridge_clinic_data)
    } else {
      showNotification("Bridge clinic functions not found.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$submit_intern_inpatient_evaluation, {
    if (exists("validate_intern_inpatient_form") && exists("collect_intern_inpatient_data")) {
      universal_submission_handler("int_ip", "Intern Inpatient", validate_intern_inpatient_form, collect_intern_inpatient_data)
    } else {
      showNotification("Intern inpatient functions not found.", type = "error", duration = 5)
    }
  })
  
  observeEvent(input$submit_senior_inpatient_evaluation, {
    if (exists("validate_senior_inpatient_form") && exists("collect_senior_inpatient_data")) {
      universal_submission_handler("res_ip", "Senior Inpatient", validate_senior_inpatient_form, collect_senior_inpatient_data)
    } else {
      showNotification("Senior inpatient functions not found.", type = "error", duration = 5)
    }
  })
  
  # ============================================================================
  # UNIVERSAL REFRESH BUTTONS AND CONTROLS
  # ============================================================================
  
  # Add refresh button to selected faculty info
  output$selected_faculty_info <- renderUI({
    if (!is.null(values$selected_faculty)) {
      faculty <- values$selected_faculty
      
      name_field <- get_field_name(faculty, c("fac_name", "name", "faculty_name"))
      dept_field <- get_field_name(faculty, c("fac_div", "department", "division"))
      role_field <- get_field_name(faculty, c("fac_fell", "role", "position"))
      
      tagList(
        div(
          strong(faculty[[name_field]]),
          br(),
          if (!is.null(dept_field) && !is.na(faculty[[dept_field]])) {
            paste("Department:", faculty[[dept_field]])
          } else {
            "Faculty member"
          },
          br(),
          if (!is.null(role_field) && !is.na(faculty[[role_field]])) {
            paste("Role:", faculty[[role_field]])
          },
          br(), br(),
          div(class = "faculty-selected", "✅ Faculty selected successfully!")
        ),
        br(),
        div(class = "text-center",
            # Universal refresh buttons
            div(class = "btn-group", role = "group",
                actionButton("universal_refresh_faculty", "🔄 Refresh Profile", 
                             class = "btn btn-outline-info btn-sm",
                             title = "Update faculty profile from database"),
                actionButton("universal_clear_form", "🧹 Clear Form", 
                             class = "btn btn-outline-secondary btn-sm",
                             title = "Clear all fields but keep faculty"),
                actionButton("universal_start_over", "🔄 Start Over", 
                             class = "btn btn-outline-warning btn-sm",
                             title = "Clear everything and start fresh")
            ),
            br(), br(),
            tags$small("🔄 Refresh Profile: Update your info | 🧹 Clear Form: Keep faculty, clear rest | 🔄 Start Over: Clear everything", 
                       style = "color: #666; font-style: italic; display: block; line-height: 1.4;")
        )
      )
    } else {
      div("Select your name from the search results above.")
    }
  })
  
  # Handle universal refresh buttons
  observeEvent(input$universal_refresh_faculty, {
    showNotification("🔄 Refreshing your faculty profile...", type = "default", duration = 2)
    universal_faculty_refresh()
  })
  
  observeEvent(input$universal_clear_form, {
    universal_smart_refresh(preserve_faculty = TRUE, show_notification = TRUE)
  })
  
  observeEvent(input$universal_start_over, {
    # Confirm before complete reset
    showModal(modalDialog(
      title = "Start Over?",
      "This will clear everything including your faculty selection. Are you sure?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_start_over", "Yes, Start Over", class = "btn btn-warning")
      )
    ))
  })
  
  observeEvent(input$confirm_start_over, {
    removeModal()
    universal_smart_refresh(preserve_faculty = FALSE, show_notification = TRUE)
  })
  
  # Add JavaScript to hide dynamic sections
  session$sendCustomMessage("addUniversalJS", list())
  
  # ============================================================================
  # FACULTY MODAL AND ADDITION
  # ============================================================================
  
  observeEvent(input$show_add_faculty_modal, {
    session$sendCustomMessage("resetFacultyForm", list())
  })
  
  observeEvent(input$reset_faculty_form, {
    updateTextInput(session, "fac_name", value = "")
    updateTextInput(session, "fac_email", value = "")
    updateSelectInput(session, "fac_clin", selected = "")
    updateSelectInput(session, "fac_div", selected = "")
    updateTextInput(session, "other_div", value = "")
    updateSelectInput(session, "fac_fell", selected = "")
    updateCheckboxGroupInput(session, "fac_med_ed", selected = character(0))
  })
  
  observeEvent(input$add_new_faculty, {
    cat("=== ADD NEW FACULTY BUTTON CLICKED ===\n")
    cat("Button clicked at:", Sys.time(), "\n")
    
    # Validation
    missing_fields <- character(0)
    
    if (is.null(input$fac_name) || trimws(input$fac_name) == "") {
      missing_fields <- c(missing_fields, "Full Name")
    }
    
    if (is.null(input$fac_email) || trimws(input$fac_email) == "") {
      missing_fields <- c(missing_fields, "Email Address")
    } else {
      email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
      if (!grepl(email_pattern, input$fac_email)) {
        missing_fields <- c(missing_fields, "Valid Email Address")
      }
    }
    
    if (is.null(input$fac_div) || input$fac_div == "") {
      missing_fields <- c(missing_fields, "Division/Section")
    }
    
    if (is.null(input$fac_fell) || input$fac_fell == "") {
      missing_fields <- c(missing_fields, "Faculty or Fellow")
    }
    
    if (length(missing_fields) > 0) {
      cat("VALIDATION FAILED - Missing fields:", paste(missing_fields, collapse = ", "), "\n")
      
      missing_list_html <- paste0(
        "<ul style='margin: 0.5rem 0 0 1.5rem; list-style-type: disc;'>",
        paste0("<li>", missing_fields, "</li>", collapse = ""),
        "</ul>"
      )
      
      session$sendCustomMessage("showValidationWarning", list(
        show = TRUE,
        fields = missing_list_html
      ))
      
      showNotification(
        paste("Please complete the following required fields:", paste(missing_fields, collapse = ", ")), 
        type = "error",
        duration = 5
      )
      return()
    }
    
    session$sendCustomMessage("showValidationWarning", list(show = FALSE))
    
    cat("All required fields validated successfully\n")
    
    # Check for preferred email domains (warning only)
    if (!grepl("@ssmhealth\\.com|@va\\.gov", input$fac_email, ignore.case = TRUE)) {
      cat("WARNING: Non-preferred email domain\n")
      showNotification("Note: Consider using @ssmhealth.com or @va.gov email if available.", 
                       type = "warning", duration = 3)
    }
    
    cat("Creating faculty data object...\n")
    
    new_faculty_data <- list(
      fac_name = trimws(input$fac_name),
      fac_email = trimws(input$fac_email),
      fac_clin = if(is.null(input$fac_clin) || input$fac_clin == "") "" else input$fac_clin,
      fac_div = input$fac_div,
      other_div = if (input$fac_div == "15") trimws(input$other_div) else "",
      fac_fell = input$fac_fell,
      fac_med_ed = if (length(input$fac_med_ed) > 0) paste(input$fac_med_ed, collapse = ",") else "",
      date_added = format(Sys.Date(), "%Y-%m-%d")
    )
    
    tryCatch({
      cat("=== STARTING FACULTY SUBMISSION ===\n")
      cat("Faculty data to submit:\n")
      print(new_faculty_data)
      
      if (exists("submit_new_faculty_to_redcap") && exists("fac_token") && exists("url")) {
        result <- submit_new_faculty_to_redcap(new_faculty_data, fac_token, url)
        cat("Submission result:", result, "\n")
      } else {
        cat("Warning: REDCap submission function or credentials not found\n")
      }
      
      showNotification(
        paste("✅ Welcome,", input$fac_name, "! Your profile has been added successfully."), 
        type = "default",
        duration = 5
      )
      
      # Create temporary faculty object for immediate use
      temp_faculty <- data.frame(
        fac_name = input$fac_name,
        fac_email = input$fac_email,
        fac_clin = switch(input$fac_clin,
                          "1" = "SSM",
                          "2" = "VA", 
                          "3" = "Other",
                          input$fac_clin),
        fac_div = switch(input$fac_div,
                         "1" = "Addiction Medicine",
                         "2" = "Allergy",
                         "3" = "Cardiology",
                         "4" = "Endocrinology",
                         "5" = "Gastroenterology",
                         "6" = "Geriatrics",
                         "7" = "GIM - Hospitalist",
                         "8" = "GIM - Primary Care",
                         "9" = "Hematology / Oncology",
                         "10" = "Infectious Disease",
                         "11" = "Nephrology",
                         "12" = "Palliative Care",
                         "13" = "Pulmonary / Critical Care",
                         "14" = "Rheumatology",
                         "15" = if (!is.null(input$other_div) && input$other_div != "") input$other_div else "Other",
                         "Other"),
        fac_fell = switch(input$fac_fell,
                          "1" = "Faculty",
                          "2" = "Fellow",
                          input$fac_fell),
        status = "active",
        stringsAsFactors = FALSE
      )
      
      values$selected_faculty <- temp_faculty
      values$current_step <- "resident"
      
      session$sendCustomMessage("closeFacultyModal", list())
      
      # Reset form inputs
      updateTextInput(session, "fac_name", value = "")
      updateTextInput(session, "fac_email", value = "")
      updateSelectInput(session, "fac_clin", selected = "")
      updateSelectInput(session, "fac_div", selected = "")
      updateTextInput(session, "other_div", value = "")
      updateSelectInput(session, "fac_fell", selected = "")
      updateCheckboxGroupInput(session, "fac_med_ed", selected = character(0))
      
    }, error = function(e) {
      cat("Error submitting faculty profile:", e$message, "\n")
      showNotification(paste("❌ Error submitting faculty profile:", e$message), 
                       type = "error", duration = 8)
    })
  })
}