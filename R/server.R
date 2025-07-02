# server.R - Faculty Evaluation App (Updated with All Evaluation Types)

server <- function(input, output, session) {
  
  # Reactive values to track app state
  values <- reactiveValues(
    selected_faculty = NULL,
    selected_resident = NULL,
    selected_eval_type = NULL,
    current_search_results = NULL,
    current_resident_results = NULL,
    current_step = "intro"  # Start with intro page
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
    values$current_step <- "intro"  # Go back to intro page
    
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
  
  # Handle faculty selection clicks
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
              showNotification(paste("Selected:", values$selected_faculty[[name_field]]), type = "default")
            }
          })
        })
      }
    }
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
              showNotification(paste("Selected resident:", values$selected_resident$name), type = "default")
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
          showNotification(paste("Selected evaluation:", eval_info$name), type = "default")
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
                               # Send resident level to JavaScript
                               session$sendCustomMessage("setResidentLevel", values$selected_resident$Level)
                               build_continuity_clinic_form()
                             } else {
                               build_form_error("Continuity Clinic form builder function not found.")
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
  
  # Render the quarter dropdown dynamically
  output$quarter_dropdown <- renderUI({
    cat("=== RENDERING QUARTER DROPDOWN (REAL DATA) ===\n")
    
    if (!exists("rdm_dict") || is.null(rdm_dict)) {
      return(div("Data dictionary not available"))
    }
    
    # Get quarter field from data dictionary
    quarter_field <- rdm_dict %>%
      filter(form_name == "assessment") %>%
      filter(field_name == "ass_cc_quart") %>%
      slice(1)
    
    if (nrow(quarter_field) == 0) {
      return(div("Quarter field not found in data dictionary"))
    }
    
    # Parse choices from data dictionary
    choices_text <- quarter_field$select_choices_or_calculations
    choices_list <- strsplit(choices_text, " \\| ")[[1]]
    choices <- list("Choose quarter/evaluation type..." = "")
    
    for (choice in choices_list) {
      parts <- strsplit(choice, ", ", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        value <- trimws(parts[1])
        label <- trimws(paste(parts[2:length(parts)], collapse = ", "))
        choices[[label]] <- value
      }
    }
    
    # Get real completion status if resident is selected
    completion_table <- NULL
    if (!is.null(values$selected_resident)) {
      cat("🔍 Getting REAL completion status for resident:", values$selected_resident$name, "\n")
      
      # Use the REAL function
      completion_data <- get_cc_completion_status(values$selected_resident$name, values$selected_resident$Level)
      
      cat("📊 Completion data received, rows:", ifelse(is.null(completion_data), 0, nrow(completion_data)), "\n")
      
      if (!is.null(completion_data)) {
        completion_table <- build_completion_status_display(completion_data)
      }
    } else {
      cat("ℹ️ No resident selected\n")
    }
    
    cat("🏁 Returning quarter dropdown UI\n")
    
    tagList(
      # Show completion status if available
      completion_table,
      
      # Quarter selection dropdown
      div(class = "eval-field-group",
          tags$label(quarter_field$field_label, class = "eval-field-label required"),
          selectInput(
            "ass_cc_quart",
            label = NULL,
            choices = choices,
            selected = "",
            width = "100%"
          )
      )
    )
  })
  
  # Render assessment subtitle based on quarter and level
  output$cc_assessment_subtitle <- renderText({
    req(input$ass_cc_quart)
    req(values$selected_resident)
    
    quarter_desc <- get_quarter_description(input$ass_cc_quart, values$selected_resident$Level)
    paste("Rate the resident's performance in:", quarter_desc)
  })
  
  output$cc_dynamic_questions <- renderUI({
    cat("=== DEBUG: CC DYNAMIC QUESTIONS ===\n")
    
    # Check requirements
    if (is.null(input$ass_cc_quart) || input$ass_cc_quart == "") {
      cat("❌ No quarter selected\n")
      return(div(style = "background: red; color: white; padding: 1rem;", "No quarter selected"))
    }
    
    if (is.null(values$selected_resident)) {
      cat("❌ No resident selected\n") 
      return(div(style = "background: red; color: white; padding: 1rem;", "No resident selected"))
    }
    
    cat("✅ Quarter:", input$ass_cc_quart, "Resident:", values$selected_resident$Level, "\n")
    
    # Get field names
    field_names <- get_cc_fields_for_quarter_and_level(input$ass_cc_quart, values$selected_resident$Level)
    cat("📋 Found", length(field_names), "fields:", paste(field_names, collapse = ", "), "\n")
    
    if (length(field_names) == 0) {
      return(div(style = "background: orange; color: white; padding: 1rem;", "No fields found"))
    }
    
    # SIMPLE TEST: Just create basic radio buttons without using data dictionary
    cat("🧪 Creating simple test radio buttons\n")
    
    simple_questions <- list(
      div(
        style = "background: lightblue; padding: 1rem; margin: 1rem 0; border-radius: 8px;",
        h5("🧪 Test Question 1: Respond to inbasket items"),
        radioButtons(
          inputId = "ass_cc_inb_resp",
          label = NULL,
          choices = list(
            "1 - Infrequently" = "1",
            "2 - Intermittently" = "2", 
            "3 - Most of the time" = "3",
            "4 - Always" = "4",
            "5 - Unable to answer" = "5"
          ),
          selected = character(0)
        )
      ),
      
      div(
        style = "background: lightgreen; padding: 1rem; margin: 1rem 0; border-radius: 8px;",
        h5("🧪 Test Question 2: Review results"),
        radioButtons(
          inputId = "ass_cc_inb_resu",
          label = NULL,
          choices = list(
            "1 - Infrequently" = "1",
            "2 - Intermittently" = "2",
            "3 - Most of the time" = "3", 
            "4 - Always" = "4",
            "5 - Unable to answer" = "5"
          ),
          selected = character(0)
        )
      ),
      
      div(
        style = "background: lightyellow; padding: 1rem; margin: 1rem 0; border-radius: 8px;",
        h5("🧪 Debug Info"),
        p("Quarter selected:", input$ass_cc_quart),
        p("Resident level:", values$selected_resident$Level),
        p("Fields expected:", paste(field_names, collapse = ", "))
      )
    )
    
    cat("✅ Returning", length(simple_questions), "test elements\n")
    return(simple_questions)
  })
  
  
  # ============================================================================
  # ALSO ADD: CC Assessment Subtitle Output (ADD TO SERVER.R)
  # ============================================================================
  
  output$cc_assessment_subtitle <- renderText({
    req(input$ass_cc_quart)
    req(values$selected_resident)
    
    quarter_desc <- get_quarter_description(input$ass_cc_quart, values$selected_resident$Level)
    paste("Rate the resident's performance in:", quarter_desc)
  })
  
  
  # Handle quarter selection change (reactive to update questions)
  observeEvent(input$cc_quarter_selected, {
    cat("Quarter selected:", input$cc_quarter_selected, "\n")
    # The dynamic questions will automatically update via the reactive output above
  })
  
  
  observeEvent(input$ass_cc_quart, {
    cat("📋 Quarter dropdown changed to:", input$ass_cc_quart, "\n")
    
    # The cc_dynamic_questions output will automatically update
    # because it's reactive to input$ass_cc_quart
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
  
  # Helper function to build error messages
  build_form_error <- function(message) {
    div(style = "text-align: center; padding: 2rem;",
        h5("Form Builder Error", style = "color: #dc3545;"),
        p(message),
        br(),
        actionButton("back_to_eval_selection", "← Back to Evaluation Types", class = "btn btn-secondary"))
  }
  
  # ============================================================================
  # EVALUATION SUBMISSION HANDLERS
  # ============================================================================
  
  # Generic submission handler function
  handle_evaluation_submission <- function(eval_type, validate_func, collect_func) {
    cat("=== SUBMITTING", toupper(eval_type), "EVALUATION ===\n")
    
    if (!exists("validate_single_day_clinic_form")) {
      showNotification("Validation function not found. Please check your form builder functions.", 
                       type = "error", duration = 5)
      return()
    }
    
    missing_fields <- validate_func(input)
    
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
    
    if (!exists("collect_single_day_clinic_data")) {
      showNotification("Data collection function not found. Please check your form builder functions.", 
                       type = "error", duration = 5)
      return()
    }
    
    eval_data <- collect_func(input, values$selected_faculty, values$selected_resident)
    
    cat("Evaluation data collected:\n")
    print(eval_data)
    
    tryCatch({
      if (!exists("submit_evaluation_data")) {
        showNotification("Submission function not found. Please check your evaluation helper functions.", 
                         type = "error", duration = 5)
        return()
      }
      
      result <- submit_evaluation_data(
        eval_data = eval_data,
        eval_type = eval_type,
        resident_id = values$selected_resident$record_id,
        token = if(exists("rdm_token")) rdm_token else NULL,
        url = if(exists("url")) url else NULL
      )
      
      cat("Submission successful:", result, "\n")
      
      eval_name <- switch(eval_type,
                          "day" = "Single Day Clinic",
                          "cons" = "Consultation", 
                          "bridge" = "Bridge Clinic",
                          "int_ip" = "Intern Inpatient",
                          "res_ip" = "Senior Inpatient",
                          eval_type
      )
      
      showNotification(
        paste0("✅ ", eval_name, " evaluation submitted successfully!"),
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
      cat("Error submitting evaluation:", e$message, "\n")
      showNotification(
        paste("❌ Error submitting evaluation:", e$message),
        type = "error",
        duration = 8
      )
    })
  }
  
  # Individual submission handlers
  observeEvent(input$submit_single_day_evaluation, {
    handle_evaluation_submission("day", validate_single_day_clinic_form, collect_single_day_clinic_data)
  })
  
  observeEvent(input$submit_consultation_evaluation, {
    handle_evaluation_submission("cons", validate_consultation_form, collect_consultation_data)
  })
  
  observeEvent(input$submit_bridge_evaluation, {
    handle_evaluation_submission("bridge", validate_bridge_clinic_form, collect_bridge_clinic_data)
  })
  
  observeEvent(input$submit_intern_inpatient_evaluation, {
    handle_evaluation_submission("int_ip", validate_intern_inpatient_form, collect_intern_inpatient_data)
  })
  
  observeEvent(input$submit_senior_inpatient_evaluation, {
    handle_evaluation_submission("res_ip", validate_senior_inpatient_form, collect_senior_inpatient_data)
  })
  
  # Generic submission for unimplemented forms
  observeEvent(input$submit_evaluation, {
    showNotification("Evaluation submission will be implemented with actual forms.", type = "message", duration = 3)
  })
  
  # ============================================================================
  # MODAL AND FACULTY ADDITION
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

build_cc_completion_reactable <- function(resident_name, resident_level) {
  tryCatch({
    # This would require the create_cc_table function to be adapted
    # For now, we'll use the simpler grid approach above
    
    completion_data <- get_cc_completion_status(resident_name, resident_level)
    
    if (is.null(completion_data)) {
      return(div(
        style = "padding: 1rem; background: #f8f9fa; border-radius: 8px; text-align: center; color: #666;",
        "📊 Completion status not available"
      ))
    }
    
    # Convert to format expected by create_cc_table if you want to use that function
    # This would require adapting your existing create_cc_table function
    
    return(div("Reactable version would go here"))
    
  }, error = function(e) {
    return(div(
      style = "padding: 1rem; background: #ffebee; border-radius: 8px; text-align: center; color: #c62828;",
      "⚠️ Error loading completion status"
    ))
  })
  
  
}