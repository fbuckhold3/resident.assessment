# server.R - Faculty Evaluation App (Simple Start)

server <- function(input, output, session) {
  
  # Reactive values to track app state
  values <- reactiveValues(
    selected_faculty = NULL,
    current_search_results = NULL
  )
  
  # Show next steps when faculty is selected
  output$show_next_steps <- reactive({
    !is.null(values$selected_faculty)
  })
  outputOptions(output, "show_next_steps", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # FACULTY SEARCH AND SELECTION
  # ============================================================================
  
  # Faculty search functionality
  filtered_faculty <- reactive({
    req(input$faculty_search)
    req(faculty_data)
    
    search_term <- tolower(trimws(input$faculty_search))
    if (nchar(search_term) < 2) return(NULL)
    
    # Search in name field - adjust field name based on your faculty database
    # Try common field names - prioritizing fac_name based on your structure
    name_field <- if ("fac_name" %in% names(faculty_data)) {
      "fac_name"
    } else if ("name" %in% names(faculty_data)) {
      "name"
    } else if ("faculty_name" %in% names(faculty_data)) {
      "faculty_name"
    } else {
      names(faculty_data)[1]  # fallback to first column
    }
    
    cat("Using name field:", name_field, "\n")
    
    filtered <- faculty_data %>%
      filter(!is.na(.data[[name_field]]) & 
               grepl(search_term, tolower(.data[[name_field]]), fixed = TRUE)) %>%
      head(10)  # Limit results
    
    cat("Faculty search for '", search_term, "' returned ", nrow(filtered), " results\n")
    
    return(filtered)
  })
  
  # Render faculty search results
  output$faculty_search_results <- renderUI({
    faculty_results <- filtered_faculty()
    
    if (is.null(faculty_results) || nrow(faculty_results) == 0) {
      if (!is.null(input$faculty_search) && nchar(trimws(input$faculty_search)) >= 2) {
        return(div(style = "padding: 10px; color: #666;", 
                   "No faculty found. Please check spelling or add yourself below."))
      } else {
        return(div(style = "padding: 10px; color: #666;", 
                   "Start typing to search for your name..."))
      }
    }
    
    # Store search results
    values$current_search_results <- faculty_results
    
    # Determine name field - prioritizing fac_name
    name_field <- if ("fac_name" %in% names(faculty_results)) {
      "fac_name"
    } else if ("name" %in% names(faculty_results)) {
      "name"
    } else if ("faculty_name" %in% names(faculty_results)) {
      "faculty_name"
    } else {
      names(faculty_results)[1]
    }
    
    result_list <- lapply(1:nrow(faculty_results), function(i) {
      faculty <- faculty_results[i, ]
      
      button_id <- paste0("select_faculty_", i)
      
      # Build display text - adjust field names based on your data
      faculty_text <- faculty[[name_field]]
      
      # Add department/division if available
      dept_field <- if ("fac_div" %in% names(faculty)) {
        "fac_div"
      } else if ("department" %in% names(faculty)) {
        "department"
      } else if ("division" %in% names(faculty)) {
        "division"
      } else {
        NULL
      }
      
      if (!is.null(dept_field) && !is.na(faculty[[dept_field]])) {
        faculty_text <- paste0(faculty_text, " - ", faculty[[dept_field]])
      }
      
      # Add faculty/fellow designation if available
      role_field <- if ("fac_fell" %in% names(faculty)) {
        "fac_fell"
      } else if ("role" %in% names(faculty)) {
        "role"
      } else if ("position" %in% names(faculty)) {
        "position"
      } else {
        NULL
      }
      
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
              
              # Determine name field for logging - prioritizing fac_name
              name_field <- if ("fac_name" %in% names(values$selected_faculty)) {
                "fac_name"
              } else if ("name" %in% names(values$selected_faculty)) {
                "name"
              } else {
                names(values$selected_faculty)[1]
              }
              
              cat("Selected faculty:", values$selected_faculty[[name_field]], "\n")
              
              showNotification(
                paste("Selected:", values$selected_faculty[[name_field]]), 
                type = "default"
              )
            }
          })
        })
      }
    }
  })
  
  # Display selected faculty info
  output$selected_faculty_info <- renderUI({
    if (!is.null(values$selected_faculty)) {
      faculty <- values$selected_faculty
      
      # Determine field names - prioritizing your field structure
      name_field <- if ("fac_name" %in% names(faculty)) {
        "fac_name"
      } else if ("name" %in% names(faculty)) {
        "name"
      } else {
        names(faculty)[1]
      }
      
      dept_field <- if ("fac_div" %in% names(faculty)) {
        "fac_div"
      } else if ("department" %in% names(faculty)) {
        "department"
      } else {
        NULL
      }
      
      role_field <- if ("fac_fell" %in% names(faculty)) {
        "fac_fell"
      } else if ("role" %in% names(faculty)) {
        "role"
      } else {
        NULL
      }
      
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
        div(class = "faculty-selected",
            "✅ Faculty selected successfully!")
      )
    } else {
      div("Select your name from the search results above.")
    }
  })
  
  # ============================================================================
  # ADD NEW FACULTY
  # ============================================================================
  
  # Handle adding new faculty
  observeEvent(input$add_new_faculty, {
    req(input$new_faculty_name)
    req(input$new_faculty_role)
    req(input$new_faculty_dept)
    
    # Validate email if provided
    if (!is.null(input$new_faculty_email) && input$new_faculty_email != "") {
      email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
      if (!grepl(email_pattern, input$new_faculty_email)) {
        showNotification("Please enter a valid email address.", type = "error")
        return()
      }
    }
    
    # Create new faculty record
    new_faculty_data <- list(
      name = input$new_faculty_name,
      role = input$new_faculty_role,
      department = input$new_faculty_dept,
      email = input$new_faculty_email %||% "",
      status = "pending_review",
      date_added = format(Sys.Date(), "%Y-%m-%d")
    )
    
    tryCatch({
      # Submit to REDCap faculty database
      submit_new_faculty_to_redcap(new_faculty_data, fac_token, url)
      
      showNotification(
        paste("Faculty profile for", input$new_faculty_name, "has been submitted for review."), 
        type = "default"
      )
      
      # Create temporary faculty object for immediate use
      temp_faculty <- data.frame(
        name = input$new_faculty_name,
        department = input$new_faculty_dept,
        role = input$new_faculty_role,
        email = input$new_faculty_email %||% "",
        status = "pending",
        stringsAsFactors = FALSE
      )
      
      values$selected_faculty <- temp_faculty
      
      # Reset form
      updateTextInput(session, "new_faculty_name", value = "")
      updateSelectInput(session, "new_faculty_role", selected = "")
      updateTextInput(session, "new_faculty_dept", value = "")
      updateTextInput(session, "new_faculty_email", value = "")
      updateCheckboxInput(session, "faculty_not_found", value = FALSE)
      
    }, error = function(e) {
      showNotification(paste("Error submitting faculty profile:", e$message), type = "error")
    })
  })
  
  # ============================================================================
  # NAVIGATION
  # ============================================================================
  
  # Start over button
  observeEvent(input$start_over, {
    values$selected_faculty <- NULL
    values$current_search_results <- NULL
    
    # Clear inputs
    updateTextInput(session, "faculty_search", value = "")
    updateCheckboxInput(session, "faculty_not_found", value = FALSE)
  })
}