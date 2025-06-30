# server.R - Faculty Evaluation App (Updated with Resident Selection)

server <- function(input, output, session) {
  
  # Reactive values to track app state
  values <- reactiveValues(
    selected_faculty = NULL,
    selected_resident = NULL,
    current_search_results = NULL,
    current_resident_results = NULL
  )
  
  # Show next steps when faculty is selected
  output$show_next_steps <- reactive({
    !is.null(values$selected_faculty)
  })
  outputOptions(output, "show_next_steps", suspendWhenHidden = FALSE)
  
  # Show evaluation step when both faculty and resident are selected
  output$show_evaluation_step <- reactive({
    !is.null(values$selected_faculty) && !is.null(values$selected_resident)
  })
  outputOptions(output, "show_evaluation_step", suspendWhenHidden = FALSE)
  
  # ============================================================================
  # FACULTY SEARCH AND SELECTION
  # ============================================================================
  
  # Faculty search functionality
  filtered_faculty <- reactive({
    req(input$faculty_search)
    req(faculty_data)
    
    search_term <- tolower(trimws(input$faculty_search))
    if (nchar(search_term) < 2) return(NULL)
    
    # Search in name field - prioritizing fac_name based on your structure
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
  # RESIDENT SEARCH AND SELECTION
  # ============================================================================
  
  # Resident search functionality
  filtered_residents <- reactive({
    req(input$resident_search)
    req(resident_data)
    
    search_term <- tolower(trimws(input$resident_search))
    if (nchar(search_term) < 2) return(NULL)
    
    # Search in name field
    filtered <- resident_data %>%
      filter(!is.na(name) & 
               grepl(search_term, tolower(name), fixed = TRUE)) %>%
      arrange(Level, name) %>%  # Sort by level then name
      head(15)  # Limit results
    
    cat("Resident search for '", search_term, "' returned ", nrow(filtered), " results\n")
    
    return(filtered)
  })
  
  # Render resident search results
  output$resident_search_results <- renderUI({
    resident_results <- filtered_residents()
    
    if (is.null(resident_results) || nrow(resident_results) == 0) {
      if (!is.null(input$resident_search) && nchar(trimws(input$resident_search)) >= 2) {
        return(div(style = "padding: 10px; color: #666;", 
                   "No residents found. Please check spelling."))
      } else {
        return(div(style = "padding: 10px; color: #666;", 
                   "Start typing to search for a resident..."))
      }
    }
    
    # Store search results
    values$current_resident_results <- resident_results
    
    result_list <- lapply(1:nrow(resident_results), function(i) {
      resident <- resident_results[i, ]
      
      button_id <- paste0("select_resident_", i)
      
      # Build display text
      resident_text <- paste0(resident$name, " (", resident$Level, ")")
      
      # Add graduation year if available
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
    
    # Add rotator option at the end
    rotator_button <- div(class = "search-result rotator-result",
                          actionButton("select_rotator", 
                                       label = "🔄 Rotator (External Resident)",
                                       style = "background: #e8f4fd; border: 1px solid #0066a1; text-align: left; width: 100%; padding: 10px; font-weight: 600;",
                                       class = "btn btn-info")
    )
    
    do.call(tagList, c(result_list, list(rotator_button)))
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
              
              showNotification(
                paste("Selected resident:", values$selected_resident$name), 
                type = "default"
              )
            }
          })
        })
      }
    }
  })
  
  # Handle rotator selection
  observeEvent(input$select_rotator, {
    # For now, create a placeholder rotator record
    rotator_record <- data.frame(
      record_id = "157",
      name = "Rotator",
      Level = "Rotator",
      type = "Rotator",
      grad_yr = NA,
      stringsAsFactors = FALSE
    )
    
    values$selected_resident <- rotator_record
    
    cat("Selected rotator\n")
    
    # Show modal or notification for rotator details (placeholder for future)
    showNotification("Rotator selected. Additional rotator features coming soon.", 
                     type = "message", duration = 3)
  })
  
  # Display selected resident info
  output$selected_resident_info <- renderUI({
    if (!is.null(values$selected_resident)) {
      resident <- values$selected_resident
      
      if (resident$record_id == "157") {
        # Special display for rotator
        div(
          strong("Rotator Selected"),
          br(),
          "External/Visiting Resident",
          br(), br(),
          div(class = "resident-selected",
              "✅ Rotator selected!")
        )
      } else {
        # Regular resident display
        div(
          strong(resident$name),
          br(),
          paste("Level:", resident$Level),
          br(),
          if (!is.na(resident$grad_yr)) {
            paste("Graduation Year:", resident$grad_yr)
          },
          br(),
          if (!is.na(resident$type)) {
            paste("Program Type:", resident$type)
          },
          br(), br(),
          div(class = "resident-selected",
              "✅ Resident selected successfully!")
        )
      }
    } else {
      div("Select a resident from the search results above.")
    }
  })
  
  # ============================================================================
  # MODAL AND FACULTY ADDITION
  # ============================================================================
  
  # Handle modal opening
  observeEvent(input$show_add_faculty_modal, {
    # Reset validation warnings when modal opens
    session$sendCustomMessage("resetFacultyForm", list())
  })
  
  # Reset form when modal opens (triggered by JavaScript)
  observeEvent(input$reset_faculty_form, {
    updateTextInput(session, "fac_name", value = "")
    updateTextInput(session, "fac_email", value = "")
    updateSelectInput(session, "fac_clin", selected = "")
    updateSelectInput(session, "fac_div", selected = "")
    updateTextInput(session, "other_div", value = "")
    updateSelectInput(session, "fac_fell", selected = "")
    updateCheckboxGroupInput(session, "fac_med_ed", selected = character(0))
  })
  
  # Updated faculty submission with modal handling
  observeEvent(input$add_new_faculty, {
    cat("=== ADD NEW FACULTY BUTTON CLICKED ===\n")
    cat("Button clicked at:", Sys.time(), "\n")
    
    # Check what inputs we received
    cat("Input values:\n")
    cat("fac_name:", if(is.null(input$fac_name)) "NULL" else paste0("'", input$fac_name, "'"), "\n")
    cat("fac_email:", if(is.null(input$fac_email)) "NULL" else paste0("'", input$fac_email, "'"), "\n")
    cat("fac_clin:", if(is.null(input$fac_clin)) "NULL" else paste0("'", input$fac_clin, "'"), "\n")
    cat("fac_div:", if(is.null(input$fac_div)) "NULL" else paste0("'", input$fac_div, "'"), "\n")
    cat("fac_fell:", if(is.null(input$fac_fell)) "NULL" else paste0("'", input$fac_fell, "'"), "\n")
    cat("other_div:", if(is.null(input$other_div)) "NULL" else paste0("'", input$other_div, "'"), "\n")
    cat("fac_med_ed:", if(is.null(input$fac_med_ed)) "NULL" else paste(input$fac_med_ed, collapse = ","), "\n")
    
    # Validation with detailed error messages
    missing_fields <- character(0)
    
    # Check required fields
    if (is.null(input$fac_name) || trimws(input$fac_name) == "") {
      missing_fields <- c(missing_fields, "Full Name")
    }
    
    if (is.null(input$fac_email) || trimws(input$fac_email) == "") {
      missing_fields <- c(missing_fields, "Email Address")
    } else {
      # Validate email format
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
    
    # If there are missing fields, show the warning
    if (length(missing_fields) > 0) {
      cat("VALIDATION FAILED - Missing fields:", paste(missing_fields, collapse = ", "), "\n")
      
      # Create the missing fields list HTML
      missing_list_html <- paste0(
        "<ul style='margin: 0.5rem 0 0 1.5rem; list-style-type: disc;'>",
        paste0("<li>", missing_fields, "</li>", collapse = ""),
        "</ul>"
      )
      
      # Show the warning div with JavaScript
      session$sendCustomMessage("showValidationWarning", list(
        show = TRUE,
        fields = missing_list_html
      ))
      
      # Also show a notification
      showNotification(
        paste("Please complete the following required fields:", paste(missing_fields, collapse = ", ")), 
        type = "error",
        duration = 5
      )
      return()
    }
    
    # Hide the validation warning if validation passes
    session$sendCustomMessage("showValidationWarning", list(show = FALSE))
    
    cat("All required fields validated successfully\n")
    
    # Check for preferred email domains (warning only, doesn't stop submission)
    if (!grepl("@ssmhealth\\.com|@va\\.gov", input$fac_email, ignore.case = TRUE)) {
      cat("WARNING: Non-preferred email domain\n")
      showNotification("Note: Consider using @ssmhealth.com or @va.gov email if available.", 
                       type = "warning", duration = 3)
    }
    
    cat("Creating faculty data object...\n")
    
    # Create new faculty record
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
      
      # Submit to REDCap faculty database
      result <- submit_new_faculty_to_redcap(new_faculty_data, fac_token, url)
      cat("Submission result:", result, "\n")
      
      showNotification(
        paste("✅ Welcome,", input$fac_name, "! Your profile has been added successfully."), 
        type = "default",
        duration = 5
      )
      
      # Create temporary faculty object for immediate use
      temp_faculty <- data.frame(
        fac_name = input$fac_name,
        fac_email = input$fac_email,
        fac_clin = case_when(
          input$fac_clin == "1" ~ "SSM",
          input$fac_clin == "2" ~ "VA", 
          input$fac_clin == "3" ~ "Other",
          TRUE ~ input$fac_clin
        ),
        fac_div = if (input$fac_div == "15") input$other_div else {
          case_when(
            input$fac_div == "1" ~ "Addiction Medicine",
            input$fac_div == "2" ~ "Allergy",
            input$fac_div == "3" ~ "Cardiology",
            input$fac_div == "4" ~ "Endocrinology",
            input$fac_div == "5" ~ "Gastroenterology",
            input$fac_div == "6" ~ "Geriatrics",
            input$fac_div == "7" ~ "GIM - Hospitalist",
            input$fac_div == "8" ~ "GIM - Primary Care",
            input$fac_div == "9" ~ "Hematology / Oncology",
            input$fac_div == "10" ~ "Infectious Disease",
            input$fac_div == "11" ~ "Nephrology",
            input$fac_div == "12" ~ "Palliative Care",
            input$fac_div == "13" ~ "Pulmonary / Critical Care",
            input$fac_div == "14" ~ "Rheumatology",
            TRUE ~ "Other"
          )
        },
        fac_fell = case_when(
          input$fac_fell == "1" ~ "Faculty",
          input$fac_fell == "2" ~ "Fellow",
          TRUE ~ input$fac_fell
        ),
        status = "active",
        stringsAsFactors = FALSE
      )
      
      values$selected_faculty <- temp_faculty
      
      # Close the modal and reset form
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
  
  # ============================================================================
  # NAVIGATION
  # ============================================================================
  
  # Start over button
  observeEvent(input$start_over, {
    values$selected_faculty <- NULL
    values$selected_resident <- NULL
    values$current_search_results <- NULL
    values$current_resident_results <- NULL
    
    # Clear inputs
    updateTextInput(session, "faculty_search", value = "")
    updateTextInput(session, "resident_search", value = "")
  })
}