# ui.R - Faculty Evaluation App (Updated with Intro Page)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly", version = 5),  # Bootstrap 5 for modal support
  
  # External CSS and JavaScript files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/assessment.css"),
    tags$script(src = "js/assessment.js")
  ),
  
  # SSM Health Header
  div(class = "container-fluid mb-4",
      div(class = "row",
          div(class = "col-12",
              div(class = "d-flex justify-content-between align-items-center py-3",
                  div(class = "navbar-brand", "IMSLU Evaluation of Residents by Faculty and Fellows"),
                  div(class = "text-muted", "Internal Medicine Residency Program")
              )
          )
      )
  ),
  
  div(class = "container",
      
      # Intro Page (Step 0)
      conditionalPanel(
        condition = "output.current_step == 'intro'",
        div(class = "intro-page-mobile",
            div(class = "intro-card-mobile",
                div(class = "intro-header-mobile",
                    h2("IMSLU Evaluation", class = "intro-title-mobile"),
                    div(class = "intro-subtitle-mobile", "Faculty & Fellow Evaluation System")
                ),
                
                div(class = "intro-content-mobile",
                    p("This form is for Faculty and Fellows evaluating residents in the SSM / SLUH Internal Medicine Residency Program."),
                    
                    p("If you haven't used this form before, you'll be able to add your information on the next step."),
                    
                    div(class = "intro-footer-mobile",
                        actionButton("begin_evaluation", "Begin Evaluation", 
                                     class = "btn btn-primary btn-lg intro-btn-mobile"),
                        br(), br(),
                        div(class = "intro-note-mobile", 
                            "For authorized faculty and fellows only.")
                    )
                )
            )
        )
      ),
      
      # Step 1: Faculty Search/Login 
      conditionalPanel(
        condition = "output.current_step == 'faculty'",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Step 1: Faculty Login"),
                p("Search for your name to begin evaluating residents", style = "margin: 0; opacity: 0.9;")
            ),
            div(style = "padding: 2rem 0;",
                fluidRow(
                  column(8,
                         div(class = "mb-3",
                             h5("Search for Your Name", class = "question-text"),
                             textInput("faculty_search", 
                                       "", 
                                       placeholder = "Start typing your name...")
                         ),
                         div(id = "faculty_search_results",
                             uiOutput("faculty_search_results")
                         )
                  ),
                  column(4,
                         div(class = "faculty-card",
                             h5("Selected Faculty", class = "question-text"),
                             uiOutput("selected_faculty_info")
                         )
                  )
                )
            ),
            
            # Add Faculty button
            div(class = "text-center mt-4",
                div(class = "add-faculty-prompt",
                    p("Can't find your name?", 
                      style = "margin-bottom: 0.5rem; color: var(--ssm-text-secondary); font-size: 0.95rem;"),
                    actionButton("show_add_faculty_modal", 
                                 "Add Yourself to Database", 
                                 class = "btn btn-outline-primary add-faculty-btn",
                                 `data-bs-toggle` = "modal",
                                 `data-bs-target` = "#addFacultyModal")
                )
            )
        )
      ),
      
      # Step 2: Resident Selection
      conditionalPanel(
        condition = "output.current_step == 'resident'",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Step 2: Select Resident"),
                p("Choose the resident you want to evaluate", style = "margin: 0; opacity: 0.9;")
            ),
            div(style = "padding: 2rem 0;",
                fluidRow(
                  column(8,
                         div(class = "mb-3",
                             h5("Search for Resident Name", class = "question-text"),
                             textInput("resident_search", 
                                       "", 
                                       placeholder = "Start typing resident name...")
                         ),
                         div(id = "resident_search_results",
                             uiOutput("resident_search_results")
                         )
                  ),
                  column(4,
                         div(class = "resident-card",
                             h5("Selected Resident", class = "question-text"),
                             uiOutput("selected_resident_info")
                         )
                  )
                )
            ),
            
            # Navigation
            div(class = "text-center mt-4",
                actionButton("back_to_faculty", "← Back to Faculty Selection", class = "btn btn-secondary")
            )
        )
      ),
      
      # Step 3: Evaluation Type Selection
      conditionalPanel(
        condition = "output.current_step == 'evaluation_type'",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Step 3: Select Evaluation Type"),
                p("Choose the type of evaluation to complete", style = "margin: 0; opacity: 0.9;")
            ),
            div(style = "padding: 2rem;",
                uiOutput("evaluation_type_buttons")
            ),
            
            # Navigation
            div(class = "text-center mt-4",
                actionButton("back_to_resident", "← Back to Resident Selection", class = "btn btn-secondary")
            )
        )
      ),
      
      # Step 4: Evaluation Form
      conditionalPanel(
        condition = "output.current_step == 'evaluation_form'",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Step 4: Complete Evaluation"),
                uiOutput("evaluation_form_header")
            ),
            div(style = "padding: 2rem;",
                uiOutput("evaluation_form_content")
            )
        )
      ),
      
      # Bootstrap 5 Modal for Adding Faculty (unchanged)
      tags$div(class = "modal fade", id = "addFacultyModal", tabindex = "-1", 
               `aria-labelledby` = "addFacultyModalLabel", `aria-hidden` = "true",
               tags$div(class = "modal-dialog modal-lg modal-dialog-scrollable",
                        tags$div(class = "modal-content faculty-modal",
                                 # Modal Header
                                 tags$div(class = "modal-header faculty-modal-header",
                                          tags$div(class = "modal-header-content",
                                                   tags$div(class = "modal-icon", "👤"),
                                                   tags$div(
                                                     tags$h4(class = "modal-title", id = "addFacultyModalLabel", "Faculty Registration"),
                                                     tags$p(class = "modal-subtitle", "Add yourself to the faculty database")
                                                   )
                                          ),
                                          tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
                                 ),
                                 
                                 # Modal Body
                                 tags$div(class = "modal-body",
                                          # Instructional Text
                                          tags$div(class = "faculty-info-alert",
                                                   tags$div(class = "info-icon", "ℹ️"),
                                                   tags$div(class = "info-content",
                                                            tags$h6("Welcome to Faculty Registration"),
                                                            tags$p("Since you're not listed in our database, please take a moment to provide your information. This is how your name will appear in the app, so please ensure accuracy.")
                                                   )
                                          ),
                                          
                                          # Validation Warning (hidden by default)
                                          tags$div(id = "faculty_validation_warning", class = "validation-warning", style = "display: none;",
                                                   tags$div(class = "warning-content",
                                                            tags$span(class = "warning-icon", "⚠️"),
                                                            tags$div(
                                                              tags$strong("Please complete all required fields:"),
                                                              tags$div(id = "missing_fields_list", class = "missing-fields")
                                                            )
                                                   )
                                          ),
                                          
                                          # Form Content
                                          tags$div(class = "faculty-form",
                                                   # Name and Email Row
                                                   tags$div(class = "form-row",
                                                            tags$div(class = "form-group",
                                                                     tags$h6("Full Name", class = "form-label required"),
                                                                     textInput("fac_name", "", placeholder = "First Name Last Name (e.g., John Smith)"),
                                                                     tags$div(class = "form-help", "This is how your name will appear to residents")
                                                            ),
                                                            tags$div(class = "form-group",
                                                                     tags$h6("Email Address", class = "form-label required"),
                                                                     textInput("fac_email", "", placeholder = "your.name@ssmhealth.com"),
                                                                     tags$div(class = "form-help", "Prefer @ssmhealth.com or @va.gov domains")
                                                            )
                                                   ),
                                                   
                                                   # Clinical Affiliate and Faculty/Fellow Row
                                                   tags$div(class = "form-row",
                                                            tags$div(class = "form-group",
                                                                     tags$h6("Main Clinical Affiliate", class = "form-label"),
                                                                     selectInput("fac_clin", "",
                                                                                 choices = list("Choose..." = "",
                                                                                                "SSM" = "1",
                                                                                                "VA" = "2", 
                                                                                                "Other" = "3"),
                                                                                 selected = "")
                                                            ),
                                                            tags$div(class = "form-group",
                                                                     tags$h6("Faculty or Fellow", class = "form-label required"),
                                                                     selectInput("fac_fell", "",
                                                                                 choices = list("Choose..." = "",
                                                                                                "Faculty" = "1",
                                                                                                "Fellow" = "2"),
                                                                                 selected = "")
                                                            )
                                                   ),
                                                   
                                                   # Division
                                                   tags$div(class = "form-group",
                                                            tags$h6("Division/Section", class = "form-label required"),
                                                            selectInput("fac_div", "",
                                                                        choices = list("Choose..." = "",
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
                                                                                       "Other" = "15"),
                                                                        selected = ""),
                                                            tags$div(class = "form-help", "Select the division you're most associated with")
                                                   ),
                                                   
                                                   # Other Division (conditional)
                                                   conditionalPanel(
                                                     condition = "input.fac_div == '15'",
                                                     tags$div(class = "form-group",
                                                              tags$h6("Specify Other Division", class = "form-label"),
                                                              textInput("other_div", "", placeholder = "Enter your division/section")
                                                     )
                                                   ),
                                                   
                                                   # Medical Education Leadership
                                                   tags$div(class = "form-group",
                                                            tags$h6("Medical Education Leadership", class = "form-label"),
                                                            tags$div(class = "form-help", "Select any that apply (optional):"),
                                                            checkboxGroupInput("fac_med_ed", "",
                                                                               choices = list("Core Faculty, IM Program" = "1",
                                                                                              "Core Faculty, Fellowship Program" = "2",
                                                                                              "APD, IM Program" = "3",
                                                                                              "APD, Fellowship Program" = "4",
                                                                                              "Fellowship PD" = "5",
                                                                                              "Learning Community" = "6",
                                                                                              "Clerkship (MS3 or MS4)" = "7",
                                                                                              "Core IM PD" = "8",
                                                                                              "MS Course" = "9",
                                                                                              "Other" = "10"),
                                                                               selected = NULL)
                                                   )
                                          )
                                 ),
                                 
                                 # Modal Footer
                                 tags$div(class = "modal-footer",
                                          tags$small("* Required fields", class = "text-muted me-auto"),
                                          tags$button(type = "button", class = "btn btn-secondary", `data-bs-dismiss` = "modal", "Cancel"),
                                          actionButton("add_new_faculty", "Add Faculty Profile", 
                                                       class = "btn btn-primary")
                                 )
                        )
               )
      )
  )
)