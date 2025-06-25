# ui.R - Faculty Evaluation App (Simple Start)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  
  # SSM Health Professional Styling (matching your resident app exactly)
  tags$head(
    tags$style("
      /* SSM Health Brand Colors & Typography */
      :root {
        --ssm-primary-blue: #003d5c;
        --ssm-secondary-blue: #0066a1;
        --ssm-light-blue: #4a90a4;
        --ssm-accent-blue: #2196f3;
        --ssm-success-green: #00a651;
        --ssm-warning-orange: #ff8c00;
        --ssm-neutral-gray: #6c757d;
        --ssm-light-gray: #f8f9fa;
        --ssm-white: #ffffff;
        --ssm-text-primary: #2c3e50;
        --ssm-text-secondary: #546e7a;
      }
      
      body {
        font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        color: var(--ssm-text-primary);
        line-height: 1.6;
      }
      
      /* Modern Header Styling */
      .navbar-brand {
        font-weight: 700;
        font-size: 1.5rem;
        color: var(--ssm-primary-blue) !important;
        letter-spacing: -0.5px;
      }
      
      /* Professional Card System */
      .ssm-card {
        background: var(--ssm-white);
        border: none;
        border-radius: 16px;
        box-shadow: 0 8px 32px rgba(0, 61, 92, 0.08);
        padding: 2rem;
        margin-bottom: 1.5rem;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        position: relative;
        overflow: hidden;
      }
      
      .ssm-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: linear-gradient(90deg, var(--ssm-primary-blue), var(--ssm-secondary-blue));
      }
      
      .ssm-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 16px 48px rgba(0, 61, 92, 0.12);
      }
      
      /* Step Headers */
      .step-header {
        background: linear-gradient(135deg, var(--ssm-primary-blue) 0%, var(--ssm-secondary-blue) 100%);
        color: var(--ssm-white);
        padding: 2rem;
        border-radius: 16px 16px 0 0;
        margin-bottom: 0;
        position: relative;
        overflow: hidden;
      }
      
      .step-header::after {
        content: '';
        position: absolute;
        top: -50%;
        right: -50%;
        width: 100%;
        height: 200%;
        background: linear-gradient(45deg, transparent 30%, rgba(255,255,255,0.1) 50%, transparent 70%);
        transform: rotate(45deg);
        animation: shimmer 3s infinite;
      }
      
      @keyframes shimmer {
        0% { transform: translateX(-100%) rotate(45deg); }
        100% { transform: translateX(100%) rotate(45deg); }
      }
      
      .step-header h3 {
        margin: 0;
        font-weight: 600;
        font-size: 1.75rem;
        letter-spacing: -0.5px;
      }
      
      /* Search Results */
      .search-result {
        background: var(--ssm-white);
        border: 1px solid rgba(0, 102, 161, 0.1);
        border-radius: 12px;
        padding: 1rem;
        margin: 0.5rem 0;
        transition: all 0.3s ease;
        cursor: pointer;
        position: relative;
      }
      
      .search-result:hover {
        border-color: var(--ssm-secondary-blue);
        box-shadow: 0 4px 16px rgba(0, 102, 161, 0.15);
        transform: translateX(4px);
      }
      
      .search-result::before {
        content: '';
        position: absolute;
        left: 0;
        top: 0;
        bottom: 0;
        width: 0;
        background: var(--ssm-secondary-blue);
        transition: width 0.3s ease;
        border-radius: 12px 0 0 12px;
      }
      
      .search-result:hover::before {
        width: 4px;
      }
      
      /* Faculty Card */
      .faculty-card {
        background: linear-gradient(135deg, var(--ssm-white) 0%, #f8f9fa 100%);
        border: 1px solid rgba(0, 102, 161, 0.15);
        border-radius: 16px;
        padding: 1.5rem;
        position: relative;
        overflow: hidden;
      }
      
      .faculty-card::before {
        content: '👩‍⚕️';
        position: absolute;
        top: 1rem;
        right: 1rem;
        font-size: 1.25rem;
        opacity: 0.4;
      }
      
      /* Modern Form Elements */
      .form-control, .form-select {
        border: 2px solid rgba(0, 102, 161, 0.1);
        border-radius: 12px;
        padding: 0.875rem 1.25rem;
        font-size: 1rem;
        transition: all 0.3s ease;
        background: var(--ssm-white);
      }
      
      .form-control:focus, .form-select:focus {
        border-color: var(--ssm-secondary-blue);
        box-shadow: 0 0 0 4px rgba(0, 102, 161, 0.1);
        outline: none;
      }
      
      /* Question Text */
      .question-text {
        font-size: 1.125rem;
        font-weight: 600;
        color: var(--ssm-text-primary);
        margin-bottom: 1rem;
        line-height: 1.5;
      }
      
      /* Modern Buttons */
      .btn-modern {
        padding: 0.875rem 2rem;
        border-radius: 50px;
        font-weight: 600;
        text-transform: none;
        letter-spacing: 0.5px;
        transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        border: none;
        position: relative;
        overflow: hidden;
      }
      
      .btn-modern::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.2), transparent);
        transition: left 0.6s;
      }
      
      .btn-modern:hover::before {
        left: 100%;
      }
      
      .btn-primary.btn-modern {
        background: linear-gradient(135deg, var(--ssm-secondary-blue), var(--ssm-accent-blue));
        color: var(--ssm-white);
      }
      
      .btn-primary.btn-modern:hover {
        background: linear-gradient(135deg, var(--ssm-primary-blue), var(--ssm-secondary-blue));
        transform: translateY(-2px);
        box-shadow: 0 8px 24px rgba(0, 102, 161, 0.3);
      }
      
      .btn-warning.btn-modern {
        background: linear-gradient(135deg, var(--ssm-warning-orange), #ffa000);
        color: var(--ssm-white);
      }
      
      .btn-secondary.btn-modern {
        background: linear-gradient(135deg, var(--ssm-neutral-gray), #868e96);
        color: var(--ssm-white);
      }
      
      /* Text Areas */
      textarea.form-control {
        border-radius: 16px !important;
        border: 2px solid rgba(0, 102, 161, 0.1) !important;
        padding: 1.25rem !important;
        font-size: 1rem !important;
        resize: vertical;
        min-height: 120px;
      }
      
      textarea.form-control:focus {
        border-color: var(--ssm-secondary-blue) !important;
        box-shadow: 0 0 0 4px rgba(0, 102, 161, 0.1) !important;
      }
      
      /* Success State */
      .faculty-selected {
        margin-top: 1rem;
        font-weight: 600;
        color: var(--ssm-success-green);
        padding: 0.75rem 1.25rem;
        background: rgba(0, 166, 81, 0.1);
        border-radius: 12px;
        border-left: 4px solid var(--ssm-success-green);
      }
      
      /* Responsive Design */
      @media (max-width: 768px) {
        .step-header {
          text-align: center;
          padding: 1.5rem;
        }
        
        .search-result {
          margin: 0.25rem 0;
        }
      }
      
      /* Accessibility Enhancements */
      .btn:focus, .form-control:focus {
        outline: 3px solid var(--ssm-accent-blue);
        outline-offset: 2px;
      }
    ")
  ),
  
  # SSM Health Header
  div(class = "container-fluid mb-4",
      div(class = "row",
          div(class = "col-12",
              div(class = "d-flex justify-content-between align-items-center py-3",
                  div(class = "navbar-brand", "IMSLU Faculty Evaluation System"),
                  div(class = "text-muted", "Internal Medicine Residency Program")
              )
          )
      )
  ),
  
  div(class = "container",
      
      # Faculty Search/Login Step
      div(class = "ssm-card",
          div(class = "step-header",
              h3("Faculty Login"),
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
          
          # Add Faculty Option
          conditionalPanel(
            condition = "input.faculty_not_found == true",
            div(class = "ssm-card mt-4",
                div(style = "border-left: 4px solid var(--ssm-secondary-blue); padding-left: 1rem;",
                    h5("Add Yourself to Faculty Database", class = "question-text"),
                    p("Please provide your information to be added to the faculty database:")
                ),
                
                # Row 1: Name and Email
                fluidRow(
                  column(6,
                         div(class = "mb-3",
                             h6("Full Name", class = "question-text", style = "font-size: 1rem;"),
                             textInput("fac_name", 
                                       "", 
                                       placeholder = "First Name first (e.g., Joe Smith)"),
                             p("Please write your name as you would like it to appear", 
                               style = "font-size: 0.875rem; color: var(--ssm-text-secondary); margin-top: 0.25rem;")
                         )
                  ),
                  column(6,
                         div(class = "mb-3",
                             h6("Email Address", class = "question-text", style = "font-size: 1rem;"),
                             textInput("fac_email", 
                                       "", 
                                       placeholder = "your.name@ssmhealth.com"),
                             p("Use @ssmhealth.com or @va.gov domain if available", 
                               style = "font-size: 0.875rem; color: var(--ssm-text-secondary); margin-top: 0.25rem;")
                         )
                  )
                ),
                
                # Row 2: Clinical Affiliate and Faculty/Fellow
                fluidRow(
                  column(6,
                         div(class = "mb-3",
                             h6("Main Clinical Affiliate", class = "question-text", style = "font-size: 1rem;"),
                             selectInput("fac_clin", 
                                         "",
                                         choices = list("Choose..." = "",
                                                        "SSM" = "1",
                                                        "VA" = "2", 
                                                        "Other" = "3"),
                                         selected = "")
                         )
                  ),
                  column(6,
                         div(class = "mb-3",
                             h6("Faculty or Fellow", class = "question-text", style = "font-size: 1rem;"),
                             selectInput("fac_fell", 
                                         "",
                                         choices = list("Choose..." = "",
                                                        "Faculty" = "1",
                                                        "Fellow" = "2"),
                                         selected = "")
                         )
                  )
                ),
                
                # Row 3: Division
                div(class = "mb-3",
                    h6("Division/Section", class = "question-text", style = "font-size: 1rem;"),
                    selectInput("fac_div", 
                                "Which Division (Section) are you most associated with?",
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
                                selected = "")
                ),
                
                # Other Division (conditional)
                conditionalPanel(
                  condition = "input.fac_div == '15'",
                  div(class = "mb-3",
                      h6("Other Division", class = "question-text", style = "font-size: 1rem;"),
                      textInput("other_div", 
                                "", 
                                placeholder = "Please specify your division/section")
                  )
                ),
                
                # Med Ed Leadership
                div(class = "mb-4",
                    h6("Medical Education Leadership", class = "question-text", style = "font-size: 1rem;"),
                    p("Please select if you are any of the following:", 
                      style = "font-size: 0.875rem; color: var(--ssm-text-secondary); margin-bottom: 1rem;"),
                    checkboxGroupInput("fac_med_ed", 
                                       "",
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
                ),
                
                br(),
                actionButton("add_new_faculty", "Add Faculty Profile", 
                             class = "btn btn-warning btn-modern"),
                p("Your profile will be added to the faculty database for immediate use.", 
                  style = "margin-top: 1rem; font-size: 0.875rem; color: var(--ssm-text-secondary);")
            )
          ),
          
          div(class = "form-check mt-4",
              checkboxInput("faculty_not_found", 
                            "Can't find your name? Add yourself here.", 
                            value = FALSE)
          )
      ),
      
      # Placeholder for next steps
      conditionalPanel(
        condition = "output.show_next_steps",
        div(class = "ssm-card",
            div(class = "step-header",
                h3("Next: Resident Selection"),
                p("This will be built next", style = "margin: 0; opacity: 0.9;")
            ),
            div(style = "padding: 2rem; text-align: center;",
                h4("Faculty Selected Successfully!", style = "color: var(--ssm-success-green);"),
                p("Next we'll add resident selection and evaluation forms."),
                actionButton("start_over", "Start Over", class = "btn btn-secondary btn-modern")
            )
        )
      )
  )
)