# Integrated Drug Development Platform
# ====================================
# From Molecular Properties to Bioequivalence Analysis

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)
library(rxode2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pracma)
library(fresh)
library(reticulate)

# Initialize Python environment and import functions
# Make sure to set the correct path to your Python environment if needed
use_python("/home/haythem/haythem/Bioequivalence-Analysis/myenv/bin/python")
py_run_file("/home/haythem/haythem/Bioequivalence-Analysis/Python_Script/molecular_pk_analysis.py")

# Create futuristic dark purple theme
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#9c88ff",
    red = "#ff6b9d", 
    green = "#4ecdc4",
    aqua = "#45b7d1",
    yellow = "#f9ca24",
    blue = "#6c5ce7",
    navy = "#2d3436",
    teal = "#00cec9",
    purple = "#a29bfe",
    orange = "#fd79a8",
    maroon = "#e84393",
    fuchsia = "#fd79a8",
    lime = "#00b894",
    olive = "#6c5ce7"
  ),
  adminlte_sidebar(
    dark_bg = "#1a0933",
    dark_hover_bg = "#2d1b69",
    dark_color = "#e17055",
    dark_submenu_bg = "#0f051a",
    dark_submenu_color = "#ddd6fe",
    dark_submenu_hover_color = "#f3f4f6"
  ),
  adminlte_global(
    content_bg = "#0d1117",
    box_bg = "#161b22",
    info_box_bg = "#21262d"
  )
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      icon("atom"),
      "Integrated Drug Development Platform",
      style = "font-weight: bold; color: #ddd6fe; text-shadow: 0 0 10px #9c88ff;"
    ),
    titleWidth = 400
  ),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      id = "tabs",
      menuItem("🧬 Molecular Input", tabName = "molecular", icon = icon("molecule")),
      menuItem("⚙️ Formulation", tabName = "formulation", icon = icon("pills")),
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("🧪 Results", tabName = "results", icon = icon("flask")),
      menuItem("📈 Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("📋 Data Tables", tabName = "data", icon = icon("table"))
    ),
    
    # Molecular input controls (streamlined sidebar)
    div(
      style = "padding: 20px;",
      h4("Molecule Input", style = "color: #ddd6fe; margin-bottom: 20px; text-shadow: 0 0 5px #9c88ff;"),
      
      textInput("smiles_input", 
                "SMILES String:", 
                value = "CC(=O)OC1=CC=CC=C1C(=O)O",  # Aspirin default
                placeholder = "Enter SMILES notation"),
      
      textInput("compound_name",
                "Compound Name:",
                value = "Aspirin",
                placeholder = "Optional compound name"),
      
      actionButton("analyze_molecule", "🔬 Analyze Molecule", 
                   class = "btn-info btn-block", 
                   style = "margin: 10px 0; font-weight: bold; background: linear-gradient(45deg, #6c5ce7, #a29bfe); border: none; box-shadow: 0 4px 15px rgba(108, 92, 231, 0.4);"),
      
      hr(style = "border-color: #2d1b69; box-shadow: 0 1px 3px rgba(156, 136, 255, 0.3);"),
      
      h5("Quick Examples:", style = "color: #ddd6fe; text-shadow: 0 0 3px #9c88ff;"),
      actionButton("load_aspirin", "Aspirin", 
                   class = "btn btn-outline-light btn-sm btn-block", 
                   style = "margin: 2px 0; background: rgba(156, 136, 255, 0.1); border: 1px solid #9c88ff; color: #ddd6fe;"),
      actionButton("load_ibuprofen", "Ibuprofen", 
                   class = "btn btn-outline-light btn-sm btn-block", 
                   style = "margin: 2px 0; background: rgba(156, 136, 255, 0.1); border: 1px solid #9c88ff; color: #ddd6fe;"),
      actionButton("load_caffeine", "Caffeine", 
                   class = "btn btn-outline-light btn-sm btn-block", 
                   style = "margin: 2px 0; background: rgba(156, 136, 255, 0.1); border: 1px solid #9c88ff; color: #ddd6fe;"),
      
      hr(style = "border-color: #2d1b69; box-shadow: 0 1px 3px rgba(156, 136, 255, 0.3);"),
      
      numericInput("n_subjects", "Number of Subjects:", 
                   value = 1000, min = 100, max = 5000, step = 100),
      
      actionButton("run_simulation", "🚀 Run BE Simulation", 
                   class = "btn-primary btn-block", 
                   style = "margin-top: 15px; font-weight: bold; background: linear-gradient(45deg, #fd79a8, #fdcb6e); border: none; box-shadow: 0 4px 15px rgba(253, 121, 168, 0.4);")
    )
  ),
  
  dashboardBody(
    use_theme(my_theme),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { 
          background: linear-gradient(135deg, #0d1117 0%, #1a0933 50%, #0d1117 100%);
          color: #e6fffa;
        }
        .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #9c88ff; }
        .box { 
          background: linear-gradient(135deg, #161b22 0%, #21262d 100%);
          border: 1px solid #30363d;
          border-radius: 12px;
          box-shadow: 0 8px 32px rgba(108, 92, 231, 0.2);
          color: #e6fffa;
        }
        .box.box-primary { 
          border-top: 3px solid #9c88ff;
          box-shadow: 0 0 20px rgba(156, 136, 255, 0.3);
        }
        .box.box-info {
          border-top: 3px solid #45b7d1;
          box-shadow: 0 0 20px rgba(69, 183, 209, 0.3);
        }
        .box.box-success {
          border-top: 3px solid #4ecdc4;
          box-shadow: 0 0 20px rgba(78, 205, 196, 0.3);
        }
        .box.box-warning {
          border-top: 3px solid #f9ca24;
          box-shadow: 0 0 20px rgba(249, 202, 36, 0.3);
        }
        .box-header .box-title {
          color: #ddd6fe;
          font-weight: bold;
          text-shadow: 0 0 5px rgba(156, 136, 255, 0.5);
        }
        .small-box { 
          background: linear-gradient(135deg, #21262d 0%, #30363d 100%);
          border-radius: 12px;
          border: 1px solid #30363d;
          box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
        }
        .small-box h3 { 
          font-size: 2.2rem; 
          font-weight: 700;
          color: #ddd6fe;
          text-shadow: 0 0 10px rgba(156, 136, 255, 0.7);
        }
        .small-box p {
          color: #8b949e;
        }
        .progress-bar { 
          background: linear-gradient(45deg, #9c88ff, #a29bfe);
          box-shadow: 0 0 10px rgba(156, 136, 255, 0.5);
        }
        .plotly { 
          border-radius: 12px; 
          box-shadow: 0 4px 20px rgba(0,0,0,0.3);
          background: #161b22;
        }
        .alert-molecule { 
          background: linear-gradient(135deg, #1a0933, #2d1b69);
          border: 1px solid #9c88ff;
          color: #ddd6fe;
          border-radius: 8px;
          box-shadow: 0 0 15px rgba(156, 136, 255, 0.2);
        }
        .form-control {
          background: #21262d;
          border: 1px solid #30363d;
          color: #e6fffa;
          border-radius: 6px;
        }
        .form-control:focus {
          background: #21262d;
          border: 1px solid #9c88ff;
          color: #e6fffa;
          box-shadow: 0 0 10px rgba(156, 136, 255, 0.3);
        }
        .dataTables_wrapper {
          color: #e6fffa;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: #e6fffa !important;
          background: #21262d;
          border: 1px solid #30363d;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background: #9c88ff !important;
          border: 1px solid #9c88ff;
        }
        table.dataTable tbody tr {
          background: #161b22;
          color: #e6fffa;
        }
        table.dataTable tbody tr:hover {
          background: rgba(156, 136, 255, 0.1) !important;
        }
        .skin-blue .main-header .navbar {
          background: linear-gradient(135deg, #1a0933 0%, #2d1b69 100%);
          border-bottom: 2px solid #9c88ff;
          box-shadow: 0 2px 10px rgba(156, 136, 255, 0.3);
        }
        .skin-blue .main-sidebar {
          background: linear-gradient(180deg, #1a0933 0%, #0f051a 100%);
          box-shadow: 2px 0 10px rgba(0, 0, 0, 0.3);
        }
        .form-group label {
          color: #ddd6fe;
          font-weight: 500;
        }
        .well {
          background: linear-gradient(135deg, #161b22 0%, #21262d 100%);
          border: 1px solid #30363d;
          border-radius: 8px;
          box-shadow: 0 4px 15px rgba(0, 0, 0, 0.2);
        }
        h1, h2, h3, h4, h5, h6 {
          color: #ddd6fe;
          text-shadow: 0 0 5px rgba(156, 136, 255, 0.3);
        }
        .alert-info {
          background: linear-gradient(135deg, rgba(156, 136, 255, 0.1), rgba(162, 155, 254, 0.05));
          border: 1px solid rgba(156, 136, 255, 0.3);
          color: #ddd6fe;
          border-radius: 8px;
        }
        .alert-success {
          background: linear-gradient(135deg, rgba(78, 205, 196, 0.1), rgba(78, 205, 196, 0.05));
          border: 1px solid rgba(78, 205, 196, 0.3);
          color: #ddd6fe;
          border-radius: 8px;
        }
        .alert-warning {
          background: linear-gradient(135deg, rgba(249, 202, 36, 0.1), rgba(249, 202, 36, 0.05));
          border: 1px solid rgba(249, 202, 36, 0.3);
          color: #ddd6fe;
          border-radius: 8px;
        }
        .selectize-control .selectize-input {
          background: #21262d;
          border: 1px solid #30363d;
          color: #e6fffa;
        }
        .selectize-control .selectize-dropdown {
          background: #21262d;
          border: 1px solid #30363d;
          color: #e6fffa;
        }
        .slider-handle {
          background: linear-gradient(45deg, #9c88ff, #a29bfe);
          border: 2px solid #9c88ff;
        }
        .slider-track {
          background: #30363d;
        }
        .slider-selection {
          background: linear-gradient(45deg, #9c88ff, #a29bfe);
        }
      "))
    ),
    
    tabItems(
      # Molecular Input Tab
      tabItem(
        tabName = "molecular",
        fluidRow(
          box(
            title = "🧬 Molecular Structure Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                     h4("Input Structure", style = "color: #ddd6fe;"),
                     textAreaInput("smiles_display", "SMILES String:", 
                                   value = "CC(=O)OC1=CC=CC=C1C(=O)O", 
                                   height = "80px"),
                     actionButton("validate_smiles", "Validate SMILES", 
                                  class = "btn-secondary",
                                  style = "background: linear-gradient(45deg, #45b7d1, #4ecdc4); border: none; color: white; box-shadow: 0 4px 15px rgba(69, 183, 209, 0.4);"),
                     br(), br(),
                     uiOutput("smiles_validation")
              ),
              column(6,
                     h4("Molecular Descriptors", style = "color: #ddd6fe;"),
                     DT::dataTableOutput("descriptors_table")
              )
            ),
            
            hr(style = "border-color: #30363d; box-shadow: 0 1px 3px rgba(156, 136, 255, 0.2);"),
            
            fluidRow(
              column(6,
                     h4("Drug-Likeness Assessment", style = "color: #ddd6fe;"),
                     uiOutput("drug_likeness_summary")
              ),
              column(6,
                     h4("Reference PK Parameters", style = "color: #ddd6fe;"),
                     DT::dataTableOutput("base_pk_table")
              )
            )
          )
        )
      ),
      
      # Formulation Tab (REDESIGNED - parameters moved to main area)
      tabItem(
        tabName = "formulation",
        # Top row: Formulation parameters (moved from sidebar)
        fluidRow(
          box(
            title = "⚙️ Test Formulation Parameters",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            div(class = "alert alert-info",
                style = "margin-bottom: 20px;",
                p("Configure your test formulation parameters below. These will be compared against the reference formulation (default molecular parameters).")
            ),
            
            fluidRow(
              column(3,
                     selectInput("test_release_mechanism", "Release Mechanism:",
                                 choices = c("Immediate Release (IR)", "Extended Release (ER)", "Enteric Coated"),
                                 selected = "Immediate Release (IR)")
              ),
              column(3,
                     selectInput("test_solubility_class", "Solubility Class:",
                                 choices = c("High", "Medium", "Low"),
                                 selected = "High")
              ),
              column(3,
                     sliderInput("test_particle_size", "Particle Size (μm):",
                                 min = 1, max = 200, value = 50, step = 1)
              ),
              column(3,
                     selectInput("test_fed_state", "Fed State:",
                                 choices = c("Fasted", "Fed"),
                                 selected = "Fasted")
              )
            ),
            
            fluidRow(
              column(6,
                     numericInput("test_tlag_override", "Lag Time Override (h):",
                                  value = 0.0, min = 0, max = 10, step = 0.1)
              ),
              column(6,
                     br(),
                     div(style = "text-align: center; margin-top: 10px;",
                         span("Real-time parameter calculation active", 
                              style = "color: #4ecdc4; font-style: italic; text-shadow: 0 0 3px rgba(78, 205, 196, 0.5);")
                     )
              )
            )
          )
        ),
        
        # Second row: Comparison details
        fluidRow(
          box(
            title = "📋 Reference vs Test Formulation Comparison",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            fluidRow(
              column(6,
                     div(
                       style = "background: linear-gradient(135deg, rgba(69, 183, 209, 0.1), rgba(69, 183, 209, 0.05)); 
                                padding: 20px; border-radius: 8px; border: 1px solid rgba(69, 183, 209, 0.3);",
                       h4("Reference Formulation", style = "color: #45b7d1; text-shadow: 0 0 5px rgba(69, 183, 209, 0.5);"),
                       uiOutput("reference_formulation_details"),
                       hr(style = "border-color: rgba(69, 183, 209, 0.3);"),
                       h5("Reference PK Parameters:", style = "color: #45b7d1;"),
                       DT::dataTableOutput("reference_pk_display")
                     )
              ),
              column(6,
                     div(
                       style = "background: linear-gradient(135deg, rgba(255, 107, 157, 0.1), rgba(255, 107, 157, 0.05)); 
                                padding: 20px; border-radius: 8px; border: 1px solid rgba(255, 107, 157, 0.3);",
                       h4("Test Formulation", style = "color: #ff6b9d; text-shadow: 0 0 5px rgba(255, 107, 157, 0.5);"),
                       uiOutput("test_formulation_details"),
                       hr(style = "border-color: rgba(255, 107, 157, 0.3);"),
                       h5("Test PK Parameters:", style = "color: #ff6b9d;"),
                       DT::dataTableOutput("test_pk_display")
                     )
              )
            )
          )
        ),
        
        # Third row: Parameter comparison
        fluidRow(
          box(
            title = "📊 Parameter Comparison Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            DT::dataTableOutput("parameter_comparison_table")
          )
        )
      ),
      
      # Dashboard Tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("decision_box", width = 4),
          valueBoxOutput("auc_box", width = 4),
          valueBoxOutput("cmax_box", width = 4)
        ),
        fluidRow(
          box(
            title = "🎯 Concentration-Time Profiles",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            plotlyOutput("concentration_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            title = "📊 Bioequivalence Assessment",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("be_intervals_plot", height = "400px")
          ),
          box(
            title = "📈 Distribution Comparison",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("distribution_plot", height = "400px")
          )
        )
      ),
      
      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "🧬 Molecular Analysis Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            uiOutput("molecular_summary_box")
          )
        ),
        fluidRow(
          box(
            title = "🧪 Statistical Test Results",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                     h4("AUC Analysis", style = "color: #45b7d1;"),
                     verbatimTextOutput("auc_test_results")
              ),
              column(6,
                     h4("Cmax Analysis", style = "color: #ff6b9d;"),
                     verbatimTextOutput("cmax_test_results")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "📊 PK Metrics Summary",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("pk_summary_table")
          )
        )
      ),
      
      # Visualizations Tab
      tabItem(
        tabName = "visualizations",
        fluidRow(
          box(
            title = "📈 AUC Ratio Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("auc_density_plot", height = "400px")
          ),
          box(
            title = "📈 Cmax Ratio Distribution",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("cmax_density_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = "📊 Individual Subject Profiles",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3,
                     numericInput("n_profiles", "Profiles to Show:",
                                  value = 20, min = 5, max = 100, step = 5)
              ),
              column(9,
                     plotlyOutput("individual_profiles_plot", height = "500px")
              )
            )
          )
        )
      ),
      
      # Data Tables Tab
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "📋 PK Metrics Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("pk_metrics_table")
          )
        ),
        fluidRow(
          box(
            title = "📋 Population Parameters",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            h5("Reference Formulation (Base Molecular)", style = "color: #45b7d1;"),
            DT::dataTableOutput("ref_params_table")
          ),
          box(
            title = "📋 Population Parameters",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            h5("Test Formulation (Modified)", style = "color: #f9ca24;"),
            DT::dataTableOutput("test_params_table")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store all results
  values <- reactiveValues(
    # Molecular analysis results
    molecular_analysis = NULL,
    current_smiles = NULL,
    reference_pk_params = NULL,  # Base molecular parameters (reference)
    test_pk_params = NULL,       # Modified formulation parameters (test)
    
    # Simulation results
    ref_sim = NULL,
    test_sim = NULL,
    combined_sim = NULL,
    pk_metrics = NULL,
    pk_wide = NULL,
    auc_result = NULL,
    cmax_result = NULL,
    pop_params_ref = NULL,
    pop_params_test = NULL
  )
  
  # Example molecule buttons
  observeEvent(input$load_aspirin, {
    updateTextInput(session, "smiles_input", value = "CC(=O)OC1=CC=CC=C1C(=O)O")
    updateTextInput(session, "compound_name", value = "Aspirin")
    updateTextInput(session, "smiles_display", value = "CC(=O)OC1=CC=CC=C1C(=O)O")
  })
  
  observeEvent(input$load_ibuprofen, {
    updateTextInput(session, "smiles_input", value = "CC(C)CC1=CC=C(C=C1)C(C)C(=O)O")
    updateTextInput(session, "compound_name", value = "Ibuprofen")
    updateTextInput(session, "smiles_display", value = "CC(C)CC1=CC=C(C=C1)C(C)C(=O)O")
  })
  
  observeEvent(input$load_caffeine, {
    updateTextInput(session, "smiles_input", value = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")
    updateTextInput(session, "compound_name", value = "Caffeine")
    updateTextInput(session, "smiles_display", value = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")
  })
  
  # Sync SMILES input fields
  observeEvent(input$smiles_input, {
    updateTextInput(session, "smiles_display", value = input$smiles_input)
  })
  
  observeEvent(input$smiles_display, {
    updateTextInput(session, "smiles_input", value = input$smiles_display)
  })
  
  # SMILES validation
  observeEvent(input$validate_smiles, {
    smiles <- input$smiles_display
    
    if (nchar(smiles) == 0) {
      showNotification("Please enter a SMILES string", type = "warning")
      return()
    }
    
    tryCatch({
      # Call Python validation function
      is_valid <- py$validate_smiles(smiles)
      
      if (is_valid) {
        showNotification("✅ Valid SMILES string!", type = "default")
      } else {
        showNotification("❌ Invalid SMILES string!", type = "error")
      }
    }, error = function(e) {
      showNotification("Error validating SMILES", type = "error")
    })
  })
  
  # Molecular analysis - calculates REFERENCE parameters
  observeEvent(input$analyze_molecule, {
    smiles <- input$smiles_input
    
    if (nchar(smiles) == 0) {
      showNotification("Please enter a SMILES string", type = "warning")
      return()
    }
    
    withProgress(message = 'Analyzing molecule...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Calculating molecular descriptors...")
        
        # Calculate BASE/REFERENCE parameters with default formulation
        result_ref <- py$get_pk_parameters_for_r(
          smiles = smiles,
          release_mechanism = "Immediate Release (IR)",  # Default reference formulation
          solubility_class = "High",                     # Default reference formulation
          particle_size_um = 50,                         # Default reference formulation
          fed_state = "Fasted",                          # Default reference formulation
          tlag_override_h = 0.0,                         # Default reference formulation
          weight_kg = 70                                 # Standard weight
        )
        
        incProgress(0.7, detail = "Processing results...")
        
        if (result_ref$success) {
          values$molecular_analysis <- result_ref
          values$current_smiles <- smiles
          values$reference_pk_params <- result_ref$pk_parameters  # These become the REFERENCE
          
          # Also calculate initial test parameters (same as reference initially)
          values$test_pk_params <- result_ref$pk_parameters
          
          incProgress(1, detail = "Complete!")
          showNotification("✅ Molecular analysis completed! Reference parameters set.", type = "default")
        } else {
          showNotification(paste("❌ Error:", result_ref$error), type = "error")
        }
        
      }, error = function(e) {
        showNotification(paste("Error in molecular analysis:", e$message), type = "error")
      })
    })
  })
  
  # Reactive function to calculate test parameters when formulation changes
  observe({
    # Only recalculate if we have a molecule analyzed and any formulation input changes
    if (!is.null(values$current_smiles)) {
      # Watch for changes in test formulation parameters
      input$test_release_mechanism
      input$test_solubility_class
      input$test_particle_size
      input$test_fed_state
      input$test_tlag_override
      
      # Recalculate test parameters with new formulation
      tryCatch({
        result_test <- py$get_pk_parameters_for_r(
          smiles = values$current_smiles,
          release_mechanism = input$test_release_mechanism,
          solubility_class = input$test_solubility_class,
          particle_size_um = input$test_particle_size,
          fed_state = input$test_fed_state,
          tlag_override_h = input$test_tlag_override,
          weight_kg = 70
        )
        
        if (result_test$success) {
          values$test_pk_params <- result_test$pk_parameters
        }
      }, error = function(e) {
        # Silently handle errors in reactive context
      })
    }
  })
  
  # Main simulation function (modified to use reference vs test parameters)
  observeEvent(input$run_simulation, {
    
    if (is.null(values$reference_pk_params) || is.null(values$test_pk_params)) {
      showNotification("Please analyze a molecule first!", type = "warning")
      return()
    }
    
    withProgress(message = 'Running bioequivalence simulation...', value = 0, {
      
      # Model setup
      incProgress(0.1, detail = "Setting up PK model...")
      model <- rxode2({
        ka <- ka
        cl <- cl
        v <- vd
        f <- f
        tlag <- tlag
        d/dt(depot) = -ka * depot
        d/dt(central) = ka * depot - (cl/v) * central
        cp = central / v
      })
      
      # Generate population parameters
      incProgress(0.2, detail = "Generating population parameters...")
      set.seed(42)
      n_subjects <- input$n_subjects
      
      # Reference parameters (base molecular calculation with default formulation)
      ref_pk <- values$reference_pk_params
      values$pop_params_ref <- tibble(
        ka = rlnorm(n_subjects, log(ref_pk$Ka), 0.1),
        cl = rlnorm(n_subjects, log(ref_pk$CL), 0.15),
        vd = rlnorm(n_subjects, log(ref_pk$Vd), 0.1),
        f = rlnorm(n_subjects, log(ref_pk$F), 0.05),
        tlag = rlnorm(n_subjects, log(max(ref_pk$Tlag, 0.01)), 0.3)
      )
      
      # Test parameters (using modified formulation parameters)
      test_pk <- values$test_pk_params
      values$pop_params_test <- tibble(
        ka = rlnorm(n_subjects, log(test_pk$Ka), 0.1),
        cl = rlnorm(n_subjects, log(test_pk$CL), 0.15),
        vd = rlnorm(n_subjects, log(test_pk$Vd), 0.1),
        f = rlnorm(n_subjects, log(test_pk$F), 0.05),
        tlag = rlnorm(n_subjects, log(max(test_pk$Tlag, 0.01)), 0.3)
      )
      
      # Define dosing event
      incProgress(0.3, detail = "Setting up dosing events...")
      event <- eventTable()
      event$add.dosing(dose = 100, nbr.doses = 1, dosing.to = 1, start.time = 0)
      event$add.sampling(seq(0, 24, by = 0.1))
      
      # Run simulations
      incProgress(0.5, detail = "Running reference simulation...")
      values$ref_sim <- rxSolve(model, params = values$pop_params_ref, events = event)
      values$ref_sim <- values$ref_sim %>% mutate(Formulation = "Reference")
      
      incProgress(0.7, detail = "Running test simulation...")
      values$test_sim <- rxSolve(model, params = values$pop_params_test, events = event)
      values$test_sim <- values$test_sim %>% mutate(Formulation = "Test")
      
      # Combine results
      incProgress(0.8, detail = "Processing results...")
      values$combined_sim <- bind_rows(values$ref_sim, values$test_sim)
      
      # Calculate PK metrics
      incProgress(0.9, detail = "Calculating PK metrics...")
      values$pk_metrics <- values$combined_sim %>%
        group_by(Formulation, sim.id) %>%
        summarize(
          Cmax = max(cp),
          Tmax = time[which.max(cp)],
          AUC = trapz(time, cp),
          .groups = "drop"
        )
      
      # Pivot for bioequivalence analysis
      values$pk_wide <- values$pk_metrics %>%
        pivot_wider(
          id_cols = sim.id,
          names_from = Formulation,
          values_from = c(Cmax, Tmax, AUC)
        ) %>%
        mutate(
          log_Cmax_T = log(Cmax_Test),
          log_Cmax_R = log(Cmax_Reference),
          log_AUC_T = log(AUC_Test),
          log_AUC_R = log(AUC_Reference),
          AUC_ratio = AUC_Test / AUC_Reference,
          Cmax_ratio = Cmax_Test / Cmax_Reference,
          log_AUC_ratio = log(AUC_ratio),
          log_Cmax_ratio = log(Cmax_ratio)
        )
      
      # Perform t-tests
      values$auc_result <- t.test(
        values$pk_wide$log_AUC_T,
        values$pk_wide$log_AUC_R,
        paired = TRUE,
        conf.level = 0.90
      )
      
      values$cmax_result <- t.test(
        values$pk_wide$log_Cmax_T,
        values$pk_wide$log_Cmax_R,
        paired = TRUE,
        conf.level = 0.90
      )
      
      incProgress(1, detail = "Complete!")
    })
    
    showNotification("Bioequivalence simulation completed!", type = "default")
  })
  
  # ==============================================================================
  # MOLECULAR ANALYSIS OUTPUTS
  # ==============================================================================
  
  # SMILES validation output
  output$smiles_validation <- renderUI({
    if (is.null(values$current_smiles)) {
      div(class = "alert alert-info", 
          "Enter a SMILES string and click 'Analyze Molecule'")
    } else {
      div(class = "alert alert-success", 
          paste("Successfully analyzed:", values$current_smiles))
    }
  })
  
  # Descriptors table
  output$descriptors_table <- DT::renderDataTable({
    if (is.null(values$molecular_analysis)) return(NULL)
    
    desc <- values$molecular_analysis$descriptors
    desc_df <- data.frame(
      Property = names(desc),
      Value = unlist(desc),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      desc_df,
      options = list(
        pageLength = 10, 
        dom = 't',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = 1:2, 
                      backgroundColor = '#161b22',
                      color = '#e6fffa')
  })
  
  # Drug-likeness summary
  output$drug_likeness_summary <- renderUI({
    if (is.null(values$molecular_analysis)) {
      return(div("Analyze molecule to see drug-likeness assessment"))
    }
    
    # Calculate drug-likeness using Python function
    tryCatch({
      drug_likeness <- py$assess_drug_likeness(values$molecular_analysis$descriptors)
      
      if (drug_likeness$overall_drug_like) {
        div(class = "alert alert-success",
            h5("Drug-Like Compound"),
            p(paste("Lipinski violations:", drug_likeness$lipinski_violations)),
            p("Suitable for oral administration")
        )
      } else {
        div(class = "alert alert-warning",
            h5("Drug-Likeness Issues"),
            p(paste("Lipinski violations:", drug_likeness$lipinski_violations)),
            if (length(drug_likeness$violations_list) > 0) {
              p(paste("Issues:", paste(drug_likeness$violations_list, collapse = ", ")))
            }
        )
      }
    }, error = function(e) {
      div("Error assessing drug-likeness")
    })
  })
  
  # Reference PK parameters table (base molecular calculation)
  output$base_pk_table <- DT::renderDataTable({
    if (is.null(values$reference_pk_params)) return(NULL)
    
    ref_pk <- values$reference_pk_params
    pk_df <- data.frame(
      Parameter = names(ref_pk),
      Value = unlist(ref_pk),
      Unit = c("h⁻¹", "L/h", "L", "", "h"),
      Description = c("Absorption Rate", "Clearance", "Volume Distribution", "Bioavailability", "Lag Time"),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      pk_df,
      options = list(
        pageLength = 10, 
        dom = 't',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE,
      caption = "Base molecular parameters (will be used as Reference)"
    ) %>%
      DT::formatStyle('Value', backgroundColor = 'rgba(69, 183, 209, 0.2)', color = '#e6fffa')
  })
  
  # ==============================================================================
  # FORMULATION TAB OUTPUTS
  # ==============================================================================
  
  # Reference formulation details
  output$reference_formulation_details <- renderUI({
    if (is.null(values$molecular_analysis)) {
      return(div("Analyze molecule first", style = "color: #8b949e;"))
    }
    
    # Show the default formulation used for reference
    div(
      tags$ul(
        style = "list-style-type: none; padding-left: 0;",
        tags$li(icon("check"), " Release: Immediate Release (IR)", 
                style = "margin: 8px 0; color: #4ecdc4;"),
        tags$li(icon("check"), " Solubility: High", 
                style = "margin: 8px 0; color: #4ecdc4;"),
        tags$li(icon("check"), " Particle Size: 50 μm", 
                style = "margin: 8px 0; color: #4ecdc4;"),
        tags$li(icon("check"), " Fed State: Fasted", 
                style = "margin: 8px 0; color: #4ecdc4;"),
        tags$li(icon("check"), " Lag Time: 0.0 h", 
                style = "margin: 8px 0; color: #4ecdc4;")
      ),
      div(class = "alert alert-info", style = "font-size: 12px; margin-top: 15px;",
          "Default formulation parameters used to calculate the reference PK parameters from your molecular structure.")
    )
  })
  
  # Test formulation details
  output$test_formulation_details <- renderUI({
    div(
      tags$ul(
        style = "list-style-type: none; padding-left: 0;",
        tags$li(icon("cog"), " Release: ", input$test_release_mechanism, 
                style = "margin: 8px 0; color: #fd79a8;"),
        tags$li(icon("cog"), " Solubility: ", input$test_solubility_class, 
                style = "margin: 8px 0; color: #fd79a8;"),
        tags$li(icon("cog"), " Particle Size: ", input$test_particle_size, " μm", 
                style = "margin: 8px 0; color: #fd79a8;"),
        tags$li(icon("cog"), " Fed State: ", input$test_fed_state, 
                style = "margin: 8px 0; color: #fd79a8;"),
        tags$li(icon("cog"), " Lag Time Override: ", input$test_tlag_override, " h", 
                style = "margin: 8px 0; color: #fd79a8;")
      ),
      div(class = "alert alert-warning", style = "font-size: 12px; margin-top: 15px;",
          "Modified formulation parameters that will be compared against the reference.")
    )
  })
  
  # Reference PK parameters display
  output$reference_pk_display <- DT::renderDataTable({
    if (is.null(values$reference_pk_params)) return(NULL)
    
    ref_pk <- values$reference_pk_params
    pk_df <- data.frame(
      Parameter = names(ref_pk),
      Value = round(unlist(ref_pk), 4),
      Unit = c("h⁻¹", "L/h", "L", "", "h"),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      pk_df,
      options = list(
        pageLength = 10, 
        dom = 't',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle('Value', backgroundColor = 'rgba(69, 183, 209, 0.2)', color = '#e6fffa')
  })
  
  # Test PK parameters display
  output$test_pk_display <- DT::renderDataTable({
    if (is.null(values$test_pk_params)) return(NULL)
    
    test_pk <- values$test_pk_params
    pk_df <- data.frame(
      Parameter = names(test_pk),
      Value = round(unlist(test_pk), 4),
      Unit = c("h⁻¹", "L/h", "L", "", "h"),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      pk_df,
      options = list(
        pageLength = 10, 
        dom = 't',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle('Value', backgroundColor = 'rgba(255, 107, 157, 0.2)', color = '#e6fffa')
  })
  
  # Parameter comparison table
  output$parameter_comparison_table <- DT::renderDataTable({
    if (is.null(values$reference_pk_params) || is.null(values$test_pk_params)) return(NULL)
    
    ref_pk <- values$reference_pk_params
    test_pk <- values$test_pk_params
    
    comparison_df <- data.frame(
      Parameter = c("Ka (h⁻¹)", "CL (L/h)", "Vd (L)", "F", "Tlag (h)"),
      Reference = c(ref_pk$Ka, ref_pk$CL, ref_pk$Vd, ref_pk$F, ref_pk$Tlag),
      Test = c(test_pk$Ka, test_pk$CL, test_pk$Vd, test_pk$F, test_pk$Tlag),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        Absolute_Diff = round(Test - Reference, 4),
        Ratio = round(Test / Reference, 3),
        Change_Percent = round((Ratio - 1) * 100, 1)
      )
    
    DT::datatable(
      comparison_df,
      options = list(
        pageLength = 10, 
        scrollX = TRUE, 
        dom = 't',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE,
      caption = "Reference (Base Molecular) vs Test (Modified Formulation) Comparison"
    ) %>%
      DT::formatStyle('Change_Percent',
                      backgroundColor = DT::styleInterval(c(-20, -5, 5, 20), 
                                                          c('rgba(255, 107, 157, 0.3)', 'rgba(249, 202, 36, 0.2)', 
                                                            'rgba(156, 136, 255, 0.1)', 'rgba(249, 202, 36, 0.2)', 
                                                            'rgba(78, 205, 196, 0.3)')),
                      color = '#e6fffa') %>%
      DT::formatStyle('Reference', backgroundColor = 'rgba(69, 183, 209, 0.2)', color = '#e6fffa') %>%
      DT::formatStyle('Test', backgroundColor = 'rgba(255, 107, 157, 0.2)', color = '#e6fffa')
  })
  
  # Molecular summary for results tab
  output$molecular_summary_box <- renderUI({
    if (is.null(values$molecular_analysis)) {
      return(div(class = "alert alert-info", "No molecular analysis available"))
    }
    
    analysis <- values$molecular_analysis
    compound_name <- if (nchar(input$compound_name) > 0) input$compound_name else "Unknown Compound"
    
    fluidRow(
      column(3,
             div(
               style = "background: linear-gradient(135deg, rgba(156, 136, 255, 0.1), rgba(156, 136, 255, 0.05)); 
                        padding: 15px; border-radius: 8px; border: 1px solid rgba(156, 136, 255, 0.3);",
               h5("Compound Information", style = "color: #9c88ff;"),
               p(strong("Name: "), compound_name, style = "color: #e6fffa;"),
               p(strong("SMILES: "), code(values$current_smiles, style = "color: #4ecdc4;"), style = "color: #e6fffa;"),
               p(strong("MW: "), paste(analysis$descriptors$MW, "g/mol"), style = "color: #e6fffa;"),
               p(strong("LogP: "), analysis$descriptors$LogP, style = "color: #e6fffa;")
             )
      ),
      column(3,
             div(
               style = "background: linear-gradient(135deg, rgba(69, 183, 209, 0.1), rgba(69, 183, 209, 0.05)); 
                        padding: 15px; border-radius: 8px; border: 1px solid rgba(69, 183, 209, 0.3);",
               h5("Reference Formulation", style = "color: #45b7d1;"),
               p(strong("Release: "), "Immediate Release (IR)", style = "color: #e6fffa;"),
               p(strong("Solubility: "), "High", style = "color: #e6fffa;"),
               p(strong("Particle Size: "), "50 μm", style = "color: #e6fffa;"),
               p(strong("Fed State: "), "Fasted", style = "color: #e6fffa;"),
               p(strong("Lag Time: "), "0.0 h", style = "color: #e6fffa;")
             )
      ),
      column(3,
             div(
               style = "background: linear-gradient(135deg, rgba(255, 107, 157, 0.1), rgba(255, 107, 157, 0.05)); 
                        padding: 15px; border-radius: 8px; border: 1px solid rgba(255, 107, 157, 0.3);",
               h5("Test Formulation", style = "color: #ff6b9d;"),
               p(strong("Release: "), input$test_release_mechanism, style = "color: #e6fffa;"),
               p(strong("Solubility: "), input$test_solubility_class, style = "color: #e6fffa;"),
               p(strong("Particle Size: "), paste(input$test_particle_size, "μm"), style = "color: #e6fffa;"),
               p(strong("Fed State: "), input$test_fed_state, style = "color: #e6fffa;"),
               p(strong("Lag Time: "), paste(input$test_tlag_override, "h"), style = "color: #e6fffa;")
             )
      ),
      column(3,
             div(
               style = "background: linear-gradient(135deg, rgba(78, 205, 196, 0.1), rgba(78, 205, 196, 0.05)); 
                        padding: 15px; border-radius: 8px; border: 1px solid rgba(78, 205, 196, 0.3);",
               h5("Comparison Overview", style = "color: #4ecdc4;"),
               if (!is.null(values$reference_pk_params) && !is.null(values$test_pk_params)) {
                 div(
                   p(strong("F Ratio: "), round(values$test_pk_params$F / values$reference_pk_params$F, 3), style = "color: #e6fffa;"),
                   p(strong("Ka Ratio: "), round(values$test_pk_params$Ka / values$reference_pk_params$Ka, 3), style = "color: #e6fffa;"),
                   p(strong("CL Ratio: "), round(values$test_pk_params$CL / values$reference_pk_params$CL, 3), style = "color: #e6fffa;"),
                   p(strong("Tlag Ratio: "), round(values$test_pk_params$Tlag / max(values$reference_pk_params$Tlag, 0.01), 3), style = "color: #e6fffa;")
                 )
               } else {
                 p("Run simulation to see comparison", style = "color: #8b949e;")
               }
             )
      )
    )
  })
  
  # ==============================================================================
  # DASHBOARD OUTPUTS
  # ==============================================================================
  
  # Value boxes
  output$decision_box <- renderValueBox({
    if (is.null(values$auc_result)) {
      valueBox(
        value = "Pending",
        subtitle = "Overall Decision",
        icon = icon("hourglass-half"),
        color = "yellow"
      )
    } else {
      auc_passes <- (values$auc_result$conf.int[1] > log(0.8)) && (values$auc_result$conf.int[2] < log(1.25))
      cmax_passes <- (values$cmax_result$conf.int[1] > log(0.8)) && (values$cmax_result$conf.int[2] < log(1.25))
      
      decision <- ifelse(auc_passes && cmax_passes, "BIOEQUIVALENT", "NOT BIOEQUIVALENT")
      color <- ifelse(auc_passes && cmax_passes, "green", "red")
      icon_name <- ifelse(auc_passes && cmax_passes, "check-circle", "times-circle")
      
      valueBox(
        value = decision,
        subtitle = "Formulation Comparison",
        icon = icon(icon_name),
        color = color
      )
    }
  })
  
  output$auc_box <- renderValueBox({
    if (is.null(values$auc_result)) {
      valueBox(
        value = "Pending",
        subtitle = "AUC Analysis",
        icon = icon("area-chart"),
        color = "blue"
      )
    } else {
      auc_passes <- (values$auc_result$conf.int[1] > log(0.8)) && (values$auc_result$conf.int[2] < log(1.25))
      status <- ifelse(auc_passes, "PASS", "FAIL")
      color <- ifelse(auc_passes, "green", "red")
      
      valueBox(
        value = status,
        subtitle = "AUC Analysis",
        icon = icon("area-chart"),
        color = color
      )
    }
  })
  
  output$cmax_box <- renderValueBox({
    if (is.null(values$cmax_result)) {
      valueBox(
        value = "Pending",
        subtitle = "Cmax Analysis",
        icon = icon("line-chart"),
        color = "purple"
      )
    } else {
      cmax_passes <- (values$cmax_result$conf.int[1] > log(0.8)) && (values$cmax_result$conf.int[2] < log(1.25))
      status <- ifelse(cmax_passes, "PASS", "FAIL")
      color <- ifelse(cmax_passes, "green", "red")
      
      valueBox(
        value = status,
        subtitle = "Cmax Analysis",
        icon = icon("line-chart"),
        color = color
      )
    }
  })
  
  # Concentration-time plot
  output$concentration_plot <- renderPlotly({
    if (is.null(values$combined_sim)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "1. Analyze molecule\n2. Run BE simulation", 
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#161b22", color = NA),
          panel.background = element_rect(fill = "#161b22", color = NA)
        )
      return(ggplotly(p))
    }
    
    summary_df <- values$combined_sim %>%
      group_by(Formulation, time) %>%
      summarize(
        mean_cp = mean(cp),
        lower = quantile(cp, 0.05),
        upper = quantile(cp, 0.95),
        .groups = "drop"
      )
    
    # Add compound name to title if available
    compound_name <- if (nchar(input$compound_name) > 0) input$compound_name else "Test Compound"
    
    p <- ggplot(summary_df, aes(x = time, y = mean_cp, color = Formulation)) +
      geom_line(linewidth = 1.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = Formulation), alpha = 0.2) +
      labs(
        title = paste("Formulation Comparison:", compound_name),
        subtitle = "Reference (Base Molecular) vs Test (Modified) | Mean ± 90% PI",
        x = "Time (h)",
        y = "Plasma Concentration (mg/L)"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid = element_line(color = "#30363d", size = 0.5),
        text = element_text(color = "#e6fffa"),
        axis.text = element_text(color = "#8b949e"),
        legend.background = element_rect(fill = "#21262d"),
        legend.text = element_text(color = "#e6fffa")
      ) +
      scale_color_manual(values = c("Reference" = "#45b7d1", "Test" = "#ff6b9d")) +
      scale_fill_manual(values = c("Reference" = "#45b7d1", "Test" = "#ff6b9d"))
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(
        hovermode = "x unified",
        plot_bgcolor = "#161b22",
        paper_bgcolor = "#161b22",
        font = list(color = "#e6fffa")
      )
  })
  
  # Bioequivalence intervals plot
  output$be_intervals_plot <- renderPlotly({
    if (is.null(values$auc_result)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Run simulation to see results", 
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#161b22", color = NA),
          panel.background = element_rect(fill = "#161b22", color = NA)
        )
      return(ggplotly(p))
    }
    
    ci_data <- tibble(
      Metric = c("AUC", "Cmax"),
      Lower = exp(c(values$auc_result$conf.int[1], values$cmax_result$conf.int[1])),
      Upper = exp(c(values$auc_result$conf.int[2], values$cmax_result$conf.int[2])),
      Mean = exp(c(values$auc_result$estimate, values$cmax_result$estimate)),
      Passes = c(
        (values$auc_result$conf.int[1] > log(0.8)) && (values$auc_result$conf.int[2] < log(1.25)),
        (values$cmax_result$conf.int[1] > log(0.8)) && (values$cmax_result$conf.int[2] < log(1.25))
      )
    )
    
    p <- ggplot(ci_data, aes(x = Metric, y = Mean, color = Passes)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "#ff6b9d", linewidth = 1) +
      geom_hline(yintercept = 1.25, linetype = "dashed", color = "#ff6b9d", linewidth = 1) +
      geom_hline(yintercept = 1.0, linetype = "solid", color = "#8b949e", alpha = 0.7) +
      labs(
        title = "90% Confidence Intervals for Formulation Comparison",
        subtitle = "Pink dashed lines: BE limits (0.8 - 1.25)",
        y = "Geometric Mean Ratio (Test / Reference)",
        x = ""
      ) +
      scale_color_manual(values = c("TRUE" = "#4ecdc4", "FALSE" = "#ff6b9d"), name = "Passes BE") +
      ylim(0.75, 1.3) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid = element_line(color = "#30363d", size = 0.5),
        text = element_text(color = "#e6fffa"),
        axis.text = element_text(color = "#8b949e"),
        legend.background = element_rect(fill = "#21262d"),
        legend.text = element_text(color = "#e6fffa")
      )
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(
        plot_bgcolor = "#161b22",
        paper_bgcolor = "#161b22",
        font = list(color = "#e6fffa")
      )
  })
  
  # Distribution comparison plot
  output$distribution_plot <- renderPlotly({
    if (is.null(values$pk_metrics)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Run simulation to see results", 
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#161b22", color = NA),
          panel.background = element_rect(fill = "#161b22", color = NA)
        )
      return(ggplotly(p))
    }
    
    pk_long <- values$pk_metrics %>%
      select(sim.id, Formulation, AUC, Cmax) %>%
      pivot_longer(cols = c(AUC, Cmax), names_to = "Metric", values_to = "Value")
    
    p <- ggplot(pk_long, aes(x = Formulation, y = Value, fill = Formulation)) +
      geom_boxplot(outlier.alpha = 0.3, color = "#e6fffa") +
      facet_wrap(~Metric, scales = "free") +
      labs(
        title = "Distribution of PK Metrics: Base vs Modified Formulation",
        y = "Value",
        x = ""
      ) +
      scale_fill_manual(values = c("Reference" = "#45b7d1", "Test" = "#ff6b9d")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid = element_line(color = "#30363d", size = 0.5),
        text = element_text(color = "#e6fffa"),
        axis.text = element_text(color = "#8b949e"),
        legend.background = element_rect(fill = "#21262d"),
        legend.text = element_text(color = "#e6fffa"),
        strip.background = element_rect(fill = "#21262d"),
        strip.text = element_text(color = "#ddd6fe")
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        plot_bgcolor = "#161b22",
        paper_bgcolor = "#161b22",
        font = list(color = "#e6fffa")
      )
  })
  
  # Density plots
  output$auc_density_plot <- renderPlotly({
    if (is.null(values$pk_wide)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Run simulation to see results", 
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#161b22", color = NA),
          panel.background = element_rect(fill = "#161b22", color = NA)
        )
      return(ggplotly(p))
    }
    
    p <- ggplot(values$pk_wide, aes(x = log_AUC_ratio)) +
      geom_density(fill = "#9c88ff", alpha = 0.6, color = "#ddd6fe") +
      geom_vline(xintercept = log(0.8), linetype = "dashed", color = "#ff6b9d", linewidth = 1) +
      geom_vline(xintercept = log(1.25), linetype = "dashed", color = "#ff6b9d", linewidth = 1) +
      geom_vline(xintercept = values$auc_result$conf.int[1], linetype = "dotted", color = "#4ecdc4", linewidth = 1.2) +
      geom_vline(xintercept = values$auc_result$conf.int[2], linetype = "dotted", color = "#4ecdc4", linewidth = 1.2) +
      labs(
        title = "Log AUC Ratio Distribution (Test/Reference)",
        subtitle = "Pink: BE limits, Teal: 90% CI",
        x = "Log AUC Ratio",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid = element_line(color = "#30363d", size = 0.5),
        text = element_text(color = "#e6fffa"),
        axis.text = element_text(color = "#8b949e")
      )
    
    ggplotly(p) %>%
      layout(
        plot_bgcolor = "#161b22",
        paper_bgcolor = "#161b22",
        font = list(color = "#e6fffa")
      )
  })
  
  output$cmax_density_plot <- renderPlotly({
    if (is.null(values$pk_wide)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Run simulation to see results", 
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#161b22", color = NA),
          panel.background = element_rect(fill = "#161b22", color = NA)
        )
      return(ggplotly(p))
    }
    
    p <- ggplot(values$pk_wide, aes(x = log_Cmax_ratio)) +
      geom_density(fill = "#fd79a8", alpha = 0.6, color = "#ddd6fe") +
      geom_vline(xintercept = log(0.8), linetype = "dashed", color = "#ff6b9d", linewidth = 1) +
      geom_vline(xintercept = log(1.25), linetype = "dashed", color = "#ff6b9d", linewidth = 1) +
      geom_vline(xintercept = values$cmax_result$conf.int[1], linetype = "dotted", color = "#4ecdc4", linewidth = 1.2) +
      geom_vline(xintercept = values$cmax_result$conf.int[2], linetype = "dotted", color = "#4ecdc4", linewidth = 1.2) +
      labs(
        title = "Log Cmax Ratio Distribution (Test/Reference)",
        subtitle = "Pink: BE limits, Teal: 90% CI",
        x = "Log Cmax Ratio",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid = element_line(color = "#30363d", size = 0.5),
        text = element_text(color = "#e6fffa"),
        axis.text = element_text(color = "#8b949e")
      )
    
    ggplotly(p) %>%
      layout(
        plot_bgcolor = "#161b22",
        paper_bgcolor = "#161b22",
        font = list(color = "#e6fffa")
      )
  })
  
  # Individual profiles plot
  output$individual_profiles_plot <- renderPlotly({
    if (is.null(values$combined_sim)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Run simulation to see results", 
                 size = 6, color = "#8b949e") +
        theme_void() +
        theme(
          plot.background = element_rect(fill = "#161b22", color = NA),
          panel.background = element_rect(fill = "#161b22", color = NA)
        )
      return(ggplotly(p))
    }
    
    n_show <- min(input$n_profiles, max(values$combined_sim$sim.id))
    subset_data <- values$combined_sim %>% filter(sim.id <= n_show)
    
    p <- ggplot(subset_data, aes(x = time, y = cp, color = Formulation)) +
      geom_line(aes(group = interaction(sim.id, Formulation)), alpha = 0.4, linewidth = 0.8) +
      facet_wrap(~Formulation) +
      labs(
        title = paste("Individual Profiles: Base vs Modified (First", n_show, "subjects)"),
        x = "Time (h)",
        y = "Plasma Concentration (mg/L)"
      ) +
      scale_color_manual(values = c("Reference" = "#45b7d1", "Test" = "#ff6b9d")) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid = element_line(color = "#30363d", size = 0.5),
        text = element_text(color = "#e6fffa"),
        axis.text = element_text(color = "#8b949e"),
        legend.background = element_rect(fill = "#21262d"),
        legend.text = element_text(color = "#e6fffa"),
        strip.background = element_rect(fill = "#21262d"),
        strip.text = element_text(color = "#ddd6fe")
      )
    
    ggplotly(p) %>%
      layout(
        plot_bgcolor = "#161b22",
        paper_bgcolor = "#161b22",
        font = list(color = "#e6fffa")
      )
  })
  
  # ==============================================================================
  # DATA TABLES
  # ==============================================================================
  
  output$pk_metrics_table <- DT::renderDataTable({
    if (is.null(values$pk_metrics)) return(NULL)
    
    display_data <- values$pk_metrics %>%
      mutate(
        Cmax = round(Cmax, 3),
        Tmax = round(Tmax, 2),
        AUC = round(AUC, 2)
      ) %>%
      arrange(Formulation, sim.id)
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "400px",
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      filter = 'top',
      rownames = FALSE,
      caption = paste("PK Metrics: Reference (Base) vs Test (Modified) -", 
                      if (nchar(input$compound_name) > 0) input$compound_name else "Test Compound")
    ) %>%
      DT::formatStyle('Formulation', 
                      backgroundColor = DT::styleEqual(c('Reference', 'Test'), 
                                                       c('rgba(69, 183, 209, 0.2)', 'rgba(255, 107, 157, 0.2)')),
                      color = '#e6fffa')
  })
  
  output$pk_summary_table <- DT::renderDataTable({
    if (is.null(values$pk_metrics)) return(NULL)
    
    summary_stats <- values$pk_metrics %>%
      group_by(Formulation) %>%
      summarize(
        N = n(),
        Cmax_Mean = round(mean(Cmax), 3),
        Cmax_SD = round(sd(Cmax), 3),
        Cmax_CV = round(sd(Cmax)/mean(Cmax) * 100, 1),
        Tmax_Median = round(median(Tmax), 2),
        AUC_Mean = round(mean(AUC), 2),
        AUC_SD = round(sd(AUC), 2),
        AUC_CV = round(sd(AUC)/mean(AUC) * 100, 1),
        .groups = "drop"
      )
    
    DT::datatable(
      summary_stats,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't',
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE,
      caption = "Summary Statistics: Reference (Base Molecular) vs Test (Modified Formulation)"
    ) %>%
      DT::formatStyle('Formulation', 
                      backgroundColor = DT::styleEqual(c('Reference', 'Test'), 
                                                       c('rgba(69, 183, 209, 0.2)', 'rgba(255, 107, 157, 0.2)')),
                      color = '#e6fffa')
  })
  
  output$ref_params_table <- DT::renderDataTable({
    if (is.null(values$pop_params_ref)) return(NULL)
    
    display_data <- values$pop_params_ref %>%
      mutate(
        Subject_ID = row_number(),
        ka = round(ka, 3),
        cl = round(cl, 2),
        vd = round(vd, 1),
        f = round(f, 3),
        tlag = round(tlag, 3)
      ) %>%
      select(Subject_ID, everything())
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "300px",
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE,
      caption = "Reference Parameters (Base Molecular Calculation)"
    ) %>%
      DT::formatStyle(columns = 1:ncol(display_data), 
                      backgroundColor = 'rgba(69, 183, 209, 0.1)',
                      color = '#e6fffa')
  })
  
  output$test_params_table <- DT::renderDataTable({
    if (is.null(values$pop_params_test)) return(NULL)
    
    display_data <- values$pop_params_test %>%
      mutate(
        Subject_ID = row_number(),
        ka = round(ka, 3),
        cl = round(cl, 2),
        vd = round(vd, 1),
        f = round(f, 3),
        tlag = round(tlag, 3)
      ) %>%
      select(Subject_ID, everything())
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "300px",
        initComplete = DT::JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#21262d', 'color': '#ddd6fe'});",
          "}"
        )
      ),
      rownames = FALSE,
      caption = paste("Test Parameters (Modified Formulation) -", 
                      if (nchar(input$compound_name) > 0) input$compound_name else "Test Compound")
    ) %>%
      DT::formatStyle(columns = 1:ncol(display_data), 
                      backgroundColor = 'rgba(255, 107, 157, 0.1)',
                      color = '#e6fffa')
  })
  
  # Statistical test results
  output$auc_test_results <- renderPrint({
    if (is.null(values$auc_result)) {
      cat("Run simulation to see AUC test results")
      return()
    }
    
    cat("AUC Formulation Comparison Results\n")
    cat("==================================\n\n")
    cat("Comparing Test (Modified) vs Reference (Base Molecular)\n")
    cat("Paired t-test on log-transformed AUC ratios\n")
    cat("90% Confidence Interval for geometric mean ratio\n\n")
    
    # Geometric mean ratio and CI
    gmr <- exp(values$auc_result$estimate)
    ci_lower <- exp(values$auc_result$conf.int[1])
    ci_upper <- exp(values$auc_result$conf.int[2])
    
    cat(sprintf("Geometric Mean Ratio: %.4f\n", gmr))
    cat(sprintf("90%% CI: [%.4f, %.4f]\n", ci_lower, ci_upper))
    cat(sprintf("p-value: %.6f\n", values$auc_result$p.value))
    
    # Decision
    passes <- (ci_lower > 0.8) && (ci_upper < 1.25)
    cat(sprintf("\nBioequivalence Decision: %s\n", ifelse(passes, "PASS", "FAIL")))
    
    if (!passes) {
      if (ci_lower <= 0.8) cat("• Lower CI bound below 0.8\n")
      if (ci_upper >= 1.25) cat("• Upper CI bound above 1.25\n")
    }
    
    cat("\nInterpretation:\n")
    if (gmr > 1.05) {
      cat("• Test formulation shows higher exposure than reference\n")
    } else if (gmr < 0.95) {
      cat("• Test formulation shows lower exposure than reference\n")
    } else {
      cat("• Test and reference formulations show similar exposure\n")
    }
  })
  
  output$cmax_test_results <- renderPrint({
    if (is.null(values$cmax_result)) {
      cat("Run simulation to see Cmax test results")
      return()
    }
    
    cat("Cmax Formulation Comparison Results\n")
    cat("===================================\n\n")
    cat("Comparing Test (Modified) vs Reference (Base Molecular)\n")
    cat("Paired t-test on log-transformed Cmax ratios\n")
    cat("90% Confidence Interval for geometric mean ratio\n\n")
    
    # Geometric mean ratio and CI
    gmr <- exp(values$cmax_result$estimate)
    ci_lower <- exp(values$cmax_result$conf.int[1])
    ci_upper <- exp(values$cmax_result$conf.int[2])
    
    cat(sprintf("Geometric Mean Ratio: %.4f\n", gmr))
    cat(sprintf("90%% CI: [%.4f, %.4f]\n", ci_lower, ci_upper))
    cat(sprintf("p-value: %.6f\n", values$cmax_result$p.value))
    
    # Decision
    passes <- (ci_lower > 0.8) && (ci_upper < 1.25)
    cat(sprintf("\nBioequivalence Decision: %s\n", ifelse(passes, "PASS", "FAIL")))
    
    if (!passes) {
      if (ci_lower <= 0.8) cat("• Lower CI bound below 0.8\n")
      if (ci_upper >= 1.25) cat("• Upper CI bound above 1.25\n")
    }
    
    cat("\nInterpretation:\n")
    if (gmr > 1.05) {
      cat("• Test formulation shows higher peak concentration than reference\n")
    } else if (gmr < 0.95) {
      cat("• Test formulation shows lower peak concentration than reference\n")
    } else {
      cat("• Test and reference formulations show similar peak concentrations\n")
    }
  })
  
  # Auto-analyze default molecule on startup
  observe({
    if (is.null(values$current_smiles)) {
      # Trigger molecular analysis automatically when app starts
      isolate({
        tryCatch({
          result <- py$get_pk_parameters_for_r(
            smiles = "CC(=O)OC1=CC=CC=C1C(=O)O",  # Default aspirin
            release_mechanism = "Immediate Release (IR)",
            solubility_class = "High",
            particle_size_um = 50,
            fed_state = "Fasted",
            tlag_override_h = 0.0,
            weight_kg = 70
          )
          
          if (result$success) {
            values$molecular_analysis <- result
            values$current_smiles <- "CC(=O)OC1=CC=CC=C1C(=O)O"
            values$reference_pk_params <- result$pk_parameters  # Set as reference
            values$test_pk_params <- result$pk_parameters       # Initially same as reference
          }
        }, error = function(e) {
          # Silently handle startup errors
        })
      })
    }
  })
  
}

# ==============================================================================
# HELPER FUNCTION FOR PYTHON SETUP
# ==============================================================================

setup_python_environment <- function() {
  # Function to help users set up Python environment
  cat("Setting up Python environment for molecular analysis...\n")
  cat("Please ensure you have:\n")
  cat("1. Python installed with rdkit, pandas, numpy\n")
  cat("2. The molecular_pk_analysis.py file in your working directory\n")
  cat("3. Reticulate package installed in R\n")
  
  # Try to detect Python and required packages
  tryCatch({
    py_config()
    cat("Python configuration detected\n")
    
    # Try importing required modules
    py_run_string("import rdkit; import pandas; import numpy")
    cat("Required Python packages available\n")
    
  }, error = function(e) {
    cat("Python setup issue detected\n")
    cat("Please install required packages: pip install rdkit pandas numpy\n")
  })
}

# Run the application
shinyApp(ui = ui, server = server)