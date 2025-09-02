# Bioequivalence Analysis Dashboard
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

# Create custom theme
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#3498db",
    red = "#e74c3c",
    green = "#2ecc71",
    aqua = "#17a2b8",
    yellow = "#f39c12",
    blue = "#007bff",
    navy = "#001f3f",
    teal = "#39cccc",
    purple = "#605ca8",
    orange = "#ff851b",
    maroon = "#d81b60",
    fuchsia = "#f012be",
    lime = "#01ff70",
    olive = "#3d9970"
  ),
  adminlte_sidebar(
    dark_bg = "#2c3e50",
    dark_hover_bg = "#34495e",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#f8f9fa"
  )
)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = tags$span(
      icon("capsules"), 
      "Bioequivalence Analysis Dashboard",
      style = "font-weight: bold; color: white;"
    ),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("⚙️ Parameters", tabName = "parameters", icon = icon("sliders-h")),
      menuItem("📊 Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("🧪 Results", tabName = "results", icon = icon("flask")),
      menuItem("📈 Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("📋 Data Tables", tabName = "data", icon = icon("table"))
    ),
    
    # Parameter controls in sidebar
    div(
      style = "padding: 20px;",
      h4("Simulation Controls", style = "color: #ecf0f1; margin-bottom: 20px;"),
      
      numericInput("n_subjects", "Number of Subjects:", 
                   value = 1000, min = 100, max = 5000, step = 100),
      
      actionButton("run_simulation", "🚀 Run Simulation", 
                   class = "btn-primary btn-block",
                   style = "margin-top: 15px; font-weight: bold;"),
      
      hr(style = "border-color: #34495e;"),
      
      h5("Quick Presets:", style = "color: #ecf0f1;"),
      actionButton("preset_bioequivalent", "✅ Bioequivalent", 
                   class = "btn-success btn-sm btn-block", 
                   style = "margin: 5px 0;"),
      actionButton("preset_not_bioequivalent", "❌ Not Bioequivalent", 
                   class = "btn-danger btn-sm btn-block", 
                   style = "margin: 5px 0;")
    )
  ),
  
  dashboardBody(
    use_theme(my_theme),
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3498db;
        }
        .box.box-primary {
          border-top-color: #3498db;
        }
        .small-box h3 {
          font-size: 2.2rem;
          font-weight: 700;
        }
        .progress-bar {
          background-color: #3498db;
        }
        .plotly {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "dashboard",
        fluidRow(
          # Summary boxes
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
      
      # Parameters Tab
      tabItem(
        tabName = "parameters",
        fluidRow(
          box(
            title = "🔧 Reference Formulation Parameters", 
            status = "info", 
            solidHeader = TRUE, 
            width = 6,
            div(
              style = "padding: 10px;",
              h5("Fixed Reference Parameters:", style = "color: #2c3e50; font-weight: bold;"),
              tags$ul(
                tags$li("Absorption rate (ka): 0.7 h⁻¹"),
                tags$li("Clearance (cl): 6 L/h"),
                tags$li("Volume of distribution (vd): 35 L"),
                tags$li("Bioavailability (f): 0.6"),
                tags$li("Lag time (tlag): 0.1 h")
              )
            )
          ),
          
          box(
            title = "⚗️ Test Formulation Parameters", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            
            sliderInput("ka_mean", "Absorption Rate (ka):",
                        min = 0.1, max = 2.0, value = 0.5, step = 0.1),
            
            sliderInput("cl_mean", "Clearance (cl):",
                        min = 2, max = 12, value = 6.0, step = 0.5),
            
            sliderInput("vd_mean", "Volume of Distribution (vd):",
                        min = 10, max = 70, value = 35.0, step = 2.5),
            
            sliderInput("f_mean", "Bioavailability (f):",
                        min = 0.3, max = 1.0, value = 0.65, step = 0.05),
            
            sliderInput("tlag_mean", "Lag Time (tlag):",
                        min = 0.0, max = 5.0, value = 2.0, step = 0.1)
          )
        ),
        
        fluidRow(
          box(
            title = "📋 Parameter Comparison", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            DT::dataTableOutput("parameter_comparison_table")
          )
        )
      ),
      
      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "🧪 Statistical Test Results", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            
            fluidRow(
              column(6,
                     h4("AUC Analysis", style = "color: #2c3e50;"),
                     verbatimTextOutput("auc_test_results")
              ),
              column(6,
                     h4("Cmax Analysis", style = "color: #2c3e50;"),
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
            h5("Reference Formulation"),
            DT::dataTableOutput("ref_params_table")
          ),
          box(
            title = "📋 Population Parameters", 
            status = "warning", 
            solidHeader = TRUE, 
            width = 6,
            h5("Test Formulation"),
            DT::dataTableOutput("test_params_table")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store simulation results
  values <- reactiveValues(
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
  
  # Preset buttons
  observeEvent(input$preset_bioequivalent, {
    updateSliderInput(session, "ka_mean", value = 0.7)
    updateSliderInput(session, "cl_mean", value = 6.0)
    updateSliderInput(session, "vd_mean", value = 35.0)
    updateSliderInput(session, "f_mean", value = 0.62)
    updateSliderInput(session, "tlag_mean", value = 0.15)
    showNotification("Preset applied: Parameters set for bioequivalence", type = "default")
  })
  
  observeEvent(input$preset_not_bioequivalent, {
    updateSliderInput(session, "ka_mean", value = 0.4)
    updateSliderInput(session, "cl_mean", value = 8.5)
    updateSliderInput(session, "vd_mean", value = 28.0)
    updateSliderInput(session, "f_mean", value = 0.45)
    updateSliderInput(session, "tlag_mean", value = 3.5)
    showNotification("Preset applied: Parameters set for non-bioequivalence", type = "warning")
  })
  
  # Main simulation function
  observeEvent(input$run_simulation, {
    
    # Show progress
    withProgress(message = 'Running simulation...', value = 0, {
      
      # Model setup
      incProgress(0.1, detail = "Setting up model...")
      
      model <- rxode2({
        ka <- ka
        cl <- cl
        v  <- vd
        f  <- f
        tlag <- tlag
        
        d/dt(depot)   = -ka * depot
        d/dt(central) =  ka * depot - (cl/v) * central
        
        cp = central / v
      })
      
      # Generate population parameters
      incProgress(0.2, detail = "Generating population parameters...")
      
      set.seed(42)
      n_subjects <- input$n_subjects
      
      # Reference parameters
      values$pop_params_ref <- tibble(
        ka = rlnorm(n_subjects, log(0.7), 0.1),
        cl = rlnorm(n_subjects, log(6), 0.15),
        vd = rlnorm(n_subjects, log(35), 0.1),
        f = rlnorm(n_subjects, log(0.6), 0.05),
        tlag = rlnorm(n_subjects, log(0.1), 0.3)
      )
      
      # Test parameters
      values$pop_params_test <- tibble(
        ka = rlnorm(n_subjects, log(input$ka_mean), 0.1),
        cl = rlnorm(n_subjects, log(input$cl_mean), 0.15),
        vd = rlnorm(n_subjects, log(input$vd_mean), 0.1),
        f = rlnorm(n_subjects, log(input$f_mean), 0.05),
        tlag = rlnorm(n_subjects, log(input$tlag_mean), 0.3)
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
          log_AUC_T  = log(AUC_Test),
          log_AUC_R  = log(AUC_Reference),
          AUC_ratio   = AUC_Test / AUC_Reference,
          Cmax_ratio  = Cmax_Test / Cmax_Reference,
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
    
    showNotification("Simulation completed successfully!", type = "default")
  })
  
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
      auc_passes <- (values$auc_result$conf.int[1] > log(0.8)) && 
        (values$auc_result$conf.int[2] < log(1.25))
      cmax_passes <- (values$cmax_result$conf.int[1] > log(0.8)) && 
        (values$cmax_result$conf.int[2] < log(1.25))
      
      decision <- ifelse(auc_passes && cmax_passes, "BIOEQUIVALENT", "NOT BIOEQUIVALENT")
      color <- ifelse(auc_passes && cmax_passes, "green", "red")
      icon_name <- ifelse(auc_passes && cmax_passes, "check-circle", "times-circle")
      
      valueBox(
        value = decision,
        subtitle = "Overall Decision",
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
      auc_passes <- (values$auc_result$conf.int[1] > log(0.8)) && 
        (values$auc_result$conf.int[2] < log(1.25))
      
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
      cmax_passes <- (values$cmax_result$conf.int[1] > log(0.8)) && 
        (values$cmax_result$conf.int[2] < log(1.25))
      
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
        annotate("text", x = 0.5, y = 0.5, label = "Run simulation to see results", 
                 size = 6, color = "gray") +
        theme_void()
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
    
    p <- ggplot(summary_df, aes(x = time, y = mean_cp, color = Formulation)) +
      geom_line(linewidth = 1.2) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = Formulation), alpha = 0.2) +
      labs(
        title = "Mean Plasma Concentration-Time Curves",
        subtitle = "Mean ± 90% Prediction Interval",
        x = "Time (h)",
        y = "Plasma Concentration (mg/L)"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("Reference" = "#3498db", "Test" = "#e74c3c")) +
      scale_fill_manual(values = c("Reference" = "#3498db", "Test" = "#e74c3c"))
    
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      layout(hovermode = "x unified")
  })
  
  # Bioequivalence intervals plot
  output$be_intervals_plot <- renderPlotly({
    if (is.null(values$auc_result)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run simulation to see results", 
                 size = 6, color = "gray") +
        theme_void()
      return(ggplotly(p))
    }
    
    ci_data <- tibble(
      Metric = c("AUC", "Cmax"),
      Lower = exp(c(values$auc_result$conf.int[1], values$cmax_result$conf.int[1])),
      Upper = exp(c(values$auc_result$conf.int[2], values$cmax_result$conf.int[2])),
      Mean  = exp(c(values$auc_result$estimate, values$cmax_result$estimate)),
      Passes = c(
        (values$auc_result$conf.int[1] > log(0.8)) && (values$auc_result$conf.int[2] < log(1.25)),
        (values$cmax_result$conf.int[1] > log(0.8)) && (values$cmax_result$conf.int[2] < log(1.25))
      )
    )
    
    p <- ggplot(ci_data, aes(x = Metric, y = Mean, color = Passes)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, linewidth = 1) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", linewidth = 1) +
      geom_hline(yintercept = 1.25, linetype = "dashed", color = "red", linewidth = 1) +
      geom_hline(yintercept = 1.0, linetype = "solid", color = "gray", alpha = 0.5) +
      labs(
        title = "90% Confidence Intervals",
        subtitle = "Red dashed lines: BE limits (0.8 - 1.25)",
        y = "Geometric Mean Ratio (Test / Reference)", 
        x = ""
      ) +
      scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"), 
                         name = "Passes BE") +
      ylim(0.75, 1.3) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # Distribution comparison plot
  output$distribution_plot <- renderPlotly({
    if (is.null(values$pk_metrics)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run simulation to see results", 
                 size = 6, color = "gray") +
        theme_void()
      return(ggplotly(p))
    }
    
    pk_long <- values$pk_metrics %>%
      select(sim.id, Formulation, AUC, Cmax) %>%
      pivot_longer(cols = c(AUC, Cmax), names_to = "Metric", values_to = "Value")
    
    p <- ggplot(pk_long, aes(x = Formulation, y = Value, fill = Formulation)) +
      geom_boxplot(outlier.alpha = 0.3) +
      facet_wrap(~Metric, scales = "free") +
      labs(
        title = "Distribution of PK Metrics",
        y = "Value", x = ""
      ) +
      scale_fill_manual(values = c("Reference" = "#3498db", "Test" = "#e74c3c")) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Density plots
  output$auc_density_plot <- renderPlotly({
    if (is.null(values$pk_wide)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run simulation to see results", 
                 size = 6, color = "gray") +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(values$pk_wide, aes(x = log_AUC_ratio)) +
      geom_density(fill = "#3498db", alpha = 0.6) +
      geom_vline(xintercept = log(0.8), linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = log(1.25), linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = values$auc_result$conf.int[1], linetype = "dotted", color = "darkblue") +
      geom_vline(xintercept = values$auc_result$conf.int[2], linetype = "dotted", color = "darkblue") +
      labs(
        title = "Log AUC Ratio Distribution",
        subtitle = "Red: BE limits, Blue: 90% CI",
        x = "Log AUC Ratio", y = "Density"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$cmax_density_plot <- renderPlotly({
    if (is.null(values$pk_wide)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run simulation to see results", 
                 size = 6, color = "gray") +
        theme_void()
      return(ggplotly(p))
    }
    
    p <- ggplot(values$pk_wide, aes(x = log_Cmax_ratio)) +
      geom_density(fill = "#e74c3c", alpha = 0.6) +
      geom_vline(xintercept = log(0.8), linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = log(1.25), linetype = "dashed", color = "red", linewidth = 1) +
      geom_vline(xintercept = values$cmax_result$conf.int[1], linetype = "dotted", color = "darkblue") +
      geom_vline(xintercept = values$cmax_result$conf.int[2], linetype = "dotted", color = "darkblue") +
      labs(
        title = "Log Cmax Ratio Distribution",
        subtitle = "Red: BE limits, Blue: 90% CI",
        x = "Log Cmax Ratio", y = "Density"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Individual profiles plot
  output$individual_profiles_plot <- renderPlotly({
    if (is.null(values$combined_sim)) {
      p <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Run simulation to see results", 
                 size = 6, color = "gray") +
        theme_void()
      return(ggplotly(p))
    }
    
    n_show <- min(input$n_profiles, max(values$combined_sim$sim.id))
    subset_data <- values$combined_sim %>%
      filter(sim.id <= n_show)
    
    p <- ggplot(subset_data, aes(x = time, y = cp, color = Formulation)) +
      geom_line(aes(group = interaction(sim.id, Formulation)), alpha = 0.3) +
      facet_wrap(~Formulation) +
      labs(
        title = paste("Individual Subject Profiles (First", n_show, "subjects)"),
        x = "Time (h)",
        y = "Plasma Concentration (mg/L)"
      ) +
      scale_color_manual(values = c("Reference" = "#3498db", "Test" = "#e74c3c")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data tables
  output$parameter_comparison_table <- DT::renderDataTable({
    if (is.null(values$pop_params_ref)) return(NULL)
    
    ref_means <- list(
      ka_mean = 0.7,
      cl_mean = 6.0,
      vd_mean = 35.0,
      f_mean = 0.6,
      tlag_mean = 0.1
    )
    
    param_comparison <- tibble(
      Parameter = c("ka (h⁻¹)", "cl (L/h)", "vd (L)", "f", "tlag (h)"),
      Reference_Mean = c(mean(values$pop_params_ref$ka), mean(values$pop_params_ref$cl), 
                         mean(values$pop_params_ref$vd), mean(values$pop_params_ref$f), 
                         mean(values$pop_params_ref$tlag)),
      Test_Mean = c(mean(values$pop_params_test$ka), mean(values$pop_params_test$cl), 
                    mean(values$pop_params_test$vd), mean(values$pop_params_test$f), 
                    mean(values$pop_params_test$tlag)),
      Test_Target = c(input$ka_mean, input$cl_mean, input$vd_mean, 
                      input$f_mean, input$tlag_mean),
      Ratio = Test_Mean / Reference_Mean,
      Percent_Change = round((Ratio - 1) * 100, 1)
    ) %>%
      mutate(
        Reference_Mean = round(Reference_Mean, 3),
        Test_Mean = round(Test_Mean, 3),
        Test_Target = round(Test_Target, 3),
        Ratio = round(Ratio, 3)
      )
    
    DT::datatable(
      param_comparison,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 't'
      ),
      rownames = FALSE
    ) %>%
      DT::formatStyle('Percent_Change',
                      backgroundColor = DT::styleInterval(c(-10, 10), c('#ffebee', '#ffffff', '#e8f5e8')))
  })
  
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
        scrollY = "400px"
      ),
      filter = 'top',
      rownames = FALSE
    ) %>%
      DT::formatStyle('Formulation',
                      backgroundColor = DT::styleEqual(c('Reference', 'Test'), 
                                                       c('#e3f2fd', '#ffebee')))
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
        dom = 't'
      ),
      rownames = FALSE,
      caption = "Summary Statistics by Formulation"
    ) %>%
      DT::formatStyle('Formulation',
                      backgroundColor = DT::styleEqual(c('Reference', 'Test'), 
                                                       c('#e3f2fd', '#ffebee')))
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
        scrollY = "300px"
      ),
      rownames = FALSE
    )
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
        scrollY = "300px"
      ),
      rownames = FALSE
    )
  })
  
  # Statistical test results
  output$auc_test_results <- renderPrint({
    if (is.null(values$auc_result)) {
      cat("Run simulation to see AUC test results")
      return()
    }
    
    cat("AUC Bioequivalence Test Results\n")
    cat("===============================\n\n")
    
    # Print basic test info
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
    cat(sprintf("\nBioequivalence Decision: %s\n", 
                ifelse(passes, "✅ PASS", "❌ FAIL")))
    
    if (!passes) {
      if (ci_lower <= 0.8) cat("• Lower CI bound below 0.8\n")
      if (ci_upper >= 1.25) cat("• Upper CI bound above 1.25\n")
    }
  })
  
  output$cmax_test_results <- renderPrint({
    if (is.null(values$cmax_result)) {
      cat("Run simulation to see Cmax test results")
      return()
    }
    
    cat("Cmax Bioequivalence Test Results\n")
    cat("================================\n\n")
    
    # Print basic test info
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
    cat(sprintf("\nBioequivalence Decision: %s\n", 
                ifelse(passes, "✅ PASS", "❌ FAIL")))
    
    if (!passes) {
      if (ci_lower <= 0.8) cat("• Lower CI bound below 0.8\n")
      if (ci_upper >= 1.25) cat("• Upper CI bound above 1.25\n")
    }
  })
  
  # Auto-run simulation on startup with default parameters
  observe({
    # Trigger simulation automatically when app starts
    if (is.null(values$ref_sim)) {
      # Simulate clicking the run button
      updateActionButton(session, "run_simulation")
      
      # Actually trigger the simulation logic
      isolate({
        # Show progress
        withProgress(message = 'Running initial simulation...', value = 0, {
          
          # Model setup
          incProgress(0.1, detail = "Setting up model...")
          
          model <- rxode2({
            ka <- ka
            cl <- cl
            v  <- vd
            f  <- f
            tlag <- tlag
            
            d/dt(depot)   = -ka * depot
            d/dt(central) =  ka * depot - (cl/v) * central
            
            cp = central / v
          })
          
          # Generate population parameters
          incProgress(0.2, detail = "Generating population parameters...")
          
          set.seed(42)
          n_subjects <- isolate(input$n_subjects)
          
          # Reference parameters
          values$pop_params_ref <- tibble(
            ka = rlnorm(n_subjects, log(0.7), 0.1),
            cl = rlnorm(n_subjects, log(6), 0.15),
            vd = rlnorm(n_subjects, log(35), 0.1),
            f = rlnorm(n_subjects, log(0.6), 0.05),
            tlag = rlnorm(n_subjects, log(0.1), 0.3)
          )
          
          # Test parameters (use current input values)
          values$pop_params_test <- tibble(
            ka = rlnorm(n_subjects, log(isolate(input$ka_mean)), 0.1),
            cl = rlnorm(n_subjects, log(isolate(input$cl_mean)), 0.15),
            vd = rlnorm(n_subjects, log(isolate(input$vd_mean)), 0.1),
            f = rlnorm(n_subjects, log(isolate(input$f_mean)), 0.05),
            tlag = rlnorm(n_subjects, log(isolate(input$tlag_mean)), 0.3)
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
              log_AUC_T  = log(AUC_Test),
              log_AUC_R  = log(AUC_Reference),
              AUC_ratio   = AUC_Test / AUC_Reference,
              Cmax_ratio  = Cmax_Test / Cmax_Reference,
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
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)