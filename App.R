library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(DT)
library(dplyr)
library(corrplot)


DIG_data <- read.csv("DIG.csv")
# UI Code
ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Analysis Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Clinical Data", tabName = "clinical", icon = icon("file-medical")),
      menuItem("Patient Outcome", tabName = "outcomes", icon = icon("user-check")),
      menuItem("Mortality Overview", tabName = "mortality", icon = icon("skull-crossbones")),
      menuItem("Adverse Events", tabName = "adverse", icon = icon("exclamation-triangle")),
      menuItem("Risk Analysis", tabName = "risk", icon = icon("chart-bar"))
    )
  ),
  
  # Dashboard Body
  dashboardBody(
    tags$head(
      
      tags$style(HTML("
      
       .main-header {
        background-color: #d8b4a0 !important;  /* Soft brown pastel */
        color: #ffffff !important;            /* White text for contrast */
      }
      .main-header .logo {
        background-color: #c9a99b !important; /* Slightly darker soft brown */
        color: #ffffff !important;
      }
      .main-header .navbar {
        background-color: #d8b4a0 !important; /* Matches the header background */
      }
      .main-sidebar {
        background-color: #f4e1d2 !important; /* Light pastel brown */
      }
      .main-sidebar .sidebar-menu > li.active > a {
        background-color: #f9c9d2 !important; /* Soft pastel pink for active item */
        color: #4a2c2a !important;            /* Darker brown text */
      }
      .main-sidebar .sidebar-menu > li > a {
        color: #4a2c2a !important;            /* Dark brown text for menu items */
      }
      .main-sidebar .sidebar-menu > li > a:hover {
        background-color: #d8b4a0 !important; /* Highlight on hover */
        color: #ffffff !important;            /* White text for contrast */
      }
        .box { 
          background-color: #f4e1d2 !important;  /* Pastel light brown */
          border-color: #d8b4a0 !important;      /* Soft brown border */
        }
        
        .box .box-header {
          background-color: #f9c9d2 !important;  /* Soft pastel pink */
          color: #4a2c2a !important;              /* Darker brown text */
        }
        
        .box .box-footer {
          background-color: #f9c9d2 !important;  /* Soft pastel pink */
          color: #4a2c2a !important;              /* Darker brown text */
        }
        
        .value-box {
          background-color: #f0d1cb !important;  /* Light pastel pink */
          color: #6a4e3c !important;              /* Deep brown text */
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Summary Statistics",
                  solidHeader = TRUE,
                  width = 12,
                  div(
                    style = "display: flex; justify-content: space-around; align-items: center; gap: 10px; padding: 10px;",
                    valueBoxOutput("total_patients", width = 3),
                    valueBoxOutput("mortality_rate", width = 3),
                    valueBoxOutput("hospitalization_rate", width = 3),
                    valueBoxOutput("median_followup", width = 3)
                  )
                )
              ),
              fluidRow(
                # Key Findings Box
                box(
                  title = "Study Overview",
                  solidHeader = TRUE,
                  width = 12,
                  textOutput("study_summary")
                )
              )
      ),
      
      # Demographics Tab
      
      tabItem(tabName = "demographics",
              fluidRow(
                # Gender and Race Distribution
                box(
                  title = "Patient Demographics",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("gender_race_plot")
                ),
                # Age Distribution
                box(
                  title = "Age Distribution",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("age_dist_plot")
                )
              ),
              fluidRow(
                # Age by Treatment Group
                box(
                  title = "Age by Treatment Group",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("age_treatment_plot")
                )
              )
      ),
      tabItem(tabName = "clinical",
              fluidRow(
                box(
                  title = "Clinical Measures", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("clinicalPlot")
                ),
                box(
                  title = "Filter Options", status = "info", solidHeader = TRUE, width = 6,
                  selectInput("clinicalVariable", "Select Variable:", choices = NULL),
                  sliderInput("ageRange", "Select Age Range:", min = 0, max = 100, value = c(20, 60))
                )
              )
      ),
      tabItem(
        tabName = "outcomes",
        fluidRow(
          box(
            title = "Outcome Trends", status = "primary", solidHeader = TRUE, width = 6,
            plotOutput("outcomeTrend")
          ),
          box(
            title = "Outcome Statistics", status = "primary", solidHeader = TRUE, width = 6,
            verbatimTextOutput("outcomeSummary")
          )
        ),
        fluidRow(
          box(
            title = "Analysis Options", status = "info", solidHeader = TRUE, width = 12,
            selectInput("outcomeMetric", "Select Metric:", choices = NULL),
            dateRangeInput("dateRange", "Select Date Range:")
          )
        )
      ),
      tabItem(tabName = "mortality",
              fluidRow(
                # Controls for dynamic table filtering
                box(
                  title = "Mortality Analysis",
                  solidHeader = TRUE,
                  width = 12,
                  column(4,
                         selectInput("mortality_var", "Variable to Stratify By:",
                                     choices = c("Age" = "AGE", "Treatment" = "TRTMT", "Gender" = "SEX", "Diabetes" = "DIABETES"))
                  ),
                  column(4,
                         selectInput("mortality_filter", "Filter by Mortality Status:",
                                     choices = c("All", "Alive", "Dead"))
                  )
                )
              ),
              fluidRow(
                # Mortality table
                box(
                  title = "Mortality Table",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("mortality_table")
                )
              )
      ),
      # Adverse Events Tab
      tabItem(tabName = "adverse",
              fluidRow(
                # Total Hospitalizations
                box(
                  title = "Number of Hospitalizations",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("total_hosp_plot")
                ),
                # Cause of Death
                box(
                  title = "Cause of Death Distribution",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("death_cause_plot")
                )
              )
      ),
      
      # Risk Analysis Tab
      tabItem(tabName = "risk",
              fluidRow(
                # Forest Plot
                box(
                  title = "Risk Factors (Hazard Ratios)",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("forest_plot")
                ),
                # Correlation Heatmap
                box(
                  title = "Variable Correlations",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("correlation_plot")
                )
              ),
              fluidRow(
                # Summary Statistics Table
                box(
                  title = "Key Statistics",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("summary_table")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Overview Tab Outputs
  output$total_patients <- renderValueBox({
    valueBox(value = nrow(DIG_data), subtitle = "Total Patients", icon = icon("user"))
  })
  
  output$mortality_rate <- renderValueBox({
    rate <- mean(DIG_data$DEATH == "Deceased") * 100
    valueBox(value = paste0(round(rate, 2), "%"), subtitle = "Mortality Rate", icon = icon("skull-crossbones"))
  })
  
  output$hospitalization_rate <- renderValueBox({
    rate <- mean(DIG_data$HOSP == 1) * 100
    valueBox(value = paste0(round(rate, 2), "%"), subtitle = "Hospitalization Rate", icon = icon("hospital"))
  })
  
  output$median_followup <- renderValueBox({
    value <- median(DIG_data$DEATHDAY, na.rm = TRUE)
    valueBox(value = paste(value, "days"), subtitle = "Median Follow-up", icon = icon("calendar"))
  })
  
  output$study_summary <- renderText({
    "This dashboard summarizes key metrics from the DIG trial, including demographic distributions, clinical measures, outcomes, and risk analyses. Explore each tab for detailed visualizations and statistics."
  })
  
  # Demographics Tab Outputs
  output$gender_race_plot <- renderPlot({
    ggplot(DIG_data, aes(x = SEX, fill = RACE)) +
      geom_bar(position = "dodge") +
      labs(title = "Gender and Race Distribution", x = "Gender", y = "Count", fill = "Race")
  })
  
  output$age_dist_plot <- renderPlot({
    ggplot(DIG_data, aes(x = AGE)) +
      geom_histogram(bins = 20, fill = "steelblue", color = "white") +
      labs(title = "Age Distribution", x = "Age", y = "Count")
  })
  
  output$age_treatment_plot <- renderPlot({
    ggplot(DIG_data, aes(x = TRTMT, y = AGE)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Age by Treatment Group", x = "Treatment Group", y = "Age")
  })
  
  # Clinical Data Tab Outputs
  updateSelectInput(session, "clinicalVariable", choices = colnames(DIG_data)[7:15])
  
  filtered_data <- reactive({
    DIG_data[DIG_data$AGE >= input$ageRange[1] & DIG_data$AGE <= input$ageRange[2], ]
  })
  
  output$clinicalPlot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = "AGE", y = input$clinicalVariable)) +
      geom_point() +
      labs(title = "Clinical Measure by Age", x = "Age", y = input$clinicalVariable)
  })
  
  # Patient Outcomes Tab Outputs
  output$outcomeTrend <- renderPlot({
    ggplot(DIG_data, aes(x = DEATHDAY, fill = DEATH)) +
      geom_histogram(bins = 20) +
      labs(title = "Outcome Trends Over Time", x = "Days Since Randomization", y = "Count", fill = "Outcome")
  })
  
  output$outcomeSummary <- renderPrint({
    summary(DIG_data[, c("DEATHDAY", "HOSP")])
  })
  
  # Mortality Tab Outputs
  output$mortality_table <- renderDT({
    # Filter data based on mortality status
    filtered_data <- if (input$mortality_filter == "All") {
      DIG_data
    } else {
      DIG_data[DIG_data$DEATH == input$mortality_filter, ]
    }
    
    # Calculate mortality rates stratified by selected variable
    mortality_summary <- filtered_data %>%
      group_by(.data[[input$mortality_var]]) %>%
      dplyr::summarise(
        Alive = sum(DEATH == "Alive"),
        Deceased = sum(DEATH == "Deceased"),
        Mortality_Rate = round((Deceased / (Alive + Deceased)) * 100, 2)
      ) %>%
      dplyr::arrange(desc(Mortality_Rate)) # Use explicit namespace
    
    # Display as a datatable
    datatable(
      mortality_summary,
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  
  # Adverse Events Tab Outputs
  output$total_hosp_plot <- renderPlot({
    ggplot(DIG_data, aes(x = HOSP)) +
      geom_bar(fill = "red") +
      labs(title = "Total Hospitalizations", x = "Hospitalization Status", y = "Count")
  })
  
  output$death_cause_plot <- renderPlot({
    ggplot(DIG_data, aes(x = factor(REASON))) +
      geom_bar(fill = "purple") +
      labs(title = "Cause of Death Distribution", x = "Cause", y = "Count")
  })
  
  # Risk Analysis Tab Outputs
  output$forest_plot <- renderPlot({
    ggplot(DIG_data, aes(x = TRTMT, y = HEARTRTE)) +
      geom_boxplot(fill = "coral") +
      labs(title = "Hazard Ratios by Treatment Group", x = "Treatment", y = "Heart Rate")
  })
  
  output$correlation_plot <- renderPlot({
    cor_data <- cor(DIG_data[, sapply(DIG_data, is.numeric)], use = "complete.obs")
    corrplot::corrplot(cor_data, method = "circle", type = "upper", tl.col = "black", tl.cex = 0.8)
  })
  
  output$summary_table <- renderDT({
    summary_stats <- DIG_data %>%
      summarise(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), 2)))
    datatable(summary_stats, rownames = FALSE, options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)