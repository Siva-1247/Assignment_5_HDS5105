# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Analysis Dashboard", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Clinical Data", tabName = "clinical", icon = icon("file-medical")),
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
                fluidRow(
                  column(width = 3, valueBoxOutput("total_patients")),
                  column(width = 3, valueBoxOutput("mortality_rate")),
                  column(width = 3, valueBoxOutput("hospitalization_rate")),
                  column(width = 3, valueBoxOutput("median_followup"))
              ))),
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
server <- function(input, output) { }

shinyApp(ui = ui, server = server)
