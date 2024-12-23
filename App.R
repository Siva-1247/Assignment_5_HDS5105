library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(DT)
library(dplyr)

DIG <- read_csv("DIG.csv")


DIG$SEX<-as.factor(DIG$SEX)
DIG$TRTMT<-as.factor(DIG$TRTMT)
DIG$HYPERTEN<-as.factor(DIG$HYPERTEN)
DIG$CVD<- as.factor(DIG$CVD)
DIG$WHF<- as.factor(DIG$WHF)
DIG$DIG<- as.factor(DIG$DIG)
DIG$HOSP<- as.factor(DIG$HOSP) 
DIG$DEATH<-as.factor(DIG$DEATH)
DIG$AGE<-as.numeric(DIG$AGE)
DIG$RACE<-as.factor(DIG$RACE)
DIG$DWHF<-as.factor(DIG$DWHF)
DIG$DIABETES<-as.factor(DIG$DIABETES)

DIG$TRTMT <- factor(DIG$TRTMT, levels = c("0", "1"), labels = c("Placebo", "Treatment"))
DIG$DEATH <- factor(DIG$DEATH, levels = c("0", "1"), labels = c("Alive", "Death"))
DIG$SEX <- factor(DIG$SEX, levels = c("1", "2"), labels = c("Male", "Female"))
DIG$HYPERTEN<-factor(DIG$HYPERTEN, levels= c("0", "1"), labels = c("No", "Yes"))
DIG$CVD<-factor(DIG$CVD, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$WHF<-factor(DIG$WHF, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$DIG<-factor(DIG$DIG, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$HOSP<-factor(DIG$HOSP, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$RACE<-factor(DIG$RACE, levels = c("1", "2"), labels = c("White", "Non-white"))
DIG$DWHF<-factor(DIG$DWHF, levels = c("0", "1"), labels = c("Alive", "Death/Hospitalisation"))
DIG$DIABETES<-factor(DIG$DIABETES, levels = c("0", "1"), labels = c("No", "Yes"))

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

server <- function(input, output) { # Add Overview Tab Metrics
  output$total_patients <- renderValueBox({
    valueBox(value = nrow(DIG), subtitle = "Total Patients", icon = icon("user")) # Highlighted: Added for total patients
  })
  output$mortality_rate <- renderValueBox({
    rate <- mean(DIG$DEATH == "Death") * 100
    valueBox(value = paste0(round(rate, 2), "%"), subtitle = "Mortality Rate", icon = icon("skull-crossbones")) # Highlighted: Added for mortality rate
  })
  
  # Mortality Table Logic
  filtered_mortality <- reactive({
    if (input$mortality_filter == "All") {
      DIG
    } else {
      DIG %>% filter(DEATH == input$mortality_filter) # Highlighted: Added reactive filtering for mortality
    }
  })
  
  output$mortality_table <- renderDT({
    filtered_mortality() %>%
      group_by(.data[[input$mortality_var]]) %>%
      summarise(
        Alive = sum(DEATH == "Alive"),
        Deceased = sum(DEATH == "Death"),
        Mortality_Rate = round((Deceased / (Alive + Deceased)) * 100, 2)
      )
  }) # Highlighted: Updated mortality logic
  
  # Summary Table for Overview
  patients <- DIG %>% group_by(TRTMT) %>% summarise(patients = n())
  mortality_rates <- DIG %>% group_by(TRTMT) %>% summarise(mortality_rate = mean(DEATH == "Death", na.rm = TRUE))
  hospitalization_rates <- DIG %>% group_by(TRTMT) %>% summarise(hosp_rate = mean(HOSP == "Yes", na.rm = TRUE))
  
  summary_table <- left_join(mortality_rates, hospitalization_rates, by = "TRTMT")
  summary_table <- left_join(summary_table, patients, by = "TRTMT")
  
  output$summary_table <- renderDT({
    summary_table %>%
      select(
        Treatment_Group = TRTMT,
        Total_Patients = patients,
        Mortality_Rate = mortality_rate,
        Hospitalization_Rate = hosp_rate
      )
  }) # Highlighted: Updated for interactivity using renderDT
}

shinyApp(ui = ui, server = server)

shinyApp(ui = ui, server = server)
