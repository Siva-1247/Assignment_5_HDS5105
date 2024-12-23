library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
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

DIG$TRTMT <- factor(DIG$TRTMT, levels = c("0", "1"), labels = c("Placebo", "Treatment"))
DIG$DEATH <- factor(DIG$DEATH, levels = c("0", "1"), labels = c("Alive", "Death"))
DIG$SEX <- factor(DIG$SEX, levels = c("1", "2"), labels = c("Male", "Female"))
DIG$HYPERTEN<-factor(DIG$HYPERTEN, levels= c("0", "1"), labels = c("No", "Yes"))
DIG$CVD<-factor(DIG$CVD, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$WHF<-factor(DIG$WHF, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$DIG<-factor(DIG$DIG, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$HOSP<-factor(DIG$HOSP, levels = c("0", "1"), labels = c("No", "Yes"))
DIG$RACE<-factor(DIG$RACE, levels = c("1", "2"), labels = c("White", "Non-white"))

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
                box(title = "Filter Demographics", solidHeader = TRUE, width = 12,
                    radioButtons(inputId = "RACE", label = "Select Patient Race", choices = c("White", "Non-white")),
                    radioButtons(inputId = "SEX", label = "Select Patient Sex", choices = c("Female", "Male")),
                    sliderInput("AGE", "Select Age Range:", min = 20, max = 100, value = c(40, 50))),
                
                box(title = "Age Distribution", solidHeader = TRUE, width = 12,
                    radioButtons(inputId = "TRTMT", label = "Select Treatment Group", choices = c("Placebo", "Treatment"))
                )
              ),
              fluidRow(
                box(title = "Patient Demographics", solidHeader = TRUE, width = 6, plotOutput("patient_demographics")),
                box(title = "Age Distribution", solidHeader = TRUE, width = 6, plotOutput("age_distribution"))
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
                  plotOutput("mortality_table")
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
                  plotOutput("summary_table")
                )
              )
      )
    )
  )
)

server <- function(input, output) {patients_demographics <- reactive({
  DIG %>%
    filter(SEX == input$SEX) %>%
    filter(RACE == input$RACE) %>%
    filter(AGE >= input$AGE[1] & AGE <=input$AGE[2])
})
output$patient_demographics <- renderPlot({ 
  patients_demographics()%>%
    ggplot(aes(x = TRTMT, fill = TRTMT)) +
    geom_bar() +
    labs(title = "Patient Demographics by Treatment Group",
         x = "Treatment Group",
         y = "Count of Patients",
         fill = "Treatment") +
    scale_fill_manual(values = c( "blue", "maroon"))
  
})
age_distribution <- reactive({
  DIG %>%
    filter(TRTMT == input$TRTMT)
})
output$age_distribution <- renderPlot({ 
  age_distribution()%>%
    ggplot(aes(x = AGE, fill = TRTMT)) +
    geom_histogram(bins = 80, fill="cyan", color = "black") +
    labs(title = "Age Distribution",
         x = "Age",
         y = "Count of Patients") +
    scale_fill_manual(values = c("cyan"))
})
}
shinyApp(ui = ui, server = server)
