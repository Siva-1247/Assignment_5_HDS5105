# Install required packages if not already installed
if (!require(pacman)) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2, dplyr, plotly, corrplot)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Patient Data", tabName = "data", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_patients", width = 3),
                valueBoxOutput("death_rate", width = 3),
                valueBoxOutput("avg_age", width = 3),
                valueBoxOutput("avg_ef", width = 3)
              ),
              fluidRow(
                box(
                  title = "Age Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("age_hist")
                ),
                box(
                  title = "Ejection Fraction Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("ef_hist")
                )
              )
      ),
      
      # Patient Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Filter Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4, 
                           sliderInput("age_range", 
                                       "Age Range:",
                                       min = 0, max = 100,
                                       value = c(0, 100)
                           )
                    ),
                    column(4,
                           sliderInput("ef_range",
                                       "Ejection Fraction Range:",
                                       min = 0, max = 100,
                                       value = c(0, 100)
                           )
                    ),
                    column(4,
                           selectInput("death",
                                       "Death Status:",
                                       choices = c("All", "Alive", "Deceased")
                           )
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Patient Records",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("patient_table")
                )
              )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Correlation Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("correlation_plot")
                ),
                box(
                  title = "Variable Relationships",
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("var1", "Select X Variable:", ""),
                  selectInput("var2", "Select Y Variable:", ""),
                  plotlyOutput("scatter_plot")
                )
              )
      ),
      
      # Survival Analysis Tab
      tabItem(tabName = "survival",
              fluidRow(
                box(
                  title = "Survival Analysis",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("group_var", 
                              "Group By:",
                              choices = c("Treatment" = "TRTMT",
                                          "Gender" = "SEX",
                                          "Previous MI" = "PREVMI",
                                          "Diabetes" = "DIABETES"),
                              selected = "TRTMT"
                  ),
                  plotlyOutput("survival_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Risk Factors",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("risk_factors")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load data
  dig_data <- reactive({
    read.csv("DIG.csv")
  })
  
  # Update select inputs based on data
  observe({
    nums <- names(select_if(dig_data(), is.numeric))
    updateSelectInput(session, "var1", choices = nums)
    updateSelectInput(session, "var2", choices = nums)
  })
  
  # Overview outputs
  output$total_patients <- renderValueBox({
    valueBox(
      nrow(dig_data()),
      "Total Patients",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$death_rate <- renderValueBox({
    death_rate <- mean(dig_data()$DEATH) * 100
    valueBox(
      paste0(round(death_rate, 1), "%"),
      "Mortality Rate",
      icon = icon("percent"),
      color = "red"
    )
  })
  
  output$avg_age <- renderValueBox({
    valueBox(
      round(mean(dig_data()$AGE), 1),
      "Average Age",
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$avg_ef <- renderValueBox({
    valueBox(
      round(mean(dig_data()$EJF_PER), 1),
      "Average EF%",
      icon = icon("heart"),
      color = "purple"
    )
  })
  
  # Age histogram
  output$age_hist <- renderPlotly({
    plot_ly(data = dig_data(), x = ~AGE, type = "histogram",
            marker = list(color = "rgba(56, 128, 139, 0.7)",
                          line = list(color = "rgba(56, 128, 139, 1)",
                                      width = 1))) %>%
      layout(title = "Age Distribution",
             xaxis = list(title = "Age"),
             yaxis = list(title = "Count"))
  })
  
  # EF histogram
  output$ef_hist <- renderPlotly({
    plot_ly(data = dig_data(), x = ~EJF_PER, type = "histogram",
            marker = list(color = "rgba(139, 56, 56, 0.7)",
                          line = list(color = "rgba(139, 56, 56, 1)",
                                      width = 1))) %>%
      layout(title = "Ejection Fraction Distribution",
             xaxis = list(title = "Ejection Fraction (%)"),
             yaxis = list(title = "Count"))
  })
  
  # Filtered data
  filtered_data <- reactive({
    data <- dig_data()
    data <- data[data$AGE >= input$age_range[1] & data$AGE <= input$age_range[2], ]
    data <- data[data$EJF_PER >= input$ef_range[1] & data$EJF_PER <= input$ef_range[2], ]
    
    if (input$death != "All") {
      data <- data[data$DEATH == (input$death == "Deceased"), ]
    }
    data
  })
  
  # Patient table
  output$patient_table <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 10,
                             scrollX = TRUE))
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlot({
    nums <- select_if(dig_data(), is.numeric)
    correlation <- cor(nums)
    corrplot(correlation, method = "color", type = "upper",
             tl.col = "black", tl.srt = 45)
  })
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    plot_ly(data = dig_data(),
            x = as.formula(paste0("~", input$var1)),
            y = as.formula(paste0("~", input$var2)),
            type = "scatter", mode = "markers",
            marker = list(color = "rgba(56, 128, 139, 0.5)")) %>%
      layout(title = paste(input$var1, "vs", input$var2),
             xaxis = list(title = input$var1),
             yaxis = list(title = input$var2))
  })
  
  # Survival plot
  output$survival_plot <- renderPlotly({
    # Create Kaplan-Meier plot using plotly
    data <- dig_data()
    group_var <- input$group_var
    
    # Simple survival curve by group
    plot_ly() %>%
      add_trace(data = data[data[[group_var]] == unique(data[[group_var]])[1], ],
                x = ~FOLLOWUP, y = ~cumsum(!DEATH)/sum(!DEATH),
                name = paste(group_var, "Group 1"),
                type = "scatter", mode = "lines") %>%
      add_trace(data = data[data[[group_var]] == unique(data[[group_var]])[2], ],
                x = ~FOLLOWUP, y = ~cumsum(!DEATH)/sum(!DEATH),
                name = paste(group_var, "Group 2"),
                type = "scatter", mode = "lines") %>%
      layout(title = paste("Survival Analysis by", group_var),
             xaxis = list(title = "Follow-up Time (days)"),
             yaxis = list(title = "Survival Probability"))
  })
  
  # Risk factors plot
  output$risk_factors <- renderPlotly({
    data <- dig_data()
    risk_factors <- data.frame(
      Factor = c("Diabetes", "Previous MI", "Treatment"),
      Death_Rate = c(
        mean(data$DEATH[data$DIABETES == 1]),
        mean(data$DEATH[data$PREVMI == 1]),
        mean(data$DEATH[data$TRTMT == 1])
      )
    )
    
    plot_ly(data = risk_factors, x = ~Factor, y = ~Death_Rate,
            type = "bar",
            marker = list(color = "rgba(56, 128, 139, 0.7)")) %>%
      layout(title = "Death Rate by Risk Factor",
             xaxis = list(title = "Risk Factor"),
             yaxis = list(title = "Death Rate"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)