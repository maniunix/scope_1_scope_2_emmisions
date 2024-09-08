library(shiny)
library(randomForest)
library(DT)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(caret)
library(dplyr)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Emissions Estimation with Random Forest"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV", accept = c(".csv")),
      pickerInput("y_var", "Select Dependent Variable", choices = NULL),
      pickerInput("x_vars", "Select Features", choices = NULL, multiple = TRUE),
      
      actionButton("train_model", "Train Model"),
      downloadButton("download_report", "Download Report")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Exploration", 
                 DT::dataTableOutput("data_table")),
        tabPanel("Data Visualization", 
                 plotOutput("scatter_plot"),
                 plotOutput("correlation_plot")),
        tabPanel("Model Performance", 
                 verbatimTextOutput("model_summary"),
                 plotOutput("feature_importance"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive Data Import
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  # Update Feature and Target Choices Dynamically
  observe({
    req(data())
    updatePickerInput(session, "y_var", choices = names(data()))
    updatePickerInput(session, "x_vars", choices = names(data()))
  })
  
  # Display Data Table
  output$data_table <- DT::renderDataTable({
    req(data())
    DT::datatable(data())
  })
  
  # Scatter Plot (First two features)
  output$scatter_plot <- renderPlot({
    req(data(), input$x_vars)
    ggplot(data(), aes_string(x = input$x_vars[1], y = input$y_var)) + 
      geom_point() + 
      theme_minimal()
  })
  
  # Correlation Plot
  output$correlation_plot <- renderPlot({
    req(data(), input$x_vars)
    corr <- cor(data()[, input$x_vars])
    ggplot(data = as.data.frame(as.table(corr)), aes(Var1, Var2, fill = Freq)) + 
      geom_tile() + 
      scale_fill_gradient2() + 
      theme_minimal()
  })
  
  # Train Model
  model <- eventReactive(input$train_model, {
    req(data(), input$y_var, input$x_vars)
    
    formula <- as.formula(paste(input$y_var, "~", paste(input$x_vars, collapse = "+")))
    
    randomForest(formula, data = data(), importance = TRUE)
  })
  
  # Display Model Summary
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  # Feature Importance Plot
  output$feature_importance <- renderPlot({
    req(model())
    varImpPlot(model(), main = "Feature Importance")
  })
  
  # Download Report
  output$download_report <- downloadHandler(
    filename = function() { paste("model_report", Sys.Date(), ".txt", sep="") },
    content = function(file) {
      req(model())
      summary <- capture.output(summary(model()))
      write(summary, file)
    }
  )
}


shinyApp(ui = ui, server = server)
