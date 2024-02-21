library(shiny)
library(ggplot2)

# Load the CSV data
csv_data <- read.csv("C:/Users/divya/OneDrive/Desktop/MBA/3rd Trimester MBA/RTSM/RS_Session_255_AU_2113_1.csv")

# UI
ui <- fluidPage(
  titlePanel("State/UTs-wise AWS and ARG Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Select Variable", choices = names(csv_data)),
      numericInput("min_value", "Min Value:", value = min(csv_data$variable), min = min(csv_data$variable), max = max(csv_data$variable)),
      numericInput("max_value", "Max Value:", value = max(csv_data$variable), min = min(csv_data$variable), max = max(csv_data$variable)),
    ),
    mainPanel(
      tableOutput("summary_table"),
      plotOutput("mean_plot"),
      plotOutput("median_plot"),
      plotOutput("min_max_plot"),
      plotOutput("histogram_plot"),
      plotOutput("regression_plot")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    variable <- input$variable
    min_value <- input$min_value
    max_value <- input$max_value
    
    filtered_data <- subset(csv_data, csv_data[[variable]] >= min_value & csv_data[[variable]] <= max_value)
    return(filtered_data)
  })
  
  # Display descriptive statistics table
  output$summary_table <- renderTable({
    summary_stats <- summary(filtered_data())
    return(summary_stats)
  })
  
  # Mean Plot
  output$mean_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = .data[[input$variable]])) +
      geom_density(fill = "blue", alpha = 0.7) +
      geom_vline(aes(xintercept = mean(.data[[input$variable]])), color = "red", linetype = "dashed", size = 1) +
      labs(title = "Mean Plot")
  })
  
  # Median Plot
  output$median_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = .data[[input$variable]])) +
      geom_density(fill = "green", alpha = 0.7) +
      geom_vline(aes(xintercept = median(.data[[input$variable]])), color = "blue", linetype = "dashed", size = 1) +
      labs(title = "Median Plot")
  })
  
  # Min-Max Plot
  output$min_max_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = .data[[input$variable]])) +
      geom_density(fill = "orange", alpha = 0.7) +
      geom_vline(aes(xintercept = min(.data[[input$variable]])), color = "purple", linetype = "dashed", size = 1) +
      geom_vline(aes(xintercept = max(.data[[input$variable]])), color = "purple", linetype = "dashed", size = 1) +
      labs(title = "Min-Max Plot")
  })
  
  # Histogram Plot
  output$histogram_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = .data[[input$variable]])) +
      geom_histogram(binwidth = 5, fill = "cyan", color = "black", alpha = 0.7) +
      labs(title = "Histogram")
  })
  
}

# Run the application
shinyApp(ui, server)

