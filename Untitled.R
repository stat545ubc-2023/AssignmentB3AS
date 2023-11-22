# Load required libraries
library(datateachr)
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)

# Load cancer_sample data
cancer_sample <- datateachr::cancer_sample

# Define UI for application
ui <- fluidPage(
  
  titlePanel("Cancer_Sample Browser"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("variable", "Select Variable:",
                  choices = names(cancer_sample)[3:6],
                  selected = "radius_mean"),
      
      sliderInput("transparency", "Transparency:",
                  min = 0, max = 1, value = 0.7),
      
      selectInput("diagnosis", "Select Diagnosis:",
                  choices = unique(cancer_sample$diagnosis),
                  selected = unique(cancer_sample$diagnosis)[1]
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Density Plot",
                 plotOutput("density_plot"),
                 h4("Total Number of Diagnosis:"),
                 textOutput("total_results"),
                 
                 downloadButton("downloadDensity", "Download Density Plot")),
        
        tabPanel("Summary Table",
                 h4("Summary Table"),
                 verbatimTextOutput("summary")),
        
        tabPanel("Data Table",
                 h4("Selected Species Data Table"),
                 DTOutput("data_table"),
                 downloadButton("downloadTable", "Download Data Table"))
      )
    )
  )
)

server <- function(input, output) {
  
  selected_data <- reactive({
    cancer_sample %>%
      filter(diagnosis == input$diagnosis)
  })
  
  output$density_plot <- renderPlot({
    ggplot(selected_data(), aes(x = !!sym(input$variable))) +
      geom_density(fill = 'green', alpha = input$transparency) +
      labs(x = input$variable,
           title = paste('Density Plot of', input$variable, 'for', input$diagnosis)) +
      theme_bw() +
      expand_limits(x = c(0, max(selected_data() %>% pull(!!sym(input$variable)), na.rm = TRUE)))  # Adjust x-axis limits
  })
  
  output$total_results <- renderText({
    paste(nrow(selected_data()), "diagnoses")
  })
  
  output$summary <- renderPrint({
    summary(selected_data() %>% pull(!!sym(input$variable)))
  })
  
  output$data_table <- renderDT({
    datatable(selected_data(), options = list(pageLength = 10))
  })
  
  output$downloadDensity <- downloadHandler(
    filename = function() {
      paste('density_plot_', input$variable, '_', input$diagnosis, '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = {
        ggplot(selected_data(), aes(x = !!sym(input$variable))) +
          geom_density(fill = 'green', alpha = input$transparency) +
          labs(x = input$variable,
               title = paste('Density Plot of', input$variable, 'for', input$diagnosis)) +
          theme_bw() +
          expand_limits(x = c(0, max(selected_data() %>% pull(!!sym(input$variable)), na.rm = TRUE)))  # Adjust x-axis limits
      }, device = "png", width = 10, height = 8)
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste('datatable_', input$diagnosis, '.csv', sep = '')
    },
    content = function(file) {
      write.csv(selected_data(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
