library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Find Your Dream HDB Flat"),
  sidebarLayout(
    sidebarPanel(
      numericInput("income", "Your Monthly Income:", value = 4000),
      numericInput("spouse_income", "Spouse's Monthly Income:", value = 4000),
      actionButton("calculate", "Calculate Best Option")
    ),
    mainPanel(
      textOutput("recommendation")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$calculate, {
    # Here you would have the logic to calculate the combined income
    # and determine which flat types are affordable, then make a recommendation.
    combined_income <- input$income + input$spouse_income
    
    # Stub for recommendation logic
    recommendation <- ifelse(combined_income > 10000, "5-Room or Executive", 
                             ifelse(combined_income > 7000, "4-Room", "3-Room"))
    
    # Output the recommendation
    output$recommendation <- renderText({
      paste("Based on your combined monthly income of $", combined_income,
            ", we recommend considering a ", recommendation, " HDB flat.", sep = "")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

