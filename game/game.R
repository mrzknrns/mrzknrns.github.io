library(shiny)
library(dplyr)


data_2022 <- read.csv("www/ResaleFlatPrices2022.csv")


average_prices <- data_2022 %>%
  group_by(flat_type, town) %>%
  summarize(average_price = mean(resale_price, na.rm = TRUE))

# Define UI
ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      numericInput("income", "Your Annual Income in SGD$:", value = 3000),
      numericInput("spouse_income", "Spouse's Annual Income in SGD$:", value = 3000),
      selectInput("flat_type", "Preferred Flat Type:", choices = unique(data_2022$flat_type)),
      selectInput("town", "Preferred Town:", choices = unique(data_2022$town)),
      numericInput("interest_rate", "Annual Interest Rate (%):", value = 2.6),
      numericInput("loan_tenure", "Loan Tenure (Years):", value = 25, min = 1, max = 30),
      actionButton("calculate", "Calculate"),
      tags$hr(),  # Horizontal line to separate the inputs from the output
      div(style = "text-align: left;", textOutput("downpayment")), br(),
      div(style = "text-align: left;", textOutput("monthly_repayment"))
    ),
    mainPanel()
  )
)


# Define server logic
server <- function(input, output) {
  
  observeEvent(input$calculate, {
    # this basically filters the average price and their towns
    selected_price <- average_prices %>%
      filter(flat_type == input$flat_type, town == input$town) %>%
      .$average_price
    
    # multiply to consider inflation
    inflation_rate <- 0.0246  # 2.46% inflation rate
    future_price <- selected_price * ((1 + inflation_rate) ^ input$loan_tenure)
    
    # calculate downpayment and loan based on the inflated price
    downpayment <- future_price * 0.2
    loan_principal <- future_price * 0.8
    
    # just show the downpayment
    output$downpayment <- renderText({
      paste("Minimum downpayment for a", input$flat_type, "in", input$town, 
            "with an expected inflation rate is $",round(downpayment, 2))
    })
    
    # this helps to calculate the monthly payment
    r <- input$interest_rate / 100 / 12  
    n <- input$loan_tenure * 12  
    monthly_repayment <- loan_principal * (r * (1 + r)^n) / ((1 + r)^n - 1) #i just got the formula online ngl
    
    # how much expected to pay
    output$monthly_repayment <- renderText({
      paste("Estimated monthly repayment over", input$loan_tenure, 
            "years with an expected inflation rate of 2.46% is $",round(monthly_repayment, 2))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
