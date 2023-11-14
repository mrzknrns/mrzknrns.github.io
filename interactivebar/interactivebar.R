library(shiny)
library(ggplot2)
library(dplyr)
library(scales) # for currency formatting

# Define global data combining function to be used in server
combine_data <- function(data1990_path, data2022_path) {
  # Load the datasets
  data1990 <- read.csv(data1990_path)
  data2022 <- read.csv(data2022_path)
  
  # Add a 'Year' column to both datasets to identify them after combining
  data1990$Year <- as.numeric(substr(data1990$month, 1, 4))
  data2022$Year <- as.numeric(substr(data2022$month, 1, 4))
  
  # Combine the two datasets
  combinedData <- rbind(data1990, data2022)
  
  return(combinedData)
}

# Define UI
ui <- fluidPage(
  titlePanel("Resale Flat Prices Box Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("flatType", "Select Flat Type:", 
                  choices = c("3 ROOM", "4 ROOM", "5 ROOM", "EXECUTIVE"), # Specify the types you want to include
                  selected = "3 ROOM",
                  multiple = TRUE),
      checkboxGroupInput("year", "Select Year(s):",
                         choices = c('1990', '2022'),
                         selected = c('1990', '2022'))
    ),
    mainPanel(
      plotOutput("boxPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Call the global data combining function
  combinedData <- combine_data("www/ResaleFlatPrices1999.csv", "www/ResaleFlatPrices2022.csv")
  
  # Update the choices for selectInput based on the combined data
  updateSelectInput(session, "flatType", 
                    choices = unique(combinedData$flat_type),
                    selected = unique(combinedData$flat_type)[1])
  
  output$boxPlot <- renderPlot({
    # Filter data based on selected flat type and year(s)
    filteredData <- filter(combinedData, 
                           flat_type %in% input$flatType,
                           Year %in% as.numeric(input$year))
    
    # Create box plot
    ggplot(filteredData, aes(x = flat_type, y = resale_price, fill = as.factor(Year))) +
      geom_boxplot() +
      scale_y_continuous(labels = label_dollar()) + # Format the y axis as dollar
      theme_minimal() +
      labs(title = "Box Plot of Resale Prices", 
           y = "Resale Price", x = "Flat Type") +
      scale_fill_brewer(palette = "Set1", name = "Year") # Differentiate years by color
  })
}

# Run the App
shinyApp(ui, server)
