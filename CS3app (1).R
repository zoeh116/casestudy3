#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
# Define UI for application that draws a histogram

ui <- fluidPage(
    
    # Application title
    titlePanel("Predicting Real Estate Prices"),
    
    "The data analyzed consists of real estate data from King's County, California.  I chose to analyze real estate data because I was interested in seeing what factors contribute to the cost of a home.  I chose this particular data set because I thought the size would work well for my analysis and it contained more variables than other data sets that I had come across.", br(),
    "A regression model was chosen to analyze this real estate data.  A regression model was chosen because they are used to determine the relationship between a single numerical dependent variable from one or more indpendent variables.  In this case the dependent variable is home price.  By toggling through the graphs, the user can see which variables have an effect on the price of a home and which do not.  For example, if the user selects transaction date in the drop down menu, they wil see that over time the price of a home has been relatively the same.  For that reason, linear regression models were only developed for number of bedrooms, number of bathrooms, and the house size in square feet.  By adusting the value on the slider for each independent variable, the user can get a predicted price of the house based on a linear regression model for each variable.",
    br(), "I was very intersted to find that certain factors like house age and transaction date don't have an effect on the price of a home.  It is intuitive that the number of bedrooms, bathrooms, ane the size of a home would change the price, but it was interesting assigning a value to these attributes.",
    
    # Sidebar 
    fluidRow(
      
        column(3, wellPanel(
            selectInput(inputId = "xvar",
                        label = "Select Variable:",
                        choices = (c("Transaction Date" = "date", 
                                     "Number of Bedrooms" = "bedrooms",
                                     "Number of Bathrooms" = "baths",
                                     "Size (sq ft.)" = "size",
                                     "Lot Size (sq ft.)" = "lotsize",
                                     "Number of Floors" = "floors",
                                     "Year Built" = "built")),
                        selected = "baths"),
            
            sliderInput(inputId = "roomInput",
                        label = "Choose Number of Bedrooms",
                        min = 1, max = 10, value = 2),
            
            sliderInput(inputId = "numbath",
                        label = "Choose Number of Bathrooms",
                        min = 1, max = 6, value = 2),
            
            sliderInput(inputId = "housesize",
                        label = "Enter House Size",
                        min = 0, max = 7500, value = 1000)
        )),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            
            
            plotOutput(outputId = "plot"),
            textOutput(outputId = "header1"),
            tableOutput(outputId = "predPrice1"),
            textOutput(outputId = "header2"),
            tableOutput(outputId = "predPrice2"),
            textOutput(outputId = "header3"),
            tableOutput(outputId = "predPrice3")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$plot <- renderPlot({
      kc_house_data <- read.csv("kc_house_data.csv")  
      clean_data <- filter(kc_house_data, price<4000000, bedrooms<10, bathrooms<6)
        data <- switch(input$xvar, 
                       "date" = clean_data$date,
                       "bedrooms" = clean_data$bedrooms,
                       "baths" = clean_data$bathrooms,
                       "size" = clean_data$sqft_living,
                       "lotsize" = clean_data$sqft_lot,
                       "floors" = clean_data$floors,
                       "built" = clean_data$yr_built)
        
        ggplot(clean_data,aes(x = data, y = clean_data$price)) +
            geom_point(color="#3C3838", size = 2, shape = 23) + xlab('xvar') + ylab('Price of Home') + ggtitle('Real Estate Prices') + geom_smooth(method=lm, se=FALSE) 
        
    })
         
        
        output$header1 <- renderText("Predicted Price Based off Number of Bedrooms:") 
        output$predPrice1 <- renderTable({   
            121365*input$roomInput + 127186
            })
        
        output$header2 <- renderText("Predicted Price Based off Number of Bathrooms")
        output$predPrice2 <- renderTable({  
            Bathrooms <- 236445*input$numbath + 36802
        })
        
        output$header3 <- renderText("Predicted Price Based off Home Size")
        output$predPrice3 <- renderTable({  
            Size <- -17188 + 266.8*input$housesize
            
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
