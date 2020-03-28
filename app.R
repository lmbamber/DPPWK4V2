#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
my_cars<- mtcars %>%
    select(-cyl, -vs,-am)
rownames(my_cars) <- NULL
my2_cars<- my_cars%>%
    select(-mpg)

My_Variable<- colnames(my2_cars)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Fuel Consumption Analyser"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=4,
            selectInput("var", 
                        label = "Choose a variable to analyse against fuel consumption(mpg)",
                        choices = My_Variable)
        ),
        
            # Input: Slider for the number of observations to generate ----
            sliderInput("val",
                    "Select a value for the variable:",
                    value = 1,
                    min = 0,
                    max = 5)
        
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Visualisation", plotOutput("Plot1")),
                    tabPanel("Model Summary", verbatimTextOutput("summary")))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Plot1 <- renderPlot({
        my_variable<- input$var
        my_cars_r <- my_cars %>%
                select(my_variable, mpg) %>%
                rename(variable= my_variable)
                
                # draw the histogram with the specified number of bins
        ggplot(my_cars_r, aes(variable, mpg, Group=1)) +
            geom_point()+
            geom_smooth(method=lm, se=FALSE)+
            xlab(my_variable)
    })
    
    output$summary <- renderPrint({
        my_variable<- input$var
        my_cars_r <- my_cars %>%
            select(my_variable, mpg) %>%
            rename(variable= my_variable)
            
            linear_model <- lm(mpg~variable, data=my_cars_r)
        summary(linear_model)
    })
    
    output$result <- renderText({
        # Renders the text for the prediction below the graph
        my_variable<- input$var
        my_value<- input$val
        my_cars_r <- my_cars %>%
            select(my_variable, mpg) %>%
            rename(variable= my_variable)
        
        linear_model <- lm(mpg~variable, data=my_cars_r)
        pred <- predict(linear_model, newdata = data.frame(variable = input$val))
        res <- paste(round(pred, digits = 1.5),"$" )
        res
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
