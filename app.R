#######
## Homework 8
#######



#load in needed packages

library(shiny)
library(tidyverse)



#set slider range to the range of color in dataset
min.depth <- min(diamonds$depth)
max.depth <- max(diamonds$depth)


# Set a vector of axis variables as characters
axis_vars <- names(diamonds)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gurl we gon look at some diamonds"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("depth.adjuster",
                     "Depth",
                     min = min.depth,
                     max = max.depth,
                     value = c(min.depth, max.depth)),
         selectInput(inputId = "xvar",
                     label = "X axis",
                     choices = axis_vars,
                     selected = "x"),
         
         selectInput(inputId = "yvar",
                     label = "Y axis",
                     choices = axis_vars,
                     selected = "y"),
         submitButton("Go!")
         
         
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("diamonds_depth_plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  depth_filt <- reactive({
    
    
    low.depth <- input$depth.adjuster[1]
    hi.depth <- input$depth.adjuster[2]
    
    depth_filt <- diamonds %>%
      filter(depth >= low.depth) %>%
      filter(depth <= hi.depth)
  })
  
   
   output$diamonds_depth_plot <- renderPlot({
     ggplot(depth_filt(), aes_string(x = input$xvar, y = input$yvar, color = "clarity")) +
       geom_point()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

