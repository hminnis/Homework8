#######
## Homework 8
#######



#load in needed packages

library(shiny)
library(tidyverse)



#set slider range to the range of color in dataset
min.depth <- min(diamonds$depth)
max.depth <- max(diamonds$depth)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Diamonds Data Set Viewer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("depth.adjuster",
                     "Depth",
                     min = min.depth,
                     max = max.depth,
                     value = c(min.depth, max.depth)),
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
      filter(color >= low.depth) %>%
      filter(color <= hi.depth)
  })
  
   
   output$diamonds_depth_plot <- renderPlot({
     ggplot(depth_filt(), aes_string(x = "depth", y = "carat", color = "clarity")) +
       geom_point()
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

