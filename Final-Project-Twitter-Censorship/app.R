#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(giphyr)

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Government Content Censorship on Twitter",
  
  
  tabPanel("About", 
           includeHTML("about.html")),
  tabPanel("Data",
           includeHTML("data.html")),
  
  tabPanel("Model",
           fluidPage(
             titlePanel("Content Removal Requests - an Overview"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "plot_type",
                   "Plot Type",
                   c("Total Requests Specified for Withdrawal  - All Countries" = "a", "Total Content Withheld - All Countries" = "b")
                 )),
               mainPanel(plotOutput("line_plot")))
           ))
  
)



server <- function(input, output) {
  output$line_plot <- renderImage({
    # Generate type based on input$plot_type from ui
    
    ifelse(
      input$plot_type == "a",
      
      # If input$plot_type is "a", plot histogram of "waiting" column 
      # from the faithful dataframe
      
      list(src = "Plot4.jpg",
           contentType = 'image/jpg',
           width = 1000,
           height = 700),
      
      # If input$plot_type is "b", plot histogram of "eruptions" column
      # from the faithful dataframe
      
      list(src = "Plot5.jpg",
           contentType = 'image/jpg',
           width = 1000,
           height = 700)
    )
    
    # Draw the histogram with the specified number of bins
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
