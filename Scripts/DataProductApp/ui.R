#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Heartbeat Estimator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("beats",
                        "Your age:",
                        min = 1,
                        max = 120,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Do you know how many times your heart has beaten so far??"),
            textOutput("beatsCalc"),
            h3("times!!"),
            h4("This simple application calculates the number of times a heart beats, considering an average of 60 beats per minute.")
            
        )
    )
))
