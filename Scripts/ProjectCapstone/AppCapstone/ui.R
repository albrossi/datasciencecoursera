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
    titlePanel("Word Prediction App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput(inputId="text1", label = "Please enter your text here: "),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h1("Lets try to predict your next word!"),
            textOutput("displayPredictWord"),
            h2("Words presence in our database sample:"),
            h2("Frequency/Count: "),
            textOutput("displayPresence"),
            h2("Percentual/Total:"),
            textOutput("displayPercent"),
        )
    )
))
