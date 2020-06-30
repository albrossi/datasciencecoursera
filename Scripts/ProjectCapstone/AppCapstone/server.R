#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("week1.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$displayPredictWord <- renderText({
        words <- ""
        if (!input$text1 == ""){
            words <- predictNextWord(input$text1)
            words[5] <- words[3]
            words[4] <- "   |   "
            words[3] <- words[2]
            words[2] <- "   |   "
        }
        return (words)
    })

    output$displayPresence <- renderText({
        presence <- ""
        if (!input$text1 == ""){
            presence <- wordPresence(input$text1)
            return (format(presence$freq, format="d", big.mark=','))
        } else {
            return (presence)
        }
    })
    
    output$displayPercent <- renderText({
        percent <- ""
        if (!input$text1 == ""){
            percent <- wordPresence(input$text1)
            return (paste0(round(percent$percent, 2), "%"))
        } else {
            return (percent)
        }
    })
    
})
