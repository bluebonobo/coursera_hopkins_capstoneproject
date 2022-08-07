#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

source("helpers.R")

shinyServer(function(input, output) {

    output$prediction <- renderText({
       paste(input$textInput,"<font size=24 color=\"#53B993\"><b>", predict(input$textInput), "</b></font>")
    })
 

})
