#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    titlePanel( div(column(width = 8, 
                            h2("JHU Coursera Data Science Specialization"), 
                            h2("Capstone Project - Word Prediction"), 
                            h4("by BlueBonobo - 2022")
                           ),
                    column(width = 2, tags$img(src = "jhu-logo-thumb.png")),
                    column(width = 2, tags$img(src = "coursera-logo-thumb.png"))),
                windowTitle="JHU Data Science Capstone"
    ),

     # Sidebar with a text input
    sidebarLayout(

      sidebarPanel(
        textInput("textInput",
                  "Type your text :"
                  )
      ),

          
      mainPanel(
        tabsetPanel(
        tabPanel("Prediction", htmlOutput("prediction"))
        ,tabPanel("Documentation", includeHTML("documentationTab.html"))
        )
      )

    )
))
