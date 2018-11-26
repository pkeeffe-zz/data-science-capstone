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
  titlePanel("Data Science Capstone - Text Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("text", 
                 h3("Enter sentence here..."), 
                value = ""),
       actionButton("predict", "Predict next word")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h3("Here are your next word predictions..."), # title with HTML helper
        textOutput("predictedWord")
    )
  )
))
