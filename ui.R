

library(markdown)
library(shiny)
library(rCharts)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "md5.js"),
    tags$script(type="text/javascript", src = "passwdInputBinding.js")
  ),
  useShinyjs(),
  
  titlePanel("WFM Business Performance - Password Protected Shiny app"),
  
  uiOutput("app")
))


