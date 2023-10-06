#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(leaflet)
library(glue)
library(shiny)
library(htmltools)

wqp_data <- readr::read_csv('data/wqp_join.csv')
wq_params <- unique(wqp_data$CharacteristicName)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Hawaii Island Water Quality Portal data"),

    # Sidebarwith 
    sidebarLayout(
      sidebarPanel(
        h5("663 Parameters in total from 197 Monitoring locations"),
        shiny::selectizeInput("parameters",
                              "Parameters:",
                              selected = "Atrazine",
                              choices = wq_params,
                              multiple = TRUE),
        br(),
        h5("Symbols in map:"),
        h5("red: detections in all samples"),
        h5("purple: variable detections"),
        h5("white: not detected")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(title = 'map', leafletOutput("map", height = 800)),
          tabPanel(title = 'data', dataTableOutput("table"))
        ))
    )
)
