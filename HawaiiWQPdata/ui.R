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
# param_groups <- readr::read_csv('HawaiiWQPdata/data/parameters.csv')
param_groups <- readr::read_csv('data/parameters.csv')

param_list <- param_groups %>% 
  dplyr::mutate(CommonName = CharacteristicName) %>%
  dplyr::select(CharacteristicName, category, CommonName) %>%
  group_by(category) %>%
  dplyr::group_split()

param_categories <- param_list %>% purrr::map_chr(~unique(.x[['category']]))
names(param_list) <- param_categories
# param_categories

params_choices <- param_list %>% 
  purrr::map(~dplyr::select(.x, CommonName, CharacteristicName)) %>%
  purrr::map(~tibble::deframe(.x))
# names(params_choices)
# params_choices$PPCPs

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Hawaii Island Water Quality Portal data"),

    # Sidebarwith 
    sidebarLayout(
      sidebarPanel(
        h5("663 Parameters tested for from 197 Monitoring locations"),
        h5("158 Parameters detected across 194 Monitoring locations"),
        h5('Map shows detections and data tab is all data for selected parameter(s)'),
        shiny::selectizeInput("parameters",
                              "Parameters:",
                              selected = "p,p'-DDT",
                              choices = list(
                                Pharmaceuticals = params_choices$PPCPs,
                                PAHs = params_choices$PAH,
                                Industrial = params_choices$industrial,
                                Pesticides = params_choices$pesticide,
                                Metals = params_choices$metals,
                                PCBs = params_choices$PCBs,
                                plasticiser = params_choices$plasticiser,
                                other = params_choices$other,
                                NotDectected = params_choices$`NA, non-detect`
                              ),
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
