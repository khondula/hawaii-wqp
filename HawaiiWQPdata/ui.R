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
param_groups <- readr::read_csv('data/parameters.csv')

param_list <- param_groups %>% 
  dplyr::mutate(CommonName = CharacteristicName) %>%
  dplyr::select(CharacteristicName, category, CommonName) %>%
  group_by(category) %>%
  dplyr::group_split()

param_categories <- param_list %>% purrr::map_chr(~unique(.x[['category']]))
names(param_list) <- param_categories

params_choices <- param_list %>% 
  purrr::map(~dplyr::select(.x, CommonName, CharacteristicName)) %>%
  purrr::map(~tibble::deframe(.x))

params_choices$Pharmaceuticals

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
                              selected = "Copper",
                              choices = list(
                                Pharmaceuticals = params_choices$Pharmaceuticals,
                                Herbicide = params_choices$Herbicide,
                                Metals = params_choices$Metals,
                                PCBs = params_choices$PCBs,
                                Insecticide = params_choices$insecticide,
                                uncategorized = params_choices$unsure
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
