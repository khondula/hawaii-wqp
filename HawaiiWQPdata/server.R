#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(glue)

wqp_join <- readr::read_csv('data/wqp_join.csv')
wq_params <- unique(wqp_join$CharacteristicName)
# wq_summary <- readr::read_csv('HawaiiWQPdata/data/wqp_join_summary.csv')
wqp_join_summary_byStation <- readr::read_csv('data/wqp_summary_by_station.csv')
  
# param_groups <- readr::read_csv('parameters.csv')

# test_subset <- wqp_join %>% dplyr::filter(CharacteristicName %in% "Atrazine")
# summary_subset <- wqp_join_summary_byStation %>% dplyr::filter(CharacteristicName %in% "Atrazine")
# 
# my_pts <- summary_subset %>% 
#   sf::st_as_sf(coords = c('Lon', 'Lat'), crs = 4269) 

pal <- colorFactor(c("red", "white", "purple"), 
                   domain = c("detect", "non detect", "variable"))

# my_pts %>%
#   leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery) %>%
#   addCircleMarkers(color = ~pal(category), 
#                    stroke = TRUE,
#                    opacity = 1,
#                    fillOpacity = 0.95,
#                    popup = ~MonitoringLocationIdentifier)


# Define server logic required to draw a histogram
function(input, output, session) {

  param_subset <- reactive({
    my_param <- dplyr::filter(wqp_join, CharacteristicName %in% input$parameters)
    my_param
    
  })
  
  output$table <- renderDataTable({
    param_subset()
  })
  
  output$map <- renderLeaflet({
    
    summary_subset <- wqp_join_summary_byStation %>% 
      dplyr::filter(CharacteristicName %in% input$parameters) %>%
      dplyr::mutate(description = glue::glue("<b>{MonitoringLocationName}</b>:\n {CharacteristicName} was
                                             {category} in {n_samps} total samples\n
                                             between {min_year} and {max_year}"))
    
    my_pts <- summary_subset %>% 
      sf::st_as_sf(coords = c('Lon', 'Lat'), crs = 4269)
    
    my_pts %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircleMarkers(color = ~pal(category), 
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0.75,
                       popup = ~description)
  })

}
