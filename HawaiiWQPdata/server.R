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
library(sf)

wqp_join <- readr::read_csv('data/wqp_join.csv')
wq_params <- unique(wqp_join$CharacteristicName)
# wq_summary <- readr::read_csv('HawaiiWQPdata/data/wqp_join_summary.csv')
# wqp_join_summary_byStation <- readr::read_csv('HawaiiWQPdata/data/wqp_summary_by_station.csv')
wqp_join_summary_byStation <- readr::read_csv('data/wqp_summary_by_station.csv')

coffee_sf <- sf::st_read('data/ag2020_Coffee.kml')
macnut_sf <- sf::st_read('data/ag2020_MacadamiaNuts.kml')
tropfruit_sf <- sf::st_read('data/ag2020_TropicalFruits.kml')
roads_sf <- sf::st_read('data/centerlines_haw.shp') %>% sf::st_transform(4326)

# hutypes are S = standard, F = frontal, W = water, C = closed
wbd_sf <- sf::st_read('data/WBDHU12.shp') %>%
  dplyr::filter(hutype %in% c("S", "F", "C")) %>% 
  dplyr::mutate(huc4 = stringr::str_sub(huc12, 1, 4)) %>%
  dplyr::filter(huc4 == '2001')

wbd_sf_merge <- sf::st_union(wbd_sf) %>% sf::st_as_sf()

nhd_sf <- sf::st_read('data/NHDFlowline.shp') %>%
  sf::st_crop(st_bbox(wbd_sf_merge)) %>%
  dplyr::filter(fcode %in% c(33600, 33601, 46003, 33601, 46006, 42801, 42803, 42807, 46000, 42809)) %>%
  dplyr::mutate(description = dplyr::case_when(fcode == 33600 ~ 'canal/ditch',
                                               fcode == 33601 ~ 'aqueduct',
                                               fcode == 46003 ~ 'intermittent stream',
                                               fcode == 46006 ~ 'perennial stream',
                                               fcode == 42801 ~ 'pipeline at or near surface',
                                               fcode == 42803 ~ 'underground pipeline',
                                               fcode == 42803 ~ 'underground pipeline',
                                               fcode == 46000 ~ 'stream or river',
                                               fcode == 42809 ~ 'penstock pipeline at or near surface',
                                               ))

# 56600 = coastline
# 33600 = canal/ditch
# 33601 = aqueduct
# 46003 = intermittent stream
# 46006 = perennial stream
# 55800 = artificial path
# 42801 = pipeline at or near surface
# 42803 = underground pipeline
# 42807 = underground pipeline
# 46000 = stream or river
# 33400  = connector 
# 42809 = penstock at or near surface

# nhd_sf %>%
#   leaflet() %>%
#   addTiles() %>%
#   addPolylines(color = 'cyan', opacity = 1, weight = 1, popup = ~glue('{gnis_name} ({description})'))
#   addPolygons(opacity = 1, color = 'blue', weight = 1, fillOpacity = 0.1, popup = ~glue::glue('{name} ({hutype}) {huc12}'))
# param_groups <- readr::read_csv('parameters.csv')
# param_groups
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
    
    wbd_sf %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'esri world imagery') %>%
      addTiles(group = 'OSM') %>%
      addPolygons(opacity = 1, color = 'blue', weight = 1, 
                  group = 'HUC 12s',
                  fillOpacity = 0.1, popup = ~glue::glue('{name} ({hutype}) {huc12}')) %>%
      addPolylines(data = nhd_sf, 
                   group = 'nhd flowlines',
                   color = 'cyan', opacity = 1, weight = 1, 
                   popup = ~glue('{gnis_name} ({description})')) %>%
      addPolylines(data = roads_sf, weight = 2, opacity = 1, color = 'gray', group = 'roads') %>%
      addPolygons(data = coffee_sf, fillOpacity = 0.5, opacity = 1, weight = 1, color = 'yellow', group = 'coffee2020') %>%
      addPolygons(data = macnut_sf, fillOpacity = 0.5, opacity = 1, weight = 1, color = 'orange', group = 'macnut2020') %>%
      addPolygons(data = tropfruit_sf, fillOpacity = 0.5, opacity = 1, weight = 1, color = 'green', group = 'fruit2020') %>%
      addLayersControl(baseGroups = c('esri world imagery', 'OSM'),
                       overlayGroups = c('coffee2020',
                                         'macnut2020', 'fruit2020', 'roads', 'HUC 12s', 'nhd flowlines'),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    summary_subset <- wqp_join_summary_byStation %>% 
      dplyr::filter(CharacteristicName %in% input$parameters) %>%
      dplyr::mutate(description = glue::glue("<b>{MonitoringLocationName}</b>:\n {CharacteristicName} was
                                             {category} in {n_samps} total samples\n
                                             between {min_year} and {max_year}"))
    
    my_pts <- summary_subset %>% 
      sf::st_as_sf(coords = c('Lon', 'Lat'), crs = 4269)
    
    leafletProxy("map") %>%
      clearGroup(group = 'sample locations') %>%
      addCircleMarkers(data = my_pts, 
                       color = ~pal(category), 
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0.75,
                       popup = ~description,
                       group = 'sample locations') %>%
        addLayersControl(baseGroups = c('esri world imagery', 'OSM'),
                         overlayGroups = c('sample locations', 'coffee2020',
                                           'macnut2020', 'fruit2020', 'roads', 'HUC 12s', 'nhd flowlines'),
                         options = layersControlOptions(collapsed = TRUE))
  })
  


}
