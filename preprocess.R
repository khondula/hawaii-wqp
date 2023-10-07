
# join site data with sample results

library(tidyverse)

stations <- readr::read_csv('HawaiiWQPdata/data/stations.csv')

stations <- stations %>% 
  dplyr::select(MonitoringLocationIdentifier,
                MonitoringLocationName,
                MonitoringLocationTypeName,
                HUCEightDigitCode,
                LatitudeMeasure,
                LongitudeMeasure,
                HorizontalCoordinateReferenceSystemDatumName,
                `VerticalMeasure/MeasureValue`,
                `VerticalMeasure/MeasureUnitCode`,
                `VerticalAccuracyMeasure/MeasureValue`,
                `VerticalAccuracyMeasure/MeasureUnitCode`,
                VerticalCollectionMethodName,
                VerticalCoordinateReferenceSystemDatumName)

wqp_data <- readr::read_csv('HawaiiWQPdata/data/results.csv')

wqp_data <- wqp_data %>% 
  dplyr::select(ActivityMediaName, 
                ActivityMediaSubdivisionName,
                ActivityStartDate,
                MonitoringLocationIdentifier,
                `SampleCollectionMethod/MethodName`,
                ResultDetectionConditionText,
                CharacteristicName,
                ResultSampleFractionText,
                ResultMeasureValue,
                `ResultMeasure/MeasureUnitCode`,
                `ResultAnalyticalMethod/MethodName`,
                `DetectionQuantitationLimitMeasure/MeasureValue`,
                `DetectionQuantitationLimitMeasure/MeasureUnitCode`)

wqp_join <- wqp_data %>% 
  dplyr::filter(!ResultDetectionConditionText %in% c("Not Reported")) %>%
  dplyr::left_join(stations, by = join_by(MonitoringLocationIdentifier))

wqp_join %>% readr::write_csv('HawaiiWQPdata/data/wqp_join.csv')

# summarize by year and detections

wqp_join_summary_byYear <- wqp_join %>% 
  dplyr::mutate(dateparse = lubridate::mdy(ActivityStartDate)) %>%
  dplyr::mutate(year = lubridate::year(dateparse)) %>%
  dplyr::mutate(detected = !ResultDetectionConditionText %in% c("Not Detected", "*Non-detect")) %>%
  dplyr::group_by( output$map <- renderLeaflet({
    
    summary_subset <- wqp_join_summary_byStation %>% 
      dplyr::filter(CharacteristicName %in% input$parameters) %>%
      dplyr::mutate(description = glue::glue("<b>{MonitoringLocationName}</b>:\n {CharacteristicName} was
                                             {category} in {n_samps} total samples\n
                                             between {min_year} and {max_year}"))
    
    my_pts <- summary_subset %>% 
      sf::st_as_sf(coords = c('Lon', 'Lat'), crs = 4269)
    
    my_pts %>%
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = 'esri world imagery') %>%
      addTiles(group = 'OSM') %>%
      addCircleMarkers(color = ~pal(category), 
                       stroke = TRUE,
                       opacity = 1,
                       fillOpacity = 0.75,
                       popup = ~description,
                       group = 'sample locations') %>%
      addLayersControl(baseGroups = c('esri world imagery', 'OSM'),
                       overlayGroups = 'sample locations',
                       options = layersControlOptions(collapsed = FALSE))
  }),
                  MonitoringLocationName,
                  CharacteristicName,
                  year, 
                  # ActivityMediaName,
                  detected) %>%
  dplyr::summarise(Lat = unique(LatitudeMeasure),
                   Lon = unique(LongitudeMeasure),
                   n_samps = n())

wqp_join_summary_byStation <- wqp_join_summary_byYear %>% 
  ungroup() %>%
  group_by(CharacteristicName, MonitoringLocationIdentifier, MonitoringLocationName, Lat, Lon) %>%
  dplyr::summarise(category = case_when(all(detected) ~ 'detect',
                                        all(!detected) ~ 'non detect',
                                        TRUE ~ 'variable'),
                   n_samps = sum(n_samps),
                   min_year = min(year),
                   max_year = max(year))

wqp_join_summary_byStation %>% readr::write_csv('HawaiiWQPdata/data/wqp_summary_by_station.csv')
wqp_join_summary %>% readr::write_csv('HawaiiWQPdata/data/wqp_join_summary.csv')
# 
# wqp_join_summary_byStation %>%
#   dplyr::select(CharacteristicName) %>%
#   distinct() %>%
#   readr::write_csv('parameters.csv')

# which chemicals are in any sites

wqp_join_summary_byStation %>%
  dplyr::select(CharacteristicName, category) %>%
  distinct() %>% dplyr::filter(category %in% c('variable', 'detect')) %>% arrange(CharacteristicName) %>% View()


wqp_join_summary_byStation %>%
  dplyr::select(MonitoringLocationName, Lat, Lon, MonitoringLocationIdentifier) %>%
  distinct() %>%
  sf::st_as_sf(coords = c("Lon", "Lat")) %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addMarkers(popup = ~MonitoringLocationName)
