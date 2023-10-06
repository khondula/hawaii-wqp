
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
  dplyr::group_by(MonitoringLocationIdentifier,
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
