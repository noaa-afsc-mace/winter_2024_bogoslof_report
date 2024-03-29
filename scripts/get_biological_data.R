# this function returns lengths/weights/ages, and labels each haul with it's survey region

get_biological_data <- function(ships, surveys, data_sets, analyses, species_code) {
  # apply function to get length/weights for the entire timeseries
  length_weight_data <- pmap_dfr(list(ships, surveys, species_code), get_survey_weights_lengths)


  # get the historical survey event data so that we can assign hauls to regions in these surveys
  get_historical_event_data <- pmap(
    list(ship = ships, survey = surveys, data_set_id = data_sets, analysis_id = analyses),
    get_event_and_interval_data_function
  )

  # pull out the hauls from this list
  historical_event_data <- c()
  for (i in 1:length(get_historical_event_data)) {
    # pull out the event dataframe
    event_data_tmp <- get_historical_event_data[[i]][[2]]

    # add it to the list
    historical_event_data <- bind_rows(historical_event_data, event_data_tmp)
  }

  # get a limited version of all the event data
  historical_event_data_summary <- historical_event_data %>%
    st_drop_geometry() %>%
    select(SURVEY, year, EVENT_ID, REPORT_NUMBER, region)

  # add the region information the biological data
  length_weight_data <- length_weight_data %>%
    left_join(historical_event_data_summary, by = c("SURVEY", "HAUL" = "EVENT_ID"))

  # return this dateframe
  return(length_weight_data)
}
