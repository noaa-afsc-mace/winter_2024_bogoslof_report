# Gather all of the SBE data for the historical timeseries

# 1. get all the temp/depth data
get_sbe_by_survey <- function(ships, surveys, data_sets, analyses) {
  # get the SBE data for each survey
  get_survey_sbe <- function(ship, survey) {
    # query to get the temp/depth values by survey
    sbe_query <- paste0(
      "SELECT survey, event_id, time_stamp, depth, temperature ",
      "FROM clamsbase2.v_sbe_data ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      "ORDER BY survey, event_id, time_stamp, depth"
    )

    # get the SBE data
    sbe_data <- dbGetQuery(db, sbe_query)

    # return this dataframe
    return(sbe_data)
  }

  # apply function to get sbe data for requested ships/surveys
  all_sbe_data <- map2_dfr(ships, surveys, get_survey_sbe)

  # get the survey region for every haul- so we can use this to limit
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
    select(SURVEY, EVENT_ID, REPORT_NUMBER, region)

  # add the region information the SBE data data
  all_sbe_data <- all_sbe_data %>%
    left_join(historical_event_data_summary, by = c("SURVEY", "EVENT_ID"))

  # return this dataframe
  return(all_sbe_data)
}
