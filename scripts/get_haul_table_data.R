# get all the data needed for the haul tables- do this on a survey-wide basis

get_haul_table_data <- function(ship, survey, data_set_id, analysis_id) {
  # get the query for the table- this will have all the info we need for the table we'll format below
  haul_query <- paste0(
    "SELECT event_id, ",
    "gear, eq_time, hb_time, duration_mins, eq_latitude, eq_longitude, ",
    "head_rope_depth, bottom_depth, head_rope_temp, surface_temp, pollock_weight, ",
    "pollock_numbers, non_pollock_weight ",
    "FROM clamsbase2.v_event_and_catch_summary ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "ORDER BY event_id ASC"
  )

  # and get the data
  haul_data <- dbGetQuery(db, haul_query)
  
  # fetch the event data
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  event_data <- event_and_interval_data[[2]]
  
  # get the name of the survey region from the event data
  region_name <- event_data %>%    
    filter(SURVEY == survey) %>%
    # keep only the ship, survey, event_id, and the 'nice' region name
    st_drop_geometry() %>%
    select(EVENT_ID, region)

  # join the region name with the haul data
  haul_data <- left_join(haul_data, region_name, by = c("EVENT_ID"))

  # force the timezone on hauls to be safe
  haul_data$EQ_TIME <- force_tz(haul_data$EQ_TIME, tzone = "UTC")

  # return dataframe
  return(haul_data)
}
