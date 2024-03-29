# get the interval data for a given ship, survey, data_Set_id, and then assign each interval to a report number to get the region names

get_event_and_interval_data_function <- function(ship, survey, data_set_id, analysis_id) {
  ####
  # get the interval data- including report_numbers, regions, descriptions, interval widths

  # get the transect locations query
  interval_query <- paste0(
    "SELECT a.ship, a.survey, a.transect, a.interval, a.start_time, a.end_time, a.start_latitude, ",
    "a.start_longitude, a.end_latitude, a.end_longitude, a.end_vessel_log, a.length, ",
    "a.mean_bottom_depth, b.report_number, c.description, b.interval_width ",
    "FROM macebase2.intervals a LEFT JOIN macebase2.interval_analysis_map b ",
    "ON (a.ship = b.ship) AND (a.survey = b.survey) ",
    "AND (a.data_set_id = b.data_set_id) AND (a.interval = b.interval) ",
    "LEFT JOIN macebase2.report_definitions c ",
    "ON (b.ship = c.ship) AND (b.survey = c.survey) AND (b.data_set_id = c.data_set_id) ",
    "AND (b.analysis_id = c.analysis_id) AND (b.report_number = c.report_number) ",
    "WHERE a.ship = ", ship, " ",
    "AND a.survey = ", survey, " ",
    "AND a.data_set_id = ", data_set_id, " ",
    "AND b.analysis_id = ", analysis_id, " ",
    "ORDER BY a.interval "
  )

  # and fetch from database
  intervals_data <- dbGetQuery(db, interval_query)

  # make sure the time is formatted as UTC
  intervals_data$START_TIME <- force_tz(intervals_data$START_TIME, tzone = "UTC")
  intervals_data$END_TIME <- force_tz(intervals_data$END_TIME, tzone = "UTC")
  
  # in the deep past there are some latitudes = 90.0 degrees; don't include these problem intervals
  intervals_data <- intervals_data %>%
    filter(START_LATITUDE != 90)

  # get nice variable names for each report number (to use in plots + text);
  # this just applies the 'get_nice_region_names' file to each description- if you need other areas to
  # map to a nice name, edit function to have more terms- it is commented within function.
  intervals_data$region <- map2_chr(intervals_data$START_LONGITUDE, intervals_data$START_LATITUDE, get_nice_region_names)

  # add an index for year- this is based on survey naming conventions having the year in first 4 numbers of survey
  intervals_data$year <- as.numeric(substr(intervals_data$SURVEY, 1, 4))

  # define interval locations as the center of the interval when possible;
  # there are cases where we only have a start or end position; in these cases take whatever is available

  # in old surveys- there may not be an end latitude; in this case set as 999 so we know not to use this in calculating midpoints
  intervals_data$END_LATITUDE <- replace_na(intervals_data$END_LATITUDE, 999)
  intervals_data$END_LONGITUDE <- replace_na(intervals_data$END_LONGITUDE, 999)

  intervals_data$lat <- ifelse(!(intervals_data$START_LATITUDE == 999) & !(intervals_data$END_LATITUDE == 999),
    (intervals_data$START_LATITUDE + intervals_data$END_LATITUDE) / 2,
    ifelse(intervals_data$START_LATITUDE == 999, intervals_data$END_LATITUDE, intervals_data$START_LATITUDE)
  )

  intervals_data$lon <- ifelse(!(intervals_data$START_LONGITUDE == 999) & !(intervals_data$END_LONGITUDE == 999),
    (intervals_data$START_LONGITUDE + intervals_data$END_LONGITUDE) / 2,
    ifelse(intervals_data$START_LONGITUDE == 999, intervals_data$END_LONGITUDE, intervals_data$START_LONGITUDE)
  )


  # make the intervals into sf objects; this uses WGS1984 as that's the standard for GPS data (and we're using GPS data here)
  intervals_data <- st_as_sf(intervals_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  # project them as with the base map layers
  intervals_data <- st_transform(intervals_data, crs = 3338)

  # also add an index to identify which management region the interval is located in; use a spatial join to get the
  # information from the management regions polygons
  intervals_data <- intervals_data %>%
    st_join(management_regions, join = st_intersects) %>%
    # rename REP_AREA to something more descriptive
    rename(management_area = NMFS_AREA)

  ####
  # now that we've got the interval data, grab the events; we'll use the nearest interval to each event to assign events to a region
  # (i.e. if located next to a shelikof interval; the haul is a shelikof haul)

  # get the hauls data query
  event_query <- paste0(
    "SELECT ship, survey, event_id, gear, eq_latitude, eq_longitude, hb_latitude, hb_longitude, ",
    "duration_mins ",
    "FROM clamsbase2.v_event_and_catch_summary ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " "
  )

  # and fetch it from the database
  event_data <- dbGetQuery(db, event_query)

  # get rid of lats/longs that are '999.00'
  event_data <- event_data %>%
    filter(EQ_LATITUDE != 999) %>%
    # also rename GEAR to 'gear type' for prettier plotting
    rename("Gear type" = GEAR)

  # make the hauls into sf objects; this uses WGS1984 as that's the standard for GPS data (and we're using GPS data here)
  event_data <- st_as_sf(event_data, coords = c("EQ_LONGITUDE", "EQ_LATITUDE"), crs = 4326, remove = FALSE)

  # project them as with the base map layers
  event_data <- st_transform(event_data, crs = 3338)

  # find the nearest interval for each haul, and assign the haul's location to the location of this interval
  # use sf function 'st_nearest_feature', which returns the row index in intervals_data that each haul is closest to
  event_data$nearest_interval_index <- st_nearest_feature(event_data, intervals_data)

  # now that we have the index for the nearest interval to each event, pull out report number and labels
  event_data$REPORT_NUMBER <- intervals_data$REPORT_NUMBER[event_data$nearest_interval_index]
  event_data$DESCRIPTION <- intervals_data$DESCRIPTION[event_data$nearest_interval_index]
  event_data$region <- intervals_data$region[event_data$nearest_interval_index]

  # add an index for year- this is based on survey naming conventions having the year in first 4 numbers of survey
  event_data$year <- as.numeric(substr(event_data$SURVEY, 1, 4))

  # return the dataframe
  return(list(intervals_data, event_data))
}
