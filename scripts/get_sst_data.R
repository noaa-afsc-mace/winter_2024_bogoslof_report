# process the sea surface temperature data, return summary dataframes and dataset

get_sst_data <- function(ship, survey, data_set_id, analysis_id, historical_scs_file_loc, apply_cal = TRUE) {
  # query the sensor data
  scs_data_query <- paste0(
    "SELECT ship, survey, time_stamp, latitude, longitude, measurement_type, measurement_value AS temp_c ",
    "FROM clamsbase2.underway_data ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    paste("AND measurement_type IN (", paste0("\'", primary_sensor, "\'"), ",", paste0("\'", backup_sensor, "\'"), ") ")
  )

  scs_data <- dbGetQuery(db, scs_data_query)

  # reshape data so each sensor is in its own column
  scs_data <- scs_data %>%
    pivot_wider(names_from = MEASUREMENT_TYPE, values_from = TEMP_C)

  # to be safe, ensure the date is in UTC
  lubridate::tz(scs_data$TIME_STAMP) <- "UTC"
  
  # grab the intervals data
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  interval_data <- event_and_interval_data[[1]]
  
  # in winter 2024, the SBE was not working reliably, and we decided to only use the backup sensor ("Mid-SeaTemp-C-Cal-VALUE_1_min_avg")
  # as the primary. So, in this case, we are just forcing it to be all from this source. Otherwise, we'll go to step 2 below as usual.
  if (survey %in% c(202401,202402,202403)){
    
    # set all the SBE data to be equal to the primary sensor so will be ignored in the below corrections!
    scs_data$`TSG-SBE38-Temp_1_min_avg` <- scs_data$`Mid-SeaTemp-C-Cal-VALUE_1_min_avg`
    
  }
  
  ###
  # 2 get the primary sensor reading whenever possible, secondary sensor when its not available; apply a correction to
  # the less accurate secondary sensor

  # the primary sensor may not always work (for example, SBE38 gets turned off when there's bad weather or its icy)
  # this correction relies on the fact that the current secondary furuno sensor consistently measures a greater temp
  # than the more accurate SBE38 sensor which is the current primary sensor

  # occasionally, the primary sensor records really high temps that aren't correct- due to strange issues including
  # the room with sensor getting really hot. We're dealing with this by getting rid of values more than 0.5 deg greater
  # than the secondary sensor (it should be lower if working)
  scs_data[[primary_sensor]] <- ifelse(scs_data[[primary_sensor]] - scs_data[[backup_sensor]] > 0.5,
    NA, scs_data[[primary_sensor]]
  )

  # if we aren't using a calibrated value for the backup sensor, make the calibration here
  if (isTRUE(apply_cal)){
    
    
    # calculate the mean difference (deg C) between sensors
    temp_offset <- scs_data[[backup_sensor]] - scs_data[[primary_sensor]]
    temp_offset <- mean(temp_offset, na.rm = TRUE)
    
    # if there's no value for the primary sensor, use the secondary sensor with the temp difference applied
    scs_data$temp <- ifelse(is.na(scs_data[[primary_sensor]]), scs_data[[backup_sensor]] - temp_offset, scs_data[[primary_sensor]])
    
  }
  
  if (!isTRUE(apply_cal)){
    
    # set the offset to zero so it clear that there isn't one applied
    temp_offset <- 0
    
    # if there's no value for the primary sensor, use the secondary sensor with NO temp difference applied
    scs_data$temp <- ifelse(is.na(scs_data[[primary_sensor]]), scs_data[[backup_sensor]], scs_data[[primary_sensor]])
    
  }
  
  # calculate the percentage of data from each sensor
  # get the total number of non-NA readings
  good_readings <- length(scs_data[[primary_sensor]][!is.na(scs_data[[primary_sensor]])])
  # and the total number of readings
  total_readings <- nrow(scs_data)
  # calculate the percent from the primary sensor
  percent_primary_sensor <- round((good_readings / total_readings) * 100, digits = 1)

  ###
  # 3 get the mean temperature for each interval in the dataset

  # do this using by taking all the records from the scs data between start and end times for each interval

  get_seatemps_within_interval_function <- function(interval_num, interval_start_time, interval_end_time) {
    # get the data within this time range
    temps_in_interval <- scs_data %>%
      filter(TIME_STAMP >= interval_start_time & TIME_STAMP <= interval_end_time) %>%
      # and append an interval number to every observation within interval
      mutate(interval = interval_num)

    # return dataframe
    return(temps_in_interval)
  }

  # apply function to get a dataframe that is just those seatemps within intervals
  seatemps_to_plot <- pmap_df(
    list(interval_data$INTERVAL, interval_data$START_TIME, interval_data$END_TIME),
    get_seatemps_within_interval_function
  )

  # and get a mean plotting location as the center of the interval, and a mean temp within the interval
  surface_data <- seatemps_to_plot %>%
    # grab columns that have lat/lon- this should work as long as the sensor has 'lat' and 'lon' in column name
    select(interval, LATITUDE, LONGITUDE, temp) %>%
    group_by(interval) %>%
    summarize(lat = mean(LATITUDE), lon = mean(LONGITUDE), temperature = mean(temp)) %>%
    # add the temperature type as 'surface'
    mutate(
      temperature_type = "Surface",
      temperature_loc = "surface"
    ) %>%
    select(interval, lat, lon, temperature, temperature_type, temperature_loc)
  
  # add a few helpful indices to the surface data- report number, region name
  intervals_to_join <- interval_data %>%
    select(SURVEY, year, INTERVAL, REPORT_NUMBER, region)

  surface_data <- left_join(surface_data, intervals_to_join, by = c("interval" = 'INTERVAL'))
   
  # get some information for the caption- %of data from each sensor, temp correction
  sst_stats <- cbind.data.frame(
    survey, total_readings, percent_primary_sensor,
    round(temp_offset, digits = 3)
  )
  colnames(sst_stats) <- c(
    "SURVEY", "total_readings", "percent_primary_sensor",
    "temp_offset_C"
  )

  # return some summary stats for use in text- min, mean, max, n
  sst_summary <- surface_data %>%
    group_by(SURVEY, temperature_type) %>%
    summarize(
      mean_temp = mean(temperature),
      min_temp = min(temperature),
      max_temp = max(temperature),
      n_readings = n()
    )

  # make the hauls into sf objects; this uses WGS1984 as that's the standard for GPS data (and we're using GPS data here)
  surface_data <- st_as_sf(surface_data, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

  # project them as with the base map layers
  surface_data <- st_transform(surface_data, crs = 3338)
  

  # return the dataframe for plotting, and some summary stats
  return(list(surface_data, sst_stats, sst_summary))
}
