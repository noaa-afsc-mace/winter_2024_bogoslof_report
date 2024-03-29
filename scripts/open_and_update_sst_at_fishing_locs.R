# open the historic record of sea surface temperatures AT FISHING LOCATIONS (as opposed to flow-through SST) and
# append the current year values (or update the current year values if user is updating the data).
open_and_update_sst_at_fishing_locs <- function(ship, survey, data_set_id, analysis_id, historical_sst_loc) {
  # open up the historical collection
  fishing_sst <- readRDS(historical_sst_loc)

  # check- does this year already exist in the historical selectivity-corrected data?
  if (survey %in% fishing_sst$cruise) {
    # get rid of this year- we'll update to be safe; this will be needed in cases where
    # a more recent analysis has changed values
    fishing_sst <- fishing_sst %>%
      filter(cruise != survey)
  }

  # add the new data to the historical data- either after you've cleaned out the current entry for the year,
  # in which case it will update, or for entering the historical data for the first time
  
  # this data comes is gathered already by the event_data and haul_table_data dataframes, so re-query those
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  event_data <- event_and_interval_data[[2]]
  haul_table_data <- get_haul_table_data(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  
  # compile those
  new_sst_vals <- left_join(event_data, haul_table_data,
    by = c("EVENT_ID", "EQ_LATITUDE", "EQ_LONGITUDE", "DURATION_MINS", "region")
  ) %>%
    filter(SURVEY == survey)

  # grab the values we'll need
  new_sst_to_join <- data.frame(
    new_sst_vals$SURVEY, new_sst_vals$year, new_sst_vals$region, "SBE",
    new_sst_vals$EVENT_ID, new_sst_vals$EQ_LATITUDE, new_sst_vals$EQ_LONGITUDE,
    new_sst_vals$SURFACE_TEMP
  )

  colnames(new_sst_to_join) <- colnames(fishing_sst)

  # add these to the dataframe
  fishing_sst <- bind_rows(fishing_sst, new_sst_to_join)

  # append the most up-top-date info to to the historical csv
  saveRDS(fishing_sst, file = historical_sst_loc)
}
