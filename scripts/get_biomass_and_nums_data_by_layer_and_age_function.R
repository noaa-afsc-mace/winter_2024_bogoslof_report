# query the biomass and numbers for by survey, dataset, analysis_id, etc, and add locations to each observation
# this script will report biomass/biomass_nm2 and numbers/numbers_nm2 within each interval and layer! This is a lot 
# slower than summing vertically but is appropriate for the bottom referenced data sets; for surface reference, we don't need per-layer 
# see get_surface_ref_biomass_and_nums_data_function for the faster version


get_biomass_and_nums_data_by_layer_and_age_function <- function(ship, survey, data_set_id, analysis_id, zones_list) {
  # check if the dataset and analysis ids are 0! Sometimes these get put in as placeholders, no data is available
  if (data_set_id != 0 & analysis_id != 0) {
    # print(paste('grabbing biomass and nums for ship:', ship, ' survey:', survey, ' dataset:', data_set_id, ' analysis:', analysis_id))

    # get the biomass/nums
    biomass_nums_query <- paste0(
      "SELECT ship, survey, data_set_id, analysis_id, report_number, transect, interval,  ",
      "layer, species_code, age, numbers, biomass, numbers_nm2, biomass_nm2 ",
      "FROM macebase2.analysis_results_by_age ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      "AND data_set_id = ", data_set_id, " ",
      "AND analysis_id = ", analysis_id, " ",
      paste("AND zone IN (", paste(zones_list, collapse = ","), ") "),
      "ORDER BY interval, layer, age"
    )

    biomass_nums_data <- dbGetQuery(db, biomass_nums_query)

    # get all intervals in the survey, labelled by the spatial survey region they occured in
    intervals_data <- get_event_and_interval_data_function(ship, survey, data_set_id, analysis_id)

    # get the intervals dataframe from transect_data list
    intervals_data <- intervals_data[[1]]

    # first, get a smaller version of the interval data to speed things up a bit
    intervals_data_to_join <- intervals_data %>%
      st_drop_geometry() %>%
      select(SHIP, SURVEY, INTERVAL, TRANSECT, REPORT_NUMBER, region, management_area)

    # get the biomass for each interval/layer
    biomass_nums_data <- left_join(biomass_nums_data, intervals_data_to_join,
      by = c("SHIP", "SURVEY", "INTERVAL", "TRANSECT", "REPORT_NUMBER")
    )

    # get the layers definitions
    intervals_and_layers_query <- paste0(
      "SELECT d.survey, d.interval, d.mean_bottom_depth, ",
      "c.layer, c.range_from_reference_upper, c.range_from_reference_lower ",
      "FROM macebase2.layers c, macebase2.intervals d ",
      "WHERE d.ship = ", ship, " ",
      "AND d.survey = ", survey, " ",
      "AND d.data_set_id = ", data_set_id, " ",
      "and d.survey = c.survey  ",
      "and d.ship = c.ship  ",
      "and d.data_set_id = c.data_set_id "
    )

    intervals_and_layers_data <- dbGetQuery(db, intervals_and_layers_query)

    # calulate pollock depth within each interval as the mean of the bottom and top of the interval
    intervals_and_layers_data$depth <- (intervals_and_layers_data$RANGE_FROM_REFERENCE_LOWER + intervals_and_layers_data$RANGE_FROM_REFERENCE_UPPER) / 2

    # add the depth info to the biomass data
    biomass_nums_data <- left_join(biomass_nums_data, intervals_and_layers_data, by = c("SURVEY", "INTERVAL", "LAYER"))
    
    # add a year index
    biomass_nums_data$year <- as.numeric(substr(biomass_nums_data$SURVEY, 1, 4))

    # return the big dataframe
    return(biomass_nums_data)
  }
}
