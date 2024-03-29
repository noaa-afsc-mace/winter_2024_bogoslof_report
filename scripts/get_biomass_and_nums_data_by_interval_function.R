# query the biomass and numbers for by survey, dataset, analysis_id, etc, and add locations to each observation
# this script will sum biomass/biomass_nm2 and numbers/numbers_nm2 vertically within each interval! This is a lot 
# faster and appropriate for the surface referenced data sets; for bottom reference, we need per-layer 
# see get_bottom_ref_biomass_and_nums_data_function

get_biomass_and_nums_data_by_interval_function <- function(ship, survey, data_set_id, analysis_id, zones_list) {
  # check if the dataset and analysis ids are 0! Sometimes these get put in as placeholders, no data is available
  if (data_set_id != 0 & analysis_id != 0) {
    # print(paste('grabbing biomass and nums for ship:', ship, ' survey:', survey, ' dataset:', data_set_id, ' analysis:', analysis_id))

    # get the biomass/nums - summed vertically within each interval
    biomass_nums_query <- paste0(
      "SELECT ship, survey, data_set_id, analysis_id, report_number, transect, interval,  ",
      "species_code, length, ",
      "sum(numbers) as numbers, sum(biomass) as biomass, sum(numbers_nm2) as numbers_nm2, sum(biomass_nm2) as biomass_nm2 ",
      "FROM macebase2.analysis_results_by_length ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      "AND data_set_id = ", data_set_id, " ",
      "AND analysis_id = ", analysis_id, " ",
      paste("AND zone IN (", paste(zones_list, collapse = ","), ") "),
      "GROUP BY ship, survey, data_set_id, analysis_id, report_number, transect, interval, species_code, length ",
      "ORDER BY report_number, transect"
    )

    biomass_nums_data <- dbGetQuery(db, biomass_nums_query)

    # get all intervals in the survey, labelled by the spatial survey region they occured in
    intervals_data <- get_event_and_interval_data_function(ship, survey, data_set_id, analysis_id)

    # get the intervals dataframe from transect_data list
    intervals_data <- intervals_data[[1]]

    # for the intervals data, we usually get the midpoint of the interval as the mean of start position/end position;
    # but there are cases with only a beginning position; in those cases, we'll just use the beginning position
    intervals_data$Lat <- ifelse(is.na(intervals_data$END_LATITUDE) | intervals_data$END_LONGITUDE == 999.0000,
                                 intervals_data$START_LATITUDE,
                                    (intervals_data$START_LATITUDE + intervals_data$END_LATITUDE) / 2
    )
    
    intervals_data$Lon <- ifelse(is.na(intervals_data$END_LONGITUDE) | intervals_data$END_LONGITUDE == 999.0000,
                                 intervals_data$START_LONGITUDE,
                                    (intervals_data$START_LONGITUDE + intervals_data$END_LONGITUDE) / 2
    )
    
    # first, get a smaller version of the interval data to speed things up a bit
    intervals_data_to_join <- intervals_data %>%
      st_drop_geometry() %>%
      select(SHIP, SURVEY, year, INTERVAL, TRANSECT, REPORT_NUMBER, region, management_area)

    # get the biomass for each interval/layer
    biomass_nums_data <- left_join(biomass_nums_data, intervals_data_to_join,
      by = c("SHIP", "SURVEY", "INTERVAL", "TRANSECT", "REPORT_NUMBER")
    )

    # return the big dataframe
    return(list(biomass_nums_data, intervals_data))
  }
}
