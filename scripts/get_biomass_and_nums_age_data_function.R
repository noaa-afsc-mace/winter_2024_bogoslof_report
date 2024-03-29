# query the biomass and numbers for by survey, dataset, analysis_id, etc, and add locations to each observation
# this query will return values by interval and age class
get_biomass_and_nums_age_data_function <- function(ship, survey, data_set_id, analysis_id, zones_list, sp_code) {
  # get the biomass/nums
  biomass_nums_query <- paste0(
    "SELECT ship, survey, data_set_id, analysis_id, report_number, transect, interval, species_code, ",
    "age, sum(numbers) as numbers, sum(biomass) as biomass, sum(biomass_nm2) as biomass_nm2 ",
    "FROM macebase2.analysis_results_by_age ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND data_set_id = ", data_set_id, " ",
    "AND analysis_id = ", analysis_id, " ",
    paste("AND zone IN (", paste(zones_list, collapse = ","), ") "),
    "AND species_code = ", sp_code, " ",
    "GROUP BY ship, survey, data_set_id, analysis_id, report_number, transect, interval, species_code, age ",
    "ORDER BY report_number, transect"
  )

  biomass_nums_data <- dbGetQuery(db, biomass_nums_query)

  # add a region location to every interval in the data to break up the plots
  # get all intervals in the survey, labelled by the spatial survey region they occured in
  intervals_data <- get_event_and_interval_data_function(ship, survey, data_set_id, analysis_id)

  # get the intervals dataframe from transect_data list
  intervals_data <- intervals_data[[1]]

  # first, get a smaller version of the interval data to speed things up a bit
  intervals_data_to_join <- intervals_data %>%
    st_drop_geometry() %>%
    select(SHIP, SURVEY, year, INTERVAL, TRANSECT, REPORT_NUMBER, region, management_area)

  # get the biomass for each interval/layer
  biomass_nums_data <- left_join(biomass_nums_data, intervals_data_to_join, by = c("SHIP", "SURVEY", "INTERVAL", "TRANSECT", "REPORT_NUMBER"))

  # return the big dataframe
  return(biomass_nums_data)
}
