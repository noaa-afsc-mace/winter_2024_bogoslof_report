# query the total biomass from the macebase2.analysis_results_by_age- to know if there's been an analysis by age

check_if_age_data <- function(ship, survey, data_set_id, analysis_id, zones_list) {
  # do this with a query of the current survey, and make the tables only if there is data in this query:
  check_ages_query <- paste0(
    "SELECT sum(numbers) AS numbers ",
    "FROM macebase2.analysis_results_by_age ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND data_set_id = ", data_set_id, " ",
    "AND analysis_id = ", analysis_id, " ",
    paste("AND zone IN (", paste(zones_list, collapse = ","), ") "),
    "AND species_code = 21740 "
  )

  # get the data
  check_if_numbers <- dbGetQuery(db, check_ages_query)

  # if you don't have data, return a zero
  if (is.na(check_if_numbers$NUMBERS)) {
    biomass_nums_data <- 0
    return(biomass_nums_data)
  }

  # otherwise return the value
  return(check_if_numbers$NUMBERS)
}
