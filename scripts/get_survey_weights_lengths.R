get_survey_weights_lengths <- function(ship, survey, species_code) {
  length_weight_query <- paste0(
    "SELECT survey, haul, species_code, subcategory, fork_length, organism_weight, age ",
    "FROM clamsbase2.v_specimen_measurements ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND species_code = ", species_code, " ",
    "AND partition = 'Codend' ",
    "AND organism_weight IS NOT NULL "
  )


  # and fetch it from the database
  length_weight_data <- dbGetQuery(db, length_weight_query)

  # return the length/weight data for the survey
  return(length_weight_data)
}
