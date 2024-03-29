# also compile the maturity data by age to report to stock assessment (if we've got age data)
get_maturity_by_age <- function(ship, survey, species_code, trawl_weights) {
  query_maturity_data_by_age <- paste0(
    "SELECT ship, survey,event_id, age, sum(number_sampled) as num_sampled, ",
    "sum(number_mature) as mature FROM clamsbase2.v_spawner_state_by_age ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND species_code = ", species_code, " ",
    "AND sex= 'Female' AND age is not null ",
    "GROUP BY ship, survey, event_id, age ORDER BY event_id, age"
  )

  maturity_by_age_data <- dbGetQuery(db, query_maturity_data_by_age)

  # calculate the # immature as # sampled- # mature
  maturity_by_age_data$IMMATURE <- maturity_by_age_data$NUM_SAMPLED - maturity_by_age_data$MATURE

  # make sure that age is numeric
  maturity_by_age_data$AGE <- as.numeric(maturity_by_age_data$AGE)

  # distribute trawl weights to the maturity by age data too
  maturity_by_age_data <- maturity_by_age_data %>%
    left_join(trawl_weights, by = c("EVENT_ID"))

  # cleanup- remove hauls not used in the analysis (equivalent to setting their weight to zero!)
  maturity_by_age_data <- maturity_by_age_data %>%
    filter(EVENT_ID %in% trawl_weights$EVENT_ID)

  # return this dataframe
  return(maturity_by_age_data)
}
