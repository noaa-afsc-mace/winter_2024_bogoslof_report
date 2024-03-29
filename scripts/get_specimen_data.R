# This query returns a simplified version of the v_speciemen_measurements table- it
# collapses all length types into a single 'length' category!

get_specimen_data <- function(ship, survey, data_set_id, analysis_id) {
  # build the query
  specimen_query <- paste0(
    "SELECT ship, survey, haul, species_code, common_name, scientific_name, partition, ",
    "fork_length, standard_length, bell_diameter, mantle_length, wing_span, total_length, ",
    "carapace_length, preanal_fin_length, ",
    "organism_weight, sex, maturity, ",
    "CASE WHEN barcode IS NOT NULL THEN 1 ELSE 0 END as otolith_taken, age ",
    "FROM clamsbase2.v_specimen_measurements ",
    "WHERE partition = 'Codend' ",
    "AND ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND specimen_id  IS NOT NULL ",
    "ORDER by haul, species_code "
  )

  # go get the data
  specimen_data <- dbGetQuery(db, specimen_query)

  # clean up a bit- simplify lengths to two columns: length, and length_type

  # identify the colums containing length data; we'll use this to clean up the length data entries
  length_cols <- c(
    "FORK_LENGTH", "STANDARD_LENGTH", "BELL_DIAMETER",
    "MANTLE_LENGTH", "WING_SPAN", "TOTAL_LENGTH",
    "CARAPACE_LENGTH", "PREANAL_FIN_LENGTH"
  )


  specimen_summary <- specimen_data %>%
    # use coalese to find the which length type actually has a value; report this value
    mutate(organism_length = coalesce(
      FORK_LENGTH, STANDARD_LENGTH, BELL_DIAMETER, MANTLE_LENGTH, WING_SPAN, TOTAL_LENGTH,
      CARAPACE_LENGTH, PREANAL_FIN_LENGTH
    )) %>%
    # also report the type of value we've got in another column
    rowwise() %>%
    mutate(length_type = names(specimen_data)[names(specimen_data) %in% length_cols]
    [!is.na(c_across(all_of(length_cols)))][1]) %>%
    # removed the summarized columns
    select(-all_of(length_cols))
  
  # get the name of the survey region from the event data
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  event_data <- event_and_interval_data[[2]]
  
  region_name <- event_data %>%
    # keep only the ship, survey, event_id, and the 'nice' region name
    filter(SURVEY == survey) %>%
    st_drop_geometry() %>%
    select(EVENT_ID, region)
  
  # join the region name with the haul data
  specimen_summary <- left_join(specimen_summary, region_name, by = c("HAUL" = "EVENT_ID"))

  # return this dataframe
  return(specimen_summary)
}
