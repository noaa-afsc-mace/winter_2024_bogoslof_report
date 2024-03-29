# get all data needed for specimen tables: this is currently designed to pull together pollock, ONLY!


get_specimen_table_data <- function(ship, survey, data_set_id, analysis_id, species_code) {
  # build the query for the table
  specimen_table_query <- paste0(
    "SELECT survey, species_code, haul, ",
    "sum(case when coalesce(FORK_LENGTH, STANDARD_LENGTH, TOTAL_LENGTH, MANTLE_LENGTH, BELL_DIAMETER, CARAPACE_LENGTH, ",
    "WING_SPAN, PREANAL_FIN_LENGTH) is not null then 1 end) AS catch_lengths, ",
    "sum(case when organism_weight is not null then 1 end) as Weights, ",
    "sum(case when maturity is not null then 1 end) as Maturities, ",
    "sum(case when barcode is not null then 1 end) as Otoliths,  ",
    "sum(case when gonad_weight is not null and maturity = 'Prespawning' then 1 end) as Ovary_Weights, ",
    "sum(case when ovary_taken = 'Yes' then 1 end) as o_taken,  ",
    "sum(case when stomach_taken = 'Yes' then 1 end) as s_taken ",
    "from clamsbase2.v_specimen_measurements ",
    "WHERE partition = 'Codend' ",
    "AND ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND species_code = ", species_code, " ",
    "GROUP BY survey, species_code, haul ",
    "ORDER BY haul "
  )


  # go get the data
  specimen_data <- dbGetQuery(db, specimen_table_query)

  # get the name of the survey region from the event data
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  event_data <- event_and_interval_data[[2]]
  
  region_name <- event_data %>%
    # keep only the ship, survey, event_id, and the 'nice' region name
    filter(SURVEY == survey) %>%
    st_drop_geometry() %>%
    select(EVENT_ID, region)

  # join the region name with the haul data
  specimen_data <- left_join(specimen_data, region_name, by = c("HAUL" = "EVENT_ID"))

  # also fetch the common name for the species you are making a table for
  common_name_query <- paste0(
    "SELECT species_code, common_name ",
    "FROM clamsbase2.species ",
    "WHERE species_code = ", species_code, " "
  )

  common_name <- dbGetQuery(db, common_name_query)

  # and format it as a character with capitalized first letters
  common_name_label <- str_to_title(common_name$COMMON_NAME)

  # join to the specimen data
  specimen_data$common_name <- common_name_label

  # return the specimen data
  return(specimen_data)
}
