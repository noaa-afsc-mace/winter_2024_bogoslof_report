# get all the data needed for the catch tables- do this on a per-region basis

get_catch_table_data <- function(ship, survey, data_set_id, analysis_id, scaling_hauls_only = TRUE){
  
  # for the bogoslof survey, combine across regions!
  
  # fetch the event data
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  event_data <- event_and_interval_data[[2]]
  
  
  # if the user requested to only report hauls that were actually used in an analysis, limit hauls to those used
  # also allow methot hauls! These will get used in an analysis as well
  if (scaling_hauls_only == TRUE) {
    # identify all the scaling hauls (regardless of region)
    scaling_hauls_query <- paste0(
      "SELECT event_id FROM macebase2.interval_scaling_key_map ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      "AND data_set_id = ", data_set_id, " ",
      "AND analysis_id = ", analysis_id, " ",
      "AND event_id is not NULL ",
      "GROUP BY event_id ",
      "ORDER BY event_id "
    )
    
    hauls_in_nh <- dbGetQuery(db, scaling_hauls_query)
    
    hauls_list <- event_data$EVENT_ID[event_data$EVENT_ID %in% hauls_in_nh$EVENT_ID]
  }
  
  # assemble the catch data query for these hauls (modified from Nate's SQL from SH1904 report to get the gear type and query all gear at one time)
  catch_query <- paste0(
    "SELECT a.species_code, a.common_name, a.scientific_name, a.gear, ",
    "sum(a.total_weight) as tot_weight, sum(a.total_number) as tot_number, sum(b.lengths) as L, sum(b.weights) as W ",
    "FROM (select a.species_code, a.common_name, a.scientific_name, a.ship, a.survey, a.partition, a.event_id, b.gear, ",
    "sum(a.weight_in_haul) as total_weight, sum(a.number_in_haul) as total_number ",
    "from clamsbase2.catch_summary a join clamsbase2.events b ",
    "ON (a.ship = b.ship and a.survey = b.survey and a.event_id = b.event_id) ",
    "WHERE a.partition = 'Codend' ",
    "AND a.ship = ", ship, " ",
    "AND a.survey = ", survey, " ",
    paste("AND a.event_id IN (", paste(hauls_list, collapse = ","), ") "),
    "group by a.species_code, a.common_name, a.scientific_name, a.ship, a.survey, a.partition, a.event_id, b.gear) a ",
    "LEFT JOIN (select species_code, partition, ship, survey, haul, ",
    "sum(case when fork_length is not null or standard_length is not null ",
    "or bell_diameter is not null or mantle_length is not null or wing_span is not null ",
    "or total_length is not null or carapace_length is not null or preanal_fin_length is not null then 1 end) as lengths, ",
    "sum(case when organism_weight is not null then 1 end) as weights ",
    "from clamsbase2.v_specimen_measurements ",
    "WHERE partition = 'Codend' ",
    "AND ship = ", ship, " ",
    "AND survey = ", survey, " ",
    paste("AND haul IN (", paste(hauls_list, collapse = ","), ") "),
    "group by species_code, partition, ship, survey, haul) b ",
    "ON (a.species_code = b.species_code and a.partition=b.partition and a.ship=b.ship and a.survey=b.survey and a.event_id=b.haul) ",
    "group by a.species_code, a.common_name, a.scientific_name, a.gear ",
    "order by tot_weight desc "
  )
  
  # get the data
  catch_data <- dbGetQuery(db, catch_query)
  
  # add the report number and region descripton to this dataset
  catch_data$region <- "Bogoslof"
  
  # return dataframe
  return(catch_data)
}
