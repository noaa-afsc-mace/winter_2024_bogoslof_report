# get and update maturity data for a given survey; this script will:
# calculate weights for each region within a given survey
# append (or update) weights for a given survey/region to the historical dataset
# note this relies on the dataframe 'event_data' existing! Just run 'get_event_and_interval_data' first to return this.
open_and_update_maturity_data <- function(historical_trawl_weights, ship, survey, data_set_id, analysis_id, species_code,
                                          random_fish_only, min_length) {
  # 1. get the historical data

  # save a file path to update the trawl weights in the historical csv later in the script
  out_file_trawl_weights <- historical_trawl_weights
  # open the historical trawl weights
  historical_trawl_weights <- readRDS(historical_trawl_weights)
  
  # get the name of the survey region from the event data
  event_and_interval_data <- get_event_and_interval_data_function(ship = ship, survey = survey, data_set_id = data_set_id, analysis_id = analysis_id)
  event_data <- event_and_interval_data[[2]]
  
  # limit the event data to the survey we are currently working with
  event_data_for_survey <- event_data %>%
    filter(SURVEY == survey)

  ############

  # get the maturity state data- again for all hauls in the specified region; do this for non-random or random and non_random fish
  # based on the random_fish_only parameter

  if (random_fish_only == TRUE) {
    query_maturity_data <- paste0(
      "SELECT event_id, fork_length, sum(number_sampled) as num_sampled, ",
      "sum(number_mature) as mature FROM clamsbase2.v_spawner_state ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      "AND species_code = ", species_code, " ",
      "AND sex= 'Female' ",
      "AND sampling_method = 'random' ",
      "GROUP BY event_id, fork_length ORDER BY event_id, fork_length"
    )

    maturity_data <- dbGetQuery(db, query_maturity_data)
  }

  if (random_fish_only != TRUE) {
    query_maturity_data <- paste0(
      "SELECT event_id, fork_length, sum(number_sampled) as num_sampled, ",
      "sum(number_mature) as mature FROM clamsbase2.v_spawner_state ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      "AND species_code = ", species_code, " ",
      "AND sex= 'Female' ",
      "AND (sampling_method = 'random' OR sampling_method = 'non_random') ",
      "GROUP BY event_id, fork_length ORDER BY event_id, fork_length"
    )

    maturity_data <- dbGetQuery(db, query_maturity_data)
  }

  # calculate the # immature as # sampled- # mature
  maturity_data$IMMATURE <- maturity_data$NUM_SAMPLED - maturity_data$MATURE

  # now figure out which hauls were actually used in nearest haul analysis
  query_weights1 <- paste0(
    "SELECT survey, event_id FROM macebase2.interval_scaling_key_map ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND data_set_id = ", data_set_id, " ",
    "AND analysis_id = ", analysis_id, " ",
    "AND event_id is not NULL ",
    "GROUP BY survey, event_id ",
    "ORDER BY event_id "
  )

  hauls_in_nh <- dbGetQuery(db, query_weights1)

  # get the numbers of fish in each nearest_haul stratum - to run a bit quicker, we
  # just pull the data by individual haul/length bin/layer/interval and sum outside the database
  # for hauls that we actually used in a nearest-haul-analysis
  # NOTE that this will only select fish greater than the length cutoff (i.e. 30 cm usually); this
  # is done because we are only using fish > this length in the abundance weighting.

  query_numbers <- paste0(
    "SELECT a.ship, a.survey, b.event_id, b.interval, b.length_key, a.layer, a.numbers, a.length  ",
    "FROM macebase2.analysis_results_by_length a  ",
    "JOIN macebase2.interval_scaling_key_map b ON  ",
    "(a.ship=b.ship AND a.survey=b.survey AND a.data_set_id=b.data_set_id AND a.analysis_id=b.analysis_id ",
    "AND a.interval=b.interval AND a.class=b.class) ",
    "WHERE a.ship = ", ship, " ",
    "AND a.survey = ", survey, " ",
    "AND a.data_set_id = ", data_set_id, " ",
    "AND a.analysis_id = ", analysis_id, " ",
    "AND a.species_code = ", species_code, " ",
    "AND a.length > ", min_length, " ",
    paste("AND b.event_id  IN (", paste(hauls_in_nh$EVENT_ID, collapse = ","), ") "),
    "ORDER BY b.event_id, b.interval "
  )

  numbers_in_nh_data <- dbGetQuery(db, query_numbers)

  # sum the numbers of fish > min length in each haul
  numbers_in_nh <- numbers_in_nh_data %>%
    # get the total number of fish in each of these hauls
    group_by(SHIP, SURVEY, EVENT_ID) %>%
    summarize(num_fish = sum(NUMBERS))

  # add the numbers of fish to the hauls info so that every haul has a total catch associated with it; only
  # keep those hauls used in a nh-analysis
  hauls <- left_join(event_data_for_survey, numbers_in_nh, by = c("SHIP", "SURVEY", "EVENT_ID")) %>%
    filter(EVENT_ID %in% hauls_in_nh$EVENT_ID) %>%
    arrange(EVENT_ID)

  # calculate haul weighting on a per-region basis
  calculate_haul_weights <- function(report_num) {
    hauls_in_region <- hauls %>%
      filter(REPORT_NUMBER == report_num) %>%
      st_drop_geometry()

    # there are very occasional cases where a haul only catches fish below the length cutoff. In these cases, we'll
    # have an NA for number of fish- set to zero instead (this will ensure the haul gets zero weight!)
    hauls_in_region$num_fish <- ifelse(is.na(hauls_in_region$num_fish), 0, hauls_in_region$num_fish)

    # calculate the weighting of each haul
    hauls_in_region$WEIGHTS <- hauls_in_region$num_fish / mean(hauls_in_region$num_fish)

    # return this dataframe
    return(hauls_in_region)
  }

  # apply function to each region in the survey
  hauls_with_weights <- map_dfr(unique(hauls$REPORT_NUMBER), calculate_haul_weights)

  # distribute these weights to the maturity data too
  maturity_data <- maturity_data %>%
    left_join(select(hauls_with_weights, SURVEY, EVENT_ID, REPORT_NUMBER, region, WEIGHTS), by = c("EVENT_ID"))

  # cleanup - give hauls not used weight of 0
  # maturity_data$WEIGHTS[is.na(maturity_data$WEIGHTS)]= 0

  # cleanup- remove hauls not used in the analysis (equivalent to setting their weight to zero!)
  maturity_data <- maturity_data %>%
    filter(EVENT_ID %in% hauls_in_nh$EVENT_ID)

  # Update the historical trawl weights  with what's been calculated here for the current surveys
  # this assumes the most recent run for the current survey is the best. If there are already values for the survey and region here,
  # it will overwrite these.

  # get the data to update (if any)
  current_data <- historical_trawl_weights %>%
    filter(survey == unique(numbers_in_nh_data$SURVEY))

  # if there is any, get rid of it
  if (nrow(current_data) > 0) {
    # remove the current data before updating it
    historical_trawl_weights <- anti_join(historical_trawl_weights, current_data, by = c("ship", "survey", "region", "dataset", "analysis", "haul", "weight", "abundance"))
  }

  # reformat it to be consistent with what's already there
  historical_update <- cbind.data.frame(
    hauls_with_weights$SHIP, hauls_with_weights$SURVEY, hauls_with_weights$region,
    data_set_id, analysis_id, hauls_with_weights$EVENT_ID, hauls_with_weights$WEIGHTS, hauls_with_weights$num_fish
  )
  colnames(historical_update) <- c("ship", "survey", "region", "dataset", "analysis", "haul", "weight", "abundance")

  # and only include the hauls with weights
  historical_update <- historical_update %>%
    filter(!is.na(ship))

  # add this to the historical data
  historical_update <- rbind.data.frame(historical_trawl_weights, historical_update)

  # save the updated file
  saveRDS(historical_update, file = out_file_trawl_weights)

  #############################################################
  # get maturities and weights for maturity and GSI plots; do this for non-random or random and non_random fish
  # based on the random_fish_only parameter

  if (random_fish_only == TRUE) {
    maturity_weight_query <- paste0(
      "SELECT haul, sex, organism_weight, maturity, ",
      "gonad_weight, fork_length as length, sampling_method  ",
      "FROM clamsbase2.v_specimen_measurements  ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      paste("AND haul  IN (", paste(hauls_in_nh$EVENT_ID, collapse = ","), ") "),
      "AND species_code = ", species_code, " ",
      "AND maturity is not null  ",
      "AND organism_weight is not null ",
      "AND partition='Codend' ",
      "AND sampling_method = 'random' ",
      "ORDER BY haul "
    )

    maturities_and_weights <- dbGetQuery(db, maturity_weight_query)
  }

  if (random_fish_only != TRUE) {
    maturity_weight_query <- paste0(
      "SELECT haul, sex, organism_weight, maturity, ",
      "gonad_weight, fork_length as length, sampling_method  ",
      "FROM clamsbase2.v_specimen_measurements  ",
      "WHERE ship = ", ship, " ",
      "AND survey = ", survey, " ",
      paste("AND haul  IN (", paste(hauls_in_nh$EVENT_ID, collapse = ","), ") "),
      "AND species_code = ", species_code, " ",
      "AND maturity is not null  ",
      "AND organism_weight is not null ",
      "AND partition='Codend' ",
      "AND (sampling_method = 'random' OR sampling_method = 'non_random') ",
      "ORDER BY haul "
    )

    maturities_and_weights <- dbGetQuery(db, maturity_weight_query)
  }
  
  # add some indices to maturities_and_weights
  maturities_and_weights <- maturities_and_weights %>%
    left_join(select(hauls_with_weights, SURVEY, EVENT_ID, REPORT_NUMBER, region), by = c("HAUL" = "EVENT_ID"))

  # and return the maturity data
  return(list(maturity_data, maturities_and_weights, hauls_in_nh))
}
