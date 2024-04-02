get_macebase_data <- function(){

  ######################################
  # step 1: compile the parameters for queries.
  
  # get the selectivity-corrected parameters (these will have the current survey year based on the user-entered parameters,
  # and the rest of the selectivity-corrected time series based on what users have entered- in the future this will be 
  # handled by the analysis_tags table!)
  sel_corr_survey_params <- read_csv(survey_params_selectivity_corrected_surveys, col_types = cols(.default = col_double()))
  surf_and_bot_historical<- read_csv(surf_and_bot_historical_path, col_types = cols(.default = col_double()))
  
  # open up the updated EVA values
  historical_eva <- readRDS(eva_path)
  
  # decide what ships and surveys to get current data for- based on what surveys were conducted
  # start with blank dataframe, update accordingly
  current_year_query_params <- data.frame(ships = numeric(0),
                             surveys = numeric(0),
                             data_sets = numeric(0),
                             analyses = numeric(0),
                             zones = numeric(0))
    
  # get all the current survey parameters 
  add_row <- data.frame(ship, survey, data_set, analysis, zones)
  colnames(add_row) <- colnames(current_year_query_params)
  # add them to the dataframe
  current_year_query_params <- rbind(current_year_query_params, add_row)
  
  ###
  # gather the historical parameters for the selecitivity-corrected era
  # start with an empty data frame and populate based on what surveys are present
  historical_params <- data.frame(ships = numeric(),
                                              surveys = numeric(),
                                              data_sets = numeric(),
                                              analyses = numeric(),
                                              report_nums = numeric(),
                                              zones = numeric())
  

  historical_params <- rbind(historical_params, sel_corr_survey_params)
  

  # same, but for surface ref/bottom ref datasets: we do a more limited comparison of vertical distribution over time (and it is 
  # slower to gather because it must be per-layer) so we gather these params separately here
  # start with an empty data frame and populate based on what surveys are present
  historical_bottom_ref_params <- data.frame(ships = numeric(),
                                              surveys = numeric(),
                                              data_sets_surf_ref = numeric(),
                                              analyses_surf_ref = numeric(),
                                              data_sets_bot_ref = numeric(),
                                              analyses_bot_ref = numeric(),
                                              zones_list = numeric())
    
  historical_bottom_ref_params <- rbind(historical_bottom_ref_params, surf_and_bot_historical)
  
  
  # now gather the analysis comparisons to grab
  # start with an empty data frame and populate based on what surveys are present
  analysis_comparisons_params <- data.frame(ships = numeric(),
                                            surveys = numeric(),
                                            comp_data_sets = numeric(),
                                            comp_analyses = numeric(),
                                            zones_list = numeric(),
                                            sp_code_list = numeric())
  
    
  # first add the primary dataset/analysis 
  add_row <- data.frame(ship, survey, data_set, analysis, zones, 21740)
  colnames(add_row) <- colnames(analysis_comparisons_params)
  analysis_comparisons_params <- rbind(analysis_comparisons_params, add_row)
    
  # now add all the comparisons
  for (i in 1:length(comp_data_sets)){
    add_row <- data.frame(ship, survey, comp_data_sets[i], comp_analyses[i], zones, 21740)
    colnames(add_row) <- colnames(analysis_comparisons_params)
    analysis_comparisons_params <- rbind(analysis_comparisons_params, add_row)
  }
  
  ################################################
  # run the queries based on the gathered parameters
  
  # get the intervals and events data for all selectivity-corrected surveys
  intervals_and_events <- purrr::pmap(list(ship = current_year_query_params$ships, 
                                           survey = current_year_query_params$surveys, 
                                           data_set_id = current_year_query_params$data_sets,
                                           analysis_id = current_year_query_params$analyses),
                                         get_event_and_interval_data_function)
  
  # transpose lists for easier unpacking
  intervals_and_events <- purrr::list_transpose(intervals_and_events)
    
  # pull out interval data
  current_year_interval_data<- dplyr::bind_rows(intervals_and_events[[1]])
  
  # pull out event data
  current_year_event_data <- dplyr::bind_rows(intervals_and_events[[2]])
  
  #### 
  # get the haul types, as identified by the trawl scientist (off-bottom, etc)
  clams_event_type <- purrr::pmap(list(ship = current_year_query_params$ships,
                                       survey = current_year_query_params$surveys),
                                  get_event_type)
  
  clams_event_type <- dplyr::bind_rows(clams_event_type)
  
  # get all available gear types- to map gear legends equally over multiple survey years
  mace_gear_types <- get_all_gear_types()
  
  #########
  # get all the biomass/nums values per interval (good for most purposes, where we don't need values by layer)
  biomass_nums_list <- pmap(
    list(ship = historical_params$ships,
         survey = historical_params$surveys, 
         data_set_id = historical_params$data_sets,
         analysis_id = historical_params$analyses,
         zones_list = historical_params$zones),
    get_biomass_and_nums_data_by_interval_function)
    
  # pull out the biomass/nums data + intervals for each survey in the time series
  biomass_nums_list <- purrr::list_transpose(biomass_nums_list)
  all_species_biomass_nums <- dplyr::bind_rows(biomass_nums_list[[1]])
  
# for many figures, we only present pollock biomass/nums from the current survey; to be clear, create species-only datafarames from current survey;
  current_year_pollock_biomass_nums <- all_species_biomass_nums %>%
    filter(SPECIES_CODE == 21740 & SURVEY == survey)
  
  # get the survey totals by length and region
  historical_surveys_pollock_totals_by_length <- all_species_biomass_nums %>%
    filter(SPECIES_CODE == 21740) %>%
    group_by(SURVEY, SPECIES_CODE, REPORT_NUMBER, LENGTH, region, year) %>%
    summarize(BIOMASS = sum(BIOMASS),
              NUMBERS = sum(NUMBERS))
  
  # get the pollock biomass by interval for the selectivity corrected era
  historical_surveys_pollock_totals_by_interval <- all_species_biomass_nums %>%
    filter(SPECIES_CODE == 21740) %>%
    group_by(SHIP, SURVEY, year, DATA_SET_ID, ANALYSIS_ID, REPORT_NUMBER, region, INTERVAL, SPECIES_CODE) %>%
    summarize(BIOMASS = sum(BIOMASS))
  
  # also get a version of the intervals data for all surveys 
  # this is used in the survey interpolations plots and in the stock assessment output
  historical_interval_data <- dplyr::bind_rows(biomass_nums_list[[2]])
  
  # For Bogoslof, just get the requested surface-referenced surveys by layer (slow!) for vertical distribution comparisons
  vertical_dist_surf_ref_biomass_nums_by_length <- pmap_dfr(
    list(
      ship = historical_bottom_ref_params$ships,
      survey = historical_bottom_ref_params$surveys,
      data_set_id = historical_bottom_ref_params$data_sets_surf_ref,
      analysis_id = historical_bottom_ref_params$analyses_surf_ref,
      zones_list = historical_bottom_ref_params$zones_list
    ),
    get_biomass_and_nums_data_by_layer_and_length_function
  )
  
  # get the analysis comparisons data- numbers/biomass at length for various datasets/analyses
  analysis_comparisons <- purrr::pmap_dfr(list(
                                           ship = analysis_comparisons_params$ships,
                                           survey = analysis_comparisons_params$surveys,
                                           data_sets = analysis_comparisons_params$comp_data_sets,
                                           analyses = analysis_comparisons_params$comp_analyses,
                                           zones_list = analysis_comparisons_params$zones_list,
                                           sp_code = analysis_comparisons_params$sp_code_list),
                                      get_analysis_comparison_data)
  
  # Get the biomass and numbers at AGE data
  
  # Because we can run reports before the otoliths are read, we want the report to print (without age plots/tables) if there's no age data, we'll skip
  # making plots. So to start with set age_data to FALSE
  age_data <- FALSE
  
  # check if we have age data- this will return a 0 if there isn't any, and a value if there is
  age_data_check <- check_if_age_data(
    ship = ship,
    survey = survey,
    data_set_id = data_set,
    analysis_id = analysis,
    zones_list = zones
  )
  
  # go get the age data, if available
  if (age_data_check > 0) {
    # set the age check to true, and go get the data
    age_data <- TRUE
    
    # try to get the age data for the selectivity-corrected period; this will return a blank dataframe if there's no current survey age data
    historical_surveys_biomass_nums_age <- pmap_dfr(
      list(
        ship = sel_corr_survey_params$ships,
        survey = sel_corr_survey_params$surveys,
        data_set_id = sel_corr_survey_params$data_sets,
        analysis_id = sel_corr_survey_params$analyses,
        zones_list = sel_corr_survey_params$zones,
        sp_code = 21740
      ),
      get_biomass_and_nums_age_data_function
    )
    
    # also save a limited version of this that only has the current survey
    current_survey_biomass_nums_age <- historical_surveys_biomass_nums_age[historical_surveys_biomass_nums_age$SURVEY == survey, ]
    
    # get the values by age
    vertical_dist_surf_ref_biomass_nums_by_age <- pmap_dfr(
      list(
        ship = surf_and_bot_historical$ships,
        survey = surf_and_bot_historical$surveys,
        data_set_id = surf_and_bot_historical$data_sets_surf_ref,
        analysis_id = surf_and_bot_historical$analyses_surf_ref,
        zones_list = surf_and_bot_historical$zones_list
      ),
      get_biomass_and_nums_data_by_layer_and_age_function
    )
    
  }
  
  # if not, just add some placeholders
  if (age_data_check == 0) {
    historical_surveys_biomass_nums_age <- c()
    current_survey_biomass_nums_age <- c()
    vertical_dist_surf_ref_biomass_nums_by_age <- c()
    vertical_dist_bot_ref_biomass_nums_by_age <- c()
  }
  
  ###################################
  # get net fishing statistics
  net_statistics_data <- purrr::pmap_dfr(list(current_year_query_params$ships,
                                             current_year_query_params$surveys),
                                        get_net_statistics)
  
  ###################################
  # get the biological data
  
  #########
  #maturities
  
  # update the trawl weights used for weighted maturities, get prop mature, maturities and weights, and scaling hauls 
  # for the current survey year
  weighted_maturity <- purrr::pmap(list(historical_trawl_weights = historical_trawl_weights_path,
                                        ship = current_year_query_params$ships,
                                        survey = current_year_query_params$surveys,
                                        data_set_id = current_year_query_params$data_sets,
                                        analysis_id = current_year_query_params$analyses,
                                        species_code = 21740,
                                        random_fish_only = maturities_random_fish_only,
                                        min_length = pollock_size_cutoff
                                        ),
                                   open_and_update_maturity_data
                                   )
  
  # unpack results to get prop mature, maturities and weights, and scaling hauls information
  weighted_maturity <- purrr::list_transpose(weighted_maturity)
  prop_mature <- dplyr::bind_rows(weighted_maturity[[1]])
  maturities_and_weights <- dplyr::bind_rows(weighted_maturity[[2]])
  scaling_hauls <- dplyr::bind_rows(weighted_maturity[[3]])
  
  # if we've got ages, also get the weighted maturity data by age
  if (age_data == TRUE) {
    # get a dataframe of hauls and weights (we've already returned all scaling hauls and their weights for
    # the maturity by length, so we'll just use that here)
    hauls_and_weights <- prop_mature %>%
      filter(region %in% c("Umnak", "Samalga" )) %>%
      group_by(EVENT_ID, REPORT_NUMBER, region) %>%
      distinct(WEIGHTS)
    
    # return the age data
    maturity_by_age <- get_maturity_by_age(
      ship = ship,
      survey = survey,
      species_code = 21740,
      trawl_weights = hauls_and_weights
    )
  }
  
  # if you don't have any ages, just add a placeholder
  if (age_data == FALSE) {
    # return the age data
    maturity_by_age <- c()
  }
  
  ######
  # pollock length-weight-age data- this gathers for the macebase2 time series
  pollock_length_weight_age_data <- get_biological_data(
    ships = historical_params$ships,
    surveys = historical_params$surveys,
    data_sets = historical_params$data_sets,
    analyses = historical_params$analyses,
    species_code = 21740
  )
  
  ################################
  # Get the data used to create report tables
  # print('getting data for reporting tables')
  
  ## get the TS relationships used for for all surveys in the current year, then pick the unique relationships to have the total collection.
  ts_relationships_data <- purrr::pmap_dfr(list(ship = current_year_query_params$ships, 
                                                survey = current_year_query_params$surveys,
                                                data_set_id = current_year_query_params$data_sets,
                                                analysis_id = current_year_query_params$analyses),
                                           get_ts_relationships)
  
  ts_relationships_data <- ts_relationships_data %>%
    distinct(TS_RELATIONSHIP)
  
  #####
  # get the haul table
  haul_table_data <- purrr::pmap_dfr(list(ship = current_year_query_params$ships,
                                          survey = current_year_query_params$surveys,
                                          data_set_id = current_year_query_params$data_sets,
                                          analysis_id = current_year_query_params$analyses),
                                     get_haul_table_data)
  
  # get catch table, for Bogoslof this has been modified to sum across regions
  catch_table_data <- purrr::pmap_dfr(list(ship = current_year_query_params$ships,
                                           survey = current_year_query_params$surveys,
                                           data_set_id = current_year_query_params$data_sets,
                                           analysis_id = current_year_query_params$analyses,
                                           scaling_hauls_only = TRUE),
                                      get_catch_table_data)
  
  # #also build a unique collection of species codes/common names/scientific names that can be used for labelling/
  # #id'ing species codes in other places
  captured_species_list <- catch_table_data %>%
    distinct(SPECIES_CODE, COMMON_NAME, SCIENTIFIC_NAME)
  
  # get the specimen data formatted for tables
  specimen_table_data <- purrr::pmap_dfr(list(ship = current_year_query_params$ships,
                                              survey = current_year_query_params$surveys,
                                              data_set_id = current_year_query_params$data_sets,
                                              analysis_id = current_year_query_params$analyses,
                                              species_code = 21740), 
                                         get_specimen_table_data)
  
  # get the 'raw' specimen table data for stock assessment authors
  raw_specimen_data <- purrr::pmap_dfr(list(ship = current_year_query_params$ships,
                                            survey = current_year_query_params$surveys,
                                            data_set_id = current_year_query_params$data_sets,
                                            analysis_id = current_year_query_params$analyses),
                                       get_specimen_data)
  
  ##################################################
  # temperature data
  
  ######
  # temperatures at trawls/fishing locations:
  # get the SBE data for each year in the timeseries; this returns indices for report number and region
  # this gathers all the SBE records from trawls in the selectivity-corrected era
  sbe_data <- get_sbe_by_survey(ships = historical_params$ships, 
                                surveys = historical_params$surveys, 
                                data_sets = historical_params$data_sets, 
                                analyses = historical_params$analyses)
  
  # update the historical trawl fishing location records records based on what is has been calculated in CLAMS 
  # requires the dataframes 'event_data' and 'haul_table_data'- which should be gathered above!
  # commented out for BOGOSLOF! Not presenting this figure yet
  
  # purrr::pwalk(list(ship = current_year_query_params$ships,
  #                   survey = current_year_query_params$surveys,
  #                   data_set_id = current_year_query_params$data_sets,
  #                   analysis_id = current_year_query_params$analyses,
  #                   historical_sst_loc = historical_trawl_sst),
  #              open_and_update_sst_at_fishing_locs)
  # 
  # sst_at_fishing_locs <- readRDS(historical_trawl_sst)
  
  #######
  # Sea surface temps/ SCS data:
  # get the SST SCS data from the database
  # this will also open and update the historical SCS record
  scs_sst_list <- purrr::pmap(list(ship = current_year_query_params$ships,
                                   survey = current_year_query_params$surveys,
                                   data_set_id = current_year_query_params$data_sets,
                                   analysis_id = current_year_query_params$analyses),
                         get_sst_data)
  
  # unpack this data
  scs_sst_list <- purrr::list_transpose(scs_sst_list)
  scs_sst <- dplyr::bind_rows(scs_sst_list[[1]])
  scs_stats <- dplyr::bind_rows(scs_sst_list[[2]])
  scs_summary <- dplyr::bind_rows(scs_sst_list[[3]])
  
  # open up the historical sst data now that its been updated
  # currently not using historical SST comparisons for Bogoslof!
  # historical_scs_sst <- readRDS(historical_scs_sst_path)
  
  ################################################
  # save all the data
  
  # get a timestamp for the run time so the user can know when data was queried
  query_run_time <- Sys.time()
  
  # save all of the objects in an .RData file
  save(historical_eva,
       current_year_query_params, 
       historical_params,
       historical_bottom_ref_params,
       analysis_comparisons_params,
       current_year_interval_data,
       current_year_event_data,
       clams_event_type,
       mace_gear_types,
       all_species_biomass_nums,
       historical_interval_data,
       current_year_pollock_biomass_nums,
       vertical_dist_surf_ref_biomass_nums_by_length,
       # not currently doing a bot-ref for Bogoslof
       #vertical_dist_bot_ref_biomass_nums_by_length,
       historical_surveys_pollock_totals_by_length,
       historical_surveys_pollock_totals_by_interval,
       analysis_comparisons,
       age_data,
       historical_surveys_biomass_nums_age,
       current_survey_biomass_nums_age,
       vertical_dist_surf_ref_biomass_nums_by_age,
       vertical_dist_bot_ref_biomass_nums_by_age,
       net_statistics_data,
       prop_mature,
       maturities_and_weights,
       scaling_hauls,
       maturity_by_age,
       pollock_length_weight_age_data,
       ts_relationships_data,
       haul_table_data,
       catch_table_data,
       captured_species_list,
       specimen_table_data,
       raw_specimen_data,
       sbe_data,
       # not currently doing this comparison
       #sst_at_fishing_locs,
       scs_sst,
       scs_stats,
       scs_summary,
       # not currently doing sst comparisons from scs
       #historical_scs_sst,
       query_run_time,
       file = paste0(data_set_path, "bogoslof_", current_year,  "_dataset.RData")
       )
}

  
