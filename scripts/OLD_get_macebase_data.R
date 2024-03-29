# This script 'front loads' all the macebase queries in one place, and saves them as a collection that can be referenced later
# NOTE: this has to be called from 'GOA_winter_report_main.Rmd as this is where all paramters are set!
# it also provides some basic info so the user knows the run time/parameters/etc

get_macebase_data <- function() {
  # grab the shumagins data first, if there's a shumagins survey
  if (isTRUE(current_shumagins_survey)) {
    print("grabbing Shumagins data")

    # interval data, with the start of the interval used as the lat/long for plotting
    intervals_and_events_shumagins <- get_event_and_interval_data_function(ship = ship, survey = survey_shumagins, data_set_id = data_set_shumagins, analysis_id = analysis_shumagins)

    # interval data, with the start of the interval used as the lat/long for plotting
    interval_data_shumagins <- intervals_and_events_shumagins[[1]]

    # event data, with survey regions assigned based on the nearest intervals to the haul (i.e. if located next to a shelikof interval;
    # the haul is a shelikof haul)
    event_data_shumagins <- intervals_and_events_shumagins[[2]]

    # also grab the survey year- this is needed because there are cases where users print old reports and we don't want to include
    # current reports in these cases (i.e. a 2020 report requested after 2021 happens)- this will create a 'max_survey_year' that can
    # be used to limit records; it relies on standard mace survey naming i.e. '202003'
    max_survey_shumagins <- as.numeric(substr(survey_shumagins, 1, 4))

    # open the survey parameters for the selectivity corrected surveys; these get used in building tables
    # for the shumagins surveys: note NA used for report_number- this is used because we're not making any tables that rely on this... so it is ignored here

    shumagins_sel_corr_survey_params <- open_and_update_sel_corr_survey_params(ship = ship, survey = survey_shumagins, data_set_id = data_set_shumagins, analysis_id = analysis_shumagins, report_num = NA, zone = zones_shumagins, sel_corr_historical_data_path = survey_params_shumagins_selectivity_corrected_surveys)

    # open historical bottom-and surface-referenced dataset parameters- this dataset won't extend back AS far as we don't
    # need to compare surface/bottom datasets as far back into the past
    shumagins_surf_and_bot_historical <- open_and_update_bot_ref_params(ship = ship, survey = survey_shumagins, data_set_surf_ref = data_set_shumagins, analysis_surf_ref = analysis_shumagins, data_set_bot_ref = shumagins_bottom_ref_data_set, analysis_bot_ref = shumagins_bottom_ref_analysis, zones_list = zones_shumagins, historical_bot_surf_path = shumagins_surf_and_bot_historical_path)

    # open and update the EVA values
    shumagins_historical_eva <- open_and_update_shumagins_eva(
      survey = survey_shumagins,
      historical_eva_path = shumagins_eva_path,
      eva_morzhovoi = eva_morzhovoi_perc,
      eva_pavlof = eva_pavlof_perc,
      eva_sanak = eva_sanak_perc,
      eva_shumagins = eva_shumagins_perc
    )

    # in some cases, a user may run a previous report (i.e. 2020) after another is available (i.e. 2021); in these cases, we don't want
    # to plot 'future' surveys.

    # remove 'future' surveys here:
    shumagins_sel_corr_survey_params <- shumagins_sel_corr_survey_params %>%
      filter(as.numeric(substr(surveys, 1, 4)) <= max_survey_shumagins)

    shumagins_surf_and_bot_historical <- shumagins_surf_and_bot_historical %>%
      filter(as.numeric(substr(surveys, 1, 4)) <= max_survey_shumagins)

    shumagins_historical_eva <- shumagins_historical_eva %>%
      filter(Year <= max_survey_shumagins)

    # also return the haul type, as identified by the trawl scientist (off-bottom, etc)
    shumagins_clams_event_type <- get_event_type(ship = ship, survey = survey_shumagins)
    
    # also get a version of the intervals data for all surveys in the selectivity-corrected timeseries (back to 2009)-
    # this is used in the survey interpolations plot
    shumagins_historical_interval_data <- get_historical_interval_data(
      ships = shumagins_sel_corr_survey_params$ships,
      surveys = shumagins_sel_corr_survey_params$surveys,
      data_sets = shumagins_sel_corr_survey_params$data_sets,
      analyses = shumagins_sel_corr_survey_params$analyses
    )

    #########
    # Get the biomass and numbers data

    # get all the 'selectivity corrected era' shelikof biomass/nums values: surface-referenced:
    shumagins_surf_ref_biomass_nums <- pmap_dfr(
      list(
        shumagins_surf_and_bot_historical$ships,
        shumagins_surf_and_bot_historical$surveys,
        shumagins_surf_and_bot_historical$data_sets_surf_ref,
        shumagins_surf_and_bot_historical$analyses_surf_ref,
        shumagins_surf_and_bot_historical$zones_list
      ),
      get_biomass_and_nums_data_function
    )

    # If there's a bot-ref dataset, get the bot-referenced surveys
    if (shumagins_bottom_ref_data_set > 0) {
      shumagins_bot_ref_biomass_nums <- pmap_dfr(
        list(
          shumagins_surf_and_bot_historical$ships,
          shumagins_surf_and_bot_historical$surveys,
          shumagins_surf_and_bot_historical$data_sets_bot_ref,
          shumagins_surf_and_bot_historical$analyses_bot_ref,
          shumagins_surf_and_bot_historical$zones_list
        ),
        get_biomass_and_nums_data_function
      )
    }

    # if not, just include a placeholder
    if (shumagins_bottom_ref_data_set == 0) {
      shumagins_bot_ref_biomass_nums <- c()
    }

    # for many figures, we only present pollock biomass/nums from the current survey; to be clear, create species-only datafarames from current survey;
    # sum vertically within each interval to save space because we don't need cell-by-cell vertical resolution in many places
    shumagins_current_survey_pollock_biomass_nums <- shumagins_surf_ref_biomass_nums %>%
      group_by(SHIP, SURVEY, DATA_SET_ID, ANALYSIS_ID, SPECIES_CODE, REPORT_NUMBER, region, management_area, TRANSECT, INTERVAL, LENGTH) %>%
      summarize(
        NUMBERS = sum(NUMBERS),
        BIOMASS = sum(BIOMASS),
        NUMBERS_NM2 = sum(NUMBERS_NM2),
        BIOMASS_NM2 = sum(BIOMASS_NM2)
      ) %>%
      mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4))) %>%
      filter(SURVEY == survey_shumagins & SPECIES_CODE == 21740)

    # also get a 'pollock only' dataframe for the entire surface-referenced series; sum within depth bins as opposed to depth
    # this is used for the violin plots
    shumagins_sel_corr_surveys_pollock_biomass_nums_surf_ref <- shumagins_surf_ref_biomass_nums %>%
      filter(SPECIES_CODE == 21740) %>%
      group_by(SHIP, SURVEY, DATA_SET_ID, ANALYSIS_ID, SPECIES_CODE, REPORT_NUMBER, region, management_area, TRANSECT, depth, LENGTH) %>%
      summarize(
        NUMBERS = sum(NUMBERS),
        BIOMASS = sum(BIOMASS),
        NUMBERS_NM2 = sum(NUMBERS_NM2),
        BIOMASS_NM2 = sum(BIOMASS_NM2)
      ) %>%
      mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4)))

    # and for the bottom referenced timeseries
    if (shumagins_bottom_ref_data_set > 0) {
      shumagins_sel_corr_surveys_pollock_biomass_nums_bot_ref <- shumagins_bot_ref_biomass_nums %>%
        filter(SPECIES_CODE == 21740) %>%
        group_by(SHIP, SURVEY, DATA_SET_ID, ANALYSIS_ID, SPECIES_CODE, REPORT_NUMBER, region, management_area, TRANSECT, depth, LENGTH) %>%
        summarize(
          NUMBERS = sum(NUMBERS),
          BIOMASS = sum(BIOMASS),
          NUMBERS_NM2 = sum(NUMBERS_NM2),
          BIOMASS_NM2 = sum(BIOMASS_NM2)
        ) %>%
        mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4)))
    }

    # if not, just include a placeholder
    if (shumagins_bottom_ref_data_set == 0) {
      shumagins_sel_corr_surveys_pollock_biomass_nums_bot_ref <- c()
    }


    # get the survey totals by length and region for the selectivity corrected era
    shumagins_sel_corr_surveys_totals_by_length_and_region <- pmap_dfr(
      list(
        ship = shumagins_sel_corr_survey_params$ships,
        survey = shumagins_sel_corr_survey_params$surveys,
        data_set_id = shumagins_sel_corr_survey_params$data_sets,
        analysis_id = shumagins_sel_corr_survey_params$analyses,
        zone = shumagins_sel_corr_survey_params$zones
      ),
      get_total_biomass_by_length
    )

    # get the analysis comparisons data- numbers/biomass at length for various datasets/analyses
    shumagins_analysis_comparisons <- get_analysis_comparison_data(
      ship = ship,
      survey = survey_shumagins,
      data_sets = shumagins_comparisons_data_sets,
      analyses = shumagins_comparisons_analyses,
      zones_list = zones_shumagins,
      sp_code = 21740
    )

    shumagins_sbe_data <- get_sbe_by_survey(
      ships = ship,
      surveys = survey_shumagins,
      data_sets = data_set_shumagins,
      analyses = analysis_shumagins
    )

    ###################################
    # get net fishing statistics

    shumagins_net_statistics_data <- get_net_statistics(ship = ship, survey = survey_shumagins)

    ###################################


    # get the maturity data with haul weighting information
    shumagins_weighted_maturity <- open_and_update_maturity_data(
      historical_trawl_weights = shumagins_historical_trawl_weights,
      event_data = event_data_shumagins,
      ship = ship,
      survey = survey_shumagins,
      data_set_id = data_set_shumagins,
      analysis_id = analysis_shumagins,
      species_code = 21740,
      random_fish_only = maturities_random_fish_only,
      min_length = pollock_size_cutoff
    )

    shumagins_prop_mature <- shumagins_weighted_maturity[[1]]
    shumagins_maturities_and_weights <- shumagins_weighted_maturity[[2]]

    # we'll take advantage of the fact that this function also identifies all intervals that have a
    # haul associated with them for scaling to get the 'total' list of hauls used in the analysis
    shumagins_scaling_hauls <- shumagins_weighted_maturity[[3]]

    # pollock length-weight-age data
    shumagins_pollock_length_weight_age_data <- get_biological_data(
      ships = shumagins_sel_corr_survey_params$ships,
      surveys = shumagins_sel_corr_survey_params$surveys,
      data_sets = shumagins_sel_corr_survey_params$data_sets,
      analyses = shumagins_sel_corr_survey_params$analyses,
      species_code = 21740
    )

    shumagins_ts_relationships_data <- get_ts_relationships(
      ship = ship,
      survey = survey_shumagins,
      data_set_id = data_set_shumagins,
      analysis_id = analysis_shumagins
    )

    shumagins_haul_table_data <- get_haul_table_data(ship = ship, survey = survey_shumagins, event_data = event_data_shumagins)

    # the catch table function is written for a single reporting region: just loop through all regions here.
    shumagins_catch_table_data <- c()
    for (i in unique(event_data_shumagins$REPORT_NUMBER)) {
      shumagins_catch_table_data[[i]] <- get_catch_table_data(
        ship = ship,
        survey = survey_shumagins,
        event_data = event_data_shumagins,
        report_num = i
      )
    }
    shumagins_catch_table_data <- bind_rows(shumagins_catch_table_data)

    # also build a unique collection of species codes/common names/scientific names that can be used for labelling/
    # id'ing species codes in other places
    shumagins_captured_species_list <- shumagins_catch_table_data %>%
      distinct(SPECIES_CODE, COMMON_NAME, SCIENTIFIC_NAME)

    shumagins_specimen_table_data <- get_specimen_table_data(
      ship = ship,
      survey = survey_shumagins,
      event_data = event_data_shumagins,
      species_code = 21740
    )

    # get the 'raw' specimen table data for stock assessment authors
    shumagins_raw_specimen_data <- get_specimen_data(ship = ship, survey = survey_shumagins)

    # open and update the historical SST records
    open_and_update_sst_at_fishing_locs(
      historical_sst_loc = historical_trawl_sst,
      event_data = event_data_shumagins,
      haul_table_data = shumagins_haul_table_data
    )

    
    
    # get the SST SCS data from the database- for now, not from DB, from a Darin .csv file
    shumagins_scs <- get_sst_data(ship = ship, survey = survey_shumagins, interval_data = interval_data_shumagins)

    # unpack this data
    shumagins_scs_sst <- shumagins_scs[[1]]
    shumagins_scs_stats <- shumagins_scs[[2]]
    shumagins_scs_summary <- shumagins_scs[[3]]
    
    # open and update the historical SCS record
    historical_scs_sst_shumagins <- open_and_update_sst_from_scs(
      historical_scs_loc = historical_scs_sst_path,
      current_scs_data = shumagins_scs_sst,
      interval_data = interval_data_shumagins
    )
    

    # save all of the Shumagins objects in an .RData file
    save(intervals_and_events_shumagins, interval_data_shumagins, event_data_shumagins, max_survey_shumagins, shumagins_sel_corr_survey_params,
      shumagins_surf_and_bot_historical, shumagins_historical_eva,
      shumagins_clams_event_type, shumagins_historical_interval_data, shumagins_current_survey_pollock_biomass_nums,
      shumagins_sel_corr_surveys_pollock_biomass_nums_surf_ref, shumagins_sel_corr_surveys_pollock_biomass_nums_bot_ref,
      shumagins_sel_corr_surveys_totals_by_length_and_region, shumagins_analysis_comparisons,
      shumagins_sbe_data, shumagins_net_statistics_data,
      shumagins_prop_mature, shumagins_maturities_and_weights, shumagins_scaling_hauls,
      shumagins_pollock_length_weight_age_data,
      shumagins_haul_table_data, shumagins_catch_table_data, shumagins_specimen_table_data,
      shumagins_raw_specimen_data, shumagins_ts_relationships_data, historical_scs_sst_shumagins,
      shumagins_scs_sst, shumagins_scs_stats, shumagins_scs_summary,
      file = paste0("data/", survey_shumagins, "_shumagins_dataset.RData")
    )
  }


  ###################################################################

  if (isTRUE(current_shelikof_survey)) {
    # print('grabbing Shelikof intervals and events data')
    print("grabbing Shelikof data")

    # get the event and interval data
    # this function returns two dataframes (one events, one intervals) and assigns each interval or event to a survey region
    intervals_and_events_shelikof <- get_event_and_interval_data_function(ship = ship, survey = survey_shelikof, data_set_id = data_set_shelikof, analysis_id = analysis_shelikof)

    # interval data, with the start of the interval used as the lat/long for plotting
    interval_data_shelikof <- intervals_and_events_shelikof[[1]]

    # event data, with survey regions assigned based on the nearest intervals to the haul (i.e. if located next to a shelikof interval;
    # the haul is a shelikof haul)
    event_data_shelikof <- intervals_and_events_shelikof[[2]]

    # also grab the survey year- this is needed because there are cases where users print old reports and we don't want to include
    # current reports in these cases (i.e. a 2020 report requested after 2021 happens)- this will create a 'max_survey_year' that can
    # be used to limit records; it relies on standard mace survey naming i.e. '202003'
    max_survey_shelikof <- as.numeric(substr(survey_shelikof, 1, 4))

    # also return the haul type, as identified by the trawl scientist (off-bottom, etc)
    shelikof_clams_event_type <- get_event_type(ship = ship, survey = survey_shelikof)

    ##########

    # also open the survey parameters for the Shelikof selectivity corrected surveys (i.e. post-2008 in Shelikof); these get used in building tables
    # For the shelikof surveys

    shelikof_sel_corr_survey_params <- open_and_update_sel_corr_survey_params(
      ship = ship,
      survey = survey_shelikof,
      data_set_id = data_set_shelikof,
      analysis_id = analysis_shelikof,
      report_num = report_number_shelikof,
      zone = zones_shelikof,
      sel_corr_historical_data_path = survey_params_shelikof_selectivity_corrected_surveys
    )

    # open historical bottom-and surface-referenced dataset parameters- this dataset won't extend back AS far as we don't
    # need to compare surface/bottom datasets as far back into the past
    shelikof_surf_and_bot_historical <- open_and_update_bot_ref_params(
      ship = ship,
      survey = survey_shelikof,
      data_set_surf_ref = data_set_shelikof,
      data_set_bot_ref = shelikof_bottom_ref_data_set,
      analysis_surf_ref = analysis_shelikof,
      analysis_bot_ref = shelikof_bottom_ref_analysis,
      zones_list = zones_shelikof,
      historical_bot_surf_path = shelikof_surf_and_bot_historical_path
    )

    # open and update the EVA values
    shelikof_historical_eva <- open_and_update_shelikof_eva(survey = survey_shelikof, historical_eva_path = shelikof_eva_path, eva_chirikof = eva_chirikof_perc, eva_marmot = eva_marmot_perc, eva_shelikof = eva_shelikof_perc)

    # in some cases, a user may run a previous report (i.e. 2020) after another is available (i.e. 2021); in these cases, we don't want
    # to plot 'future' surveys.

    # remove 'future' surveys here:
    shelikof_sel_corr_survey_params <- shelikof_sel_corr_survey_params %>%
      filter(as.numeric(substr(surveys, 1, 4)) <= max_survey_shelikof)

    shelikof_surf_and_bot_historical <- shelikof_surf_and_bot_historical %>%
      filter(as.numeric(substr(surveys, 1, 4)) <= max_survey_shelikof)

    shelikof_historical_eva <- shelikof_historical_eva %>%
      filter(Year <= max_survey_shelikof)

    # also get a version of the intervals data for all surveys in the selectivity-corrected timeseries (back to 2008)-
    # this is used in the survey interpolations plot
    shelikof_historical_interval_data <- get_historical_interval_data(
      ships = shelikof_sel_corr_survey_params$ships,
      surveys = shelikof_sel_corr_survey_params$surveys,
      data_sets = shelikof_sel_corr_survey_params$data_sets,
      analyses = shelikof_sel_corr_survey_params$analyses
    )

    #########
    # Get the biomass and numbers data
    # print('grabbing Shelikof biomass and numbers data- this will take a while!')

    # get all the 'selectivity corrected era' shelikof biomass/nums values: surface-referenced:
    shelikof_surf_ref_biomass_nums <- pmap_dfr(
      list(
        shelikof_surf_and_bot_historical$ships,
        shelikof_surf_and_bot_historical$surveys,
        shelikof_surf_and_bot_historical$data_sets_surf_ref,
        shelikof_surf_and_bot_historical$analyses_surf_ref,
        shelikof_surf_and_bot_historical$zones_list
      ),
      get_biomass_and_nums_data_function
    )

    # If there's a bot-ref dataset, get the bot-referenced surveys
    if (shelikof_bottom_ref_data_set > 0) {
      shelikof_bot_ref_biomass_nums <- pmap_dfr(
        list(
          shelikof_surf_and_bot_historical$ships,
          shelikof_surf_and_bot_historical$surveys,
          shelikof_surf_and_bot_historical$data_sets_bot_ref,
          shelikof_surf_and_bot_historical$analyses_bot_ref,
          shelikof_surf_and_bot_historical$zones_list
        ),
        get_biomass_and_nums_data_function
      )
    }

    # if not, just include a placeholder
    if (shelikof_bottom_ref_data_set == 0) {
      shelikof_bot_ref_biomass_nums <- c()
    }

    # for many figures, we only present pollock biomass/nums from the current survey; to be clear, create species-only datafarames from current survey;
    # sum vertically within each interval to save space because we don't need cell-by-cell vertical resolution in many places
    shelikof_current_survey_pollock_biomass_nums <- shelikof_surf_ref_biomass_nums %>%
      group_by(SHIP, SURVEY, DATA_SET_ID, ANALYSIS_ID, SPECIES_CODE, REPORT_NUMBER, region, management_area, TRANSECT, INTERVAL, LENGTH) %>%
      summarize(
        NUMBERS = sum(NUMBERS),
        BIOMASS = sum(BIOMASS),
        NUMBERS_NM2 = sum(NUMBERS_NM2),
        BIOMASS_NM2 = sum(BIOMASS_NM2)
      ) %>%
      mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4))) %>%
      filter(SURVEY == survey_shelikof & SPECIES_CODE == 21740 & !is.na(REPORT_NUMBER))

    # also get a 'pollock only' dataframe for the entire surface-referenced series; sum within depth bins as opposed to depth
    # this is used for the violin plots
    shelikof_sel_corr_surveys_pollock_biomass_nums_surf_ref <- shelikof_surf_ref_biomass_nums %>%
      filter(SPECIES_CODE == 21740) %>%
      group_by(SHIP, SURVEY, DATA_SET_ID, ANALYSIS_ID, SPECIES_CODE, REPORT_NUMBER, region, management_area, TRANSECT, depth, LENGTH) %>%
      summarize(
        NUMBERS = sum(NUMBERS),
        BIOMASS = sum(BIOMASS),
        NUMBERS_NM2 = sum(NUMBERS_NM2),
        BIOMASS_NM2 = sum(BIOMASS_NM2)
      ) %>%
      mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4)))

    # and for the bottom referenced timeseries- if it is present
    if (shelikof_bottom_ref_data_set > 0) {
      shelikof_sel_corr_surveys_pollock_biomass_nums_bot_ref <- shelikof_bot_ref_biomass_nums %>%
        filter(SPECIES_CODE == 21740) %>%
        group_by(SHIP, SURVEY, DATA_SET_ID, ANALYSIS_ID, SPECIES_CODE, REPORT_NUMBER, region, management_area, TRANSECT, depth, LENGTH) %>%
        summarize(
          NUMBERS = sum(NUMBERS),
          BIOMASS = sum(BIOMASS),
          NUMBERS_NM2 = sum(NUMBERS_NM2),
          BIOMASS_NM2 = sum(BIOMASS_NM2)
        ) %>%
        mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4)))
    }

    # if not, just include a placeholder
    if (shelikof_bottom_ref_data_set == 0) {
      shelikof_sel_corr_surveys_pollock_biomass_nums_bot_ref <- c()
    }

    # get the survey totals by length and region for the selectivity corrected era
    shelikof_sel_corr_surveys_totals_by_length_and_region <- pmap_dfr(
      list(
        ship = shelikof_sel_corr_survey_params$ships,
        survey = shelikof_sel_corr_survey_params$surveys,
        data_set_id = shelikof_sel_corr_survey_params$data_sets,
        analysis_id = shelikof_sel_corr_survey_params$analyses,
        zone = shelikof_sel_corr_survey_params$zones
      ),
      get_total_biomass_by_length
    )

    # get the pollock biomass by interval for historical selectivity-corrected Shelikof surveys back to 2008 -

    historical_biomass_interval_data <- pmap_dfr(
      list(
        ship = shelikof_sel_corr_survey_params$ships,
        survey = shelikof_sel_corr_survey_params$surveys,
        data_set_id = shelikof_sel_corr_survey_params$data_sets,
        analysis_id = shelikof_sel_corr_survey_params$analyses,
        zone = shelikof_sel_corr_survey_params$zones
      ),
      get_historical_biomass_data
    )

    # get a year column, and spit out what we need for the COG data plot
    shelikof_historical_interval_biomass_data <- historical_biomass_interval_data %>%
      mutate(year = as.numeric(substr(as.character(SURVEY), 1, 4)))

    # #get a summary of the historical, surface-referenced, selectivity-corrected, mint chocolate chip, back to 2008 pollock data by interval for mapping
    # shelikof_historical_interval_biomass_data= historical_biommas_interval_data%>%
    #   mutate(year = as.numeric(substr(as.character(SURVEY), 1,4)))

    # get the analysis comparisons data- numbers/biomass at length for various datasets/analyses
    shelikof_analysis_comparisons <- get_analysis_comparison_data(
      ship = ship,
      survey = survey_shelikof,
      data_sets = shelikof_comparisons_data_sets,
      analyses = shelikof_comparisons_analyses,
      zones_list = zones_shelikof,
      sp_code = 21740
    )


    # Get the biomass and numbers at AGE data (for Shelikof only since we never get Shumagins ages)

    # Because we can run reports before the otoliths are read, we want the report to print (without age plots/tables) if there's no age data, we'll skip
    # making plots. So to start with set shelikof_age_data to FALSE
    shelikof_age_data <- FALSE

    # check if we have age data
    age_data_check <- check_if_age_data(
      ship = ship,
      survey = survey_shelikof,
      data_set_id = data_set_shelikof,
      analysis_id = analysis_shelikof,
      zones_list = zones_shelikof
    )

    # go get the age data, if available
    if (age_data_check > 0) {
      # set the age check to true, and go get the data
      shelikof_age_data <- TRUE

      # try to get the age data for the selectivity-corrected period; this will return a blank dataframe if there's no current survey age data
      shelikof_sel_corr_surveys_biomass_nums_age <- pmap_dfr(
        list(
          ship = shelikof_sel_corr_survey_params$ships,
          survey = shelikof_sel_corr_survey_params$surveys,
          data_set_id = shelikof_sel_corr_survey_params$data_sets,
          analysis_id = shelikof_sel_corr_survey_params$analyses,
          zones_list = shelikof_sel_corr_survey_params$zones,
          sp_code = 21740
        ),
        get_biomass_and_nums_age_data_function
      )

      # also save a limited version of this that only has the current survey
      shelikof_current_survey_biomass_nums_age <- shelikof_sel_corr_surveys_biomass_nums_age[shelikof_sel_corr_surveys_biomass_nums_age$SURVEY == survey_shelikof, ]
    }

    # if not, just add some placeholders
    if (age_data_check == 0) {
      shelikof_sel_corr_surveys_biomass_nums_age <- c()
      shelikof_current_survey_biomass_nums_age <- c()
    }




    #####################################
    # get the SBE data for each year in the timeseries; this returns indices for report number and region as
    # print('grabbing Shelikof sbe data')

    shelikof_sbe_data <- get_sbe_by_survey(
      ships = ship,
      surveys = survey_shelikof,
      data_sets = data_set_shelikof,
      analyses = analysis_shelikof
    )

    ###################################
    # get net fishing statistics

    shelikof_net_statistics_data <- get_net_statistics(ship = ship, survey = survey_shelikof)

    ###################################
    # get the biological data

    shelikof_weighted_maturity <- open_and_update_maturity_data(
      historical_trawl_weights = shelikof_historical_trawl_weights,
      event_data = event_data_shelikof,
      ship = ship,
      survey = survey_shelikof,
      data_set_id = data_set_shelikof,
      analysis_id = analysis_shelikof,
      species_code = 21740,
      random_fish_only = maturities_random_fish_only,
      min_length = pollock_size_cutoff
    )

    shelikof_prop_mature <- shelikof_weighted_maturity[[1]]
    shelikof_maturities_and_weights <- shelikof_weighted_maturity[[2]]

    # we'll take advantage of the fact that this function also identifies all intervals that have a
    # haul associated with them for scaling to get the 'total' list of hauls used in the analysis
    shelikof_scaling_hauls <- shelikof_weighted_maturity[[3]]

    # if we've got ages, also return the weighted maturity data by age
    if (shelikof_age_data == TRUE) {
      # get a dataframe of hauls and weights (we've already returned all scaling hauls and their weights for
      # the maturity by length, so we'll just use that here)
      shelikof_hauls_and_weights <- shelikof_prop_mature %>%
        group_by(EVENT_ID, REPORT_NUMBER, region) %>%
        distinct(WEIGHTS)

      # return the age data
      shelikof_maturity_by_age <- get_maturity_by_age(
        ship = ship,
        survey = survey_shelikof,
        species_code = 21740,
        trawl_weights = shelikof_hauls_and_weights
      )
    }

    # if you don't have any ages, just add a placeholder
    if (shelikof_age_data == FALSE) {
      # return the age data
      shelikof_maturity_by_age <- c()
    }





    # pollock length-weight-age data
    shelikof_pollock_length_weight_age_data <- get_biological_data(
      ships = shelikof_sel_corr_survey_params$ships,
      surveys = shelikof_sel_corr_survey_params$surveys,
      data_sets = shelikof_sel_corr_survey_params$data_sets,
      analyses = shelikof_sel_corr_survey_params$analyses,
      species_code = 21740
    )


    ################################
    # Get the data used to create report tables
    # print('getting data for Shelikof reporting tables')

    shelikof_ts_relationships_data <- get_ts_relationships(
      ship = ship,
      survey = survey_shelikof,
      data_set_id = data_set_shelikof,
      analysis_id = analysis_shelikof
    )

    shelikof_haul_table_data <- get_haul_table_data(ship = ship, survey = survey_shelikof, event_data = event_data_shelikof)

    # the catch table function is written for a single reporting region: just loop through all regions here.
    shelikof_catch_table_data <- c()
    for (i in unique(event_data_shelikof$REPORT_NUMBER)) {
      shelikof_catch_table_data[[i]] <- get_catch_table_data(
        ship = ship,
        survey = survey_shelikof,
        event_data = event_data_shelikof,
        report_num = i
      )
    }
    shelikof_catch_table_data <- bind_rows(shelikof_catch_table_data)

    # #also build a unique collection of species codes/common names/scientific names that can be used for labelling/
    # #id'ing species codes in other places
    shelikof_captured_species_list <- shelikof_catch_table_data %>%
      distinct(SPECIES_CODE, COMMON_NAME, SCIENTIFIC_NAME)

    # get the specimen data formatted for tables
    shelikof_specimen_table_data <- get_specimen_table_data(
      ship = ship,
      survey = survey_shelikof,
      event_data = event_data_shelikof,
      species_code = 21740
    )

    # get the 'raw' specimen table data for stock assessment authors
    shelikof_raw_specimen_data <- get_specimen_data(ship = ship, survey = survey_shelikof)

    # open and update the historical SST records
    open_and_update_sst_at_fishing_locs(
      historical_sst_loc = historical_trawl_sst,
      event_data = event_data_shelikof,
      haul_table_data = shelikof_haul_table_data
    )

    # get the SST SCS data from the database
    shelikof_scs <- get_sst_data(ship = ship, survey = survey_shelikof, interval_data = interval_data_shelikof)

    # unpack this data
    shelikof_scs_sst <- shelikof_scs[[1]]
    shelikof_scs_stats <- shelikof_scs[[2]]
    shelikof_scs_summary <- shelikof_scs[[3]]

    # open and update the historical SCS record
    historical_scs_sst_shelikof <- open_and_update_sst_from_scs(
      historical_scs_loc = historical_scs_sst_path,
      current_scs_data = shelikof_scs_sst,
      interval_data = interval_data_shelikof
    )

    # save all of the Shumagins objects in an .RData file
    # print(paste0('saving Shelikof dataset as ', paste0(survey_shelikof, '_shelikof_dataset.RData')))

    save(intervals_and_events_shelikof, interval_data_shelikof, event_data_shelikof, max_survey_shelikof,
      shelikof_sel_corr_survey_params, shelikof_surf_and_bot_historical, shelikof_historical_eva,
      shelikof_clams_event_type, shelikof_historical_interval_data,
      shelikof_historical_interval_biomass_data, shelikof_current_survey_pollock_biomass_nums, shelikof_sel_corr_surveys_pollock_biomass_nums_surf_ref,
      shelikof_sel_corr_surveys_pollock_biomass_nums_bot_ref,
      shelikof_sel_corr_surveys_totals_by_length_and_region, shelikof_analysis_comparisons, shelikof_age_data,
      shelikof_sel_corr_surveys_biomass_nums_age, shelikof_current_survey_biomass_nums_age, shelikof_sbe_data,
      shelikof_net_statistics_data,
      shelikof_prop_mature, shelikof_maturities_and_weights, shelikof_scaling_hauls,
      shelikof_maturity_by_age, shelikof_pollock_length_weight_age_data,
      shelikof_haul_table_data, shelikof_catch_table_data, shelikof_specimen_table_data,
      shelikof_raw_specimen_data, shelikof_ts_relationships_data,
      shelikof_scs_sst, shelikof_scs_stats, shelikof_scs_summary, historical_scs_sst_shelikof,
      file = paste0("data/", survey_shelikof, "_shelikof_dataset.RData")
    )
  }

  # get the universal datasets for winter surveys

  # get a timestamp for the run time so the user can know when data was queried
  query_run_time <- Sys.time()
  # get all available gear types- to map gear legends equally over multiple survey years
  mace_gear_types <- get_all_gear_types()

  # get all the species captured, in all surveys
  if (current_shumagins_survey == TRUE) {
    captured_species_list <- bind_rows(shumagins_captured_species_list, shelikof_captured_species_list)

    # only keep the distinct species (no need to repeat species between surveys that are caught in both)
    captured_species_list <- captured_species_list %>%
      distinct(SPECIES_CODE, COMMON_NAME, SCIENTIFIC_NAME)
  }

  if (current_shumagins_survey != TRUE) {
    captured_species_list <- shelikof_captured_species_list
  }

  # save all of the Shumagins objects in an .RData file
  # print(paste0('saving common information dataset as ', paste0(substr(survey_shelikof, 1,4), '_common_dataset.RData')))

  save(query_run_time, mace_gear_types, captured_species_list,
    file = paste0("data/", substr(survey_shelikof, 1, 4), "_common_dataset.RData")
  )
}
