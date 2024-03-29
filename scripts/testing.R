testing <- function(){

  ######################################
  # step 1: compile the parameters for queries. This should handle cases where we do a Shelikof AND/OR a Shumagins survey and will 
  # line up what we need to grab based on that, for both the current survey year and the selectivity-corrected portion of the 
  # total survey time series
  
  # get the selectivity-corrected parameters (these will have the current survey year based on the user-entered parameters,
  # and the rest of the selectivity-corrected time series based on what users have entered- in the future this will be 
  # handled by the analysis_tags table!)
  shumagins_sel_corr_survey_params <- read_csv(survey_params_shumagins_selectivity_corrected_surveys, col_types = cols(.default = col_double()))
  shumagins_surf_and_bot_historical <- read_csv(shumagins_surf_and_bot_historical_path, col_types = cols(.default = col_double()))
  
  shelikof_sel_corr_survey_params <- read_csv(survey_params_shelikof_selectivity_corrected_surveys, col_types = cols(.default = col_double()))
  shelikof_surf_and_bot_historical<- read_csv(shelikof_surf_and_bot_historical_path, col_types = cols(.default = col_double()))
  
  # open up the updated EVA values
  historical_eva <- readRDS(eva_path)
  
  # decide what ships and surveys to get current data for- based on what surveys were conducted
  # start with blank dataframe, update accordingly
  current_year_query_params <- data.frame(ships = numeric(0),
                                          surveys = numeric(0),
                                          data_sets = numeric(0),
                                          analyses = numeric(0),
                                          zones = numeric(0))
  
  if (current_shumagins_survey == TRUE){
    
    # get all the current survey parameters 
    add_row <- data.frame(ship_shumagins, survey_shumagins, data_set_shumagins, analysis_shumagins, zones_shumagins)
    colnames(add_row) <- colnames(current_year_query_params)
    # add them to the dataframe
    current_year_query_params <- rbind(current_year_query_params, add_row)
    
  }
  
  if (current_shelikof_survey == TRUE){
    
    # get all the current survey parameters 
    add_row <- data.frame(ship_shelikof, survey_shelikof, data_set_shelikof, analysis_shelikof, zones_shelikof)
    colnames(add_row) <- colnames(current_year_query_params)
    # add them to the dataframe
    current_year_query_params <- rbind(current_year_query_params, add_row)
    
  }
  
  ###
  # gather the historical parameters for the selecitivity-corrected era
  # start with an empty data frame and populate based on what surveys are present
  historical_selectivity_corrected_params <- data.frame(ships = numeric(),
                                                        surveys = numeric(),
                                                        data_sets = numeric(),
                                                        analyses = numeric(),
                                                        report_nums = numeric(),
                                                        zones = numeric())
  
  if (current_shumagins_survey == TRUE){
    
    historical_selectivity_corrected_params <- rbind(historical_selectivity_corrected_params, shumagins_sel_corr_survey_params)
    
  }
  
  if (current_shelikof_survey == TRUE){
    
    historical_selectivity_corrected_params <- rbind(historical_selectivity_corrected_params, shelikof_sel_corr_survey_params)
    
  }
  
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
  
  if (current_shumagins_survey == TRUE){
    
    historical_bottom_ref_params <- rbind(historical_bottom_ref_params, shumagins_surf_and_bot_historical)
    
  }
  
  
  
  if (current_shelikof_survey == TRUE){
    
    historical_bottom_ref_params <- rbind(historical_bottom_ref_params, shelikof_surf_and_bot_historical)
    
  }
  
  
  # now gather the analysis comparisons to grab
  # start with an empty data frame and populate based on what surveys are present
  analysis_comparisons_params <- data.frame(ships = numeric(),
                                            surveys = numeric(),
                                            comp_data_sets = numeric(),
                                            comp_analyses = numeric(),
                                            zones_list = numeric(),
                                            sp_code_list = numeric())
  
  if (current_shumagins_survey == TRUE){
    
    # first add the primary dataset/analysis 
    add_row <- data.frame(ship_shumagins, survey_shumagins, data_set_shumagins, analysis_shumagins, zones_shumagins, 21740)
    colnames(add_row) <- colnames(analysis_comparisons_params)
    analysis_comparisons_params <- rbind(analysis_comparisons_params, add_row)
    
    # now add all the comparisons
    for (i in 1:length(comp_data_sets_shumagins)){
      add_row <- data.frame(ship_shumagins, survey_shumagins, comp_data_sets_shumagins[i], comp_analyses_shumagins[i], zones_shelikof, 21740)
      colnames(add_row) <- colnames(analysis_comparisons_params)
      analysis_comparisons_params <- rbind(analysis_comparisons_params, add_row)
    }
  }
  
  if (current_shelikof_survey == TRUE){
    
    # first add the primary dataset/analysis 
    add_row <- data.frame(ship_shelikof, survey_shelikof, data_set_shelikof, analysis_shelikof, zones_shelikof, 21740)
    colnames(add_row) <- colnames(analysis_comparisons_params)
    analysis_comparisons_params <- rbind(analysis_comparisons_params, add_row)
    
    # now add all the comparisons
    for (i in 1:length(comp_data_sets_shelikof)){
      add_row <- data.frame(ship_shelikof, survey_shelikof, comp_data_sets_shelikof[i], comp_analyses_shelikof[i], zones_shelikof, 21740)
      colnames(add_row) <- colnames(analysis_comparisons_params)
      analysis_comparisons_params <- rbind(analysis_comparisons_params, add_row)
    }
  }
  
  ################################################
  # run the queries based on the gathered parameters
  
  #######
  # Sea surface temps/ SCS data:
  # get the SST SCS data from the database
  # this will also open and update the historical SCS record
  scs_sst_list <- purrr::pmap(list(ship = current_year_query_params$ships,
                                   survey = current_year_query_params$surveys,
                                   data_set_id = current_year_query_params$data_sets,
                                   analysis_id = current_year_query_params$analyses,
                                   historical_scs_file_loc = historical_scs_sst_path),
                              get_sst_data)
  
  # unpack this data
  scs_sst_list <- purrr::list_transpose(scs_sst_list)
  scs_sst <- dplyr::bind_rows(scs_sst_list[[1]])
  scs_stats <- dplyr::bind_rows(scs_sst_list[[2]])
  scs_summary <- dplyr::bind_rows(scs_sst_list[[3]])
  
  
  # purrr::pwalk(list(survey = current_year_query_params$surveys,
  #                   historical_scs_file_loc = historical_scs_sst_path),
  #              open_and_update_sst_from_scs)
  
  # open up the historical sst data now that its been updated
  historical_scs_sst <- readRDS(historical_scs_sst_path)
  
  return(historical_scs_sst)
  
    
}

