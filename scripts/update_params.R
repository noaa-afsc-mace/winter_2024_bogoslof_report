# update the historical survey parameters to include the newest surveys
# do this on a per-survey basis. NOTE: this currently only handles Shelikof and Shumagins! Update if we have other surveys.

update_params <- function(){
  
  # open the survey parameters for the selectivity corrected surveys; these get used in building tables
  # note NA used for report_number- this is used because we're not making any tables that rely on this... so it is ignored here
  sel_corr_survey_params <- open_and_update_sel_corr_survey_params(
    ship = ship, 
    survey = survey, 
    data_set_id = data_set, 
    analysis_id = analysis, 
    report_num = NA, 
    zone = zones, 
    sel_corr_historical_data_path = survey_params_selectivity_corrected_surveys)

  # open historical bottom-and surface-referenced dataset parameters- this dataset won't extend back AS far as we don't
  # need to compare surface/bottom datasets as far back into the past
  surf_and_bot_historical <- open_and_update_bot_ref_params(
    ship = ship, 
    survey = survey, 
    data_set_surf_ref = data_set, 
    analysis_surf_ref = analysis, 
    data_set_bot_ref = bot_ref_data_set,
    analysis_bot_ref = bot_ref_analysis, 
    zones_list = zones, 
    historical_bot_surf_path = surf_and_bot_historical_path)
  
}
    
    

  
  
