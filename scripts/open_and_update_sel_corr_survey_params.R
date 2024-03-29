# open the parameters .csv and update the historic selectivity-corrected data paramters with the current survey
# this assumes the most recent run for the current survey is the best. If there are already values for the survey,
# it will overwrite these (this is intentional!)

# add the current survey parameters to the historic selectivity-corrected parameters; also update the historic parameters with these new ones

open_and_update_sel_corr_survey_params <- function(ship, survey, data_set_id, analysis_id, report_num, zone, sel_corr_historical_data_path) {
  # open the historic selectivity-corrected data
  sel_corr_historical_data <- read_csv(sel_corr_historical_data_path, col_types = cols(.default = col_double()))

  # if the current survey is in the historical data, assume that the dataset/id/etc that is currently being used is the analysis
  # folks want to present, and replace whatever is there. If the data isn't there, add it as a row.

  # check- does this year already exist in the historical selectivity-corrected data?
  if (survey %in% sel_corr_historical_data$surveys) {
    # get rid of this year- we'll update to be safe; this will be needed in cases where
    # a more recent analysis has changed values
    sel_corr_historical_data <- sel_corr_historical_data %>%
      filter(surveys != survey)
  }

  # add the new data to the historical data- either after you've cleaned out the current entry for the year,
  # in which case it will update, or for entering the historical data for the first time
  sel_corr_historical_data <- rbind.data.frame(sel_corr_historical_data, c(ship, survey, data_set_id, analysis_id, report_num, zone))


  # append the most up-top-date info to to the historical csv
  write_csv(sel_corr_historical_data, file = sel_corr_historical_data_path)

  # return the parameters as a dataframe
  return(sel_corr_historical_data)
}
