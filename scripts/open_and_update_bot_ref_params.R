# open the surface- and bottom- referenced parameters csv and update with the current survey parameters;
# also set a fixed color scale based on the survey year for consistent plotting across regions (i.e. if you
# have a region with 5 years and another with 3, each year will still be the same color)

open_and_update_bot_ref_params <- function(ship, survey, data_set_surf_ref, analysis_surf_ref, data_set_bot_ref, analysis_bot_ref, zones_list, historical_bot_surf_path) {
  # open the historic bot- and surf data
  bot_surf_ref_historical <- read_csv(historical_bot_surf_path, col_types = cols(.default = col_double()))

  # if the current survey is in the historical data, assume that the dataset/id/etc that is currently being used is the analysis
  # folks want to present, and replace whatever is there. If the data isn't there, add it as a row.

  # check- does this year already exist in the historical selectivity-corrected data?
  if (survey %in% bot_surf_ref_historical$surveys) {
    # get rid of this year- we'll update to be safe; this will be needed in cases where
    # a more recent analysis has changed values
    bot_surf_ref_historical <- bot_surf_ref_historical %>%
      filter(surveys != survey)
  }

  # add the new data to the historical data- either after you've cleaned out the current entry for the year,
  # in which case it will update, or for entering the historical data for the first time
  bot_surf_ref_historical <- rbind.data.frame(bot_surf_ref_historical, c(
    ship, survey, data_set_surf_ref, analysis_surf_ref, data_set_bot_ref, analysis_bot_ref,
    zones_list
  ))
  # to be safe, set the colnames
  colnames(bot_surf_ref_historical) <- c("ships", "surveys", "data_sets_surf_ref", "analyses_surf_ref", "data_sets_bot_ref", "analyses_bot_ref", "zones_list")

  # append the most up-top-date info to to the historical csv
  write_csv(bot_surf_ref_historical, file = historical_bot_surf_path)
}
