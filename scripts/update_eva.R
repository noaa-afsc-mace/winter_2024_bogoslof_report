# open and update the EVA values
# previous values based on the previously reported value, and
# current value based on user's input
# this is because we're still reliant on 'virtual box' EVA analysis software approach

# this is written to simply replace any values in the current survey year any time that datasets are updated. This ensures the number 
# will be up-to-date with what the user has listed in the params at 'main_cruise_report_XXXX.Rmd'

update_eva <- function(){
  
  # open up the historic EVA values
  historical_eva <- readRDS(eva_path)
  
  # if the current survey is in the historical data, assume that the dataset/id/etc that is currently being used is the analysis
  # folks want to present, and replace whatever is there. If the data isn't there, add it as a row.
  
  # check- does this year already exist in the historical selectivity-corrected data?
  if (current_year %in% unique(historical_eva$Year)) {
    # get rid of this year- we'll update to be safe; this will be needed in cases where
    # a more recent analysis has changed values
    historical_eva <- historical_eva %>%
      filter(Year != current_year)
  }
  
  # add the new data to the historical data- either after you've cleaned out the current entry for the year,
  # in which case it will update, or for entering the historical data for the first time
  current_eva <- data.frame("Year" = current_year,
                            "region" =  c("Bogoslof"),
                            "eva_percent" = c(eva_perc))
  
  # add it to the total
  historical_eva <- rbind(historical_eva, current_eva)
  
  # save the update
  saveRDS(historical_eva, eva_path)
  
}
  