# calculate the mean historical biomass for the region
# This is amended for bogoslof, removing the separation between pre and post
# selectivity data, since we are not making a distinction.


calculate_mean_historical_biomass_function <- function(data) {
  ########
  # Sum up the data
  biomass_data <- data %>%
    group_by(year) %>%
    summarize(biomass_thousand_tons = sum(BIOMASS) / 1e6) 


  #######
  # remove the current year from the comparison data
  region_hist_biomass_data <-biomass_data[biomass_data$year != current_year,]

  ### mean historical biomass in thousand tons-
  meanHistBiomass <- mean(region_hist_biomass_data$biomass_thousand_tons, na.rm = TRUE)

  return(meanHistBiomass)
}
