# calculate the mean historical biomass for the region


calculate_mean_historical_biomass_function <- function(pre_sel_corr_data_path, 
                                                       post_sel_corr_data,
                                                       region_id) {
  #######
  # 1: open up the old pre-selectivity corrected data
  pre_sel_corr_biomass <- readRDS(pre_sel_corr_data_path) 
  
  # sum up by year, region, and limit to the requested region
  pre_sel_corr_biomass <- pre_sel_corr_biomass %>%
    filter(region == region_id) %>%
    group_by(year, region) %>%
    summarize(biomass_thousand_tons = sum(biomass_thousand_tons))

  ########
  # 2. sum up the post-selectivity corrected data
  sel_corr_biomass <- post_sel_corr_data %>%
    filter(region == region_id) %>%
    group_by(year, region) %>%
    summarize(biomass_thousand_tons = sum(BIOMASS) / 1e6) 

  ###########
  
  # add the pre- and post- sel corr eras together
  biomass_data <- rbind.data.frame(pre_sel_corr_biomass, sel_corr_biomass)

  #######
  # remove the current year from the comparison data
  region_hist_biomass_data <-biomass_data[biomass_data$year != current_year,]

  ### mean historical biomass in thousand tons-
  meanHistBiomass <- mean(region_hist_biomass_data$biomass_thousand_tons, na.rm = TRUE)

  return(meanHistBiomass)
}
