### Updated version using bubbleplots to replace ridgeline plots for historical numbers- and biomass-at-age

# create the historical biomass and numbers plots for shelikof/shumagins surveys
# append the current survey to the historical data to the historical dataset as well
# This will 'update in cases where the data currently exists, which assumes that the user
# wants the most updated biomass and number values (which should be the case!)


plot_historical_age_comps_bubble_function <- function(historical_data_path, 
                                                      biomass_and_numbers_data,
                                                      region_name, 
                                                      max_age) {
  # get the numbers and biomass summarized by length interval
  biomass_and_nums <- biomass_and_numbers_data %>%
    # limit to the requested region
    filter(region == region_name) %>%
    group_by(AGE, year) %>%
    # sum the weight and numbers, convert biomass (kg) to 1000s of tons, and fish (individuals) to millions of fish
    summarize(number_million_fish = sum(NUMBERS) / 1e6, biomass_thousand_tons = sum(BIOMASS) / 1e6) %>%
    # make the names match
    rename(Age = AGE)

  # open the historical data
  historical_data <- readRDS(historical_data_path) %>%
    # filter to the requested regions
    filter(region == region_name) %>%
    select(- region) %>%
    arrange(year, Age)
  
  # combine the old pre-selectivity values with the post-selectivity corrected era
  biomass_and_nums <- bind_rows(biomass_and_nums, historical_data) %>%
    arrange(year, Age)
  
  # identify the surveyed years
  survey_years <- unique(biomass_and_nums$year)
  
  # get an age vector that is equal to the 'traditional' historical data spread; get this as a dataframe to add to the current data
  age_vec <- seq(min(biomass_and_nums$Age), max(biomass_and_nums$Age), 1)

  # add the year as well- this is needed to show where there were no fish in a given year
  year_vec <- seq(min(biomass_and_nums$year, na.rm = TRUE), max(biomass_and_nums$year, na.rm = TRUE), 1)
  
  all_ages <- c()
  for (i in year_vec){
    age_vec_tmp <- data.frame('Age' = age_vec)
    age_vec_tmp$year <- i
    all_ages <- rbind(all_ages, age_vec_tmp)
  }
  
  # ADD the age vector that is the same as the historical data- populate the age bins that have no data with NAs
  biomass_and_nums <- left_join(all_ages, biomass_and_nums, by = c("Age", "year")) %>%
    replace_na(list(number_million_fish = 0, biomass_thousand_tons = 0))

  # if a given year has all NAs- it means no survey- get rid of these
  biomass_and_nums <- biomass_and_nums %>%
    filter(year %in% survey_years)
  
  ###########
  ###########
  # format the biomass and numbers data so that we can make histograms:
  # consolidate data by previously defined max_age

  year_t <- c(min(biomass_and_nums$year):max(biomass_and_nums$year))
  historical_plot <- NULL
  for (t in 1:length(year_t)) {
    if (year_t[t] %in% unique(biomass_and_nums$year)) {
      # select data in year t
      historical_data_t <- biomass_and_nums[biomass_and_nums$year == year_t[t], ]
      # reorder data by age
      historical_data_t <- historical_data_t[order(historical_data_t$Age), ]
      # sum Num fish & biomass for all ages >= max_age
      num <- sum(historical_data_t$number_million_fish[historical_data_t$Age >= max_age], na.rm = T)
      bio <- sum(historical_data_t$biomass_thousand_tons[historical_data_t$Age >= max_age], na.rm = T)
      # exclude all rows for ages > max_age
      historical_plot_t <- historical_data_t[historical_data_t$Age <= max_age, ]
      # replace max_age values with num & bio sums
      historical_plot_t[historical_plot_t$Age == max_age, c("number_million_fish", "biomass_thousand_tons")] <- rbind(c(num, bio))
      # store results
      historical_plot <- rbind(historical_plot, historical_plot_t)
      rm(historical_plot_t)
    }
  }
  
  # now plot the data as bubbleplots
  par(mfrow = c(2, 1), mar = c(2, 2, 1, 1), oma = c(2, 2.5, 1, 0))
  ylim <- c(0, max_age + 2)
  # Plot numbers age comps
  plot(x = historical_plot$year, y = historical_plot$Age, pch = 1, cex = sqrt(historical_plot$number_million_fish / 100) / 2, ann = F, ylim = ylim, xaxt = "n", yaxt = "n", yaxs = "i")
  axis(side = 2, las = 2, at = c(1:max_age), labels = c(1:(max_age - 1), paste0(max_age, "+")), cex.axis = 0.8)
  axis(side = 1, at = c(1980:current_year), labels = F, lwd = 1, lwd.ticks = 0.75, tck = -0.02)
  axis(side = 1, at = seq(1980, 2020, 5), lwd = 0, lwd.ticks = 1, tck = -0.04, cex.axis = 0.8)
  text(x = 2001, y = max_age + 1.25, "Age composition by numbers of fish (millions)")
  mtext(side = 2, las = 2, outer = F, line = 1, font = 2, at = 14, "A)")
  # Plot biomass age comps
  plot(x = historical_plot$year, y = historical_plot$Age, pch = 1, cex = sqrt(historical_plot$biomass_thousand_tons) / 10, ann = F, ylim = ylim, xaxt = "n", yaxt = "n", yaxs = "i")
  axis(side = 2, las = 2, at = c(1:max_age), labels = c(1:(max_age - 1), paste0(max_age, "+")), cex.axis = 0.8)
  axis(side = 1, at = c(1980:current_year), labels = F, lwd = 1, lwd.ticks = 0.75, tck = -0.02)
  axis(side = 1, at = seq(1980, 2020, 5), lwd = 0, lwd.ticks = 1, tck = -0.04, cex.axis = 0.8)
  text(x = 2001, y = max_age + 1.25, "Age composition by biomass (1000s t)")
  mtext(side = 1, outer = F, line = 2.5, "Year")
  mtext(side = 2, outer = T, line = 1, "Age")
  mtext(side = 2, las = 2, outer = F, line = 1, font = 2, at = 14, "B)")

}
