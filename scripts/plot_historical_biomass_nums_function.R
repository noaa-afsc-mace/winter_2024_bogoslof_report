# create the historical biomass and numbers plots for shelikof/shumagins surveys
# append the current survey to the historical data to the historical dataset as well
# This will 'update in cases where the data currently exists, which assumes that the user
# wants the most updated biomass and number values (which should be the case!)

# historical data is now only pre-selectivity corrected data and biomass and numbers data is post-selectivity *including
# current year- that's written in the get_macebase_data code, so the current year doesn't need to be appended, but
#two datasets are being combined

plot_historical_biomass_nums_function <- function(historical_data_path,
                                                  biomass_and_numbers_data,
                                                  region_name,
                                                  log10_numbers_scale = NULL,
                                                  sqrt_numbers_scale = NULL) {
  # get the numbers and biomass summarized by length interval
  biomass_and_nums <- biomass_and_numbers_data %>%
    # this will keep shelikof or shumagins data only based on the spatial regions we're using; ok to use both regions here because each survey
    # limit to the requested region
    filter(region == region_name) %>%
    # add a year variable for adding this to the historical data
    #mutate(year = as.numeric(substr(SURVEY, start = 1, stop = 4))) %>%
    group_by(LENGTH, year) %>%
    # sum the weight and numbers, convert biomass (kg) to 1000s of tons, and fish (individuals) to millions of fish
    summarize(number_million_fish = sum(NUMBERS) / 1e6, biomass_thousand_tons = sum(BIOMASS) / 1e6) %>%
    # make the names match
    rename(length = LENGTH)
  
  # save a file path to update the data in the historical csv
  # open up the pre-selectivity data; limit to the requested region
  historical_data <- readRDS(historical_data_path) %>%
    filter(region == region_name) %>%
    select(- region) %>%
    # historical record has NAs for some years with no data- clear these out here so they don't show in plot
    filter(!is.na(number_million_fish))
  
  # check- make sure there are actually old data available, alert user if not!
  if (nrow(historical_data) == 0){
    
    warning("No pre-selectivity data is available! Are you ok with that?")
    
    historical_data <- data.frame(NA, NA, NA, NA) 
    colnames(historical_data) <- colnames(biomass_and_nums)
    
  }
  
  # compile these two into a single dataframe 
  biomass_and_nums <- bind_rows(biomass_and_nums, historical_data) %>%
    arrange(year, length)
  
  # identify the surveyed years
  survey_years <- unique(biomass_and_nums$year)
  
  # get a length vector that is equal to the 'traditional' historical data spread
  length_vec <- seq(min(biomass_and_nums$length, na.rm = TRUE), max(biomass_and_nums$length, na.rm = TRUE), 1)
  
  # and repeat this so it is present for every possible year
  year_vec <- seq(min(biomass_and_nums$year, na.rm = TRUE), max(biomass_and_nums$year, na.rm = TRUE), 1)
  
  all_lengths <- c()
  for (i in year_vec){
    length_vec_tmp <- data.frame('length' = length_vec)
    length_vec_tmp$year <- i
    all_lengths <- rbind(all_lengths, length_vec_tmp)
  }
  
  # ADD the length vector that is the same as the historical data- populate the length bins that have no data with NAs
  biomass_and_nums <- left_join(all_lengths, biomass_and_nums, by = c("length", "year")) %>%
    replace_na(list(number_million_fish = 0, biomass_thousand_tons = 0))
  
  # if a given year has all NAs- it means no survey- get rid of these
  biomass_and_nums <- biomass_and_nums %>%
    filter(year %in% survey_years)
  
  ###########
  # now plot the data using ggridges

  # to scale plots identically, reshape data so that we can use the measurement type (biomass or nums) as a faceting variable

  # if the user hasn't requested a log10 scale for the numbers (helpful for some surveys), just take the values as is

  if (is.null(log10_numbers_scale)) {
    # and reshape
    historical_plot <- biomass_and_nums %>%
      pivot_longer(cols = c(number_million_fish, biomass_thousand_tons), names_to = "measurement_type", values_to = "measurement_value")

    # and name this plot to reflect regular, not log10, values
    num_plot_name <- expression(paste("Numbers (million fish)"))
    biomass_plot_name <- expression(paste("Biomass (thousand tons)"))

    # add a column containing the new facet label names, and use this to make a 'biomass' and a 'numbers' plot
    historical_plot$facets <- factor(historical_plot$measurement_type,
      labels = c(biomass_plot_name, num_plot_name)
    )

    # get annotations- max values for each abundance measure
    max_values <- historical_plot %>%
      group_by(measurement_type) %>%
      slice(which.max(measurement_value)) %>%
      mutate(measurement_text = ifelse(measurement_type == "number_million_fish",
        paste(formatC(round(measurement_value, digits = 1), big.mark = ","), "\nmillion fish"),
        paste(formatC(round(measurement_value, digits = 1), big.mark = ","), "\nthousand tons")
      ))
  }

  # if the user requested a log10 scale, take the log10 val of the numbers, then reshape for plotting
  if (!is.null(log10_numbers_scale)) {
    historical_plot <- biomass_and_nums %>%
      mutate(number_million_fish = log10(number_million_fish + 1)) %>%
      pivot_longer(cols = c(number_million_fish, biomass_thousand_tons), names_to = "measurement_type", values_to = "measurement_value")

    # and name this plot to reflect log10, values
    num_plot_name <- expression(paste("Numbers", " (log"["10"], " million fish)"))
    biomass_plot_name <- expression(paste("Biomass (thousand tons)"))

    # add a column containing the new facet label names, and use this to make a 'biomass' and a 'numbers' plot
    historical_plot$facets <- factor(historical_plot$measurement_type,
      labels = c(biomass_plot_name, num_plot_name)
    )


    # get annotations- max values for each abundance measure- in linear terms
    max_values <- historical_plot %>%
      group_by(measurement_type) %>%
      slice(which.max(measurement_value)) %>%
      mutate(measurement_text = ifelse(measurement_type == "number_million_fish",
        paste(formatC(round(10^measurement_value, digits = 1), big.mark = ","), "\nmillion fish"),
        paste(formatC(round(measurement_value, digits = 1), big.mark = ","), "\nthousand tons")
      ))
  }

  # if the user requested a sqrt scale, take the sqrt val of the numbers, then reshape for plotting
  if (!is.null(sqrt_numbers_scale)) {
    historical_plot <- biomass_and_nums %>%
      mutate(number_million_fish = sqrt(number_million_fish)) %>%
      pivot_longer(cols = c(number_million_fish, biomass_thousand_tons), names_to = "measurement_type", values_to = "measurement_value")

    # and name this plot to reflect log10, values
    # num_plot_name = expression(paste('Numbers', ' (square root million fish)'))
    num_plot_name <- expression(paste("Numbers (", sqrt("million fish"), ")"))
    biomass_plot_name <- expression(paste("Biomass (thousand tons)"))

    # add a column containing the new facet label names, and use this to make a 'biomass' and a 'numbers' plot
    historical_plot$facets <- factor(historical_plot$measurement_type,
      labels = c(biomass_plot_name, num_plot_name)
    )


    # get annotations- max values for each abundance measure- in linear terms
    max_values <- historical_plot %>%
      group_by(measurement_type) %>%
      slice(which.max(measurement_value)) %>%
      mutate(measurement_text = ifelse(measurement_type == "number_million_fish",
        paste(formatC(round(10^measurement_value, digits = 1), big.mark = ","), "\nmillion fish"),
        paste(formatC(round(measurement_value, digits = 1), big.mark = ","), "\nthousand tons")
      ))
  }


  # make the plot
  ridges_plot <-
    ggplot(historical_plot, aes(x = length, y = year, height = measurement_value, group = year, fill = measurement_type)) +
    geom_density_ridges(stat = "identity", scale = 6, alpha = 0.8) +
    # add the maximum values for each bar: to place the maximum for each bar near the top of the biggest value, use the fact that
    #' scale' parameter will be set to be at the top of each bar- subtract 'scale' parameter value to the y value, plus a little buffer
    # add an arrow pointing to the biggest value as well
    # geom_curve(data = max_values, aes(x = length+10, y = year-6.2, xend = length, yend = year-6.2), arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2)+
    # geom_label(data = max_values, mapping = aes(x = length+18, y=year-6, label = measurement_text),
    #           fill = 'transparent', color = 'grey20', fontface = 'bold', label.padding = unit(0.15, "lines"), alpha =0.65, size = 2)+
    # note we're faceting by the 'facets' column created above, and using 'label_parsed' to print subscripts where needed
    facet_wrap(~facets, ncol = 2, labeller = label_parsed) +
    scale_y_reverse(breaks = seq(min(historical_plot$year), max(historical_plot$year), 1), expand = c(0.01, 0.01)) +
    scale_x_continuous(breaks = seq(min(historical_plot$length), max(historical_plot$length), 10), expand = c(0.01, 0.01)) +
    scale_fill_manual(values = c("#a50f15", "#08519c")) +
    labs(x = "Fork length (cm)", y = "Survey year") +
    theme_bw() +
    cruise_report_plots_theme +
    theme(
      strip.background = element_blank(),
      legend.position = "none",
      axis.text = element_text(size = 10)
    )

  # print the plot
  print(ridges_plot)

  # and return a dataframe for building the caption
  min_year <- min(historical_plot$year)
  max_year <- max(historical_plot$year)
  region_name <- region_name
  summary_df <- cbind.data.frame(min_year, max_year, region_name)

  return(summary_df)
}
