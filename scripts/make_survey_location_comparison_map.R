# produce a simple map comparing where the current survey went vs historical surveys

make_survey_location_comparison_map <- function(all_survey_intervals, current_survey_intervals){
  
  #join the 'ideal' survey to the current one for plotting
  full_plot <- all_survey_intervals %>%
    # remove the current survey from the background data
    filter(year != unique(current_survey_intervals$year)) %>%
    # get rid of historical 'GOA Shelf' winter survey region (out towards Middleton)
    filter(region != "GOA Shelf") %>%
    select(region) %>%
    mutate(type = 'historical surveys')
  
  current_plot <- current_survey_intervals %>%  
    select(region) %>%
    mutate(type = 'current survey')
  
  plot_data <- bind_rows(full_plot, current_plot)
  
  # and set the factor order so they plot correctly
  plot_data$type <- factor(plot_data$type, levels = c('historical surveys', 'current survey'), ordered = TRUE)
  
  # build the map
  basemap <- get_basemap_layers(plot_data, bathy = FALSE, contours = c(100, 200, 1000))
  
  comp_map <-
    basemap +
    geom_sf(data = plot_data, aes(color = type, size = type)) +
    guides(color = guide_legend(title = "Survey transects")) +
    scale_size_manual(values = c(3, 1), guide = "none") +
    scale_color_manual(values = c("grey80", "#4575b4")) +
    labs(title = paste0('Winter ', unique(current_survey_intervals$year), ' survey extent \nsurvey intervals starting in ', min(all_survey_intervals$year), ' are displayed for context'))+
    theme_bw()+
    cruise_report_maps_theme +
    cruise_report_plots_theme +
    theme(legend.position = 'bottom')
  
  # return the table and figure as a list
  return(comp_map)
  
}