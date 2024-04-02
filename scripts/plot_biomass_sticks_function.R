# stickplots of biomass by length class (if no ages) or age class (if ages exist)
# FOR BOGOSLOF- this is written to sum across all regions, and does not report by age or length class!

plot_biomass_sticks_function <- function(biomass_data, 
                                                  rotation, 
                                                  max_bar_height,
                                                  length_class_cutoff, 
                                                  age_class_cutoff = NULL) {
  
  # check if ages exist, and set the grouping variable as ages if so. This should only find pollock ages!
  if ("AGE" %in% colnames(biomass_data)){
    
    # sum the biomass data above and below the age cutoff, within each interval
    plot_biomass <- biomass_data %>%
      #filter(region == region_name) %>%
      # label observations as coming from above or below the size class cutoff
      # mutate(size_class = ifelse(AGE >= age_class_cutoff, paste0(age_class_cutoff, "+"),
      #                            paste0("< ", age_class_cutoff)
      # )) %>%
      # now sum the biomass above and below the size classes, within each interval
      group_by(INTERVAL) %>%
      summarize(BIOMASS_NM2 = sum(BIOMASS_NM2)) %>%
      arrange(INTERVAL)
    
  }
  
  if ("LENGTH" %in% colnames(biomass_data)){
    
    # sum the biomass data above and below the length cutoff, within each interval
    plot_biomass <- biomass_data %>%
      #filter(region == region_name) %>%
      # label observations as coming from above or below the size class cutoff
      # mutate(size_class = ifelse(LENGTH <= length_class_cutoff, paste0("<= ", length_class_cutoff),
      #                            paste0("> ", length_class_cutoff)
      # )) %>%
      # now sum the biomass above and below the size classes, within each interval
      group_by(INTERVAL) %>%
      summarize(BIOMASS_NM2 = sum(BIOMASS_NM2)) %>%
      arrange(INTERVAL)
    
  }
  
  
  # for context, get intervals ready to plot

  # make all the intervals into linestrings, grouped by transect number, just to add to plots for context
  intervals_plot <- current_year_interval_data %>%
    #filter(region == region_name) %>%
    group_by(TRANSECT) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    filter(!is.na(TRANSECT))

  # join lats/lons to biomass data
  plot_biomass <- left_join(plot_biomass, current_year_interval_data, by = c("INTERVAL")) %>%
    # get rid of the rand few intervals without lats/longs for plotting
    filter(!(is.na(START_LATITUDE)))
  
  # get the hauls in the region for context
  hauls_plot <- current_year_event_data #%>%
    #filter(region == region_name)
  
  

  # TODO maybe: allow for an arbitrary number of bars to be 'cut off' and continue outside plot window- not here yet.
  # get a vector that is the maximum values sorted in descending order
  # get_max_val <- sort(plot_biomass$BIOMASS_NM2, decreasing = TRUE)


  # #select the max bar height- either the max value, or the user-specified cutoff
  # max_bar_value = ifelse(cutoff_n_bars == 0, max(get_max_val), get_max_val[cutoff_n_bars+1])
  #
  # #turn this into a scaling factor- the bar height/the biggest biomass value
  # scaling_factor = max_bar_height/max_bar_value
  #
  # #apply this to all the biomass values- this will make any  user-specified 'outliers' jump outside of plot area, which is ok.
  # plot_biomass$scale_vals = plot_biomass$BIOMASS_NM2 * scaling_factor

  # return the sticks as sf objects projected as Albers AK (EPSG 3338)
  sticks <- MACEReports::build_sf_sticks(
    x = plot_biomass$lon,
    y = plot_biomass$lat,
    z = plot_biomass$BIOMASS_NM2, 
    #group_variable = plot_biomass$size_class,
    rotation = rotation,
    bar_scale = max_bar_height
  )

  # also set factor levels on size classes so that we plot the 'small' size class in front of the 'large' size class
  # if ("AGE" %in% colnames(biomass_data)){
  #   
  #   big_class <- str_subset(unique(sticks$size_class), "\\+")
  #   small_class <- str_subset(unique(sticks$size_class), "<")
  #   
  # }
  # 
  # if ("LENGTH" %in% colnames(biomass_data)){
  #   
  #   big_class <- str_subset(unique(sticks$size_class), ">")
  #   small_class <- str_subset(unique(sticks$size_class), "<")
  #   
  # }
  # 
  # # order so big class plots first
  # sticks$size_class <- factor(sticks$size_class, levels = c(big_class, small_class), ordered = TRUE)

  # get a basemap based around the stick limits
  basemap <- MACEReports::get_basemap_layers(plot_limits_data = sticks, plot_expansion = .1, bathy = FALSE, contours = c(100,200,1000))

  # get a legend
  stick_legend <- MACEReports::build_stick_legend(stick_data = sticks, legend_color = 'black')

  # get area labels- but only those reasonably close to the plot region!
  
  # define a region as a polygon around the the intervals plus a buffer
  # and get a zoom-in around the region to limit plots
  region_zoom_poly <- intervals_plot %>%
    summarize(geometry = st_union(geometry)) %>%
    st_convex_hull() %>%
    # add a 25 nmi buffer
    st_buffer(10 * 1852)
  
  local_plot_labels <- goa_labels[region_zoom_poly,]
  
  # if the region name happens to be a label, get rid of it (redundant since we already have it in caption, other figs)
  # if (region_name %in% local_plot_labels$area_name){
  # 
  #   local_plot_labels <- local_plot_labels %>%
  #     filter(area_name != region_name)
  # 
  # }

  # plot it
  stickplot <-
    basemap +
    # add intervals for context
    geom_sf(data = intervals_plot, color = 'black', alpha = 0.6) +
    # add the sticks
    geom_sf(data = sticks, linewidth = 1.0, alpha = 0.5, color = "#0072B2") +
    #geom_sf(data = sticks, aes(color = size_class), linewidth = 1.0, alpha = 0.5) +
    # add the hauls for context
    #geom_sf(data = hauls_plot, color = "#5e3c99", shape = 1, stroke = 2, alpha = 0.5)+
    #scale_color_manual(values = c("#0072B2", "#cb181d")) +
    guides(
      color = guide_legend(
        ifelse("AGE" %in% colnames(biomass_data), "Age", "Fork length (cm)"),
        nrow = 2,
        label.theme = element_text(color = "black", family = "Times"),
        title.theme = element_text(color = "black", family = "Times")
      ),
      #linetype = guide_legend(override.aes = unit(1, "in")),
      fill = "none"
    ) +
    stick_legend +
    #add labels, and make sure they don't overlap each other with ggrepel
    # ggrepel::geom_label_repel(
    #   data = local_plot_labels,
    #   aes(label = area_name, geometry = geom),
    #   stat = "sf_coordinates",
    #   size = 3,
    #   family = "Times",
    #   fontface = "bold",
    #   color = "black",
    #   # set the min size for arrows to be made
    #   min.segment.length = unit(0.04, "npc"),
    #   # Add extra padding around each data point.
    #   # point.padding = unit(3, 'lines'),
    #   nudge_y = ifelse(local_plot_labels$area_name %in% goa_arrow_list, 1852 * 20, - 1852 * 5),
    #   nudge_x = ifelse(local_plot_labels$area_name %in% goa_arrow_list, - 1852 * 10, 0),
    #   # Color of the line segments.
    #   segment.color = "black",
    #   # Width of the line segments.
    #   segment.size = 0.4,
    #   # Draw an arrow from the label to the data point.
    #   arrow = arrow(length = unit(0.01, "npc")),
    #   # Strength of the repulsion force.
    #   force = 2,
    #   label.size = NA,
    #   fill = alpha(c("white"), 0.35)
    # ) +
    # use the standard maps + plots theme; shift legend a bit
    theme_bw() +
    cruise_report_maps_theme +
    cruise_report_plots_theme +
    theme(
      legend.position = c(0.2, 0.9),
      # overwrite a few things in the standard maps/plots- get a white legend box
      legend.background = element_rect(fill = "white"),
      legend.box.background = element_rect(fill = "white")
    )

  # return the stickplot and the plotted region name for the caption
  return(stickplot)
}
