# Plot the transects and hauls for each survey

plot_intervals_hauls_map_function <- function(region = NULL, gear_type_options, add_haul_numbers) {
  # if a user requested a region, get the data for this region only
  if (!is.null(region)){
  
    # get the intervals and the hauls for the requested region
    interval_data <- current_year_interval_data[current_year_interval_data$region == region,]
    event_data <- current_year_event_data[current_year_event_data$region == region,]
  
  }
  
  # if not, plot all the survey regions
  if (is.null(region)){
    
    # get the intervals and the hauls for the requested region
    interval_data <- current_year_interval_data
    event_data <- current_year_event_data
    
  }
  
  # make transect points into linestrings, grouped by transect number
  intervals_plot <- interval_data %>%
    filter(!is.na(TRANSECT)) %>%
    group_by(region, TRANSECT) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")

  # turn this back into a sf datafarme
  intervals_plot <- st_as_sf(as.data.frame(intervals_plot))

  # get shapes for every single gear type, and assign each gear type to a specific shape
  # to be consistent between plots (from report to report- so we'll just build colors for all
  # possible gear types here)
  gear_lengend_colors_shapes <- gear_type_options %>%
    filter(GEAR %in% unique(event_data$`Gear type`))

  # give a few really common gear types their own special shapes
  if ("LFS1421" %in% gear_lengend_colors_shapes$GEAR) gear_lengend_colors_shapes$gear_shapes[gear_lengend_colors_shapes$GEAR == "LFS1421"] <- 1
  if ("AWT" %in% gear_lengend_colors_shapes$GEAR) gear_lengend_colors_shapes$gear_shapes[gear_lengend_colors_shapes$GEAR == "AWT"] <- 5
  if ("PNE" %in% gear_lengend_colors_shapes$GEAR) gear_lengend_colors_shapes$gear_shapes[gear_lengend_colors_shapes$GEAR == "PNE"] <- 0
  if ("Methot" %in% gear_lengend_colors_shapes$GEAR) gear_lengend_colors_shapes$gear_shapes[gear_lengend_colors_shapes$GEAR == "Methot"] <- 6

  # define custom color and shape scales
  gear_shapes_scale <- scale_shape_manual(values = gear_lengend_colors_shapes$gear_shapes)
  # gear_colors_scale = scale_color_manual(values = gear_lengend_colors_shapes$gear_colors)

  # don't plot the NMFS area 541 boundary- it is here just for summing any biomass/nums in the 'Aleutians' region
  # and clutters up the plot
  management_regions_plot <- management_regions %>%
    filter(NMFS_AREA %in% c(610, 620, 630, 640, 649))

  # clip any region labels outside of the plotting area

  # compute the plot area as the area encompassed by the intervals, turn into a polygon
  plot_area <- st_bbox(intervals_plot)
  plot_area <- st_as_sf(st_as_sfc(plot_area))
  
  # and add a little buffer to the plot area to get labels at the margins
  plot_area <- st_buffer(plot_area, dist = 1852 * 25)

  # and keep these labels to plot
  goa_plot_labels <- st_join(goa_labels, plot_area, join = st_intersects, left = FALSE)

  # get a colorblind-safe qualitative palette for the regions
  region_colors <- scale_color_manual(values = cb_safe_palette[1:length(unique(intervals_plot$region))])

  # make the plot- with or without haul numbers based on user's choice

  # return a basemap that extends to cover all intervals
  basemap <- MACEReports::get_basemap_layers(plot_limits_data = plot_area, plot_expansion = .1)

  # build map
  intervals_and_hauls_map <-
    basemap +
    # add management regions
    geom_sf(data = management_regions_plot, color = "white", fill = "transparent") +
    # add land too- finer scale near shore
    geom_sf(data = ak_land, fill = "#616161", color = "black") +
    # add the intervals
    geom_sf(data = intervals_plot, aes(color = region), linewidth = 1.5) +
    region_colors +
    # and the hauls
    geom_sf(
      data = event_data, aes(shape = `Gear type`), color = "#542788", fill = "transparent",
      size = 1.5, stroke = 1.5, alpha = 0.9
    ) +
    gear_shapes_scale +
    {
      if (add_haul_numbers == TRUE) {
        geom_sf_text(
          data = event_data, aes(label = EVENT_ID), size = 4, nudge_x = 5000, nudge_y = 0,
          fontface = "bold", color = "black"
        )
      }
    } +
    # add management region labels
    geom_sf_text(
      data = management_regions_plot, aes(label = NMFS_AREA),
      fontface = "bold", family = "Times", color = "white",
      nudge_y = ifelse(management_regions_plot$NMFS_AREA == 610, 1852 * 55, 0),
      nudge_x = ifelse(management_regions_plot$NMFS_AREA == 610, 1852 * 125, -1852 * 20)
    ) +
    # since area labels have been handled in the overview map, ignore here except for the big ones
    geom_sf_label(
      data = goa_plot_labels[goa_plot_labels$area_name %in% c("Alaska Peninsula", "Kodiak Island"),],
      aes(label = area_name),
      size = 3, color = "black", fontface = "bold", family = "Times",
      fill = alpha(c("white"), 0.35)
    ) +
    # add labels, and make sure they don't overlap each other with ggrepel
    # ggrepel::geom_label_repel(
    #   data = goa_plot_labels,
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
    #   nudge_y = ifelse(goa_plot_labels$area_name %in% goa_arrow_list, 1852 * 50, - 1852 * 5),
    #   nudge_x = ifelse(goa_plot_labels$area_name %in% goa_arrow_list, - 1852 * 10, 0),
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
    # add some labels
    labs(shape = "Gear \nType", color = "Region", fill = "Bottom depth (m)") +
    guides(
      shape = guide_legend(ncol = 1, keywidth = 0.25, keyheight = 0.25, default.unit = "inch"),
      fill = guide_colorbar(title.position = "top", barwidth = unit(1, "in")), 
      color = guide_legend(ncol = 2, keywidth = 0.25, keyheight = 0.25, default.unit = "inch")
    ) +
    # get rid of grey background
    theme_bw() +
    # enforce some plot limits
    MACEReports::easy_plot_limits(plot_limits_data = plot_area) +
    # use the standard maps + plots theme; adjust legend position
    cruise_report_maps_theme +
    cruise_report_plots_theme +
    theme(
      legend.position =c (0, 1),
      legend.justification = c(-.05, 1),
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.spacing.y = unit(0.1, "inch"),
      legend.text = element_text(color = "black", size = 8),
      legend.title = element_text(color = "black", size = 8),
      legend.box.background = element_blank()
    ) 
    

  # return the plot
  return(intervals_and_hauls_map)
}
