# this function creates a simple map based on the labels you've defined in the 'goa_labels_to_get' variable
# in the GOA_summer_report_main_doc.Rmd' parameters. You can define the extent of the plot by limiting the 'interval_data'
# dataset that you specifiy

plot_survey_overview_map <- function(interval_data) {
  # make transect points into linestrings, grouped by transect number
  intervals_plot <- interval_data %>%
    group_by(region, TRANSECT) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")

  # turn this back into a sf datafarme
  intervals_plot <- st_as_sf(as.data.frame(intervals_plot))

  # don't plot the NMFS area 541 boundary- it is here just for summing any biomass/nums in the 'Aleutians' region
  # and clutters up the plot
  management_regions_plot <- management_regions %>%
    filter(NMFS_AREA %in% c(610, 620, 630, 640, 649))

  # clip any region labels outside of the plotting area

  # compute the plot area as the area encompassed by the intervals, turn into a polygon
  plot_area <- st_bbox(intervals_plot)
  plot_area <- st_as_sf(st_as_sfc(plot_area))

  # and add a little buffer to the plot area to get labels at the margins
  plot_area <- st_buffer(plot_area, dist = 1852 * 50)

  # and keep these labels to plot
  goa_plot_labels <- st_join(goa_labels, plot_area, join = st_intersects, left = FALSE)

  # add an index for what management region plots are in; this can be used to make it easier to see
  # where regions are with arrows (see geom_label_repel call below)
  goa_plot_labels <- st_join(goa_plot_labels, management_regions, join = st_nearest_feature, left = TRUE)

  # get a basemap using the plot area defined above
  basemap <- MACEReports::get_basemap_layers(plot_limits_data = plot_area)

  # plot it all
  overview_plot <-
    basemap +
    # add management regions
    geom_sf(data = management_regions_plot, color = "white", fill = "transparent") +
    # add land too- finer scale near shore
    geom_sf(data = ak_land, fill = "#616161", color = "black") +
    # add the intervals
    geom_sf(data = intervals_plot, linewidth = 1.5, color = "#5e3c99", alpha = 0.5) +
    # add management region labels
    geom_sf_text(
      data = management_regions_plot, aes(label = NMFS_AREA),
      fontface = "bold", family = "Times", color = "white",
      nudge_y = ifelse(management_regions_plot$NMFS_AREA == 610, 1852 * 55, 0),
      nudge_x = ifelse(management_regions_plot$NMFS_AREA == 610, 1852 * 125, 0)
    ) +
    # add labels, and make sure they don't overlap each other with ggrepel
    ggrepel::geom_label_repel(
      data = goa_plot_labels,
      aes(label = area_name, geometry = geom),
      stat = "sf_coordinates",
      size = 3,
      family = "Times",
      fontface = "bold",
      color = "black",
      fill = alpha(c("white"), 0.35)
      # min.segment.length = 0,
      #nudge_y = ifelse(goa_plot_labels$NMFS_AREA <= 620 & goa_plot_labels$area_name != "Kalsin Bay", -1852 * 10, 1852 * 5),
      # # nudge_x = ifelse(goa_plot_labels$NMFS_AREA >=620, - 1852 * 10, 0),
      # # Color of the line segments.
      # segment.color = "black",
      # # Width of the line segments.
      # segment.size = 1.0,
      # # Draw an arrow from the label to the data point.
      # arrow = arrow(length = unit(0.01, "npc")),
      # # Strength of the repulsion force.
      # force = .1,
      # seed = 12,
      # label.size = NA,
      # fill = alpha(c("white"), 0.55)
    ) +
    # set a final buffer around this
    coord_sf(
      label_axes = "--EN",
      xlim = c(min(st_coordinates(plot_area)[, 1]) - (1852 * 15), max(st_coordinates(plot_area)[, 1])) + (1852 * 15),
      ylim = c(min(st_coordinates(plot_area)[, 2]) - (1852 * 15), max(st_coordinates(plot_area)[, 2])) + (1852 * 15),
      expand = TRUE
    ) +
    labs(fill = "Bottom depth (m)") +
    guides(fill = guide_colorbar(title.position = "top", barwidth = unit(1.25, "in"))) +
    # use the standard maps + plots theme; adjust legend position
    theme(
      legend.position = c(0.85, 0.1),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.margin = margin(r = 0.15, l = 0.15, unit = "in"),
      legend.box.background = element_rect(fill = alpha("white", 0.55), color = "transparent"),
      legend.text = element_text(size = 10),
      legend.key = element_rect(fill = "transparent"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # remove margin around plot)
      plot.margin = unit(c(0.5, 0, 0, 0), "cm")
    ) +
    cruise_report_plots_theme

  # return the overview plot
  return(overview_plot)
}
