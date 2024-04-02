# for the BOGOSLOF, this is written to plot SST survey-wide (not by umnak/samalga regions)

plot_sst_map <- function(temperature_plot_data) {
  # limit the data to the requested region
  temperature_plot_data <- temperature_plot_data 
    
  # build a basemap
  basemap <- MACEReports::get_basemap_layers(plot_limits_data = temperature_plot_data, bathy = FALSE, contours = c(100,200,1000), plot_expansion = 0.1)

  # make the plot
  sst_plot <-
    basemap +
    # add the data
    geom_sf(data = temperature_plot_data, aes(color = temperature), size = 2) +
    # add some place labels
    # geom_sf_text(data = area_labels, aes(label = name), family = 'Times', size = 3, fontface = 'bold')+
    scale_color_distiller(palette = "RdYlBu") +
    guides(color = guide_colorbar(keywidth = 2, keyheight = 0.25, default.unit = "inch", barwidth = 2), fill = "none") +
    # add some labels
    # labs(x ="Longitude", y = "Latitude", color = expression(' Temperature ('*~degree*C*')'))+
    # the above label inserts a space after ( and before the degree C, below solves it.
    labs(x = "Longitude", y = "Latitude", color = expression("Temperature "(degree * C))) +
    # get rid of grey background
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    ) +
    cruise_report_maps_theme +
    cruise_report_maps_theme

  # and return the plot
  return(sst_plot)
}
