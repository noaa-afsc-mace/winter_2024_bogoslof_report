# This script will either do the kriging for each survey, and save the output, OR
# simply open this output if it's already been created and there are no differences (i.e. no new changes
# to macebase analysis) because it is very time consuming to create the interpolated values

plot_interpolated_survey_comparisons_winter_function <- function(biomass_data, interval_data, current_survey, basemap) {

  # limit the data being used in the COG calculations to Dave's shelikof footprint
  # this adds a "Contour" column which is NA when the interval is outside the Shelikof basemap
  interval_data <- st_join(interval_data, basemap)

  interval_data <- interval_data %>%
    filter(!is.na(Contour)) %>%
    # get projected coordinates
    mutate(
      lat_proj = sf::st_coordinates(.)[, 2],
      lon_proj = sf::st_coordinates(.)[, 1]
    )

  # limit the intervals to the position information we need to join to the biomass/nums
  intervals_to_join <- interval_data %>%
    # and drop the sf spatial values; they don't stick
    st_drop_geometry() %>%
    select(SHIP, SURVEY, TRANSECT, INTERVAL, Lat, Lon, lat_proj, lon_proj)
  #
  # and add these to the biomass data- do this as a right join so cases where there's an interval present but
  # NO biomass/nums are represented
  historical_vals_by_interval <- left_join(biomass_data, intervals_to_join, by = c("SHIP", "SURVEY", "INTERVAL"))
  # historical_vals_by_interval2 = right_join(biomass_data, intervals_to_join %>% as.data.frame(), by = c('SHIP', 'SURVEY', 'INTERVAL'))
  # historical_vals_by_interval2 %<>% st_sf(sf_column_name = 'geometry')


  # for intervals where measurements were made, but there was NO pollock present- these are now NA's-
  # @change those to 0's so that interpolation will account for these
  historical_vals_by_interval <- historical_vals_by_interval %>%
    rename(biomass = BIOMASS) %>%
    mutate(biomass = replace_na(biomass, 0))

  #########################################
  # step 2: Calculate center of gravity using rgeostats; this copies Dave McGowan's approach but does it in
  # projected (Albers equal area Alaska) instead of geographic space to be comparable to other plots

  # this function returns the axes to plot for each year
  cog_by_year <- function(survey_year) {
    # survey_year = 2015
    # subset data within survey
    cog_tmp <- historical_vals_by_interval %>%
      filter(year == survey_year)

    # %>%
    #   st_drop_geometry()
    #
    # limit to the variables we actually need- lat/long/weighting factor (biomass)
    biomass_proj <- data.frame("Lon_proj" = cog_tmp$lon_proj, "Lat_proj" = cog_tmp$lat_proj, "w" = cog_tmp$biomass)

    # create database & specify parameters
    biomass_proj_db <- db.create(biomass_proj[, c("Lon_proj", "Lat_proj", "w")], ndim = 2, flag.grid = FALSE, autoname = F)
    biomass_proj_db <- db.locate(biomass_proj_db, "w", "w") # define weight

    # Calculate center of gravity and inertia in projected space weighted by biomass and add to plot as segments
    cgi_proj <- SI.cgi(biomass_proj_db, name = db.getname(biomass_proj_db, "w", 1), flag.plot = F, flag.inertia = T)

    # Save center-of-gravity point estimates
    cog_est <- data.frame("Year" = survey_year, "Eastings" = cgi_proj$center[1], "Northings" = cgi_proj$center[2])

    # get the inertia stats we need
    start_pos <- data.frame(x = c(cgi_proj$axes[1, 1], cgi_proj$axes[3, 1]), y = c(cgi_proj$axes[1, 2], cgi_proj$axes[3, 2]))
    end_pos <- data.frame(x = c(cgi_proj$axes[2, 1], cgi_proj$axes[4, 1]), y = c(cgi_proj$axes[2, 2], cgi_proj$axes[4, 2]))
    cog_plot <- bind_rows(start_pos, end_pos)

    # convert to a spatial dataframe;
    cog_plot <- st_as_sf(cog_plot, coords = c("x", "y"), crs = 3338, remove = FALSE)

    # connect segments 1+3; 2+4
    cog_plot$ind <- seq(1, 4, 1)
    cog_plot$ind[1] <- 1
    cog_plot$ind[3] <- 1
    cog_plot$ind[2] <- 2
    cog_plot$ind[4] <- 2

    cog_axes <- cog_plot %>%
      group_by(ind) %>%
      summarize() %>%
      st_cast("LINESTRING") %>%
      mutate(year = survey_year)

    # return the axes & COG point estimates in a list to plot
    return(list(cog_axes, cog_est))
  }

  # build a dataframe that represents the survey years/management areas to calculate
  get_vals <- biomass_data %>%
    group_by(year) %>%
    distinct(year)
  # reorder get_vals by year
  get_vals <- get_vals[order(get_vals$year), ]

  # only report 610/620/630/640
  # filter(management_area %in% c(610, 620, 630, 640))

  # apply function to year to generate list of inertia axes & COG estimates
  cgi_est <- map(get_vals$year, cog_by_year)

  # separate inertia axes & COG point estimates in separate objects
  plot_axes <- NULL # to store inertia axes (2 rows per year)
  plot_cog <- NULL # to store COG point estimates (1 row per year)
  for (t in 1:length(get_vals$year)) {
    # Extract inertia axes estimates in year t (2 rows per year)
    plot_axes_t <- cgi_est[[t]][[1]]
    plot_axes <- rbind(plot_axes, plot_axes_t)
    # Extract COG point estimates in year t (1 row per year)
    plot_cog_t <- cgi_est[[t]][[2]]
    plot_cog <- rbind(plot_cog, plot_cog_t)
    rm(plot_axes_t, plot_cog_t)
  }

  curryearaxes <- plot_axes %>%
    filter(year == current_year)

  ###########################################
  # step 3: plot the interpolated data and the center of gravity plot


  # set the colors- make sure the most recent survey is ordered first
  # year_levels = -sort(desc(unique(plot_axes$year)))
  # plot_axes$year = factor(plot_axes$year, levels = -sort(desc(unique(plot_axes$year))), labels = -sort(desc(unique(plot_axes$year))))
  #
  # this palette is a homemade version of the 'Okabe-Ito' colorblind-safe palette for qualitative comparisons
  # https://mikemol.github.io/technique/colorblind/2018/02/11/color-safe-palette.html
  # cog_year_cols = rep(c('gray50'),2)
  cog_year_cols <- "gray50"


  # cog_year_cols = rep(c('#000000', '#E69F00', '#56B4E9', '#0072B2', '#D55E00', '#CC79A7'),2)
  cog_plot_labels <- plot_axes %>%
    filter(ind == 2)

  # set as a custom color scale limited to the number of years in the plot
  cog_plot_cols <- cog_year_cols[1:length(unique(plot_axes$year))]

  # set a custom size scale too- most common year thicker than others
  # cog_plot_sizes = c(1.25, rep(1, length(unique(plot_axes$year))-1))

  # generate map of COG axes
  center_of_gravity_plot_map <-
    ggplot() +
    # add high-res bathy raster
    geom_raster(data = bathy_w, aes(x = x, y = y, fill = z)) +
    bathy_colors +
    # add management regions
    # geom_sf(data = management_regions_plot, color = 'white', fill = 'transparent')+
    # add land too- finer scale near shore
    geom_sf(data = ak_land, fill = "#616161", color = "black") +
    # add the cog crosses
    geom_sf(data = plot_axes, color = "#7570b3", size = 0.75) +
    # scale_colour_grey()+
    # scale_color_manual(values = cog_plot_cols, name = 'Survey year')+
    # scale_size_manual(values = cog_plot_sizes, guide = FALSE)+
    geom_sf(data = curryearaxes, color = "#d95f02", size = 1.25) +
    # set axes limits! note that we're expanding limits a bunch for context
    MACEReports::easy_plot_limits(plot_limits_data = interval_data, plot_expansion = .15) +
    theme_bw() +
    cruise_report_maps_theme +
    cruise_report_plots_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

  # insert NA for 2011
  if (all(plot_cog$Year != 2011) == TRUE) plot_cog <- rbind(plot_cog[1:3, ], c(2011, NA, NA), plot_cog[4:nrow(plot_cog), ])

  # Center centroid values by 15-year mean (2008-2022)
  N.mn <- mean(plot_cog$Northings, na.rm = T)
  E.mn <- mean(plot_cog$Eastings, na.rm = T)

  ## plot COG point estimates by Northings & Eastings
  # turn off plot display
  win.metafile()
  dev.control("enable") # enable display list
  # create 2x1 multipanel plot
  par(mfrow = c(2, 1), mar = c(0, 3, 0, 0), oma = c(3, 3, 2, 1))
  # Northings plot
  plot(x = plot_cog$Year, y = (plot_cog$Northings - N.mn) / 1000, type = "l", ann = F, xaxt = "n", yaxt = "n", ylim = c(-110, 110), col = "#7570b3")
  points(x = plot_cog$Year, y = (plot_cog$Northings - N.mn) / 1000, pch = 16, cex = 0.8, col = ifelse(plot_cog$Year == current_year, "#d95f02", "#7570b3"), )
  abline(h = 0, lty = 2, col = "gray50")
  axis(side = 2, las = 2, at = seq(-100, 100, 50))
  mtext(side = 2, outer = F, line = 3, "Northings")
  # Eastings plot
  plot(x = plot_cog$Year, y = (plot_cog$Eastings - E.mn) / 1000, type = "l", ann = F, xaxt = "n", yaxt = "n", ylim = c(-110, 110), col = "#7570b3")
  points(x = plot_cog$Year, y = (plot_cog$Eastings - E.mn) / 1000, pch = 16, cex = 0.8, col = ifelse(plot_cog$Year == current_year, "#d95f02", "#7570b3"), )
  abline(h = 0, lty = 2, col = "gray50")
  axis(side = 1, at = c(plot_cog$Year[1]:plot_cog$Year[length(plot_cog$Year)]), labels = F, tck = -0.02, lwd = 0, lwd.ticks = 0.75)
  axis(side = 1, at = seq(2010, current_year, 5), tck = -0.04, lwd = 1)
  axis(side = 2, las = 2, at = seq(-100, 100, 50))
  mtext(side = 2, outer = F, line = 3, "Eastings")
  mtext(side = 2, outer = T, line = 1.5, "Distance from mean distribution (km)")
  # Save 2x1 multipanel plot as object
  COG_pt_est <- recordPlot()
  # Turn plot display back on
  dev.off()

  # Export complex multipanel plot that combines map and line plots
  center_of_gravity_plot <- cowplot::plot_grid(center_of_gravity_plot_map, COG_pt_est, ncol = 2, byrow = TRUE, align = "hv")

  # save COG & inertia estimates
  saveRDS(plot_cog, file = "geostat_outputs/COG_point_estimates.rds")
  saveRDS(plot_axes, file = "geostat_outputs/Inertia_axes_estimates.rds")

  # return the map plot to the document
  return(center_of_gravity_plot)
}
