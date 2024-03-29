# maks violin plots as an alternative to the 4-panel surface- and bottom- referenced plot for a given survey region

plot_depth_violins <- function(bottom_referenced_data, 
                               surface_referenced_data, 
                               interval_data,
                               length_class_cutoff, 
                               age_class_cutoff,
                               species_code, 
                               region_name = NULL) {

  # check if ages exist, and set the grouping variable as ages if so. This should only find pollock ages!
  if ("AGE" %in% colnames(surface_referenced_data)){
    
    # for the bottom-referenced data, report depths as heights and then calculate the total biomass/nums within each layer
    bottom_referenced_data <- bottom_referenced_data %>%
      filter(region == region_name & SPECIES_CODE == species_code) %>%
      # report height (i.e. inverse of depth) of upper ref line, this will set bottom bin @ o (seafloor);
      # label observations by size class and year
      mutate(
        height = -depth,
        size_class = ifelse(AGE < pollock_age_cutoff, "juvenile_layer_biomass", "adult_layer_biomass")
      ) %>%
      # sum biomass/numbers in each layer/size class
      group_by(year, height, size_class) %>%
      summarize(
        layer_biomass = sum(BIOMASS),
        layer_nums = sum(NUMBERS)
      )
    
    # for the surface-referenced data, calculate the total biomass/nums within each layer
    surface_referenced_data <- surface_referenced_data %>%
      filter(region == region_name & SPECIES_CODE == species_code) %>%
      # report depth as the range from upper reference to be consistent;
      # label observations by size class and year
      mutate(
        size_class = ifelse(AGE < pollock_age_cutoff, "juvenile_layer_biomass", "adult_layer_biomass")
      ) %>%
      # sum biomass/numbers in each layer/size class
      group_by(year, depth, size_class) %>%
      summarize(
        layer_biomass = sum(BIOMASS),
        layer_nums = sum(NUMBERS)
      )
    
    # id this plot as an age plot!
    age_plot = TRUE
  }
  
  if ("LENGTH" %in% colnames(surface_referenced_data)){
    
    # for the bottom-referenced data, report depths as heights and then calculate the total biomass/nums within each layer
    bottom_referenced_data <- bottom_referenced_data %>%
      filter(region == region_name & SPECIES_CODE == species_code) %>%
      # report height (i.e. inverse of depth) of upper ref line, this will set bottom bin @ o (seafloor);
      # label observations by size class and year
      mutate(
        height = -depth,
        year = substr(as.character(SURVEY), 1, 4),
        size_class = ifelse(LENGTH <= length_class_cutoff, "juvenile_layer_biomass", "adult_layer_biomass")
      ) %>%
      # sum biomass/numbers in each layer/size class
      group_by(year, height, size_class) %>%
      summarize(
        layer_biomass = sum(BIOMASS),
        layer_nums = sum(NUMBERS)
      )
    
    # for the surface-referenced data, calculate the total biomass/nums within each layer
    surface_referenced_data <- surface_referenced_data %>%
      filter(region == region_name & SPECIES_CODE == species_code) %>%
      # report depth as the range from upper reference to be consistent;
      # label observations by size class and year
      mutate(
        year = substr(as.character(SURVEY), 1, 4),
        size_class = ifelse(LENGTH <= length_class_cutoff, "juvenile_layer_biomass", "adult_layer_biomass")
      ) %>%
      # sum biomass/numbers in each layer/size class
      group_by(year, depth, size_class) %>%
      summarize(
        layer_biomass = sum(BIOMASS),
        layer_nums = sum(NUMBERS)
      )
    
    # id this plot as an length plot!
    age_plot = FALSE
    
  }
  
  # define a color scale that will be fixed for all plots - this fixes colors based on the survey year, regardless of what
  # regions are present for a given year

  # get a list of 11 colors (we're really really unlikely to ever compare more than 5 or 10); black is the first (for current survey)
  col_list <- c("grey80", "#4575b4", "#d73027", "#984ea3", "#74add1", "#abd9e9", "#fdae61", "#fee090", "#35978f", "#bf812d", "#80cdc1")
  # get a color for each survey year
  col_list <- rep_len(col_list, length.out = length(unique(surf_ref_dataset$year)))
  # assign colors to each survey name, from newest (black) to oldest (other colors)
  names(col_list) <- rev(levels(as.factor(surf_ref_dataset$year)))
  # define this mapping as a custon color scale.
  col_fill_historic_comparisons <- scale_fill_manual(name = "Year", values = col_list)

  ###
  # calculate max depth as the deepest point with data in the dataset- this will be used to scale y-axis in plots

  # get the deepest point for each class- this will be the same in the bottom or surfaced referenced data, so fine to do this just in one dataset
  max_adult_depth <- max(surface_referenced_data$depth[surface_referenced_data$layer_biomass != 0 &
    surface_referenced_data$size_class == "adult_layer_biomass"])
  max_juvenile_depth <- max(surface_referenced_data$depth[surface_referenced_data$layer_biomass != 0 &
    surface_referenced_data$size_class == "juvenile_layer_biomass"])

  # and select whatever is deeper as the max depth
  max_depth <- ifelse(max_adult_depth > max_juvenile_depth, max_adult_depth, max_juvenile_depth)

  ######

  # get some summary stats for the plots- mean weighted depth
  SR_mwd <- surface_referenced_data %>%
    group_by(year, size_class) %>%
    summarize(mwd = sum(depth * layer_biomass) / sum(layer_biomass))

  BR_mwd <- bottom_referenced_data %>%
    group_by(year, size_class) %>%
    summarize(mwd = sum(height * layer_biomass) / sum(layer_biomass))

  # finally- get the mean bottom depth in the most recent survey to add to surface-referenced plots; do this within
  # a given region or survey-wide depending on what user requested
  if (!is.null(region_name)) {
    mean_bot_depth <- mean(interval_data$MEAN_BOTTOM_DEPTH[interval_data$region == region_name])
  }

  if (is.null(region_name)) {
    mean_bot_depth <- mean(interval_data$MEAN_BOTTOM_DEPTH)
  }


  surface_violins_adult <-
    ggplot() +
    # violin plot - add quantiles as lines, and narrow the default widths for more contrast w/ 'adjust'
    geom_violin(
      data = surface_referenced_data[surface_referenced_data$size_class == "adult_layer_biomass" &
        !(is.nan(surface_referenced_data$layer_biomass)), ],
      aes(x = factor(year), y = depth, weight = layer_biomass, fill = factor(year)),
      draw_quantiles = c(0.5), adjust = 0.25
    ) +
    # add the mean weighted depth as a single point
    geom_point(data = SR_mwd[SR_mwd$size_class == "adult_layer_biomass", ], aes(x = factor(year), y = mwd), size = 2) +
    scale_y_reverse() +
    # get better colors for the surveys- using the custom color scale we already created
    col_fill_historic_comparisons +
    # add the mean bottom depth as a dashed line
    geom_hline(yintercept = mean_bot_depth, linetype = "dashed") +
    # label axis 
    {if (!isTRUE(age_plot)) labs(x = "", y = "Depth from surface (m)",
                                                               fill = "Year", 
                                                               title = paste("Pollock >", pollock_size_cutoff, "cm FL")) 
    } +
    {if (isTRUE(age_plot)) labs(x = "", y = "Depth from surface (m)",
                                                               fill = "Year", 
                                                               title = paste("Age ", pollock_age_cutoff,  " + pollock")) 
    } +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  bottom_violins_adult <-
    ggplot() +
    # violin plot - add quantiles as lines, and narrow the default widths for more contrast w/ 'adjust'
    geom_violin(
      data = bottom_referenced_data[bottom_referenced_data$size_class == "adult_layer_biomass" &
        !(is.nan(bottom_referenced_data$layer_biomass)), ],
      aes(x = factor(year), y = height, weight = layer_biomass, fill = factor(year)),
      draw_quantiles = c(0.5), adjust = 0.25
    ) +
    # add the mean weighted depth as a single point
    geom_point(data = BR_mwd[BR_mwd$size_class == "adult_layer_biomass", ], aes(x = factor(year), y = mwd), size = 2) +
    # get better colors for the surveys- using the custom color scale we already created
    col_fill_historic_comparisons +
    # label axis
    labs(x = "Year", y = "Height above seafloor (m)", fill = "Year") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  surface_violins_juvenile <-
    ggplot() +
    # violin plot - add quantiles as lines, and narrow the default widths for more contrast w/ 'adjust'
    geom_violin(
      data = surface_referenced_data[surface_referenced_data$size_class == "juvenile_layer_biomass" &
        !(is.nan(surface_referenced_data$layer_biomass)), ],
      aes(x = factor(year), y = depth, weight = layer_biomass, fill = factor(year)),
      draw_quantiles = c(0.5), adjust = 0.25
    ) +
    # add the mean weighted depth as a single point
    geom_point(data = SR_mwd[SR_mwd$size_class == "juvenile_layer_biomass", ], aes(x = factor(year), y = mwd), size = 2) +
    scale_y_reverse() +
    # get better colors for the surveys- using the custom color scale we already created
    col_fill_historic_comparisons +
    # add the mean bottom depth as a dashed line
    geom_hline(yintercept = mean_bot_depth, linetype = "dashed") +
    # label axis
    labs(x = "", y = "", fill = "Year", title = paste("Pollock ≤", pollock_size_cutoff, "cm FL")) +
    {if (!isTRUE(age_plot)) labs(x = "", y = "",
                                 fill = "Year", 
                                 title = paste("Pollock ≤", pollock_size_cutoff, "cm FL")) 
    } +
    {if (isTRUE(age_plot)) labs(x = "", y = "",
                                fill = "Year", 
                                title = paste("Pollock < age", pollock_age_cutoff)) 
    } +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  bottom_violins_juvenile <-
    ggplot() +
    # violin plot - add quantiles as lines, and narrow the default widths for more contrast w/ 'adjust'
    geom_violin(
      data = bottom_referenced_data[bottom_referenced_data$size_class == "juvenile_layer_biomass" &
        !(is.nan(bottom_referenced_data$layer_biomass)), ],
      aes(x = factor(year), y = height, weight = layer_biomass, fill = factor(year)),
      draw_quantiles = c(0.5), adjust = 0.25
    ) +
    # add the mean weighted depth as a single point
    geom_point(data = BR_mwd[BR_mwd$size_class == "juvenile_layer_biomass", ], aes(x = factor(year), y = mwd), size = 2) +
    # get better colors for the surveys- using the custom color scale we already created
    col_fill_historic_comparisons +
    # label axis
    labs(x = "Year", y = "", fill = "Year") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )


  # build the combined figure
  combined_fig <- ggarrange(surface_violins_adult, surface_violins_juvenile, bottom_violins_adult, bottom_violins_juvenile,
    ncol = 2, nrow = 2, labels = c("A)", "B)", "C)", "D)"), font.label = list(size = 12, face = "bold", family = "Times")
  )

  # and return the combined fig
  return(list(combined_fig, region_name))
}
