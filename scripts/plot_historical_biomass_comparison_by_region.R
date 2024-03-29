# compare the historical surveys with the current year for shelikof, marmot, chirikof, as in Figure 24, SH1904 report

# again split things up into a pre- and post- selectivity corrected era...
# the post-selectivity era is queried from macebase2, in case analyses are updated further.
# the pre-selectivity era is kept in a .rda- static values that won't change.

plot_historical_biomass_comparison_by_region <- function(pre_sel_corr_data_path, post_sel_corr_data, historical_eva_vals, region_name) {
  #########
  # 1: open up the old pre-selectivity corrected data
  pre_sel_corr_biomass <- readRDS(pre_sel_corr_data_path)
  
  # sum up by year, region, and limit to the requested region
  pre_sel_corr_biomass <- pre_sel_corr_biomass %>%
    filter(region == region_name) %>%
    group_by(year, region) %>%
    summarize(biomass_thousand_tons = sum(biomass_thousand_tons))

  ########
  # 2. sum up the post-selectivity corrected data
  sel_corr_biomass <- post_sel_corr_data %>%
    filter(region == region_name) %>%
    group_by(year, region) %>%
    summarize(biomass_thousand_tons = sum(BIOMASS) / 1e6)
  
  # to be safe, make sure there's no post-selectivity corrected biomass in the historical data!
  pre_sel_corr_biomass <- pre_sel_corr_biomass %>%
    filter(year < min(sel_corr_biomass$year))

  ############
  # 3. Combine the pre- and post- selectivity corrected data
  plot_biomass <- rbind.data.frame(pre_sel_corr_biomass, sel_corr_biomass)

  #########
  
  # add EVA estimates & convert EVA to numeric values
  plot_dat <- left_join(x = plot_biomass, y = historical_eva_vals, by = c("year" = "Year", "region"))
  plot_dat$eva_percent <- as.numeric(plot_dat$eva_percent) / 100

  # calculate 95% CI from EVA estimate following Honkalehto et al. 2011 CJFAS
  plot_dat$eva_ucl <- plot_dat$biomass_thousand_tons + (plot_dat$biomass_thousand_tons * plot_dat$eva_percent * 1.96)
  plot_dat$eva_lcl <- plot_dat$biomass_thousand_tons - (plot_dat$biomass_thousand_tons * plot_dat$eva_percent * 1.96)

  first_year <- min(plot_dat$year) 
  
  thisyear <- plot_dat%>%
    filter(year == current_year)
  
  Time_series <-
    ggplot(plot_dat, aes(year, biomass_thousand_tons)) +
    geom_ribbon(aes(ymin = eva_lcl, ymax = eva_ucl), fill = "grey")+
    geom_line() +
    geom_point(size = 3) +
    geom_point(data = thisyear, aes(x = year, y = biomass_thousand_tons, size = 3, color = "red")) +
    # label every other year, but add ticks at every year
    # add ticks at beginning and end of each year, but only label one of them with year
    scale_x_continuous(breaks=seq(min(plot_dat$year), max(plot_dat$year),2)
                       #   breaks = c(seq(min(plot_dat$Year), max(plot_dat$Year), 1), x_tick),
                       #   labels = seq(min(plot_dat$Year), max(plot_dat$Year), 5)
                       #   expand = expansion(add = .1)
    ) +
    labs(x = "Year", y = "Biomass (thousand tons)") +
    theme_bw() +
    cruise_report_plots_theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 270, hjust = 0.5, vjust = 0.5)
    )
  
  # and print to the plot
  return(list(Time_series, region_name))
}
