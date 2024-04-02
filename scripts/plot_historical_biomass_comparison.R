# compare the historical surveys for BOGOSLOF- this differs from the Shelikof report where things are 'pre-'
# and 'post-' selectivity corrected!
# all data is queried from macebase2, if you want records prior to 1994 they will need to be appended in from elsewhere.



plot_historical_biomass_comparison <- function(timeseries_data, historical_eva_vals) {
  
  ########
  # 2. sum up the data by year- for the recent-ish past only! Things don't equal what's in database prior to 2002 
  # (survey region is quite variable)
  biomass_by_year <- timeseries_data %>%
    group_by(year) %>%
    summarize(biomass_thousand_tons = sum(BIOMASS) / 1e6) %>%
    arrange(year)
  
  #########

  # add EVA estimates & convert EVA to numeric values
  plot_dat <- left_join(x = biomass_by_year, y = historical_eva_vals, by = c("year" = "Year"))
  plot_dat$eva_prop <- as.numeric(plot_dat$eva_percent) / 100

  # calculate 95% CI from EVA estimate following Honkalehto et al. 2011 CJFAS
  plot_dat$eva_ucl <- plot_dat$biomass_thousand_tons + (plot_dat$biomass_thousand_tons * plot_dat$eva_prop * 1.96)
  plot_dat$eva_lcl <- plot_dat$biomass_thousand_tons - (plot_dat$biomass_thousand_tons * plot_dat$eva_prop * 1.96)

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
    scale_x_continuous(breaks= seq(min(plot_dat$year), max(plot_dat$year), 1))+
    labs(x = "Year", y = "Biomass (thousand tons)") +
    theme_bw() +
    cruise_report_plots_theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 270, hjust = 0.5, vjust = 0.5)
    )
  
  # and print to the plot
  return(Time_series)
}
