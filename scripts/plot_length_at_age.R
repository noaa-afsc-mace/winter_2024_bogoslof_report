# this will plot a combined length and weight at age panel, with comparisons to historical data where possible
# this is intentionally limited to 

plot_length_at_age <- function(length_weight_data, current_survey, region_name) {
  
  #limit the hauls in a given region; compute a summary for the most recent survey
  age_summary <- length_weight_data %>%
    filter(region == region_name & SURVEY == current_survey) %>%
    # only retain the aged fish
    filter(!is.na(AGE)) %>%
    group_by(region, AGE) %>%
    summarize(
      mean_wt_at_age = mean(ORGANISM_WEIGHT, na.rm = TRUE),
      sd_wt_at_age = sd(ORGANISM_WEIGHT, na.rm = TRUE),
      mean_length_at_age = mean(FORK_LENGTH, na.rm = TRUE),
      sd_length_at_age = sd(FORK_LENGTH, na.rm = TRUE),
      n_wt_at_age = n()
    )
  
  # if the region doesn't have ages, stop!
  if (nrow(age_summary) ==0){
    stop(paste0('No age data for region ', region_name, '. Go check that out!'))
  }
  
  # for plotting, plot the 'old' surveys separately than the current survey as points
  historical_survey_comparison <- length_weight_data %>%
    filter(region == region_name & SURVEY != current_survey & !is.na(AGE))
  
  # add a simple mean line
  historical_mean <- historical_survey_comparison %>%
    group_by(AGE) %>%
    summarize(
      mean_length = mean(FORK_LENGTH),
      sd_length = sd(FORK_LENGTH),
      mean_wt = mean(ORGANISM_WEIGHT),
      sd_wt = sd(ORGANISM_WEIGHT)
    )

  # if there's data available, plot it
  mean_length_at_age <-
    ggplot() +
    geom_point(
      data = historical_survey_comparison, aes(x = AGE, y = FORK_LENGTH), color = "grey80",
      shape = 0, size = 2
    ) +
    geom_ribbon(
      data = historical_mean, aes(
        x = AGE, ymin = mean_length - sd_length,
        ymax = mean_length + sd_length
      ),
      color = "grey80", fill = "grey80", alpha = 0.5
    ) +
    geom_point(
      data = age_summary, aes(x = AGE, y = mean_length_at_age), color = "red",
      fill = "red", shape = 22, size = 2
    ) +
    geom_errorbar(
      data = age_summary, aes(
        x = AGE, ymin = mean_length_at_age - sd_length_at_age,
        ymax = mean_length_at_age + sd_length_at_age
      ),
      color = "red", width = 0.2
    ) +
    # add the historical trendline
    geom_line(data = historical_mean, aes(x = AGE, y = mean_length), color = "black") +
    # set x axis limits as 1 year greater than the oldest fish in the current survey; this will exclude bigger fish from the past
    # and you'll get a 'removed XXXXX rows' warning; this is intentional
    scale_x_continuous(limits = c(1, max(age_summary$AGE) + 1), breaks = seq(0, max(age_summary$AGE) + 1, 1)) +
    # on y-axis, label every 10 cm, from 0-80 cm
    scale_y_continuous(breaks = seq(0, 80, 10)) +
    labs(x = NULL, y = "Fork length (cm)") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12, family = "Times"),
      axis.title = element_text(size = 12, family = "Times"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    ) +
    cruise_report_plots_theme
  
  # for wt-at-age, set y axis as the maximum weight in the historical comparison, unless there isn't any historical data in this region!
  if (nrow(historical_survey_comparison) > 1) {
    wt_age_y_lims <- scale_y_continuous(breaks = seq(0, ceiling(max(historical_survey_comparison$ORGANISM_WEIGHT)), 0.5))
  }
  
  if (nrow(historical_survey_comparison) < 1) {
    wt_age_y_lims <- scale_y_continuous(breaks = seq(0, ceiling(max(age_summary$mean_wt_at_age)), 0.5))
  }
  
  mean_wt_at_age <-
    ggplot() +
    geom_point(
      data = historical_survey_comparison, aes(x = AGE, y = ORGANISM_WEIGHT),
      color = "grey80", shape = 0, size = 2
    ) +
    geom_point(
      data = age_summary, aes(x = AGE, y = mean_wt_at_age),
      color = "red", fill = "red", shape = 22, size = 2
    ) +
    geom_ribbon(
      data = historical_mean, aes(
        x = AGE, ymin = mean_wt - sd_wt,
        ymax = mean_wt + sd_wt
      ),
      color = "grey80", fill = "grey80", alpha = 0.5
    ) +
    geom_errorbar(
      data = age_summary, aes(x = AGE, ymin = mean_wt_at_age - sd_wt_at_age, ymax = mean_wt_at_age + sd_wt_at_age),
      color = "red", width = 0.2
    ) +
    # add the historical mean trendline
    geom_line(data = historical_mean, aes(x = AGE, y = mean_wt), color = "black") +
    # set x axis limits as 1 year greater than the oldest fish in the current survey; this will exclude bigger fish from the past
    # and you'll get a 'removed XXXXX rows' warning; this is intentional
    scale_x_continuous(limits = c(1, max(age_summary$AGE) + 1), breaks = seq(0, max(age_summary$AGE) + 1, 1)) +
    # on y-axis, label every 0.5 kg
    wt_age_y_lims +
    labs(x = "Age", y = "Weight (kg)") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12, family = "Times"),
      axis.title = element_text(size = 12, family = "Times"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    ) +
    cruise_report_plots_theme
  
  # combine figures
  length_at_age_plot <- ggarrange(mean_length_at_age, mean_wt_at_age,
                                  ncol = 1, nrow = 2,
                                  labels = c("A)", "B)"), vjust = 1.0,
                                  font.label = list(family = "Times", size = 12)
  )
  
  # return this figure
  return(list(length_at_age_plot, age_summary, historical_mean, region_name))

}


