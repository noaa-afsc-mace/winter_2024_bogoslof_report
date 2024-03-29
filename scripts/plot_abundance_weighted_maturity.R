plot_abundance_weighted_maturity <- function(ship, survey, region_id) {
  # #limit proportion mature data to the requested report
  # maturity_data = prop_mature_data%>%
  #   filter(REPORT_NUMBER == report_num)

  # limit proportion mature data to the requested report
  maturity_data <- prop_mature %>%
    filter(region == region_id)

  # and get the haul weights from this
  hauls <- maturity_data %>%
    distinct(EVENT_ID, WEIGHTS)

  # and use the hauls in this dataset to limit the maturities and weights data to hauls in report
  maturities_and_weights_data <- maturities_and_weights %>%
    filter(SURVEY == survey) %>%
    filter(HAUL %in% unique(maturity_data$EVENT_ID))

  ####################################
  # 1. Plot the maturity-at-length based on the data gathered in 'open_and_update_maturity_data.R'

  # run the model
  glm.out <- glm(formula = cbind(MATURE, IMMATURE) ~ FORK_LENGTH, data = maturity_data, weights = WEIGHTS, family = binomial(logit))
  summary(glm.out)

  # plot the results

  # get each of the unique lengths
  lengths <- sort(unique(maturity_data$FORK_LENGTH))
  # and an index position for each length
  ind <- match(lengths, maturity_data$FORK_LENGTH)
  # get the fitted values at this length- this works because there is only one unique fitted value per length
  glm_fitted_values <- glm.out$fitted.values[ind]

  # combine lengths and values into a dataframe for  plotting
  glm_vals <- cbind.data.frame(lengths, glm_fitted_values)

  # #plot observed and predicted maturity values as a test here
  # plot(glm_vals$lengths, glm_vals$glm_fitted_values, type = 'l')
  # points(maturity_data$FORK_LENGTH,maturity_data$MATURE/maturity_data$NUM_SAMPLED)
  # text(20,0.8,paste("L50 = ",toString(round(-glm.out$coefficients[1]/glm.out$coefficients[2],digits=2))))

  # remake the same plot for the report-

  GLM_maturities_fig <-
    ggplot(glm_vals, aes(x = lengths, y = glm_fitted_values, xmin = 20, xmax = 70)) +
    geom_line(aes(color = "Predicted"), linewidth = 1.5) +
    geom_point(data = maturity_data, aes(x = FORK_LENGTH, y = MATURE / NUM_SAMPLED, color = "Observed"), shape = 1, size = 2) +
    # create a custom legend combining the lines and the points
    scale_color_manual(
      breaks = c("Observed", "Predicted"),
      values = c("Observed" = "#08306b", "Predicted" = "red"),
      guide = guide_legend(override.aes = list(
        linetype = c("blank", "solid"),
        shape = c(1, NA)
      ))
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      expand = c(0, 0.1)
    ) +
    labs(x = NULL, y = "Proportion Mature") +
    # add the model fit value
    annotate("text",
      x = min(glm_vals$lengths) + 2, y = 0.5,
      label = paste("~L[50]==~", round(-glm.out$coefficients[1] / glm.out$coefficients[2], digits = 1)), parse = TRUE
    ) +
    theme_bw() +
    # cruise_report_plots_theme+
    # move the legend, make the fonts a bit bigger
    theme(
      legend.position = c(.1, .8),
      axis.text = element_text(size = 12, family = "Times"),
      axis.title = element_text(size = 12, family = "Times"),
      text = element_text(size = 12, family = "Times"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      legend.title = element_blank(),
      legend.spacing.y = unit(0, "mm"),
      legend.background = element_blank(),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    ) # ,
  # legend.box.background = element_rect(colour = "black"))

  # save L50 and some basic stats for text
  L50 <- round(-glm.out$coefficients[1] / glm.out$coefficients[2], digits = 1)
  glm_summary <- cbind.data.frame(ship, survey, unique(maturity_data$REPORT_NUMBER), unique(maturity_data$region), L50, nrow(maturity_data))
  colnames(glm_summary) <- c("ship", "survey", "report_number", "region", "L50", "n")

  ############################
  ## get the haul weights for hauls that have unsexed fish > 40 cm.
  # This is important in cases with hauls that have a weight (i.e. caught fish) but didn't have any mature fish.
  # if these hauls are included, your total # of fish won't sum to 100%; the denominator for weighting should only
  # include hauls that actually had mature fish!
  mat_hauls_denominator_female <- maturities_and_weights_data %>%
    filter(SEX == "Female" & LENGTH > 40) %>%
    distinct(HAUL) %>%
    # join the haul weights
    left_join(hauls, by = c("HAUL" = "EVENT_ID"))

  mat_hauls_denominator_male <- maturities_and_weights_data %>%
    filter(SEX == "Male" & LENGTH > 40) %>%
    distinct(HAUL) %>%
    # join the haul weights
    left_join(hauls, by = c("HAUL" = "EVENT_ID"))

  # Determine maturities by stage for female & male separately, use standard 40cm maturity cutoff
  maturities_male <- maturities_and_weights_data %>%
    filter(SEX == "Male" & LENGTH > 40) %>%
    group_by(HAUL, SEX, MATURITY) %>%
    # calulcate the number at each maturity by haul, sex
    summarize(num_mat = length(MATURITY)) %>%
    # calculate the percent at each maturity by haul, sex
    mutate(percent_mat = num_mat * 100 / sum(num_mat)) %>%
    # join the haul weights
    left_join(hauls, by = c("HAUL" = "EVENT_ID")) %>%
    # weight the percent by maturity stage by, weighted by local abundance IN THE HAULS with mature fish only
    group_by(SEX, MATURITY) %>%
    summarize(wt_percent = sum(percent_mat * WEIGHTS, na.rm = TRUE) / sum(mat_hauls_denominator_male$WEIGHTS))

  # Determine maturities by stage for female & male separately, use standard 40cm maturity cutoff
  maturities_female <- maturities_and_weights_data %>%
    filter(SEX == "Female" & LENGTH > 40) %>%
    group_by(HAUL, SEX, MATURITY) %>%
    # calulcate the number at each maturity by haul, sex
    summarize(num_mat = length(MATURITY)) %>%
    # calculate the percent at each maturity by haul, sex
    mutate(percent_mat = num_mat * 100 / sum(num_mat)) %>%
    # join the haul weights
    left_join(hauls, by = c("HAUL" = "EVENT_ID")) %>%
    # weight the percent by maturity stage by, weighted by local abundance IN THE HAULS with mature fish only
    group_by(SEX, MATURITY) %>%
    summarize(wt_percent = sum(percent_mat * WEIGHTS, na.rm = TRUE) / sum(mat_hauls_denominator_female$WEIGHTS))

  # and add them together in one dataframe
  maturities <- rbind.data.frame(maturities_female, maturities_male)

  # create a dataframe that has all levels of possible data:
  # in any given survey, not all categories (i.e. 'immature male') will have fish, and
  # in these cases we want to plot a 0% instead of an empty category
  MATURITY <- rep(c("Immature", "Developing", "Prespawning", "Spawning", "Spent"), 2)
  SEX <- c(rep("Male", 5), rep("Female", 5))
  wt_percent <- rep(0, 10)
  all_categories <- cbind.data.frame(MATURITY, SEX)
  all_categories$MATURITY <- as.character(all_categories$MATURITY)
  all_categories$SEX <- as.character(all_categories$SEX)

  # add this dataframe to the real data with calculated percentages where they exist;
  maturities <- left_join(all_categories, maturities, by = c("MATURITY", "SEX"))

  # indicate empty categories with 0%
  maturities$wt_percent <- replace_na(maturities$wt_percent, 0)

  # get the sample sizes for males and females too
  sample_sizes <- maturities_and_weights_data %>%
    filter(SEX != "Unsexed" & LENGTH > 40) %>%
    group_by(SEX) %>%
    summarize(n = length(SEX))

  # reorder maturities and sexes- this is only for plotting to be consistent with our old plots
  maturities$MATURITY <- factor(maturities$MATURITY, levels = c("Immature", "Developing", "Prespawning", "Spawning", "Spent"), ordered = TRUE)
  maturities$SEX <- factor(maturities$SEX, levels = c("Male", "Female"), ordered = TRUE)

  # make the plot
  maturities_barplot <-
    ggplot(maturities, aes(x = MATURITY, y = wt_percent, fill = SEX)) +
    geom_col(position = position_dodge(), width = 0.9, color = "black") +
    geom_text(aes(label = paste(round(wt_percent, digits = 0), "%")), hjust = 0.3, vjust = -0.5, position = position_dodge(width = 1)) +
    scale_fill_manual(
      values = c("#08306b", "#7f2704"),
      labels = c(
        paste("Male n =", sample_sizes$n[sample_sizes$SEX == "Male"]),
        paste("Female n =", sample_sizes$n[sample_sizes$SEX == "Female"])
      )
    ) +
    # scale_x_discrete(drop=FALSE)+
    labs(x = "Maturity stage", y = "Percent") +
    scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
    theme_bw() +
    # cruise_report_plots_theme+
    # move the legend, make the fonts a bit bigger, get rid of box
    theme(
      legend.position = c(.16, .8),
      legend.key = element_rect(color = NA, fill = alpha("transparent", 0.5)),
      axis.text = element_text(size = 12, family = "Times"),
      axis.title = element_text(size = 12, family = "Times"),
      text = element_text(size = 12, family = "Times"),
      legend.title = element_blank(),
      legend.spacing.y = unit(0, "mm"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    )

  #########################################################################
  ########
  # 5. GSI data

  # save a file path to update the GSI's in the historical csv later in the script
  out_file_GSI <- historical_GSI_path
  # open the historical GSI
  historical_GSI <- readRDS(historical_GSI_path)

  # Determine GSI for females > 40cm maturity cutoff
  GSI <- maturities_and_weights_data %>%
    # only keep female fish >40 cm; also only keep the prespawning females!
    # in cases where we've collected ovaries, there may be non pre-spawners with weights that shouldn't be in GSI calc
    filter(SEX == "Female" & LENGTH > 40 & MATURITY == "Prespawning") %>%
    # calulcate female GSI as gonad wt/organism weight
    mutate(GSI = GONAD_WEIGHT / ORGANISM_WEIGHT) %>%
    # get rid of NAs (cases without both gonad and organism weight)
    filter(!is.na(GSI))

  # also calculate mean GSI and std dev, weighted by abundance

  # get the mean GSI on a per-haul basis
  GSI_stats <- GSI %>%
    group_by(HAUL) %>%
    summarize(
      mean_GSI = mean(GSI),
      n_GSI_fish = n()
    ) %>%
    # join the haul weights
    left_join(hauls, by = c("HAUL" = "EVENT_ID"))

  # get the mean weighted GSI using the haul weights generated in step 2 above
  mean_wtd_GSI <- sum(GSI_stats$mean_GSI * GSI_stats$WEIGHTS, na.rm = TRUE) / sum(GSI_stats$WEIGHTS, na.rm = TRUE)
  # get the standard deviation, also weighted by weights, as in Nate's Matlab code
  std_wtd_GSI <- sqrt(Hmisc::wtd.var(GSI_stats$mean_GSI, GSI_stats$WEIGHTS, method = "ML"))

  #####
  # Update the historical GSI values with what's been calculated here for the current surveys
  # this assumes the most recent run for the current survey is the best. If there are already values for the survey and region here,
  # it will overwrite these.

  # get the data to update (if any)
  current_data <- historical_GSI %>%
    filter(survey == unique(maturity_data$SURVEY) & region == unique(maturity_data$region))

  # if there is any, get rid of it
  if (nrow(current_data) > 0) {
    # remove the current data before updating it
    historical_GSI <- anti_join(historical_GSI, current_data, by = c("ship", "survey", "region", "mean_weighted_GSI", "std_weighted_GSI"))
  }

  # now update the historical data with the newest records
  update_historic_GSI <- cbind.data.frame(ship, survey, unique(maturity_data$region), mean_wtd_GSI, std_wtd_GSI)
  colnames(update_historic_GSI) <- c("ship", "survey", "region", "mean_weighted_GSI", "std_weighted_GSI")

  # add this to the historical data
  historical_update_GSI <- rbind.data.frame(historical_GSI, update_historic_GSI)

  # save the updated file
  saveRDS(historical_update_GSI, file = out_file_GSI)

  # get the historical weighted mean +/- SD to annotate plot

  # this mean and sd will not include the current survey, which is good
  summary_data <- historical_GSI %>%
    # and filter to applicable region
    filter(region == unique(maturity_data$region))

  # for plotting/calculation here- be sure that there aren't any 'future' surveys included (i.e. if a 2021 survey exists but user wants
  # a 2020 report). This will exclude the future from means
  if (nrow(summary_data) > 0) {
    summary_data <- summary_data[as.numeric(substr(summary_data$survey, 1, 4)) <= as.numeric(substr(survey, 1, 4)), ]
  }

  wtd_mean_without_current_survey <- mean(summary_data$mean_weighted_GSI, na.rm = TRUE)
  wtd_std_without_current_survey <- mean(summary_data$std_weighted_GSI, na.rm = TRUE)

  # store in a dataframe to return to text too

  # calculate total number of GSI fish for text:
  n_gsi_females <- sum(GSI_stats$n_GSI_fish)

  summary_gsi_stats <- cbind(update_historic_GSI, wtd_mean_without_current_survey, wtd_std_without_current_survey, n_gsi_females)

  # make the GSI plot
  GSI_plot <-
    ggplot(GSI, aes(x = LENGTH, y = GSI)) +
    geom_point(color = "#08306b", shape = 1, size = 2) +
    # copy the axes limits that are standard in the report
    scale_x_continuous(limits = c(20, 70), breaks = seq(20, 70, 10)) +
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, .3, .1)) +
    # add the mean, std, n
    annotate("text",
      x = 23, y = 0.25,
      label = paste0("Mean = ", round(mean_wtd_GSI, digits = 2), "\nStd. = ", round(std_wtd_GSI, digits = 2), "\nn = ", nrow(GSI))
    ) +
    geom_hline(yintercept = wtd_mean_without_current_survey) +
    geom_hline(yintercept = wtd_mean_without_current_survey - wtd_std_without_current_survey, linetype = "dashed") +
    geom_hline(yintercept = wtd_mean_without_current_survey + wtd_std_without_current_survey, linetype = "dashed") +
    geom_hline(yintercept = wtd_mean_without_current_survey) +
    labs(x = "Fork Length (cm)", y = "Gonatosomatic index") +
    theme_bw() +
    theme(
      legend.position = c(23, 0.25),
      axis.text = element_text(size = 12, family = "Times"),
      axis.title = element_text(size = 12, family = "Times"),
      text = element_text(size = 12, family = "Times"),
      legend.title = element_blank(),
      legend.spacing.y = unit(0, "mm"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))
    )

  # combine all 3 plots for presentation
  combined_fig <- ggarrange(maturities_barplot, GLM_maturities_fig, GSI_plot,
    ncol = 1,
    labels = c("A)", "B)", "C)"), vjust = 1, hjust = -1.25,
    font.label = list(family = "Times", size = 12)
  )

  # and print this figure
  # print(combined_fig)

  # return summary stats for text: proportion at each stage, L50, GSI, sample sizes
  summary_stats <- list(glm_summary, maturities, summary_gsi_stats, combined_fig, sample_sizes)

  return(summary_stats)
}
