# compare the numbers- and biomass- at length for multiple analyses- this function will take a list of data sets and analyses to compare,
# and return a plot showing the differences by survey region
# This relies on the data gathered in the 'get_analysis_comparison_data' function
# NOTE: It also currently only is written to handle the case where you compare the selectivity-corrected dataset (primary) with a 
# secondary non-selectivity corrected dataset

# FOR BOGOSLOF - this is modified to sum across regions (no umnak/samalge)

plot_analyses_comparisons <- function(analysis_comparison_data,
                                      primary_dataset, 
                                      primary_analysis) {
  # report as numbers (millions) and biomass (thousands of tons), for each survey region and dataset/analysis combo
  comparison_summary <- analysis_comparison_data %>%
    group_by(DATA_SET_ID, ANALYSIS_ID, LENGTH) %>%
    mutate(num_millions = NUMBERS / 1e6, biomass_thousand_tons = BIOMASS / 1e6) %>%
    arrange(ANALYSIS_ID, LENGTH)
  
    # make the primary survey bigger and orange, the secondary comparisons smaller and in shades of grey

  # reorder with the primary data_set and analysis_id at the top of the list
  comparison_summary$comp_type <- ifelse(comparison_summary$DATA_SET_ID == primary_dataset & comparison_summary$ANALYSIS_ID == primary_analysis,
    "primary", "secondary"
  )

  # and reorder by comparison type- this will work because 'p' comes before 's' and we're sorting alphabetically
  comparison_summary <- comparison_summary %>%
    arrange(comp_type, ANALYSIS_ID, LENGTH)

  # also get summary dataframe in case folks want to make a summary table
  totals_comparison <- comparison_summary %>%
    group_by(ANALYSIS_ID) %>%
    summarize(
      num_millions = sum(num_millions, na.rm = TRUE),
      biomass_thousand_tons = sum(biomass_thousand_tons, na.rm = TRUE)
    )

  # get the totals per analysis
  get_totals <- function(analysis_id){
     # get the data limited to this region
    region_total <- totals_comparison %>%
      filter(ANALYSIS_ID == analysis_id)

    # get the numbers and biomass as a single 'chunk' of text
    # this relies on primary analysis being selectivity corrected!
    analysis_lab <- ifelse(analysis_id == primary_analysis,
                           "Selectivity corrected",
                           "Non selectivity corrected")
    
    nums_totals <- paste(paste0(analysis_lab, ": ", 
      format(region_total$num_millions, digits = 2, big.mark = ",", format = "d", nsmall = 1)
    ))
    biomass_totals <- paste(paste0(analysis_lab, ": ",
      format(region_total$biomass_thousand_tons,
        digits = 2, big.mark = ",", format = "d",
        nsmall = 1
      )
    ))

    totals_text <- cbind.data.frame(nums_totals, biomass_totals, stringsAsFactors = FALSE)
    totals_text$analysis_id <- unique(region_total$ANALYSIS_ID)

    return(totals_text)
    
  }
  
  # return a dataframe with a row for every analysis
  annotation_vals <- map_dfr(unique(totals_comparison$ANALYSIS_ID), get_totals)

  # get some good contrasting colors: this is just a list of 4 contrasting colors (because we're not likely to ever have that many reports on this plot...)
  # cols_list = c('#feb24c', 'grey30', 'grey70', 'grey80', 'grey80', 'grey90')
  cols_list <- c("blue", "red", "magenta", "cyan3", "salmon", "gray50")
  # and some linestyles too: again, just a list of 5 sizes
  linesize_list <- c(2, 1.0, 0.75, 0.75, 0.50, 0.50)
  lty <- c(1:3)
  linestyle_list <- c("solid", "dashed", "twodash", "F1", "dotted", "dotdash")

  # take as many colors as we have regions in the plot
  plot_cols <- cols_list[1:length(unique(comparison_summary$ANALYSIS_ID))]
  plot_lines <- linesize_list[1:length(unique(comparison_summary$ANALYSIS_ID))]
  plot_dashes <- linestyle_list[1:length(unique(comparison_summary$ANALYSIS_ID))]

  # add colors/lines/dashes to a named list, with the primary analysis at the front;
  # this is needed to make sure it will plot first, even if it has a higher number than comparison analyses
  names(plot_cols) <- c(primary_analysis, unique(comparison_summary$ANALYSIS_ID[comparison_summary$ANALYSIS_ID != primary_analysis]))
  names(plot_lines) <- c(primary_analysis, unique(comparison_summary$ANALYSIS_ID[comparison_summary$ANALYSIS_ID != primary_analysis]))
  names(plot_dashes) <- c(primary_analysis, unique(comparison_summary$ANALYSIS_ID[comparison_summary$ANALYSIS_ID != primary_analysis]))

  # #plot the numbers comparison as well
  numbers_comparison_plot <-
     ggplot()+
     geom_line(data = comparison_summary, aes(x = LENGTH, y = num_millions, 
                                              color = factor(ANALYSIS_ID), 
                                              linetype = factor(ANALYSIS_ID)),
                                              linewidth = 1.5)+
     #plot for each report number, and get the nice names for the plot
     #facet_wrap(~REPORT_NUMBER, scales = 'free', ncol = 1, labeller = as_labeller(get_names))+
     labs(x= 'Fork length (cm)', y = 'Numbers of fish (millions)')+
     scale_color_manual(values = plot_cols, name = 'Analysis number')+
     scale_linetype_manual(values = plot_dashes, name = 'Analysis number')+
     scale_x_continuous(limits = c(0,75), breaks = seq(0,75,10))+
     #add the totals labels
     annotate(geom = 'text', x = -Inf, y = Inf, label = 'Total abundance:', color = "black",
             hjust   = -0.1 , vjust = 1.2, size = 4, family = 'Times', fontface = 'bold') + 
     annotate(geom = 'text', x = -Inf, y = Inf, label = annotation_vals$nums_totals[1], color = plot_cols[1],
              hjust   = -0.1 , vjust = 3.2, size = 4, family = 'Times') +
     annotate(geom = 'text', x = -Inf, y = Inf, label = annotation_vals$nums_totals[2], color = plot_cols[2],
             hjust   = -0.1 , vjust = 5.2, size = 4, family = 'Times') +
     theme_bw()+
     guides(linetype =  "none", size =  "none", color = "none")+
     cruise_report_plots_theme+
     #move the legend, make the fonts a bit bigger
     theme(legend.position = 'none',
           axis.text=element_text(size=12, family = "Times"),
           axis.title = element_text(size=12, family = "Times"),
           text = element_text(size = 12, family = "Times"),
           strip.background = element_blank(),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           legend.spacing.y = unit(0, "mm"),
           legend.background = element_blank(),
           legend.box.background = element_blank(),
           axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
           axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5)))
  
  # #plot lines for each analysis: separate plots for biomass and numbers
  biomass_comparison_plot <-
    ggplot()+
    geom_line(data = comparison_summary, aes(x = LENGTH, y = biomass_thousand_tons, 
                                              color = factor(ANALYSIS_ID), 
                                              linetype = factor(ANALYSIS_ID)),
              linewidth = 1.5)+
    #plot for each report number, and get the nice names for the plot
    #facet_wrap(~REPORT_NUMBER, scales = 'free', ncol = 1, labeller = as_labeller(get_names))+
    labs(x= 'Length (cm)', y = 'Biomass (1000s t)')+
    scale_color_manual(values = plot_cols, name = 'Analysis Number')+
    scale_linetype_manual(values = plot_dashes, name = 'Analysis number')+
    scale_x_continuous(limits = c(0,75), breaks = seq(0,75,10))+
    #add the totals labels
    annotate(geom = 'text', x = -Inf, y = Inf, label = 'Total biomass:', color = "black",
             hjust   = -0.1 , vjust = 1.2, size = 4, family = 'Times', fontface = 'bold') + 
    annotate(geom = 'text', x = -Inf, y = Inf, label = annotation_vals$biomass_totals[1], color = plot_cols[1],
             hjust   = -0.1 , vjust = 3.2, size = 4, family = 'Times') +
    annotate(geom = 'text', x = -Inf, y = Inf, label = annotation_vals$biomass_totals[2], color = plot_cols[2],
             hjust   = -0.1 , vjust = 5.2, size = 4, family = 'Times') +
    theme_bw()+
    guides(linetype =  "none", size =  "none", color = "none")+
    #cruise_report_plots_theme+
    #move the legend, make the fonts a bit bigger
    theme(legend.position = 'none',
          axis.text=element_text(size=12, family = "Times"),
          axis.title = element_text(size=12, family = "Times"),
          text = element_text(size = 12, family = "Times"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.spacing.y = unit(0, "mm"),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 5)))

  # combine the 2 figures
  combined_plot = ggarrange(numbers_comparison_plot, biomass_comparison_plot, ncol = 1,
                           labels = c('A)', 'B)'), vjust = 1, hjust=-1.25,
                           common.legend = TRUE, legend = 'bottom',
                           font.label = list(family = 'Times', size = 12))

  
  # commenting out plot version for now to work for multiple regions
  
  ### Plot version
  # width = 6.5
  # height = 8
  # tiff(filename = paste0(getwd(),'/figures/analyses_comparisons.tif'), width=width*504, height=height*504, pointsize=12, type="cairo", units="px", res = 504, compression = "lzw")
  
  # # turn off plot display
  # win.metafile()
  # dev.control("enable") # enable display list
  # 
  # par(mfrow = c(2, 1), mar = c(3, 2, 1, 1), oma = c(1, 2.5, 1, 0))
  # # Number of analyses to be plotted
  # n_plot <- length(unique(comparison_summary$ANALYSIS_ID))
  # # Describe analyses - typically there should only be 2
  # if(n_plot==1) legend_descrip <- "selectivity corrected  "
  # if(n_plot==2) legend_descrip <- c("selectivity corrected  ", "no selectivity correct.")
  # if (n_plot > 2) {
  #   for (i in 3:n_plot) legend_descrip[i] <- append(legend_descrip, paste0("Analysis ", totals_comparison$ANALYSIS_ID[i]))
  # }
  # 
  # ## Abundance estimates
  # # define y-axis limits for numbers
  # ylim_num <- c(0, round(ceiling(max(comparison_summary$num_millions)), 0))
  # plot(
  #   x = comparison_summary$LENGTH[comparison_summary$ANALYSIS_ID == 1], y = comparison_summary$num_millions[comparison_summary$ANALYSIS_ID == 1], type = "l",
  #   xlim = c(0, 70), ann = F, xaxt = "n", xaxs = "i", ylim = ylim_num, yaxt = "n", col = cols_list[1], lty = lty[1], lwd = 2.5
  # )
  # if (n_plot > 1) {
  #   for (i in 2:n_plot) {
  #     lines(
  #       x = comparison_summary$LENGTH[comparison_summary$ANALYSIS_ID == i], y = comparison_summary$num_millions[comparison_summary$ANALYSIS_ID == i],
  #       col = cols_list[i], lty = lty[i], lwd = 2
  #     )
  #   }
  # }
  # legend( "topleft", xjust=0, col = cols_list[1:n_plot], lty = lty[1:n_plot], lwd = c(2, rep(1, n_plot - 1)), legend = paste0(legend_descrip, ": ", round(totals_comparison$num_millions, 1)),
  #         text.col = cols_list[1:n_plot], bty = "n", title = "Total abundance (millions of fish)", title.col = "black", title.adj = - 1)
  # axis(side = 2, las = 2, at = seq(0, ylim_num[2], 10), labels = T, cex.axis = 1)
  # axis(side = 1, at = seq(0, 70, 10), labels = T, lwd = 0, lwd.ticks = 0.75, tck = -0.04, cex.axis = 1)
  # mtext(side = 2, las = 2, outer = F, line = 1, font = 2, at = ylim_num[2] * 1.1, "A)")
  # mtext(side = 2, line = 3, outer = F, "Numbers of fish (millions)")
  # ## Biomass estimates
  # # define y-axis limits for numbers
  # ylim_bms <- c(0, round(ceiling(max(comparison_summary$biomass_thousand_tons)), 0))
  # plot(
  #   x = comparison_summary$LENGTH[comparison_summary$ANALYSIS_ID == 1], y = comparison_summary$biomass_thousand_tons[comparison_summary$ANALYSIS_ID == 1], type = "l",
  #   xlim = c(0, 70), ann = F, xaxt = "n", xaxs = "i", ylim = ylim_bms, yaxt = "n", col = cols_list[1], lty = lty[1], lwd = 2.5
  # )
  # if (n_plot > 1) {
  #   for (i in 2:n_plot) {
  #     lines(
  #       x = comparison_summary$LENGTH[comparison_summary$ANALYSIS_ID == i], y = comparison_summary$biomass_thousand_tons[comparison_summary$ANALYSIS_ID == i],
  #       col = cols_list[i], lty = lty[i], lwd = 2
  #     )
  #   }
  # }
  # legend( "topleft", xjust=0, col = cols_list[1:n_plot], lty = lty[1:n_plot], lwd = c(2, rep(1, n_plot - 1)), legend = paste0(legend_descrip, ": ", round(totals_comparison$biomass_thousand_tons, 1)),
  #   text.col = cols_list[1:n_plot], bty = "n", title = "Total biomass (1000s t)", title.col = "black"
  # )
  # axis(side = 2, las = 2, at = seq(0, ylim_bms[2], 5), labels = T, cex.axis = 1)
  # axis(side = 1, at = seq(0, 70, 10), labels = T, lwd = 0, lwd.ticks = 0.75, tck = -0.04, cex.axis = 1)
  # mtext(side = 2, las = 2, outer = F, line = 1, font = 2, at = ylim_bms[2] * 1.1, "B)")
  # mtext(side = 2, line = 3, outer = F, "Biomass (1000s t)")
  # mtext(side = 1, line = 2.5, outer = F, "Fork length (cm)")
  # 
  # # # Save 2x1 multipanel plot as object
  # combined_plot <- recordPlot()
  # # Turn plot display back on
  # dev.off()

  # print the combined figure
  # print(combined_plot)

  # return a list that contains both the plot and the comparisons
  returns_list <- list(combined_plot, totals_comparison)
}
