#################################################################################################
###
### Plot historical SST anomalies from trawl locations (loc) and ship's flow-through (scs)
###
#################################################################################################

plot_historical_sst_anomalies <- function(historical_sst_loc, historical_sst_scs, region) {
  # open up the historical data
  historical_sst_loc_data <- readRDS(historical_sst_loc)
  historical_sst_scs_data <- historical_scs_sst

  
  # limit to the requested region
  plot_sst_scs <- historical_sst_scs_data[historical_sst_scs_data$region == region, ]
  plot_sst_loc <- historical_sst_loc_data[historical_sst_loc_data$region == region, ]

  #only plot Shumagins after '95 sbecause earlier surveys not done in Feb
  if ("Shumagin Islands" %in% region) {
    plot_sst_loc <- filter(plot_sst_loc, year > 1994)
  }
  
  # summarizing trawl location temp data
  historical_comp_trawl = plot_sst_loc %>% 
    group_by(year, region) %>%
    summarise(mean_sst_loc = mean(surface_temp, na.rm = TRUE), 
              sd_sst_loc = sd(surface_temp, na.rm = TRUE)) %>%
    mutate(plot_col = ifelse(unique(year) == current_year, "#d73027", "black")
    )
    
  # summarizing SCS temp data
  historical_comp_scs = plot_sst_scs %>% 
    group_by(year, region) %>%
    summarise(mean_sst_scs = mean(temperature, na.rm = TRUE), 
              sd_sst_scs = sd(temperature, na.rm = TRUE)) %>%
    mutate(plot_col = ifelse(unique(year) == current_year, "#d73027", "black")
    ) 
   

  historical_comp <- full_join(historical_comp_trawl, historical_comp_scs)
  
 
  ## Calculate SST summary stats for the period between 2006-2022 to standardize anomaly plots
  # ...
  loc_mean <- mean(historical_comp$mean_sst_loc[historical_comp$year %in% c(2006:2022)], na.rm = TRUE)
  loc_sd <- sd(historical_comp$mean_sst_loc[historical_comp$year %in% c(2006:2022)], na.rm = TRUE)
  # SCS values
  scs_mean <- mean(historical_comp$mean_sst_scs[historical_comp$year %in% c(2006:2022)], na.rm = TRUE)
  scs_sd <- sd(historical_comp$mean_sst_scs[historical_comp$year %in% c(2006:2022)], na.rm = TRUE)
  
  # Calculate SST anomaly indices based on 2006-2022 period, centered and scaled using 2009-2022 anomaly summary stats
  loc_index <- (historical_comp$mean_sst_loc - loc_mean) / loc_sd
  scs_index <- (historical_comp$mean_sst_scs - scs_mean) / scs_sd
  
  # Save anomaly index values for export
  historical_comp$sst_loc_anomaly <- loc_index
  historical_comp$sst_scs_anomaly <- scs_index
  
  
  year_anomaly = historical_comp%>%
    group_by(year, region)%>%
    mutate(trawlpos = sst_loc_anomaly >= 0,
           scspos = sst_scs_anomaly >= 0)
  
  
  #This makes the anomalies plots from ggplot
  trawl <- ggplot(year_anomaly, aes(year, sst_loc_anomaly, fill = trawlpos))+
    theme_classic()+
    #theme(axis.text.x = element_blank())+
    geom_col(position = "identity", colour = "black", linewidth = 0.25)+
    scale_fill_manual(values = c("#0000cd", "#FF0000"), guide = "none")+
    scale_x_continuous(n.breaks = 8) +
    ylim(-2,2)+
    labs(
      title = "Trawl Locations",
      x = "Year",
      y = "SST Anomaly")+
    geom_hline(aes(yintercept=0))
  

 scs <- ggplot(year_anomaly, aes(year, sst_scs_anomaly, fill = scspos))+
    theme_classic()+
    geom_col(position = "identity", colour = "black", linewidth = 0.25)+
    scale_fill_manual(values = c("#0000cd", "#FF0000"), guide = "none")+
    scale_x_continuous(n.breaks = 8) +
   ylim(-2,2)+
   labs(
     title = "Entire Survey",
     x = "Year",
     y = "SST Anomaly")+
   geom_hline(aes(yintercept=0))

  
 # put the plots together using patchwork package 
 SST_anomalies <- (trawl/scs)
  
return(list(SST_anomalies, historical_comp))

}

