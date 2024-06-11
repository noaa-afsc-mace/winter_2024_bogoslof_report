#################################################################################################
###
### Plot historical SST anomalies from trawl locations (loc) and ship's flow-through (scs)
###
#################################################################################################

plot_historical_sst_anomalies <- function(historical_sst_scs) {
  # open up the historical data
  historical_sst_scs_data <- historical_scs_sst

  
  # limit to the requested region
  plot_sst_scs <- historical_sst_scs_data

  
    
  # summarizing SCS temp data
  historical_comp_scs = plot_sst_scs %>% 
    group_by(year, region) %>%
    summarise(mean_sst_scs = mean(temperature, na.rm = TRUE), 
              sd_sst_scs = sd(temperature, na.rm = TRUE)) %>%
    mutate(plot_col = ifelse(unique(year) == current_year, "#d73027", "black")
    ) 
   
  
 
  ## Calculate SST summary stats for the period between 2006-2022 to standardize anomaly plots
  # ...
  # SCS values
  scs_mean <- mean(historical_comp_scs$mean_sst_scs[historical_comp_scs$year %in% c(2006:2022)], na.rm = TRUE)
  scs_sd <- sd(historical_comp_scs$mean_sst_scs[historical_comp_scs$year %in% c(2006:2022)], na.rm = TRUE)
  
  # Calculate SST anomaly indices based on 2006-2022 period, centered and scaled using 2009-2022 anomaly summary stats
  scs_index <- (historical_comp_scs$mean_sst_scs - scs_mean) / scs_sd
  
  # Save anomaly index values for export
  historical_comp_scs$sst_scs_anomaly <- scs_index
  
  
  year_anomaly = historical_comp_scs%>%
    group_by(year, region)%>%
    mutate(scspos = sst_scs_anomaly >= 0)
  

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

  
return(list(scs, historical_comp_scs))

}

