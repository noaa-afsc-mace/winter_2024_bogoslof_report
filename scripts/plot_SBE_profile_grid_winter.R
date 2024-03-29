
plot_SBE_profile_grid_winter = function(historical_sbe_data, event_data, current_survey, core_survey_area){

# #   historical_sbe_data = all_sbe_data
# # current_survey = survey
# whole_deal = c(core_survey_area, 'EBS Northern Extension')
# 
# 
# historical_sbe_data_tidy = historical_sbe_data%>%
#   mutate(region = case_when(region == 'SCA' ~ 'East of 170',
#                             region == 'West of 170' ~ 'West of 170',
#                             region == 'East of 170' ~ 'East of 170',
#                             region == 'EBS Northern Extension' ~ 'EBS Northern Extension',
#                             region == 'EBS Cross Transect' ~ 'EBS Cross Transect'))
# 
# 
# event_data = event_data%>%
#   mutate(region = case_when(region == 'SCA' ~ 'East of 170',
#                             region == 'West of 170' ~ 'West of 170',
#                             region == 'East of 170' ~ 'East of 170',
#                             region == 'EBS Northern Extension' ~ 'EBS Northern Extension' ,
#                             region == 'EBS Cross Transect' ~ 'EBS Cross Transect'))%>%
#   filter(region %in% c(whole_deal))

# # #make this for core areas- E/W 170 (SCA), N. Extension to 0.5m, sort the regions so they make sense
# event_data = biomass_data%>%
#   filter(region %in% c(core_survey_area, "EBS Northern Extension"))%>%
#   arrange(match(region, c("East of 170", "West of 170",  "EBS Northern Extension")))

# #get the SBE temperature and depth records for each of these region in the current survey and region
# sbe_data = historical_sbe_data_tidy%>%
#   filter(SURVEY == current_survey & region %in% c(whole_deal))

#get the SBE temperature and depth records for each of these regions in the
  # current survey and region- no need to filter anything out for winter (I don't think?)
sbe_data = historical_sbe_data

# %>%
#   filter(SURVEY == current_survey & region %in% c(whole_deal))

# sbe_data = historical_sbe_data%>%
#   filter(SURVEY == current_survey)

#if there's SBE data in the region, plot it
#if (nrow(sbe_data) > 0){
  
  ########
  
  #this function takes the SBE data from a haul and cleans it up as in the old MATLAB plot:
  #1. Limits data to the 'down' path of the trawl,from the last time it is at 1m depth to the maximum depth
  #2. Bins readings to 1m depth bins
  #3. In cases where bins are missing, does a simple linear interpolation to fill in blanks
  
  get_haul_sbe_function = function(haul_number){
    
    #get the data for that haul only
    haul_data = sbe_data%>%
      filter(EVENT_ID == haul_number)
    
    #add a check- only keep hauls with more than 5 records > 5m depth; otherwise, don't report the haul
    if (nrow(haul_data[haul_data$DEPTH >5,]) >5){
      
      #get a summary of the data: first limit the data to the portion of the trawl we want to use
      haul_summary = haul_data%>%
        #only keep the period where the net is going from the surface to maximum depth (to be consistent with old scripts)
        #first get rid of anything below max
        slice(1:which.max(DEPTH))%>%
        #from the downcast, keep data from the last time the net goes below 1m depth (assume shallower than this is 'on deck' time, etc)
        #to the maximum depth (i.e. the way down only- in keeping with previous analyses)
        slice(tail(which(DEPTH<1),1)+1:which.max(DEPTH))
      
      
      
      #get a vector of depths from min:max depth within this segment of the trawl;
      #this will get used to interpolate temps in bins where there are no temps
      #depths_in_haul = as.data.frame(seq(round(min(haul_summary$SBEDepth), digits = 0),max(ceiling(haul_summary$SBEDepth)), 1))
      depths_in_haul = as.data.frame(seq(round(min(haul_summary$DEPTH), digits = 0), round(max(haul_summary$DEPTH), digits = 0), 1))
      colnames(depths_in_haul) = c('bin_depth')
      depths_in_haul$EVENT_ID = haul_number
      
      #use these depths to interpolate cases where we don't have a depth in a bin
      haul_summary = haul_summary%>%
        #bin depths by 1m bins
        mutate(bin_depth = round(DEPTH, digits = 0))%>%
        #take the average temp in each bin
        group_by(bin_depth)%>%
        summarize(temp_av = mean(TEMPERATURE))%>%
        #add the complete vector of depths, for interpolating missing depths (again in keeping with previous analyses)
        full_join(depths_in_haul, by = c('bin_depth'))%>%
        arrange(bin_depth)%>%
        #get a simple linear interpolation of the temperature in missing bins
        mutate(interp_temps = (approx(bin_depth, temp_av, xout = bin_depth, ties = 'mean', method = 'linear')$y))%>%
        #keep the 'real' temp where measured, interpolated where it wasn't
        mutate(plot_temp = ifelse(is.na(temp_av), interp_temps, temp_av))
      
      #return this dataframe
      return(haul_summary)
    }
    
    #if there's under 5 observations for the haul, something probably went wrong w/SBE. Move on
    if (nrow(haul_data[haul_data$DEPTH <= 20,]) >5){
      return(NULL)
    }
    
  }
  
  
  #apply this function to every haul in the sbe data
  sbe_summary = map_df(unique(sbe_data$EVENT_ID), get_haul_sbe_function)
  
  #add the assigned region to each haul
  sbe_summary = left_join(sbe_summary, event_data, by = c('EVENT_ID'))

  haul_count = sbe_summary%>%
    group_by(region, EVENT_ID)%>%
    summarise()%>%
    count()
  
  sbe_summary =  right_join(sbe_summary, haul_count, by = 'region')
  
  # #for the report plot, get a mean temp +/- 1 SD in each depth bin
  # temp_plot_data = sbe_summary%>%
  #   group_by(region, bin_depth)%>%
  #   summarize(temp_c = mean(plot_temp), st_dev = sd(plot_temp, na.rm = TRUE))%>%
  #   #this sorts the regions in the order you want them plotted
  #   arrange(match(region, c("East of 170", "West of 170",  "EBS Northern Extension")))
 
  
  #for the report plot, get a mean temp +/- 1 SD in each depth bin
  temp_plot_data = sbe_summary%>%
    group_by(region, bin_depth, n)%>%
    summarize(temp_c = mean(plot_temp), st_dev = sd(plot_temp, na.rm = TRUE))%>%
    ungroup() %>%
    mutate(region = paste0(region, "; n=", n))
  
  
 # regionlabels = c("East of 170",  "West of 170",  "EBS Northern Extension")
  
  regionlabels = sbe_region_list
  
  
  #factor(region, levels = regionlabels, labels = regionlabels)
  
  #plot it
  sbe_plot = 
    ggplot(temp_plot_data, aes(x = temp_c, y = bin_depth))+
    geom_path(linewidth = 1)+
    scale_y_reverse(breaks = seq(0, ceiling(round(max(temp_plot_data$bin_depth), digits = -1))+25, 50),
                    limits = c(ceiling(round(max(temp_plot_data$bin_depth), digits = -1))+25, 0),
                    expand = c(0,0))+ #make sure ticks at 25 m intervals, start at 0 depth
    geom_ribbon(aes(xmin= (temp_c- st_dev), xmax = (temp_c + st_dev)), fill = 'grey20', linetype = 2, alpha = 0.3)+
    #facet_wrap(~factor (region, levels = c(regionlabels)))+
    facet_wrap(~region)+
    
    scale_x_continuous(position = "top", 
                       limits = c(round(floor(min(temp_plot_data$temp_c- temp_plot_data$st_dev)), digits = 0), 
                                  round(ceiling(max(temp_plot_data$temp_c+ temp_plot_data$st_dev)), digits = 0)))+
    labs(x = expression(' Temperature ('*~degree*C*')'), y = 'Depth (m)')+
    # annotate("text", x = max(temp_plot_data$temp_c)*.8, y = max(temp_plot_data$bin_depth) *.9, 
    #          # label = paste0(unique(event_data$region[event_data$report_number == report_num]),
    #          #                     ' \n(n = ', length(unique(sbe_summary$EVENT_ID)), ' hauls)'), fontface = 'bold')+
    #          label = paste0('(n = ', length(unique(sbe_summary$EVENT_ID)), ')'), fontface = 'bold', family = 'Times', size = 4)+
    theme_bw()+
    #theme(legend.key.width = unit(.75, "in"))+
    theme(text=element_text(family="Times"),
          axis.text=element_text(size=10), 
          axis.title = element_text(size = 10),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
  
#}

    # sbe_plot
  
  
# #if there's no SBE data for the region, note with empty plot
# if (nrow(sbe_data) == 0){
#   
#   sbe_plot=
#     ggplot()+
#     geom_point(aes(x = 1, y = 1), color = 'transparent')+
#     annotate(x = 1, y = 1, geom = 'text', label = 'No SBE \nprofile data',
#              fontface = 'bold', family = 'Times', size = 4)+
#     theme_void()
#   
#   
  return(list(sbe_plot, sbe_summary))
  
}

#return the plot
