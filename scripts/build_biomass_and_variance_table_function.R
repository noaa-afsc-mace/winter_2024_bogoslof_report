# build the biomass totals and EVA estimates/year/region tables for the Shelikof survey

# because the old data is so heavily annotated and relys on lots of 'off books' stuff like EVA estimates that don't
# live in the database, we're just opening the old data as it was formatted (i.e. values from previous surveys are the static values
# as they were reported)

build_biomass_and_variance_tables <- function(pre_sel_corr_data_path,
                                              post_sel_corr_data,
                                              historical_eva_vals, 
                                              region_name) {
  #######
  # 1. Get the old pre-selectivity corrected estimates 
  pre_sel_corr_data <- readRDS(pre_sel_corr_data_path) 
  
  # 2. sum up the post-selectivity corrected data by year, region, and limit to the requested region
  pre_sel_corr_biomass <- pre_sel_corr_data %>%
    filter(region == region_name) %>%
    group_by(year, region) %>%
    summarize(biomass_thousand_tons = sum(biomass_thousand_tons)) %>%
    rename("Year" = "year")

  ########
  # 2. sum up the post-selectivity corrected data
  sel_corr_biomass <- post_sel_corr_data %>%
    filter(region == region_name) %>%
    group_by(year, region) %>%
    summarize(biomass_thousand_tons = sum(BIOMASS) / 1e6) %>%
    rename("Year" = "year")
  
  ###########
  
  # 3. combine pre- and post- selectivity-corrected data

  biomass_data <- rbind.data.frame(pre_sel_corr_biomass, sel_corr_biomass) 

  ###########
  # 4. build table, get the footnotes by region as needed
  # make sure every year is in the dataset, regardless of whether there was a survey or not...
  
  all_years <- data.frame(seq(min(as.numeric(biomass_data$Year)), max(as.numeric(biomass_data$Year)), 1))
  colnames(all_years) <- c("Year")
  
  # join all the years so that there are NAs where necessary
  biomass_data <- full_join(biomass_data, all_years, by = c("Year"))

  # add in the EVA estimates
  table_eva_vals <- historical_eva_vals %>%
    filter(region == region_name)
  
  # and format the values consistently
  table_eva_vals$eva_percent <- round(as.numeric(table_eva_vals$eva_percent), digits = 1)
  table_eva_vals$eva_percent <- ifelse(is.na(table_eva_vals$eva_percent), table_eva_vals$eva_percent,
                                            paste0(sprintf("%.1f", table_eva_vals$eva_percent), "%")
  )
  
  # merge and put each value in it's own column
  biomass_data <- left_join(biomass_data, historical_eva_vals, by = c("Year", "region"))
  
  # reorder by year, drop region as there is only one by definition
  biomass_data <- biomass_data %>%
    arrange(Year) %>%
    select(- region)
  
  #############
  # make into a flextable
  
  biomass_table <- flextable(biomass_data)
  
  # get header rows in a dataframe that : 1) has the column names (col_keys),
  # 2), defines first row of header (row1, below), 3) defines second row (row2, below)
  header_keys <- data.frame(
    col_keys = c(
      "Year", "biomass_thousand_tons", "eva_percent"),
    row1 = c("Year", region_name, ""),
    row2 = c("", "Biomass", "Est. error"),
    stringsAsFactors = FALSE
  )
  
  # define a border above and below caption and at bottom of table
  table_border <- officer::fp_border(color = "black", width = 0.75)
  
  # add the header
  biomass_table <- set_header_df(biomass_table, mapping = header_keys, key = "col_keys") %>%
  # merge the header cells for 'Catch' label and
    flextable::merge_at(i = 1, j = 2:3, part = "header") %>%
    # get rid of padding around cells
    flextable::padding(padding = 0, part = "body") %>%
      # align text: center justify everything
    flextable::align(align = "center", part = "all") %>%
    # format numbers- in the main table; 1000s separated with commas and empty values with '-'
    flextable::colformat_double(j = c("biomass_thousand_tons"),
                                big.mark = ",", digits = 1, na_str = " "
    ) %>%
    # format numbers- format the year to have no separating comma
    flextable::colformat_double(j = c("Year"), big.mark = "", digits = 0, na_str = " ") %>%
    # add horizontal border on top and bottom of table
    flextable::hline_top(part = "all", border = table_border) %>%
    flextable::hline_bottom(part = "all", border = table_border) %>%
    # add a line under things in the first row too
    flextable::hline(i = 1, j = c(2:3), part = "header", border = table_border) %>%
    # manually fix column widths to end up w/a ~7.0 in table
    # adjust everything to be 1 in to start
    flextable::width(width = 1) %>%
    flextable::width(j = c("Year"), width = 0.5) %>%
    # set the font and font size
    flextable::font(fontname = "times", part = "all") %>%
    flextable::fontsize(size = 10, part = "header") %>%
    flextable::fontsize(size = 10, part = "body") %>%
    # set the row height
    flextable::height(height = 0.15, part = "body") %>%
    flextable::hrule(rule = "exact", part = "body")

  
  if (region_name == "Marmot Bay"){
  
    # add footnotes to explain the marmot zigzag
    zigzag <- flextable::as_paragraph(paste0(
      "During these years, outer Marmot was surveyed in a zig-zag pattern, rather than parallel transects. Inner ",
      "Marmot was surveyed with parallel transects. Relative estimation error was determined by combining ",
      "estimation of error for biomass within the inner bay (1-D) and outer bay (2-D). "
    ))
    
    # for any years with zigzag, add the footnote:
    biomass_table <- footnote(biomass_table,
                              i = ~ Year %in% c("2016", "2018"),
                              j = c("biomass_thousand_tons"),
                              value = zigzag,
                              ref_symbols = c("1"),
                              part = "body"
    )
    
  }
  
  if (region_name %in% c("Morzhovoi Bay", "Pavlof Bay", "Sanak Trough", "Shumagin Islands")){
    
    # add footnotes to explain the problem years
    past_peak <- flextable::as_paragraph("Survey conducted after peak spawning had occurred.")
    partial_survey <- flextable::as_paragraph("Partial survey.")
    
    if (1994 %in% unique(biomass_data$Year)){
      
      biomass_table <- flextable::footnote(biomass_table,
                                           i = ~ Year == "1994",
                                           j = c("biomass_thousand_tons"),
                                           value = past_peak,
                                           ref_symbols = c("1"),
                                           part = "body"
      )
    }
    
    if (1996 %in% unique(biomass_data$Year)){
      
      biomass_table <- flextable::footnote(biomass_table,
                                           i = ~ Year == "1996",
                                           j = c("biomass_thousand_tons"),
                                           value = partial_survey,
                                           ref_symbols = c("2"),
                                           part = "body"
      )
      
    }
    
  }
  
  
  # build the caption
  cap_text <- paste0(
    "Estimates of walleye pollock biomass (thousands of metric tons) and relative estimation error for the ", region_name,  ". Estimates for ", min(sel_corr_biomass$Year), "-", max(sel_corr_biomass$Year), " reflect selectivity corrections for escapement of juveniles. Blank values indicate no survey or estimation error was completed within a given region and year."
  )
  
  return(list(biomass_table, cap_text))
  
}
