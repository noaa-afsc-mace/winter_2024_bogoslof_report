# build the catch by species and numbers by gear and region as in table 5 Winter 2019 Shelikof report;
# this function is reliant on the data gathered in the 'get_catch_table_data' function that was called when gathering data

build_catch_table_by_region <- function(region_name) {
  # limit the catch data to the requested region
  catch_data_for_region <- catch_table_data %>%
    filter(region == region_name)

  # get a nice name for the caption from the event_data too
  region_caption <- as.character(unique(catch_data_for_region$region))

  ###############
  # function to build the table for each gear type

  make_table_for_gear_function <- function(gear_type) {
    # limit the catch to given gear type, and calculate the percent by wt/number for each catch item too
    catch_table_summary <- catch_data_for_region %>%
      filter(GEAR == gear_type) %>%
      # calculate the oercent wt/percent number
      mutate(
        percent_wt = TOT_WEIGHT / sum(TOT_WEIGHT, na.rm = TRUE) * 100,
        percent_num = TOT_NUMBER / sum(TOT_NUMBER, na.rm = TRUE) * 100
      ) %>%
      # make sure things are ordered by weight
      arrange(desc(TOT_WEIGHT)) %>%
      # drop the gear column- we don't need that in the table as a column;
      # also reorder columns for making the table
      select(COMMON_NAME, SCIENTIFIC_NAME, TOT_WEIGHT, percent_wt, TOT_NUMBER, percent_num, L, W)

    # add a few separators in for table formatting
    catch_table_summary <- cbind.data.frame(
      catch_table_summary$COMMON_NAME, catch_table_summary$SCIENTIFIC_NAME, catch_table_summary$TOT_WEIGHT, catch_table_summary$percent_wt,
      catch_table_summary$TOT_NUMBER, catch_table_summary$percent_num, "", catch_table_summary$L, catch_table_summary$W
    )
    colnames(catch_table_summary) <- c("COMMON_NAME", "SCIENTIFIC_NAME", "TOT_WEIGHT", "percent_wt", "TOT_NUMBER", "percent_num", "sep", "L", "W")


    # make the catch table into a flextable
    catch_table <- flextable(catch_table_summary)

    # get header rows in a dataframe that : 1) has the column names (col_keys),
    # 2), defines first row of header (row1, below), 3) defines second row (row2, below)
    header_keys <- data.frame(
      col_keys = c(
        "COMMON_NAME", "SCIENTIFIC_NAME", "TOT_WEIGHT", "percent_wt", "TOT_NUMBER", "percent_num", "",
        "L", "W"
      ),
      row1 = c("", "", "Catch", "", "", "", "", "Measurements", ""),
      row2 = c("Species name", "Scientific name", "Weight (kg)", "%", "Number", "%", "", "Length", "Weight"),
      stringsAsFactors = FALSE
    )

    # add the header
    catch_table <- set_header_df(catch_table, mapping = header_keys, key = "col_keys")

    # calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there are lots
    # of na's, which is fine)

    # also format things for the footer- 1000's separated by commas
    footer_row <- c(
      "Total",
      "",
      format(sum(catch_table_summary$TOT_WEIGHT, na.rm = TRUE),
        big.mark = ",", na_str = "-", nsmall = 1, digits = 1, scientific = FALSE
      ),
      "",
      format(sum(catch_table_summary$TOT_NUMBER, na.rm = TRUE),
        big.mark = ",", na_str = "-", nsmall = 0, scientific = FALSE
      ),
      "",
      "",
      format(sum(catch_table_summary$L, na.rm = TRUE),
        big.mark = ",", na_str = "-", nsmall = 0, scientific = FALSE
      ),
      format(sum(catch_table_summary$W, na.rm = TRUE),
        big.mark = ",", na_str = "-", nsmall = 0, scientific = FALSE
      )
    )


    # add the footer row
    catch_table <- add_footer_row(catch_table, top = FALSE, values = footer_row, colwidths = rep(1, length(catch_table_summary)))

    # merge the header cells for 'Catch' label and
    catch_table <- merge_at(catch_table, i = 1, j = 3:6, part = "header")
    catch_table <- merge_at(catch_table, i = 1, j = 8:9, part = "header")

    # get rid of padding around cells
    catch_table <- padding(catch_table, padding = 0, part = "all")

    # align text: center justify everything
    catch_table <- align(catch_table, align = "center", part = "all")

    # align header text: left for common name, scientific name
    catch_table <- align(catch_table, j = 1:2, align = "left", part = "all")
    # center everything else in 2nd row
    catch_table <- align(catch_table, j = 3:9, align = "center", part = "all")

    # add a border above and below caption and at bottom of table
    table_border <- fp_border(color = "black", width = 0.75)

    # add horizontal border on top and bottom of table
    catch_table <- hline_top(catch_table, part = "all", border = table_border)
    catch_table <- hline_bottom(catch_table, part = "all", border = table_border)

    # add a line under things in the first row too
    catch_table <- hline(catch_table, i = 1, j = c(3:6, 8:9), part = "header", border = table_border)


    # manually fix column widths to end up w/a ~7.5 in table
    # catch_table = fit_to_width(catch_table, max_width = 7.5)
    # separators can be really thin, make smaller numbers narrow as well
    catch_table <- width(catch_table, j = c("sep"), width = 0.05)
    # separators need to be bigger for some columns to fit caption
    catch_table <- width(catch_table, j = c("percent_wt", "TOT_NUMBER", "percent_num", "L", "W"), width = 0.5)
    catch_table <- width(catch_table, j = c("COMMON_NAME", "SCIENTIFIC_NAME"), width = 1.75)
    catch_table <- width(catch_table, j = c("TOT_WEIGHT"), width = 0.75)

    # format catch weights and percents and numbers percents conditionally: If values are <=0.1,
    # label as '<0.1'
    # otherwise, format the value as having a single decimal point for display
    catch_table <- set_formatter(catch_table,
      TOT_WEIGHT = function(x) {
        ifelse(x > 0.1, formatC(x, format = "f", digits = 1, big.mark = ","), "<0.1")
      },
      TOT_NUMBER = function(x) {
        ifelse(x > 0.1, formatC(x, format = "d", big.mark = ","), "<0.1")
      },
      percent_wt = function(x) {
        ifelse(x > 0.1, formatC(x, format = "f", digits = 1, big.mark = ","), "<0.1")
      },
      percent_num = function(x) {
        ifelse(x > 0.1, formatC(x, format = "f", digits = 1, big.mark = ","), "<0.1")
      }
    )


    # format individual measurements columns; add '-' where no data exists
    # whole integer columns
    catch_table <- colformat_double(catch_table, j = c("L", "W"), digits = 0, na_str = "-", big.mark = ",")

    # try to format the scientific names correctly
    # Rule: if the species name doesn't have '(' as in (class), (order), etc, don't italicize
    # but allow 'sp.' to be italicized
    # this should catch and italicize species names
    catch_table <- italic(catch_table,
      i = ~ (!grepl("\\(", SCIENTIFIC_NAME)),
      j = ~SCIENTIFIC_NAME,
      part = "body"
    )


    # gather the total number of hauls for the caption text
    n_hauls <- length(current_year_event_data$EVENT_ID[current_year_event_data$region == region_name & current_year_event_data$`Gear type` == gear_type])

    # build the caption
    cap_text <- paste0(
      "Catch by species and numbers of length and weight measurements taken from ", n_hauls, " ", gear_type,
      " hauls during the ", unique(current_year_event_data$year), " acoustic-trawl survey of walleye pollock in ", region_caption,
      "."
    )

    # set the font and font size
    catch_table <- flextable::font(catch_table, fontname = "times", part = "all")
    catch_table <- fontsize(catch_table, size = 10, part = "header")
    catch_table <- fontsize(catch_table, size = 10, part = "body")

    # return the table and the caption in a list
    return(list(catch_table, cap_text, gear_type))
  }

  # apply this function to make a table for each gear type in the region
  map(sort(unique(catch_data_for_region$GEAR)), make_table_for_gear_function)
}
