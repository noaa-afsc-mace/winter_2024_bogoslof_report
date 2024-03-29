# build the pollock-only specimen table, as in Table 4, Winter 2019 Shelikof report; it relies on the dataframe
# gathered by the funtion 'get_specimen_table_data'

build_specimen_table_function <- function(specimen_table_data) {
  # reorder table with columns where they should be
  specimen_data <- specimen_table_data[, c(
    "HAUL", "region", "CATCH_LENGTHS", "WEIGHTS", "MATURITIES",
    "OTOLITHS", "OVARY_WEIGHTS", "O_TAKEN", "S_TAKEN"
  )]


  # we only occasionally collect stomachs. In surveys where we don't collect any, make a table without this column;
  # in surveys with stomachs, add this column


  # for surveys without stomach collection, make this table
  if (sum(specimen_data$S_TAKEN, na.rm = TRUE) < 1) {
    specimen_data <- specimen_data %>%
      select(-S_TAKEN)

    # make specimen data into a flextable
    specimen_table <- flextable(specimen_data)


    # get header rows in a dataframe that : 1) has the column names (col_keys),
    # 2), defines first row of header (row1, below), 3) defines second row (row2, below)
    header_keys <- data.frame(
      col_keys = c(
        "HAUL", "region", "CATCH_LENGTHS", "WEIGHTS", "MATURITIES", "OTOLITHS",
        "OVARY_WEIGHTS", "O_TAKEN"
      ),
      row1 = c("Haul", "Region", "Catch", "", "", "", "Ovary", "Ovaries"),
      row2 = c("no.", "name", "lengths", "Weights", "Maturities", "Otoliths", "weights", "collected"),
      stringsAsFactors = FALSE
    )

    # add the header
    specimen_table <- set_header_df(specimen_table, mapping = header_keys, key = "col_keys")


    # calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there are lots
    # of na's, which is fine)

    # also format things for the footer- 1000's separated by commas
    footer_row <- c(
      "Total",
      "",
      format(sum(specimen_data$CATCH_LENGTHS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$WEIGHTS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$MATURITIES, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$OTOLITHS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$OVARY_WEIGHTS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$O_TAKEN, na.rm = TRUE), big.mark = ",", na_str = "-")
    )

    # add the footer row
    specimen_table <- add_footer_row(specimen_table, top = FALSE, values = footer_row, colwidths = rep(1, length(specimen_data)))


    # add a border above and below caption and at bottom of table
    table_border <- fp_border(color = "black")

    # add horizontal border on top and bottom of table
    specimen_table <- hline_top(specimen_table, part = "all", border = table_border)
    specimen_table <- hline_bottom(specimen_table, part = "all", border = table_border)

    # #set row height as a pretty small height in the header
    # specimen_table = height(specimen_table, height = 0.03, part = 'header')
    # specimen_table = height(specimen_table, height = 0.06, part = 'body')

    # align  text: center justify everything
    specimen_table <- align(specimen_table, align = "center", part = "all")

    # format numbers- in the main table; 1000s separated with commas and empty values with '-'
    specimen_table <- colformat_double(specimen_table, j = c(
      "CATCH_LENGTHS", "WEIGHTS", "MATURITIES", "OTOLITHS",
      "OVARY_WEIGHTS", "O_TAKEN"
    ), big.mark = ",", digits = 0, na_str = "-")

    # limit padding around cells
    # with the exception of the last row of the header, set padding to 0 pts; add a bit to to last row of header
    specimen_table <- padding(specimen_table, i = 1, padding.bottom = 0, part = "header")
    specimen_table <- padding(specimen_table, i = 2, padding.bottom = 2, part = "header")
    specimen_table <- padding(specimen_table, padding = 0, part = "body")

    # manually fix column widths to end up w/a ~7.5 in table
    # specimen_table = fit_to_width(specimen_table, max_width = 7.5)
    # set region name to be wider and fit long locations
    specimen_table <- width(specimen_table, j = c("region"), width = 1.5)
    # 0.5" columns
    specimen_table <- width(specimen_table, j = c("HAUL"), width = 0.5)
    # 0.70" columns
    specimen_table <- width(specimen_table, j = c(
      "CATCH_LENGTHS", "WEIGHTS", "MATURITIES", "OTOLITHS",
      "OVARY_WEIGHTS", "O_TAKEN"
    ), width = 0.73)

    # get some caption text

    cap_text <- paste0(
      "Numbers of ", tolower(unique(specimen_table_data$common_name)), " measured and biological samples collected during the winter ",
      current_year, " acoustic-trawl survey of ",
      combine_words(unique(as.character(specimen_data$region))), "."
    )

    # set the font and font size
    specimen_table <- flextable::font(specimen_table, fontname = "times", part = "all")
    specimen_table <- fontsize(specimen_table, size = 10, part = "all")

    # #send the caption and table to the document
    # specimen_table = set_caption(specimen_table, cap_text, autonum = table_numbering)
    #
    # #print table
    # flextable_to_rmd(x = specimen_table, ft.align = 'center', ft.split = TRUE, print = TRUE)
    # #make a pagebreak by adding \pagebreak command into document
    # cat('\\pagebreak')

    # return table and caption
    return(list(specimen_table, cap_text))
  }


  # for surveys with stomach collection, make this table
  if (sum(specimen_data$S_TAKEN, na.rm = TRUE) > 0) {
    # make specimen data into a flextable
    specimen_table <- flextable(specimen_data)


    # get header rows in a dataframe that : 1) has the column names (col_keys),
    # 2), defines first row of header (row1, below), 3) defines second row (row2, below)
    header_keys <- data.frame(
      col_keys = c(
        "HAUL", "region", "CATCH_LENGTHS", "WEIGHTS", "MATURITIES", "OTOLITHS",
        "OVARY_WEIGHTS", "O_TAKEN", "S_TAKEN"
      ),
      row1 = c("Haul", "Region", "Catch", "", "", "", "Ovary", "Ovaries", "Stomachs"),
      row2 = c("no.", "name", "lengths", "Weights", "Maturities", "Otoliths", "weights", "collected", "collected"),
      stringsAsFactors = FALSE
    )

    # add the header
    specimen_table <- set_header_df(specimen_table, mapping = header_keys, key = "col_keys")


    # calculate a row of sums for the bottom of table. Note na.rm = TRUE (because there are lots
    # of na's, which is fine)

    # also format things for the footer- 1000's separated by commas
    footer_row <- c(
      "Total",
      "",
      format(sum(specimen_data$CATCH_LENGTHS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$WEIGHTS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$MATURITIES, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$OTOLITHS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$OVARY_WEIGHTS, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$O_TAKEN, na.rm = TRUE), big.mark = ",", na_str = "-"),
      format(sum(specimen_data$S_TAKEN, na.rm = TRUE), big.mark = ",", na_str = "-")
    )

    # add the footer row
    specimen_table <- add_footer_row(specimen_table, top = FALSE, values = footer_row, colwidths = rep(1, length(specimen_data)))


    # add a border above and below caption and at bottom of table
    table_border <- fp_border(color = "black")

    # add horizontal border on top and bottom of table
    specimen_table <- hline_top(specimen_table, part = "all", border = table_border)
    specimen_table <- hline_bottom(specimen_table, part = "all", border = table_border)

    # #set row height as a pretty small height
    # specimen_table = height(specimen_table, height = 0.06, part = 'header')
    # specimen_table = height(specimen_table, height = 0.06, part = 'body')

    # align  text: center justify everything
    specimen_table <- align(specimen_table, align = "center", part = "all")

    # format numbers- in the main table; 1000s separated with commas and empty values with '-'
    specimen_table <- colformat_double(specimen_table, j = c(
      "CATCH_LENGTHS", "WEIGHTS", "MATURITIES", "OTOLITHS",
      "OVARY_WEIGHTS", "O_TAKEN", "S_TAKEN"
    ), big.mark = ",", digits = 0, na_str = "-")

    # limit padding around cells
    # with the exception of the last row of the header, set padding to 0 pts; add a bit to to last row of header
    specimen_table <- padding(specimen_table, i = 2, padding.bottom = 2, part = "header")
    specimen_table <- padding(specimen_table, padding = 0, part = "body")

    # manually fix column widths to end up w/a ~7.5 in table
    # specimen_table = fit_to_width(specimen_table, max_width = 7.5)
    # set region name to be wider and fit long locations
    specimen_table <- width(specimen_table, j = c("region"), width = 1.5)
    # 0.5" columns
    specimen_table <- width(specimen_table, j = c("HAUL"), width = 0.5)
    # 0.70" columns
    specimen_table <- width(specimen_table, j = c(
      "CATCH_LENGTHS", "WEIGHTS", "MATURITIES", "OTOLITHS",
      "OVARY_WEIGHTS", "O_TAKEN", "S_TAKEN"
    ), width = 0.70)

    # get some caption text
    cap_text <- paste0(
      "Numbers of ", tolower(unique(specimen_table_data$common_name)), " measured and biological samples collected during the winter ",
      substr(unique(as.character(specimen_table_data$SURVEY)), 1, 4), " acoustic-trawl survey of ",
      combine_words(unique(as.character(specimen_data$region))), "."
    )

    # set the font and font size
    specimen_table <- flextable::font(specimen_table, fontname = "times", part = "all")
    specimen_table <- fontsize(specimen_table, size = 10, part = "all")

    # #send the caption and table to the document
    # specimen_table = set_caption(specimen_table, cap_text, autonum = table_numbering)
    #
    # #print table
    # flextable_to_rmd(x = specimen_table, ft.align = 'center', ft.split = TRUE, print = TRUE)
    # #make a pagebreak by adding \pagebreak command into document
    # cat('\\pagebreak')

    # return table and caption
    return(list(specimen_table, cap_text))
  }
}
