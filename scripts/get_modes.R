# this function takes a summary (total biomass/nums by length) and identifies the mode

get_modes <- function(length_data_summary, weights_val) {
  # convert the summary values into a continuous variable
  region_by_length <- uncount(length_data_summary, weights = length_data_summary[[weights_val]])

  # report the range of lengths and modes present in the data. Calculation of modes relies on the r package
  #' multimode'.

  # test: is the data multimodel?
  # null hypothesis here is that data is unimodel, so a p-value <0.05 implies that data IS multimodal
  test_modes <- multimode::modetest(region_by_length$LENGTH, mod0 = 1)

  # if the data IS multimodel, report top n modes.
  # Let the p-value be a bit higher than usual  <0.05, because we are just trying to qualitiatively describe
  if (test_modes$p.value < 0.1) {
    # calculate how many modes are present
    n_length_modes <- multimode::nmodes(region_by_length$LENGTH,
      bw = 2,
      lowsup = min(region_by_length$LENGTH),
      uppsup = max(region_by_length$LENGTH)
    )

    # if there's not much data, we need to increase the bandwidth- to check for this,
    # see if the number of modes returned above is unreasonable (i.e. >4)
    if (n_length_modes > 4) {
      n_length_modes <- multimode::nmodes(region_by_length$LENGTH,
        bw = 3,
        lowsup = min(region_by_length$LENGTH),
        uppsup = max(region_by_length$LENGTH)
      )
    }

    # identify this number of modes
    modes <- multimode::locmodes(region_by_length$LENGTH, mod0 = n_length_modes)

    # and get the locations- these are the odd values below
    modes_locations <- modes$locations[seq(1, length(modes$locations), 2)]

    # if there's very little data, there may be an NA mode- remove any of these
    if (any(is.na(modes_locations))) {
      modes_locations <- modes_locations[!is.na(modes_locations)]
    }

    # print as a rounded version
    modes_locations <- round(modes_locations, digits = 0)
  }

  # if the data IS NOT multimodel, just get the simple mode, print as a rounded version
  if (test_modes$p.value >= 0.1) {
    modes_locations <- Mode(region_by_length$LENGTH)
    modes_locations <- round(modes_locations, digits = 0)
  }

  # return the mode locations
  return(modes_locations)
}
