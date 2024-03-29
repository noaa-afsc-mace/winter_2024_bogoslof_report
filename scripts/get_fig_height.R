# this function just assigns a figure height in inches based on the number of regions to include in the figure; this figure height can then
# be called to set the figure height

get_fig_height <- function(n_regions) {
  # if the plot has only 1 region, make it 6 inches tall
  if (n_regions == 1) {
    comp_height <- 6
  } # if there's 2, make it 6 inches tall
  else if (n_regions == 2) {
    comp_height <- 8
  } # if there's more than that, make it 8 inches tall
  else {
    comp_height <- 8
  }

  return(comp_height)
}
