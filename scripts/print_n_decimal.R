# this function is just a wrapper around formatC for easier number formatting in text- arguments are
# the number you want to format, and the number of decimals you want formatted. It will separate 1000's with a comma,.

print_n_decimal <- function(x, n_decimals) {
  print_val <- formatC(round(x, digits = n_decimals), format = "f", big.mark = ",", digits = n_decimals)
  return(print_val)
}
