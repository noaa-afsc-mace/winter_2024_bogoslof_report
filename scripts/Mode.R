# function to calculate the mode (i.e. to be used to get the most common transect spacing)
# from : https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
