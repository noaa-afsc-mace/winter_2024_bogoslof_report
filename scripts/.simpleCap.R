## "Mixed Case" Capitalizing - toupper( every first letter of a word ) :
# define a new function to deal with capitalizing in text
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " "
  )
}