# decide whether we need to italicize the species name: logic here is that if the common name and
# scientific name are the same, italicize- if it has a different common name, don't
print_species_name <- function(species_name) {
  # find the scientific name
  sci_name <- captured_species_list$SCIENTIFIC_NAME[captured_species_list$COMMON_NAME == species_name]

  # check- same as common name?
  print_name <- ifelse(species_name == sci_name, paste0("*", species_name, "*"), species_name)

  # customize a few common ones- if 'euphausiid unid.', change to euphausiids
  if (print_name == "euphausiid unid.") {
    print_name <- "euphausiids"
  }

  # customize a few common ones- if any other  'unid.', change to 'sp.'
  if (stringr::str_detect(print_name, "unid")) {
    # take the first part, but not 'unid'
    print_name <- stringr::str_split(print_name, " ")[[1]][1]

    # and add 'sp.'
    print_name <- paste0(print_name, " sp.")
  }

  # change 'walleye pollock' to 'pollock'
  if (stringr::str_detect(print_name, "walleye pollock")) {
    # take the first part, but not 'unid'
    print_name <- "pollock"
  }

  # return true/false
  return(print_name)
}
