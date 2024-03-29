# get shapes for every single gear type, and assign each gear type to a specific shape
# to be consistent between plots (from report to report- so we'll just build colors for all
# possible gear types here)
get_all_gear_types <- function() {
  # query all the gear types
  gear_query <- paste0(
    "SELECT gear, description ",
    "FROM clamsbase2.gear ",
    "ORDER BY gear "
  )

  # and fetch it from the database
  gear_types <- dbGetQuery(db, gear_query)

  # we currently have 16 gear types, make a list of shapes that's longer than this (~twice this long)
  # these are just r pch shape codes
  gear_shapes <- rep(seq(2, 6), 6)
  # and a list of gear colors; these are the again a repeating sequence of colors with good contrast
  gear_colors <- rep(c("#a50026", "#4575b4", "#fdae61", "#b2df8a", "#e31a1c", "#6a3d9a"), 3)

  # add the shapes and colors to the dataframe
  gear_types$gear_shapes <- gear_shapes[1:nrow(gear_types)]

  gear_types$gear_colors <- gear_colors[1:nrow(gear_types)]

  # return this dataframe
  return(gear_types)
}
