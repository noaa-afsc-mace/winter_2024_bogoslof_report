# get the haul type, as identified by the trawl scientist during the CLAMS fishing event

get_event_type <- function(ship, survey) {
  event_type_query <- paste0(
    "SELECT a.ship, a.survey, a.event_id, a.gear, a.event_type, b.description ",
    "FROM clamsbase2.events a LEFT JOIN clamsbase2.event_types b ",
    "ON (a.event_type = b.event_type) ",
    "WHERE a.ship = ", ship, " ",
    "AND a.survey = ", survey, " ",
    "ORDER BY a.event_id"
  )

  # and get the data
  event_type_data <- dbGetQuery(db, event_type_query)

  # add a column for 'bottom' or 'midwater' index based on the event_type
  # event_type_data$trawl_type = NA

  # now add an index for 'bottom' or 'midwater'
  midwater_event_types <- c(1, 2, 8, 10)
  bottom_event_types <- c(0, 3, 6)

  # label each trawl as midwater or bottom based on the event_type number
  label_trawl_type <- function(event_type) {
    if (!(event_type %in% c(midwater_event_types, bottom_event_types))) {
      event_type <- NA
    }

    if (event_type %in% midwater_event_types) {
      event_type <- "midwater"
    }

    if (event_type %in% bottom_event_types) {
      event_type <- "bottom"
    }

    return(event_type)
  }

  # apply this function to each event_type
  event_type_data$haul_type <- map_chr(event_type_data$EVENT_TYPE, label_trawl_type)

  # return this dataframe
  return(event_type_data)
}
