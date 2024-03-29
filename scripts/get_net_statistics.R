# gather net measurment statistics- this isn't currently reported as a table, but
# is used in the text

get_net_statistics <- function(ship, survey) {
  # get the net vertical opening and speed over ground on a per-haul basis
  net_stats_query <- paste0(
    "SELECT a.ship, a.survey, a.event_id, b.gear, a.sog, a.net_vertical_opening ",
    "FROM clamsbase2.v_event_stream_data a LEFT JOIN clamsbase2.events b ",
    "ON (a.ship = b.ship) AND (a.survey = b.survey) AND (a.event_id = b.event_id) ",
    "WHERE a.ship = ", ship, " ",
    "AND a.survey = ", survey, " ",
    "ORDER BY a.event_id "
  )

  net_stats_data <- dbGetQuery(db, net_stats_query)

  # get the tomweights (for midwater trawls) on a per-haul basis
  tomweights_query <- paste0(
    "SELECT ship, survey, event_id, gear_accessory_option as tomweight_lb ",
    "FROM clamsbase2.gear_accessory ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND gear_accessory = 'Tomweights' "
  )

  tomweights_data <- dbGetQuery(db, tomweights_query)

  # join the net opening/speed data to the tomweights; this will put an NA in cases where there
  # were no tomweights (methots/bottom trawls/etc)
  net_stats_data <- left_join(net_stats_data, tomweights_data, by = c("SHIP", "SURVEY", "EVENT_ID"))

  # return this dataframe
  return(net_stats_data)
}
