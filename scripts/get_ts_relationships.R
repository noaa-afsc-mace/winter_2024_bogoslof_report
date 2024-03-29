# query all the TS relationships used in the survey

get_ts_relationships <- function(ship, survey, data_set_id, analysis_id) {
  ts_query <- paste0(
    "SELECT UNIQUE(ts_relationship) ",
    "FROM macebase2.ts_sp_comp ",
    "WHERE ship = ", ship, " ",
    "AND survey = ", survey, " ",
    "AND data_set_id = ", data_set_id, " ",
    "AND analysis_id = ", analysis_id, " ",
    "AND ts_relationship NOT IN ('non_scaling')"
  )

  ts_relationships_used <- dbGetQuery(db, ts_query)

  # return the dataframe
  return(ts_relationships_used)
}
