# get the numbers- and biomass- at length for each set dataset/analyses; this will gather the totals by length for each ship/survey/dataset/analysis
# this summarizes data as needed for biomass comparison figures

get_analysis_comparison_data <- function(ship, survey, data_sets, analyses, zones_list, sp_code) {
  biomass_nums_query <- paste0(
    "SELECT a.ship, a.survey, a.data_set_id, a.analysis_id, a.report_number, a.length,  b.description, ",
    "SUM(a.numbers) as numbers, SUM(a.biomass) as biomass ",
    "FROM macebase2.analysis_results_by_length a LEFT JOIN macebase2.report_definitions b ",
    "ON (a.ship = b.ship) AND (a.survey = b.survey) AND (a.data_set_id = b.data_set_id) AND (a.analysis_id = b.analysis_id) ",
    "AND (a.report_number = b.report_number) ",
    "WHERE a.ship = ", ship, " ",
    "AND a.survey = ", survey, " ",
    paste("AND a.data_set_id IN (", paste(data_sets, collapse = ","), ") "),
    paste("AND a.analysis_id IN (", paste(analyses, collapse = ","), ") "),
    paste("AND a.zone IN (", paste(zones_list, collapse = ","), ") "),
    "AND a.species_code = ", sp_code, " ",
    "GROUP BY a.ship, a.survey, a.data_set_id, a.analysis_id, a.report_number, a.length, b.description ",
    "ORDER BY a.report_number "
  )

  biomass_nums_data <- dbGetQuery(db, biomass_nums_query)
  
  # return this dataframe
  return(biomass_nums_data)
}
