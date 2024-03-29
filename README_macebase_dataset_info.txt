So, what data are we getting from Macebase, anyway?

We are grabbing all the macebase queries ‘up front’ in these markdown reports. This allows us to create a static dataset that can be stowed with the report. 
Down the road, if analyses change, we will at least have a time capsule of the values at a given moment- and can track down differences as needed.

All of the Macebase queries are done in a single R script- ‘get_macebase_data.R’. This outputs up to 3 .Rdata files- one for the Shelikof survey, 
one for the Shumagins survey, and one featuring shared data. In the descriptions below, ‘s*’ should be read as ‘Shelikof OR shumagins’.
There should be a number of indices available for summing up your values as needed: Interval, management area (610/620/…), report number (1,2,3…), region name (Shelikof, Marmot…).
This describes the date gathered in the ‘get_macebase_data.R’ script. This data is then used to create tables/figures via the functions called 
in ‘main_Cruise_report_2022.Rmd’; ideally, it gets everything needed in the text too. Feel free to add! 


Intervals and hauls:
interval_data_s* = interval information for the current survey

event_data_s* = haul data for the current survey

s*_haul_table_data = dataframe needed to create standard haul tables (positions/depths/temps at surface and fishing depth/etc/Eq times, etc).


Survey parameters:
s*_sel_corr_survey_params = parameters needed to query the modern ‘selectivity corrected era’ surveys; surface-referenced only! 
Starts in 2008 in the Shelikof, 2009 in the Shumagins.

s*_surf_and_bot_historical = parameters (ship/survey/data_set_id/analysis_id) to query a recent collection of surface and bottom referenced surveys; 
this doesn’t extend back as far as s*_sel_corr_survey_params because we only present a few years of bottom-referenced data for comparisons (i.e. starting in 2015 in the Shelikof/2021 in in Shumagins).

s*_historical_eva = collection of historical EVA values; the current value is automatically appended onto this if it has been included as a parameter.

ts_relationships_data = a dataframe of all the TS relationships used in the current survey data_set_id/analysis_id; used to create TS relationships table.


Biomass and numbers: 
s*_current_survey_pollock_biomass_nums = Numbers and biomass at length per interval for the current survey only 
(values have been summed vertically within intervals to save space here as it is a massive dataframe otherwise  we don’t report these value by depth bin in report).

s*_sel_corr_surveys_pollock_biomass_nums_surf_ref  = Numbers and biomass at length per interval for recent surface-referenced surveys
using parameters specified in s*_surf_and_bot_historical. 

s*_sel_corr_surveys_pollock_biomass_nums_bot_ref  = Numbers and biomass at length per interval for recent bottom-referenced surveys using parameters specified in s*_surf_and_bot_historical.

s*_sel_corr_surveys_totals_by_length_and_region = Numbers and biomass at length within report_numbers/survey regions 
(summed across all intervals within a given reporting region) for all surveys specified in s*_sel_corr_survey_params.

s*_analysis_comparisons = Numbers and biomass at length the current survey for the primary and alternate analyses 
(alternate analysis parameters grabbed from ‘GOA_winter_cruise_report_main.Rmd’).

shelikof_sel_corr_surveys_biomass_nums_age = (Shelikof-only because Shumgins doesn’t get aged). Numbers and biomass at age within
 report_numbers/survey regions (summed across all intervals within a given reporting region) for all surveys specified in s*_sel_corr_survey_params.


SBE data
s*_sbe_data = depth/temp from SBE 39s for all hauls in the current survey
*So, what if you want SST instead? These values aren’t stored in macebase; use the function ‘seatemps_plot_function’ to calculate stats. It returns many stats in a list that you can pull items from.

Biological data

s*_prop_mature = number of biological samples at at length/maturity stage from the trawl samples; used to calculate weighted maturity stats.

s*_maturities_and_weights = specimen weights/lengths/maturities; used to calculate weighted maturity stats.

s*_ pollock_length_weight_age_data = all the lengths/weights/ages by haul; used to calculate weighted maturity stats.
*So- what if you actually just want the stats (percent at each stage weighted for local abundance, etc)? You can use the function
 ‘plot_abundance_weighted_maturity’ to return these (it returns many stats in a list that you can pull items from).

s*_catch_table_data = dataframe needed to create standard catch tables (numbers/weights/specimens within each survey region, by species, etc).

s*_specimen_table_data = dataframe needed to create standard pollock specimen tables (number of lengths/weights/maturities/otoliths/etc per haul, etc).
