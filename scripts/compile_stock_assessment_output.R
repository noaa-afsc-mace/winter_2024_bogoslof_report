# Complile stock assessment outputs and save for sharing with stock assessment staff -
# shelikof survey

compile_stock_assessment_output <- function() {
  # check for the file path- make it if it doesn't exist
  test_path <- "stock_assessment_output/"

  # if we don't have one, create it!
  if (!dir.exists(test_path)) {
    dir.create(test_path, recursive = TRUE)
  }

  # start with the comparisons to old data set to FALSE; make TRUE if we have comparisons to previously sent data
  lengths_data_check <- FALSE
  ages_data_check <- FALSE
  old_dataset <- NULL
  old_analysis <- NULL

  # if a user identified a location for comparison to old results - try to open
  if (dir.exists(stock_assessment_path)) {
    # get the available files
    comp_file_list <- list.files(stock_assessment_path, pattern = "*.csv", full.names = TRUE)

    # if there are files, open some up to check some basics for comparison
    if (length(comp_file_list > 0) &
      any(stringr::str_detect(string = comp_file_list, pattern = "pollock_biomass_num_length_by_region"))) {
      # find the file with biomass totals
      old_bio_by_length_loc <- comp_file_list[which(stringr::str_detect(string = comp_file_list, pattern = "pollock_biomass_num_length_by_region"))]

      # open it up
      old_bio_by_length <- readr::read_csv(old_bio_by_length_loc)

      # sum up the total
      old_biomass_length_t <- sum(old_bio_by_length$biomass_ton)

      # set the old_lengths variable to true
      lengths_data_check <- TRUE

      # check- are there ages?
      ages_data_check <- any(stringr::str_detect(string = comp_file_list, pattern = "pollock_biomass_num_age"))

      # if so- open up the old values
      if (ages_data_check == TRUE) {
        # get the old location
        old_bio_by_age_loc <- comp_file_list[which(stringr::str_detect(string = comp_file_list, pattern = "pollock_biomass_num_age"))]

        # open it up
        old_bio_by_age <- readr::read_csv(old_bio_by_age_loc)

        # sum up the total
        old_biomass_age_t <- sum(old_bio_by_age$biomass_ton)
      }

      # also- identify the date for the old data
      old_file_info <- file.info(old_bio_by_length_loc)
      old_run_time <- old_file_info$mtime

    }
  }

  #################################################################
  # 1. Pollock biomass and abundance by 1 cm length bins

  # sum pollock biomass at length for each region
  stock_assess_bio_nums_length_region <- current_year_pollock_biomass_nums %>%
    group_by(LENGTH, region, SURVEY) %>%
    rename("length_cm" = LENGTH) %>%
    summarize(
      number_million_fish = sum(NUMBERS) / 1e6,
      biomass_ton = sum(BIOMASS) / 1e3
    ) %>%
    arrange(SURVEY,region, length_cm)

  # save as .csv
  write_csv(stock_assess_bio_nums_length_region,
    file = paste0(
      "stock_assessment_output/pollock_biomass_num_length_by_region_",
      current_year, ".csv"
    )
  )

  ################################################################
  # 2. Pollock biomass and abundance by 1 year age bins for the GOA summer survey by region (if you've got ages)
  if (age_data == TRUE) {
    # sum up all the age totals, by region
    shelikof_stock_assess_bio_nums_age_region <- shelikof_current_survey_biomass_nums_age %>%
      # only report Shelikof Strait here! This should be handled by the aging request (we don't need to age the other fish)
      # but to be safe, get it here too
      filter(region == "Shelikof Strait") %>%
      group_by(AGE, region) %>%
      summarize(
        number_million_fish = sum(NUMBERS) / 1e6,
        biomass_ton = sum(BIOMASS) / 1e3
      ) %>%
      arrange(region, AGE)

    # save as .csv
    write_csv(shelikof_stock_assess_bio_nums_age_region,
      file = paste0(
        "stock_assessment_output/pollock_biomass_num_age_by_region_",
        current_year, ".csv"
      )
    )
  }

  ##############################################################
  # 3. Total pollock biomass by management area and survey- totals only, not by length or age
  
  ##TODO!!! Get together with Darin, 2023 authors to check on all this.

  # sum things up by region, management; report biomass (t) and numbers (million)
 biomass_nums_by_region_and_nmfs_area <- current_year_pollock_biomass_nums %>%
    group_by(SURVEY, management_area) %>%
    rename("INPFC_area" = management_area) %>%
    summarize(
      number_million_fish = sum(NUMBERS) / 1e6,
      biomass_ton = sum(BIOMASS) / 1e3
    ) %>%
    arrange(INPFC_area,SURVEY)

  # save as .csv
  write_csv(biomass_nums_by_region_and_nmfs_area,
    file = paste0(
      "stock_assessment_output/pollock_biomass_num_length_by_INPFC_area_",
      current_year, ".csv"
    )
  )

  ##############################################################
  # 4. Relative Estimation error for pollock

  # this relies on the values that users have entered as parameters!
  current_survey_eva_vals <- data.frame(
    "survey_region" = c("Shelikof Strait", "Chirikof Shelfbreak", "Marmot Bay", 
                        "Shumagin Islands", "Pavlof Bay", "Morzhovoi Bay", "Sanak Trough"),
    "relative_estimation_error_percent" =
      c(eva_shelikof_perc, eva_chirikof_perc, eva_marmot_perc, 
        eva_shumagins_perc, eva_pavlof_perc, eva_morzhovoi_perc, eva_sanak_perc)
  )

  # clean up: only save EVA values for the areas we estimated
  current_survey_eva_vals <- current_survey_eva_vals %>%
    filter(!is.na(relative_estimation_error_percent))


  # save as csv
  write_csv(current_survey_eva_vals,
    file = paste0(
      "stock_assessment_output/pollock_relative_estimation_error_",
      current_year, ".csv"
    )
  )

  ##############################################################
  # 5. Number of bottom trawls and number of midwater trawls
  
  # add an index indicating if the hauls were used in the analysis
  scaling_hauls_for_stock_assessment <- scaling_hauls
  
  # set these hauls to TRUE - these are the ones used in the analysis
  scaling_hauls_for_stock_assessment$haul_used <- TRUE
  
  # add this to the hauls data
  hauls_for_stock_assessment <- left_join(current_year_event_data, scaling_hauls_for_stock_assessment, 
                                          by = c("SURVEY", "EVENT_ID"))
  
  # note that this only reports the number used in the analysis!
  stock_assess_haul_summary <- hauls_for_stock_assessment %>%
    # drop the sf spatial dataframe information
    st_drop_geometry() %>%
    # add the event type, as noted by the trawl scientist during the clams event
    left_join(clams_event_type, by = c("SHIP", "SURVEY", "EVENT_ID")) %>%
    # limit to analysis hauls only
    filter(haul_used == TRUE) %>%
    # count the number of hauls by gear type
    group_by(SURVEY, region, `Gear type`, haul_type) %>%
    summarize(n_trawls = n())

  # save as csv
  write_csv(stock_assess_haul_summary,
    file = paste0(
      "stock_assessment_output/n_trawls_",
      current_year, ".csv"
    )
  )

  ##############################################################
  # 6. Number of measured fish (by length) by sex
  
  # add the scaling hauls info
  raw_specimen_data_for_stock_assessment <- left_join(raw_specimen_data, scaling_hauls_for_stock_assessment,
                                                      by =  c("SURVEY", "HAUL"= "EVENT_ID"))

  # again, limited to the assessment hauls only!
  n_lengths_by_sex <- raw_specimen_data_for_stock_assessment %>%
    # limit to analysis hauls only, and to pollock only
    filter(haul_used == TRUE & SPECIES_CODE == 21740) %>%
    # CLAMS calls pollock specimen fish <20 cm 'NA' sex, as opposed to 'Unsexed' as with lengths
    # so, here, we will identify those as 'unsexed'
    mutate(SEX = replace_na(data = SEX, replace = "Unsexed")) %>%
    # round lengths into 1 cm bins
    mutate(length_cm = round(organism_length, digits = 0)) %>%
    group_by(SURVEY, region,length_cm, SEX) %>%
    summarize(n_lengths = n()) %>%
    # go to 'wide' dataframe so each sex is in its own column
    pivot_wider(names_from = SEX, values_from = n_lengths) %>%
    # and add a total column
    mutate(total_lengths = sum(Unsexed, Female, Male, na.rm = TRUE))

  # save as csv
  write_csv(n_lengths_by_sex,
    file = paste0(
      "stock_assessment_output/n_pollock_lengths_by_sex_",
      current_year, ".csv"
    )
  )

  ##############################################################
  # 7. Number of aged fish by sex
  

  if (age_data == TRUE) {
    # again, limited to the assessment hauls only!
    shelikof_n_ages_by_sex <- raw_specimen_data_for_stock_assessment %>%
      # limit to analysis hauls only, and to pollock only
      filter(haul_used == TRUE & SPECIES_CODE == 21740) %>%
      # only keep the aged fish as well
      filter(!is.na(AGE)) %>%
      # CLAMS calls pollock specimen fish <20 cm 'NA' sex, as opposed to 'Unsexed' as with lengths
      # so, here, we will identify those as 'unsexed'
      mutate(SEX = replace_na(data = SEX, replace = "Unsexed")) %>%
      # add up the numbers age/sex
      group_by(SURVEY, region, AGE, SEX) %>%
      summarize(n_ages = n()) %>%
      # go to 'wide' dataframe so each sex is in its own column
      pivot_wider(names_from = SEX, values_from = n_ages) %>%
      # and add a total column
      mutate(total_ages = sum(Unsexed, Female, Male, na.rm = TRUE))

    # save as csv
    write_csv(shelikof_n_ages_by_sex,
      file = paste0(
        "stock_assessment_output/n_pollock_ages_by_sex_",
        current_year, ".csv"
      )
    )
  }

  ##############################################################
  # 8. specimen data including ages and maturity determinations.

  # in this case, also limit to pollock from scaling hauls only
  pollock_specimen_data <- raw_specimen_data_for_stock_assessment %>%
    # limit to analysis hauls only, and to pollock only
    filter(haul_used == TRUE  & SPECIES_CODE == 21740) %>%
    select(- haul_used)

   # save as csv
  write_csv(pollock_specimen_data,
    file = paste0(
      "stock_assessment_output/pollock_specimen_data_",
      current_year, ".csv"
    )
  )

  ##############################################################
  # 8. Abundance-weighted maturity at length/age data - not included for summer GOA reports.

  # function to build GLM: abundance-weighted by length- build on a per-region basis
  L50_by_region <- function(survey_region) {
    # get the maturity data for the region
    region_prop_mature <- prop_mature %>%
      filter(region == survey_region)

    # run the GLM
    region_maturity_length_glm <- glm(
      formula = cbind(MATURE, IMMATURE) ~ FORK_LENGTH,
      data = region_prop_mature, weights = WEIGHTS, family = binomial(logit)
    )

    # pull out the terms we need to report
    L50 <- -region_maturity_length_glm$coefficients[1] / region_maturity_length_glm$coefficients[2]
    slope <- region_maturity_length_glm$coefficients[2]
    intercept <- region_maturity_length_glm$coefficients[1]
    V <- vcov(region_maturity_length_glm)
    sample <- MASS::mvrnorm(n = 1000, region_maturity_length_glm$coefficients, V)
    L50_sample <- -sample[, 1] / sample[, 2]
    qs <- quantile(L50_sample, probs = c(0.05, 0.95))
    lower_ci <- qs[1]
    upper_ci <- qs[2]

    # save the parameters to a dataframe
    region_outvals_weighted <- data.frame(
      "survey" = survey_shelikof,
      "region" = survey_region,
      "L50" = L50,
      "upper_ci" = upper_ci,
      "lower_ci" = lower_ci,
      row.names = NULL
    )

    # and return this dataframe
    return(region_outvals_weighted)
  }

  # apply this function to every survey region with maturity data
  L50_data <- map_dfr(unique(prop_mature$region), L50_by_region)

  # save as csv
  write_csv(L50_data,
    file = paste0(
      "stock_assessment_output/pollock_weighted_L50_mature_",
      current_year, ".csv"
    )
  )


  # if you've got the age data, report A50 + proportion predicted mature at age
  if (age_data == TRUE) {
    # function to build GLM: abundance-weighted by length- build on a per-region basis
    A50_by_region <- function(survey_region) {
      # get the maturity data for the region
      region_prop_mature_by_age <- shelikof_maturity_by_age %>%
        filter(region == survey_region)

      # run the GLM
      region_maturity_age_glm <- glm(
        formula = cbind(MATURE, IMMATURE) ~ AGE,
        data = region_prop_mature_by_age, weights = WEIGHTS, family = binomial(logit)
      )

      # get model info for all ages 1-15
      ages <- data.frame(AGE = seq(1, 15, 1))
      results <- matrix(nrow = 0, ncol = length(ages$AGE))
      A50 <- -region_maturity_age_glm$coefficients[1] / region_maturity_age_glm$coefficients[2]
      slope <- region_maturity_age_glm$coefficients[2]
      intercept <- region_maturity_age_glm$coefficients[1]
      V <- vcov(region_maturity_age_glm)
      sample <- MASS::mvrnorm(n = 1000, region_maturity_age_glm$coefficients, V)
      A50_sample <- -sample[, 1] / sample[, 2]
      qs <- quantile(A50_sample, probs = c(0.05, 0.95))
      lower_ci <- qs[1]
      upper_ci <- qs[2]
      pred <- predict(region_maturity_age_glm, ages, type = "response")
      results <- rbind(results, pred)

      # save the parameters to a dataframe
      region_outvals_weighted_age <- data.frame(
        "survey" = survey_shelikof,
        "region" = survey_region,
        "A50" = A50,
        "upper_ci" = upper_ci,
        "lower_ci" = lower_ci,
        row.names = NULL
      )

      # save the predictors as well
      preds_weighted <- data.frame(results)
      colnames(preds_weighted) <- ages$AGE

      # return both dataframes
      return(list(survey_region, region_outvals_weighted_age, preds_weighted))
    }

    # return the list of dataframes
    shelikof_A50_data <- map(unique(shelikof_maturity_by_age$region), A50_by_region)

    # save both as csv's
    for (j in 1:length(shelikof_A50_data)) {
      # pull out the region name
      region_name_for_ages <- shelikof_A50_data[[j]][[1]]

      # save each file
      write_csv(shelikof_A50_data[[j]][[2]],
        file = paste0(
          "stock_assessment_output/pollock_weighted_A50_mature_",
          region_name_for_ages, "_", current_year, ".csv"
        )
      )

      write_csv(shelikof_A50_data[[j]][[3]],
        file = paste0(
          "stock_assessment_output/pollock_weighted_prop_mature_at_age_",
          region_name_for_ages, "_", current_year, ".csv"
        )
      )
    }
    
    # also present as proportion, with all ages >10 grouped in a 10+ category
    pk_by_age <- shelikof_current_survey_biomass_nums_age %>%
      # create a new age vector with all 10+ grouped
      mutate(age = ifelse(AGE < 10, AGE, 10)) %>%
      group_by(SURVEY, region, age) %>%
      summarize(
        number_million_fish = sum(NUMBERS)/1e6,
        biomass_ton = sum(BIOMASS)/1e3
      ) %>%
      mutate(
        prop_num = number_million_fish / sum(number_million_fish),
        prop_biomass = biomass_ton / sum(biomass_ton),
        # label nicely
        age = ifelse(age == 10, "10+", as.character(age))
      )
    
    write_csv(pk_by_age,
              file = paste0(
                "stock_assessment_output/shelikof_prop_at_age_",
                current_year, ".csv")
    )
    
  }
    
  # print a cover page for the pollock report
  rmarkdown::render("stock_assessment_coversheet.Rmd", output_file = paste0("stock_assessment_output/", current_year, "_pollock_data_summary.docx"), quiet = TRUE)
}
