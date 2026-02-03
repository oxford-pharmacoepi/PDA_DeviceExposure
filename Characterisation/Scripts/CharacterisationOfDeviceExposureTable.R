results <- list()

cdm[["device_exposure"]] <- cdm[["device_exposure"]] |>
  filter(device_exposure_start_date <= as.Date("2025-02-17"))  |>
  compute(temporary = FALSE, name = "device_exposure")

# Database diagnostics ----
results[["summarise_omop_snapshot"]] <- summariseOmopSnapshot(cdm)
results[["summarise_person_table"]]  <- summarisePerson(cdm)
results[["summarise_observation_period"]] <- summariseObservationPeriod(cdm)

# Device diagnostics -----
results[["summarise_clinical_records"]] <- summariseClinicalRecords(cdm,
                                                                    omopTableName = "device_exposure",
                                                                    missingData = FALSE)

cdm[["device_exposure1"]] <- cdm[["device_exposure"]] |>
  compute(temporary = FALSE, name = "device_exposure1")
cdm[["device_exposure"]] <- cdm[["device_exposure"]] |>
  mutate("device_source_concept_id" = 0L,
         "device_source_value" = " ") |>
  compute(temporary = FALSE, name = "device_exposure")
results[["summarise_code_use"]] <- summariseCodeUse(x = newCodelist(list("device_concepts_id" = unique(cdm$device_exposure |> pull("device_concept_id")))),
                                                    cdm, 
                                                    byYear = TRUE)
cdm[["device_exposure"]] <- cdm[["device_exposure1"]] |>
  compute(temporary = FALSE, name = "device_exposure")

# Summarise devices to procedures (any) ----
cdm[["all_devices_cohort"]] <- cdm[["device_exposure"]] |>
  mutate("cohort_definition_id" = as.integer(dense_rank(device_concept_id))) |>
  select("cohort_definition_id",
         "subject_id" = "person_id",
         "cohort_start_date" = "device_exposure_start_date",
         "cohort_end_date" = "device_exposure_start_date",
         "device_concept_id", "unique_device_id") |>
  mutate(row_id = row_number()) |>
  left_join(
    cdm[["concept"]] |>
      select("device_concept_id" = "concept_id", 
             "device_concept_name" = "concept_name"),
    by = "device_concept_id"
  ) |> 
  compute(temporary = FALSE, name = "all_devices_cohort")

settings <- cdm[["all_devices_cohort"]] |>
  select("cohort_definition_id", "cohort_name" = "device_concept_name", "device_concept_id") |>
  distinct() |>
  collect() |>
  mutate("cohort_name" = toSnakeCase(cohort_name),
         "cohort_definition_id" = as.integer(cohort_definition_id)) 

cdm[["all_devices_cohort"]] <- cdm[["all_devices_cohort"]] |>
  newCohortTable("cohortSetRef" = settings, .softValidation = TRUE)

cdm[["procedures_cohort"]] <- cdm[["all_devices_cohort"]] |>
  left_join(
    cdm[["procedure_occurrence"]] |>
      select("subject_id" = "person_id", "procedure_concept_id", "cohort_start_date" = "procedure_date"),
    by = c("subject_id", "cohort_start_date")
  ) |> 
  select(-"unique_device_id") |>
  mutate("procedure_concept_id" = dplyr::coalesce(procedure_concept_id, -1)) |> 
  compute(temporary = FALSE, name = "procedures_cohort")

cdm[["procedures_cohort"]] <- cdm[["procedures_cohort"]] |>
  left_join(
    cdm[["concept"]] |>
      select("procedure_concept_id" = "concept_id", "procedure_concept_name" = "concept_name"),
    by = "procedure_concept_id"
  ) |>
  mutate("procedure_concept_name" = coalesce(procedure_concept_name, "No procedure identified")) |>
  distinct() |> 
  compute(temporary = FALSE, name = "procedures_cohort")

cohort_ids <- cdm[["procedures_cohort"]] |> 
  settings() |>
  pull("cohort_definition_id")

procedures_dictionary <- cdm[["procedures_cohort"]] |>
  select("procedure_concept_id", "procedure_concept_name") |>
  distinct() |>
  collect()

for(i in cohort_ids){
  cdm[["devices_to_procedures_cohort"]] <- cdm[["procedures_cohort"]] |>
    subsetCohorts(cohortId = i, name = "devices_to_procedures_cohort")
  
  cdm[["devices_to_procedures_cohort"]] <- cdm[["devices_to_procedures_cohort"]] |> 
    mutate("value" = 1L) |>
    pivot_wider("names_from" = procedure_concept_id, 
                "values_from" = value,
                names_prefix = "proc_") |>
    mutate(across(starts_with("proc_"), ~as.character(coalesce(., "0")))) |>
    distinct() |> 
    compute(temporary = FALSE, name = "devices_to_procedures_cohort")
  
  proc_cols <- colnames(cdm[["devices_to_procedures_cohort"]])[grepl("proc_",colnames(cdm[["devices_to_procedures_cohort"]]))]
  
  results[[paste0("summarise_procedures_",i)]] <- summariseCharacteristics(
    cdm[["devices_to_procedures_cohort"]],
    demographics = FALSE,
    otherVariables = proc_cols
  )
  
  device_concept_id <- cdm[["devices_to_procedures_cohort"]] |> settings() |> pull("device_concept_id")
  
  results[[paste0("summarise_procedures_",i)]] <- results[[paste0("summarise_procedures_",i)]] |>
    filter(is.na(variable_level) | variable_level == 1) |>
    mutate("procedure_concept_id" = as.integer(str_extract(variable_name, "\\d+")),
           "device_concept_id" = device_concept_id) |>
    left_join(
      procedures_dictionary,
      by = "procedure_concept_id"
    ) |>
    select(-c("additional_name", "additional_level")) |>
    uniteAdditional(cols = c("procedure_concept_id", "procedure_concept_name", "device_concept_id")) 
}

# Summarise UDIs ----
cohort_ids <- cdm[["all_devices_cohort"]] |> 
  settings() |>
  pull("cohort_definition_id")

for(i in cohort_ids){
  cdm[["devices_udi"]] <- cdm[["all_devices_cohort"]] |>
    subsetCohorts(cohortId = i, name = "devices_udi")
  
  cdm[["devices_udi"]] <- cdm[["devices_udi"]] |> 
    mutate("value" = if_else(unique_device_id == "0", "0", "1")) |>
    # mutate("value" = 1L) |>
    # pivot_wider("names_from" = unique_device_id, 
    #             "values_from" = value,
    #             names_prefix = "udi_") |>
    # mutate(across(starts_with("udi"), ~as.character(coalesce(., "0")))) |>
    compute(temporary = FALSE, name = "devices_udi")
  
  # udi_cols <- colnames(cdm[["devices_udi"]])[grepl("udi_",colnames(cdm[["devices_udi"]]))]
  
  results[[paste0("summarise_udi_",i)]] <- summariseCharacteristics(
    cdm[["devices_udi"]],
    demographics = FALSE,
    otherVariables = "value"
    # otherVariables = udi_cols
  )
  
  device_concept_id <- cdm[["devices_udi"]] |> settings() |> pull("device_concept_id")
  
  results[[paste0("summarise_udi_",i)]] <- results[[paste0("summarise_udi_",i)]] |>
    # filter(is.na(variable_level) | variable_level == 1) |>
    mutate(
      # "unique_device_identifier" = as.integer(str_extract(variable_name, "udi ")),
      "device_concept_id" = device_concept_id) |>
    select(-c("additional_name", "additional_level")) |>
    uniteAdditional(cols = c("device_concept_id")) 
  # uniteAdditional(cols = c("unique_device_identifier", "device_concept_id")) 
}

# Summarise devices used by procedures ----
pda_closure_with_prosthesis <- newCodelistWithDetails(list("pda_closure" = getDescendants(cdm, conceptId = 4019228)))
cdm[["pda_closure_with_prosthesis"]] <- conceptCohort(cdm,
                                                      conceptSet = pda_closure_with_prosthesis,
                                                      name = "pda_closure_with_prosthesis")

pda_division <- newCodelistWithDetails(list("pda_division" = getDescendants(cdm, conceptId = 4047352)))
cdm[["pda_division"]] <- conceptCohort(cdm,
                                       conceptSet = pda_division,
                                       name = "pda_division")
pda_ligature <- newCodelistWithDetails(list("pda_ligature" = getDescendants(cdm, conceptId = 4019226)))
cdm[["pda_ligature"]] <- conceptCohort(cdm,
                                       conceptSet = pda_ligature,
                                       name = "pda_ligature")
fluoroscopy <- newCodelistWithDetails(list("fluoroscopy" = getDescendants(cdm, conceptId = 35622113)))
cdm[["fluoroscopy"]] <- conceptCohort(cdm,
                                      conceptSet = fluoroscopy,
                                      name = "fluoroscopy")
surgery <- newCodelistWithDetails(list("surgery" = getDescendants(cdm, conceptId = c(44791517,4250892,44517227,44517228,4110974,44517226,44791517,44803149))))
cdm[["surgery"]] <- conceptCohort(cdm,
                                  conceptSet = surgery,
                                  name = "surgery")
cdm <- bind(cdm[["pda_closure_with_prosthesis"]],
            cdm[["pda_division"]], 
            cdm[["pda_ligature"]],
            cdm[["fluoroscopy"]],
            cdm[["surgery"]],
            name = "procedures_cohort")

cdm[["procedures_cohort"]] <- cdm[["procedures_cohort"]] |>
  left_join(
    cdm[["device_exposure"]] |>
      select("subject_id" = "person_id", "cohort_start_date" = "device_exposure_start_date", "device_concept_id"),
    by = c("subject_id", "cohort_start_date")
  ) |>
  mutate("device_concept_id" = coalesce(device_concept_id, -1)) |> 
  compute(temporary = FALSE, name = "procedures_cohort")

cdm[["procedures_cohort"]] <- cdm[["procedures_cohort"]] |>
  left_join(
    cdm[["concept"]] |>
      select("device_concept_id" = "concept_id", "device_concept_name" = "concept_name"),
    by = "device_concept_id"
  ) |>
  mutate("device_concept_name" = coalesce(device_concept_name, "No device identified")) |>
  compute(temporary = FALSE, name = "procedures_cohort")

cohort_ids <- cdm[["procedures_cohort"]] |> 
  settings() |>
  pull("cohort_definition_id")

devices_dictionary <- cdm[["procedures_cohort"]] |>
  select("device_concept_id", "device_concept_name") |>
  distinct() |>
  collect()

for(i in cohort_ids){
  cdm[["procedures_to_devices_cohort"]] <- cdm[["procedures_cohort"]] |>
    subsetCohorts(cohortId = i, name = "procedures_to_devices_cohort")
  
  cdm[["procedures_to_devices_cohort"]] <- cdm[["procedures_to_devices_cohort"]] |> 
    mutate("value" = 1L) |>
    pivot_wider("names_from" = device_concept_id, 
                "values_from" = value,
                names_prefix = "dev_") |>
    mutate(across(starts_with("dev_"), ~as.character(coalesce(., "0")))) |>
    compute(temporary = FALSE, name = "procedures_to_devices_cohort")
  
  dev_cols <- colnames(cdm[["procedures_to_devices_cohort"]])[grepl("dev_",colnames(cdm[["procedures_to_devices_cohort"]]))]
  
  results[[paste0("summarise_devices_",i)]] <- summariseCharacteristics(
    cdm[["procedures_to_devices_cohort"]],
    demographics = FALSE,
    otherVariables = dev_cols
  )
  
  results[[paste0("summarise_devices_",i)]] <- results[[paste0("summarise_devices_",i)]] |>
    filter(is.na(variable_level) | variable_level == 1) |>
    mutate("device_concept_id" = as.integer(str_extract(variable_name, "\\d+"))) |>
    mutate("device_concept_id" = if_else(device_concept_id == 1, -1, device_concept_id)) |>
    left_join(
      devices_dictionary,
      by = "device_concept_id"
    ) |>
    select(-c("additional_name", "additional_level")) |>
    uniteAdditional(cols = c("device_concept_id", "device_concept_name")) 
}


# Export summarised result ----
exportSummarisedResult(bind(results), 
                       minCellCount = 5, 
                       fileName = "device_exposure_characterisation_v2.csv", 
                       path = here("Results"))


# Import summarised result and fix counts in UDIs ----
rb <- importSummarisedResult(path = "Results/device_exposure_characterisation_v2.csv")
old_settings <- rb |>
  filterSettings(c(result_type == "summarise_characteristics" & table_name  == "devices_udi")) |>
  settings()

rb <- rb |>
  filterSettings(!c(result_type == "summarise_characteristics" & table_name  == "devices_udi")) |> 
  bind(
    rb |>
      filterSettings(result_type == "summarise_characteristics",
                     table_name  == "devices_udi") |>
      group_by(group_level, variable_name) |>
      mutate("value" = if_else(any(estimate_value == "-", na.rm = TRUE), "TRUE", "0")) |>
      mutate("estimate_value" = case_when(
        value == "TRUE" & estimate_value != "-" & estimate_name == "percentage" ~ as.character(round(as.numeric(estimate_value))),
        value == "TRUE" & estimate_value != "-" & estimate_name == "count" ~ "-",
        .default = estimate_value
      )) |> 
      select(-"value") |>
      ungroup() |>
      newSummarisedResult("settings" = old_settings)
  )
exportSummarisedResult(rb, path = "Results", fileName = "device_exposure_characterisation_v2_clean.csv")


