# shiny is prepared to work with this resultList:
resultList <- list(
  summarise_omop_snapshot = list(result_type = "summarise_omop_snapshot"),
  summarise_observation_period = list(result_type = "summarise_observation_period"),
  summarise_person = list(result_type = "summarise_person"),
  summarise_device_exposure = list(result_type = "summarise_clinical_records"),
  summarise_devices_to_procedures = list(table_name = "devices_to_procedures_cohort"),
  summarise_procedures_to_devices = list(table_name = "procedures_to_devices_cohort"),
  summarise_udi = list(table_name ="devices_udi"),
  summarise_code_use = list(result_type = "code_use")
)

source(file.path(getwd(), "functions.R"))

result <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))
data <- prepareResult(result, resultList)
values <- getValues(result, resultList)

values$summarise_devices_to_procedures_cohort_name <- str_to_sentence(gsub("_", " ",values$summarise_devices_to_procedures_cohort_name))
values$summarise_udi_cohort_name <- str_to_sentence(gsub("_", " ",values$summarise_udi_cohort_name))
values$summarise_procedures_to_devices_cohort_name <- case_when(
  values$summarise_procedures_to_devices_cohort_name == "pda_division" ~ "Division of Patent Ductus Arteriosus",
  values$summarise_procedures_to_devices_cohort_name == "pda_ligature" ~ "Ligature of Patent Ductus Arteriosus",
  values$summarise_procedures_to_devices_cohort_name == "pda_closure" ~ "Closure of Patent Ductus Arteriosus (With Prosthesis)",
  values$summarise_procedures_to_devices_cohort_name == "fluoroscopy"  ~ "Fluoroscopy",
  values$summarise_procedures_to_devices_cohort_name == "surgery" ~ "Surgery") 

# edit choices and values of interest
choices <- values
selected <- getSelected(values)

selected$summarise_devices_to_procedures_cohort_name <- values$summarise_devices_to_procedures_cohort_name[1]
selected$summarise_procedures_to_devices_cohort_name <- values$summarise_procedures_to_devices_cohort_name[1]

selected$summarise_code_use_year <- "overall"

save(data, choices, selected, values, file = file.path(getwd(), "data", "studyData.RData"))

rm(result, values, choices, selected, resultList, data)
