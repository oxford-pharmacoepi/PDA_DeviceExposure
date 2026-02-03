library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(here)
library(CodelistGenerator)
library(omopgenerics)
library(OmopConstructor)
library(CohortConstructor)
library(here)
library(readr)
library(PhenotypeR)
library(purrr)
library(CohortCharacteristics)
library(PatientProfiles)
library(httr)
library(jsonlite)
library(xml2)
library(httr)
library(stringr)
library(OmopViewer)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "...",
                      Server   = "...",
                      Database = "...",
                      TrustServerCertificate="yes",
                      Authentication = "...")

cdmSchema <- "..." ## name of the schema that contains the OMOP CDM with patient-level data
writeSchema <- "..." ## name of the schema where results tables will be created
prefix <- "..." ## prefix that will be used when creating any tables during the study execution

cdm <- CDMConnector::cdmFromCon(con = con,
                                cdmSchema = cdmSchema, 
                                writeSchema = c(schema = writeSchema,
                                                prefix = prefix))

min_cell_count = 5

source(here("Scripts", "CharacterisationOfDeviceExposureTable.R"))
