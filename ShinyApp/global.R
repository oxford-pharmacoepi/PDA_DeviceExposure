# renv::activate()

library(bslib)
library(CohortCharacteristics)
library(dplyr)
library(DT)
library(ggplot2)
library(gt)
library(markdown)
library(omopgenerics)
library(OmopSketch)
library(plotly)
library(purrr)
library(rlang)
library(shiny)
library(shinycssloaders)
library(shinyTree)
library(shinyWidgets)
library(sortable)
library(tidyr)
library(visOmopResults)
library(yaml)
library(stringr)
library(colorspace)
library(systemfonts)

# preprocess data if it has not been done
fileData <- file.path(getwd(), "data", "studyData.RData")
if (!file.exists(fileData)) {
  source(file.path(getwd(), "rawData", "preprocess.R"))
}

# uncomment to load the raw data
# rawData <- omopgenerics::importSummarisedResult(file.path(getwd(), "rawData"))

# load shiny data
load(fileData)

# source functions
source(file.path(getwd(), "functions.R"))
