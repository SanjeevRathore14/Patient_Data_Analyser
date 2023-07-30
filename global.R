#loading packages
library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(reactable)
library(dplyr)
library(ggplot2)
library(ggridges)
library(rmarkdown)

# adding project specific folders
if(dir.exists("R")) addResourcePath(prefix = "R", directoryPath = "R")
if(dir.exists("data")) addResourcePath(prefix = "data", directoryPath = "data")
if(dir.exists("markdown_files")) addResourcePath(prefix = "markdown_files", directoryPath = "markdown_files")

#loading demographic dataset
demographic_dataset <- readRDS(file = "data/demographic_dataset.rds")
#calculating AGE groups from AGE variable in demographic dataset
demographic_dataset <- demographic_dataset %>% 
  dplyr::mutate(AGEGROUPS = dplyr::if_else(AGE > 80, "GT 80",
                                           dplyr::if_else(AGE > 70,"71 to 80",
                                                          dplyr::if_else(AGE > 60,"61 to 70","50 to 60"))))

#loading Adverse Event dataset
adverse_event_dataset <- readRDS(file = "data/adverse_event_dataset.rds")
#mapping AGE and SEX variables from demographic dataset into adverse event dataset using "USUBJID"
adverse_event_dataset <- dplyr::left_join(x = adverse_event_dataset,
                                          y = demographic_dataset[c("USUBJID","AGE","SEX")],
                                          by = "USUBJID")

#sourcing data preview module
source(file.path("R","data_preview_module.R"))