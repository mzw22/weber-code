
# Setup--------------------------------------------------------------------------------------------------------------------

# Clear workspace
#rm(list=ls())

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(tidyverse)
library(fs) #paths
library(assertive.base) # for bracketing strings

# Set working directory and import all study info/results
models_folder <- "../results/data_extraction/models"
datasets <- list.files(models_folder)

# Load in model results
rm("model", "models")
for (dataset in datasets) {
  model <- read.csv(file=path(models_folder, dataset))
  # Standardise names (linear and binomial models have diff outputs)
  colnames(model)[colnames(model) == "t.value"] <- "test.statistic"
  colnames(model)[colnames(model) == "z.value"] <- "test.statistic"
  colnames(model)[colnames(model) == "Pr...t.."] <- "p.value"
  colnames(model)[colnames(model) == "Pr...z.."] <- "p.value"
  
  if (!exists("models")){
    models <- model
  } else {
    models <- rbind(models, model)
  }
}

# Add more info to models
models <- models %>%
  relocate(filename) # move ID to the left

# Export list of models 
write.csv(models, "../results/data_extraction/models_combined.csv", row.names=FALSE)

# Combine models with study metadata -------------------------------------------

### Import data
# Import list of studies
study_list <- read.csv("../data/study_list.csv")
# Combine the study info and the results
k_results <- left_join(study_list, models, by="filename")

# Add columns for labels etc.
k_results <- k_results %>%
  mutate(species = ifelse(species_1 == "", species_2,
                          paste(species_1, species_2, sep=" "))) %>%
  mutate(initial = ifelse(species_1 == "", "",
                          paste(substr(species_1, 1, 1), ". ", sep=""))) %>% #First initial
  mutate(species_short = paste(initial, species_2, sep="")) %>% # Short species name for labels
  mutate(reference = paste(authors, year)) %>%
  #reorder
  relocate(reference) %>% # move ID to the left
  relocate(filename)

# Calculate standard error from 95% conf intervals
k_results <- k_results %>%
  mutate(standard_error = (k_upper_95 - k_lower_95)/3.92) #SE formula
#formula: cochrane handbook
#(or: 95% conf interval on each side = 1.96*SE)

# Export combined list of k estimates
write.csv(k_results, "../results/k_results.csv", row.names=FALSE)

