
# 1. Setup ---------------------------------------------------------------------

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Download my package for Weber's law analysis
#library(devtools)
#install_github("mzw22/kber")

# Load packages: install first using install.packages("tidyverse") etc
library(tidyverse)
library(fmsb) #for calculating Nagelkerke's R^2
library(kber) #for Weber's law analysis
library(readxl) #reading excel spreadsheets

# Load source code
source("../analysis_functions.R") #plots/summaries

# 2. Standardise data ----------------------------------------------------------

# Import data
input_data <- read.csv("../../data/langen1999/PairedChoiceSJ.csv")

# Subset: pebbles experiment
data <- input_data %>%
  mutate(weight.max = ifelse(weight.a > weight.b, weight.a, weight.b)) %>%
  mutate(a = weight.a, b = weight.b,
         response = ifelse(weight.chosen == weight.max, 1, 0)) %>% #binary data
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/langen1999_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 16

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-10, 20),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="Chose higher weight y/n",
                     stimulus="Weight",
                     ref="Langen 1999",
                     file_prefix="langen1999",
                     n_pairs=n_pairs)

