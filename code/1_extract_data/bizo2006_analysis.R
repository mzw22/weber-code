
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

# Load source code
source("../analysis_functions.R") #plots/summaries

# 2. Standardise data ----------------------------------------------------------

# Import data
input_data <- read.csv("../../data/bizo2006/bizo2006_fig1.csv")

# Standardise data
data <- input_data %>%
  # Calculate proportion of correct choices
  mutate(prop_correct = ifelse(test_time > ref_time,
                               prop_long_responses,
                               1 - prop_long_responses)) %>%
  mutate(a = test_time, b = ref_time,
         response = prop_correct) %>%
  mutate(N = round(200/nrow(data), 0)) %>% #From paper: last 200 trials used
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/bizo2006_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 110

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(0.5, 2.0),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response, weights=data$N,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="duration",
                     ref="Bizo et al. 2006",
                     file_prefix="bizo2006",
                     n_pairs=n_pairs)

