
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
input_data <- read.csv("../../data/schulze1999/schulze_scheich_Fig1_data.csv")

# Correct for false alarms
# corrected hit rate = (HIT - FA)/(100 - FA)
data <- input_data %>%
  mutate(hitrate = (percent_plus - percent_minus)/(100 - percent_minus))

# Standardise data
data <- data %>%
  mutate(a = freq_1, b = freq_2,
         response = hitrate,
         N = 3) %>% #1 trial, 3 animals
  mutate(response = ifelse(response > 1, 1, response)) %>% #correct out of range
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/schulze1999_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 6

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-3, 7),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# Can't fit
