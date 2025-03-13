
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
input_data <- read.csv("../../data/garderes2024/garderes2024_Fig1D.csv",
                       row.names=1, header=FALSE)

# Calculate percentage correct choices
data <- as.data.frame(t(input_data)) %>%
  mutate(left_correct = F1 > F2) %>%
  mutate(percent_lick_correct = ifelse(left_correct, percent_lick_left,
                               1-percent_lick_left))

# Standardise data
data <- data %>%
  mutate(a = F1, b = F2,
         response = percent_lick_correct,
         N = 1) %>% #N MISSING - this is a placeholder to check the AIC graph
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))
warning("N MISSING (N = 1 used as placeholder)")

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/garderes2024_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 48

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-2, 5),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# I was going to email to ask for a sample size
# but nvm this AIC plot is horrible regardless of N anyway
