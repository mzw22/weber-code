
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

# Import hit/FA data
input_data <- read.csv("../../data/colbert2004/colbert_data.csv")

# Correct for FAs: corrected hitrate = (HIT - FA)/(100 - FA)
data <- input_data %>%
  pivot_longer(cols=prop_0.5:prop_0.065,
               names_to="test_conc", names_prefix="prop_") %>%
  pivot_wider(names_from=result) %>%
  mutate(hitrate = (HIT - FA)/(100 - FA))

# Standardise data
data <- data %>%
  mutate(a = as.numeric(test_conc),
         b = 0.05, #standard concentration
         response = hitrate,
         N = 15*10) %>% # 15 sessions of testing
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/colbert2004_standard.csv",
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
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-1, 4),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response, weights=data$N,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="concentration",
                     ref="Colbert et al. 2004",
                     file_prefix="colbert2004",
                     n_pairs=n_pairs)
