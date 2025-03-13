
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
input_data <- read.csv("../../data/maier1990/maier1990_Fig1.csv")

# Standardise data
data <- input_data %>%
  mutate(a = 2000, #reference freq
         b = 2000 + 2000*weber_fraction*delta_direction,
         response = discrimination_d) %>%
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data)

# Export standardised data
write.csv(data, "../../results/standard_data/maier1990_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-3, 3),
                         response=data$response, weights=NULL,
                         family=gaussian(link="identity"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response, weights=NULL,
                     family=gaussian(link="identity"),
                     response_lab="discrimination d'",
                     stimulus="frequency",
                     ref="Maier and Klump 1990",
                     file_prefix="maier1990",
                     n_pairs=n_pairs)
