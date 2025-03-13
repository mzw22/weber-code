
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
input_data <- read.csv("../../data/lind2013/lind2013_fig2.csv")

# Any other changes go here
data <- input_data %>%
  mutate(contrast = round(contrast, 2)) %>%
  # Michelson contrast = (Imax-Imin)/(Imax+Imin)
  #symmetric around average of 47
  # therefore: Imax+Imin=47*2
  # therefore: Imax-Imin = contrast*(47*2)
  mutate(difference = contrast*(47*2))

# Standardise data
data <- data %>%
  mutate(a = 47 + difference/2,
         b = 47 - difference/2,
         response = fraction_correct,
         N = 40) %>% #from figure 2 caption: 40 choices
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/lind2013_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-1, 3),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# mean is the same for everything: can't estimate
