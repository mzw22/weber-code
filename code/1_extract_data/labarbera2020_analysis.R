
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

# 2. Data inspection -----------------------------------------------------------

# Import data
input_data <- read.csv("../../data/labarbera2020/published_data/LaBarbera_etal2020_prop_proc_data.csv")

# Standardise data
data <- input_data %>%
  filter(Response == "Y") %>% #remove trials with no response
  filter(!is.na(Choice)) %>%
  mutate(longer_option = ifelse(Leading > Lagging, "Lead", "Lag")) %>%
  mutate(chose_longer = ifelse(Choice == longer_option, 1, 0)) %>%
  mutate(a = Lagging, b = Leading, #no. pulses
         response = chose_longer) %>% #binary data
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/labarbera_etal_2020_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 15

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(0, 2.5),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="Call pulse no.",
                     ref="LaBarbera et al. 2020",
                     file_prefix="labarbera2020",
                     n_pairs=n_pairs)

