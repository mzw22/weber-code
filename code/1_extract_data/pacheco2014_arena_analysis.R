
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
input_data <- read.csv("../../data/pacheco2014/male-SPL-influences-phonotaxis-comparing-techniques.csv")

# Convert responses from left/right to correct/incorrect
data <- input_data %>%
  mutate(focal_louder = ifelse(AMP.Call > 0, 1, -1)) %>%
  mutate(z_louder = Original.Data*focal_louder) %>% #movement towards louder speaker
  filter(Experiment.Method == "Arena") # Only arena (no treadmill)

# Import actual spl values
call_spl <- read.csv("../../data/pacheco2014/call_spl.csv")
spl <- call_spl$spl_actual
# Transfer spl values to data
data$b <- spl[3]
data <- data %>%
  mutate(call_row = AMP.Call + 3) %>%
  mutate(a = spl[call_row])

# Standardise data
data <- data %>%
  mutate(response = z_louder) %>%
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data)

# Export standardised data
write.csv(data, "../../results/standard_data/pacheco2014_arena_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 4

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-1, 2.0),
                         response=data$response,
                         family=gaussian(link="identity"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=gaussian(link="identity"),
                     response_lab="Phonotaxis (Z score)",
                     stimulus="Call SPL",
                     ref="Pacheco and Bertram 2014 (arena)",
                     file_prefix="pacheco2014_arena",
                     n_pairs=n_pairs)

