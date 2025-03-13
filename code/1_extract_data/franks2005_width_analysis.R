
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
input_data <- read.csv("../../data/franks2005/table1.csv")

# Remove experiments changing multiple variables (lines 1 and 5)
sub_data <- input_data[-c(1, 5),]

# Subset: number experiment
data <- sub_data %>%
  mutate(num_a = str_split(nest_A_entrancenoxwidth, "x", simplify = TRUE)[,1],
         width_a = str_split(nest_A_entrancenoxwidth, "x", simplify = TRUE)[,2],
         num_b = str_split(nest_B_entrancenoxwidth, "x", simplify = TRUE)[,1],
         width_b = str_split(nest_B_entrancenoxwidth, "x", simplify = TRUE)[,2]) %>%
  mutate(num_a=as.numeric(num_a), num_b=as.numeric(num_b),
         width_a=as.numeric(width_a), width_b=as.numeric(width_b)) %>%
  # Ants prefer fewer entrances
  mutate(total_chose = no.colonies.nest.A + no.colonies.nest.B,
         prop_chose = ifelse(width_a > width_b, no.colonies.nest.B/total_chose,
                           no.colonies.nest.A/total_chose)) %>%
  mutate(a = width_a, b = width_b,
         response = prop_chose, N=total_chose) %>% #binomial data
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, N, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/franks2006_num_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 5

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-5, 10),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# Can't estimate k
