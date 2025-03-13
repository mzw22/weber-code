
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
input_data <- read.csv("../../data/iwanir2019/iwanir2019_chemotaxis.csv")

# ANy other changes go here
# Correct values out of range (due to graphgrabber inaccuracy)

data <- input_data %>%
  mutate(StimA = str_match(Stimulus.A, "'\\s*(.*?)\\s*-")[,2],
         ConcIndexA = str_match(Stimulus.A, "-\\s*(.*?)\\s*'")[,2],
         StimB = str_match(Stumulus.B, "'\\s*(.*?)\\s*-")[,2],
         ConcIndexB = str_match(Stumulus.B, "-\\s*(.*?)\\s*'")[,2]) %>%
  filter(StimA == StimB) %>%
  filter(StimA != "DA" & StimA != "PD" & StimA != "IA" &
           StimA != "Py" & StimA != "TT") %>% #volatiles
  # volatiles: DA, PD, IA, Py, and TT
  mutate(ConcA = as.numeric(ConcIndexA),
         ConcB = as.numeric(ConcIndexB))

# Standardise data
data <- data %>%
  mutate(a = as.numeric(ConcA),
         b = as.numeric(ConcB),
         response = Mean.chemotaxis.index..A.relative.to.B.,
         N = 100) %>%
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/iwanir2019_salts_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 10

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-1, 3),
                         response=data$response, weights=data$N,
                         family=gaussian(link="identity"))
k_estimate

# Can't estimate k
