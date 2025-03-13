
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

library(R.matlab) #for importing mat files

# Import matlab files
files <- list.files("../../data/thiele2024/V1paper/")
files
test <- readMat(file.path("../../data/thiele2024/V1paper/", files[1]))
test <- as.data.frame(test[[1]])

data_M1 <- readMat(file.path("../../data/thiele2024/V1paper/","M1_v1_1_fig5.mat"))
# cont=[5, 10, 15, 20, 22, 25, 28, 32, 35, 40, 45, 50, 60,  90];
# days=[343:1:359];
# chan_nums=[8 9 11 15 17 19 21 23 25 26 27 28 29 31 44 45 46 48 61 62 63 64]

data_M2 <- readMat(file.path("../../data/thiele2024/V1paper/","M2_v1_1_fig5.mat"))
# cont=[5, 10, 15, 20, 22, 25, 28, 32, 35, 40, 45, 50, 60,  90];
#days=[51:1:72];
#chan_nums=[7 9 11 12 13 14 15 16 17 18 19 20 21 22 23 25 26 27 28 29 30 31 32 51 55]

# Get data from the graph instead
input_data <- read.csv("../../data/thiele2024/monkeys.csv")

# Standardise data
data <- input_data %>%
  mutate(prop_correct = ifelse(contrast > reference, prop_higher, 1-prop_higher)) %>%
  mutate(a = contrast, b = reference,
         response = prop_correct) %>% #binomial data
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))
# We really need sample size here, but let's just see how the graph turns out

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/thiele2024_standard.csv",
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
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-20, 20),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# Plot is wonky so let's not go any further
