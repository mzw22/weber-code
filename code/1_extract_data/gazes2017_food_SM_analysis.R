
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
input_data <- read.csv("../../data/gazes2017/GazesEtAlData.csv")

# Check how many conditions there are
unique(input_data$Stimuli)
unique(input_data$Species)

# Subset: food experiment
data <- input_data %>%
  filter(Stimuli == "Food", Species == "SM") %>%
  mutate(a = LeftChoiceQuantity, b = RightChoiceQuantity,
         response = Correct) %>% #binary data
  filter(response==0 | response==1) %>% #remove NAs
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/gazes2017_food_SM_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 11

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-1, 3),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="Chose higher quantity y/n",
                     stimulus="Quantity (food)",
                     ref="Gazes et al. 2017 (food, spider monkey)",
                     file_prefix="gazes2017_food_SM",
                     n_pairs=n_pairs)

