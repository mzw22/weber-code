
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

# Import data with pandas (.pickle file)
library("reticulate")
py_install("pandas")
source_python("../../data/benozzo2022/read_pickle.py") #python script
pickle <- read_pickle_file("../../data/benozzo2022/email_data/behavioral_data.pickle")

#- acc: the correct response rate for each stimulus pair
#- n_corr_tot: the number of correct response for each stimulus pair
#- n_nncorr_tot: error responses
#- cartesian_prod: stimulus pair

input_data <- data.frame(acc = pickle$acc,
                         n_corr_tot = pickle$n_corr_tot,
                         n_nncorr_tot = pickle$n_nncorr_tot)
a <- c()
b <- c()
for (pair in pickle$cartesian_prod){
  a_i <- pair[[1]]
  b_i <- pair[[2]]
  a <- c(a, a_i)
  b <- c(b, b_i)
}
input_data$a <- a
input_data$b <- b

# Standardise data
data <- input_data %>%
  mutate(N = n_corr_tot+n_nncorr_tot) %>% # Get total n trials for each pair
  mutate(response = n_corr_tot/N) %>%
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/benozzo2022_standard.csv",
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
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-2, 3),
                         response=data$response, weights=data$N,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response, weights=data$N,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="distance",
                     ref="Benozzo et al. 2022",
                     file_prefix="benozzo2022",
                     n_pairs=n_pairs)
