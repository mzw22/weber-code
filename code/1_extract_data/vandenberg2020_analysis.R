
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

# Import data with original name to make the same changes as original
input_data <- read.csv("../../data/vandenberg2020/published_data/Data_JEB.csv")
input_reflectances <- read.csv("../../data/vandenberg2020/reflectances_list.csv")

reflectances <- mutate(input_reflectances, stimulus = NA)

for(row in seq(1, nrow(reflectances), 1)){
  if (reflectances[row, "Stimulus"] == "background"){
    reflectances[row, "stimulus"] <- reflectances[row + 1, "Stimulus"]
  } else {
    reflectances[row, "stimulus"] <- reflectances[row, "Stimulus"]
  }
}

reflectances <- mutate(reflectances, stimulus_or_bg = ifelse(Stimulus == "background",
                                                             "background_dbl", "stimulus_dbl")) %>%
  mutate(background = gsub(" Background", "", Background)) %>%
  mutate(new_ID = paste(stimulus, background, sep=" ")) %>%
  dplyr::select(c("new_ID", "stimulus", "stimulus_or_bg", "dbl_mean"))
reflectances_wide <- pivot_wider(reflectances, id_cols = new_ID,
                                 names_from=stimulus_or_bg, values_from=c(dbl_mean))

data <- input_data %>%
  mutate(new_background = ifelse(background == "2bright", "Bright", "Dark")) %>%
  mutate(new_ID = paste(stimulus, new_background, sep = " "))

data <- left_join(reflectances_wide, data, by = join_by(new_ID))

# ANy other changes go here

# Standardise data
data <- data %>%
  mutate(a = stimulus_dbl,
         b = background_dbl,
         response = correct) %>% #binary data
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/vandenberg2020_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 24

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(0, 3),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="luminance",
                     ref="van den Berg et al. 2020",
                     file_prefix="vandenberg2020",
                     n_pairs=n_pairs)

