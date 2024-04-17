
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
input_data <- read.csv("../../data/caves2023/published_data/Body_size_two_choice_data.csv")

# This needs lots of data wrangling for my particular analysis, bear with
rearranged_data <- input_data # create intermediate dataframe

# Group trials in LR pairs
rearranged_data <- rearranged_data %>%
  mutate(new_ID = rep(seq(1, nrow(rearranged_data)/2, 1), each = 2))
         #this pivots nicely later so I think it worked

# Put one trial on each row
rearranged_data <- rearranged_data %>%
  group_by(new_ID) %>%
  dplyr::select(c(new_ID, Area_large, Area_small, Stimulus,
                  Stimulus_Side, Time_with_Stim)) %>%
  pivot_wider(names_from="Stimulus_Side",
              values_from=c("Stimulus", "Time_with_Stim",
                            "Area_large", "Area_small")) %>%
  ungroup() %>%
  mutate(Area_large = Area_large_Left,
         Area_small = Area_small_Left,) #should be the same on both sides

# Get time with larger/smaller instead of time with left/right
rearranged_data <- rearranged_data %>%
  mutate(time_with_larger = ifelse(Stimulus_Left > Stimulus_Right,
                                   Time_with_Stim_Left, Time_with_Stim_Right)) %>%
  mutate(time_with_smaller = ifelse(Stimulus_Left < Stimulus_Right,
                                    Time_with_Stim_Left, Time_with_Stim_Right)) %>%
  mutate(total_time = time_with_larger + time_with_smaller)

# Remove trials with no time spent on either side
rearranged_data <- rearranged_data %>%
  filter(total_time > 0)

# Add discrimination index
rearranged_data <- rearranged_data %>%
  mutate(prop_time_larger = time_with_larger/total_time)

# NOW standardise data
data <- rearranged_data %>%
  mutate(a = Area_large, b = Area_small,
         response = prop_time_larger) %>% #linear response variable
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/caves2023_standard.csv",
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
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(0, 4),
                         response=data$response,
                         family=gaussian(link="identity"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=gaussian(link="identity"),
                     response_lab="prop. time with larger fish",
                     stimulus="body size",
                     ref="Caves and Kelley 2023",
                     file_prefix="caves2023",
                     n_pairs=n_pairs)

