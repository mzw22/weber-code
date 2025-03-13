
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
library(reshape2) #matlab data wrangling

# Import matlab files
files <- list.files("../../data/zhang2023/Data for Neurons in the primary visual cortex")
files

# Read matlab file
input.data <- readMat(file.path("../../data/zhang2023/Data for Neurons in the primary visual cortex",
                          "visual_behaviordata_byunit.mat"))
# Make it into a dataframe
input.data.in <- input.data[[1]]
data.imp <- t(as.data.frame(input.data.in))
data.flat <- as.data.frame(data.imp)

# Unnest data!
data.long <- data.flat[0, ]
for (row in 1:nrow(data.flat)){
  print(row)
  data.row <- data.flat[row, ] # slice one row at a time
  # Get dimensions
  slice.length <- length(data.row[, 3][[1]])
  slice.width <- length(colnames(data.row))
  # Expand to the correct size
  data.slice <- matrix(c(rep.int(NA, slice.width)),
                       nrow=slice.length,
                       ncol=slice.width)
  colnames(data.slice) = colnames(data.row)  
  data.slice <- as.data.frame(data.slice)
  #data.slice$i <- 1:slice.length
  
  # Extract metadata
  subject <- unlist(data.row[, 1])
  session <- unlist(data.row[, 2])
  # Fill these into the columns
  data.slice[1] <- rep(subject, slice.length)
  data.slice[2] <- rep(session, slice.length)
  # Fill out the columns
  for (colname in colnames(data.row)[3:27]){
    col <- as.vector(data.row[, colname][[1]])
    data.slice[, colname] <- col
  }
  # Add to the big data
  data.long <- rbind(data.long, data.slice)
}

# Standardise data
data <- data.long %>%
  # Standardise
  mutate(a = LowerFieldDotFraction,
         b = 1 - LowerFieldDotFraction) %>% #binary data
  # Transform to binomial data just to save processing power lmao
  #group_by(a, b) %>%
  filter(!is.na(Correct)) %>%
  mutate(response = Correct) %>%
  #summarise(response=sum(Correct)/n(), N=n()) %>%
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/zhang2023_standard.csv",
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
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-2, 2),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="Correct choice y/n",
                     stimulus="Proportion of dots",
                     ref="Zhang et al. 2023",
                     file_prefix="zhang2023",
                     n_pairs=n_pairs)
