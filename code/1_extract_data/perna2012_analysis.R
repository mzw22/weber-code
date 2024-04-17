
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

# Get a list of all the files with radius 1
# (don't want to overcomplicate with bigger radius)
r1_files <- list.files("../../data/perna2012/email_data/r1")

# Remove the second line from each text file and save as a csv
r1_all <- data.frame() #empty dataframe for later
for (file in r1_files){
  # Read in .txt file, remove line 2, and save to extracted_data
  r1_lines <- readLines(file.path("../../data/perna2012/email_data/r1", file))
  r1_lines <- r1_lines[-2]
  writeLines(r1_lines, file.path("../../data/perna2012/extracted_data/r1_lines", file))
  # Read in again as a dataframe and save as a .csv
  r1_data <- read.table(file.path("../../data/perna2012/extracted_data/r1_lines", file),
                        sep = "\t", header=TRUE, row.names=NULL)
  # Correct janky col names
  colnames(r1_data) <- colnames(r1_data)[2:17]
  r1_data <- r1_data %>% select(1:16)
  # Rename file and save as a .csv
  new_file <- gsub(".txt", ".csv", file)
  write.csv(r1_data, file.path("../../data/perna2012/extracted_data/r1_data", new_file),
            row.names=FALSE)
  
  r1_all <- rbind(r1_all, r1_data) #combine r1 files
}
# Export combined r1 file
write.csv(r1_all, "../../data/perna2012/extracted_data/r1_all.csv",
          row.names=FALSE)

# Get relationship between concentrations and turning probability

# the sums sum1 to sum8 are the sums of pheromone inside each one of eight
# octants of the circle around the ant. Each octant covers 45 degrees,
# starting from 1.front-right, and proceeding clockwise 2.right-front,
# 3.right-back, 4.back-right, 5.back-left, 6.left-back, 7.left-front, 8
# front-left.
r1_FB <- r1_all %>%
  mutate(left_conc = (somma1 + somma2 + somma3 + somma4)/4,
         right_conc = (somma5 + somma6 + somma7 + somma8)/4) %>%
  mutate(right_bigger = ifelse(right_conc > left_conc, 1, -1)) %>%
  mutate(abs_angle = Angle*right_bigger) 

# Export this too
write.csv(r1_FB, "../../data/perna2012/extracted_data/perna2012_FB_data.csv",
          row.names=FALSE)

# NOW standardise data
data <- r1_FB %>%
  mutate(a = right_conc, b = left_conc,
         response = ifelse(abs_angle > 0, 1, 0)) %>% #binary: turn towards/away
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  filter(mean_ab > 0) %>% #can't have a magnitude of 0
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response, N))

# Test plot (k = 1)
#plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))
  #slow - only run if necessary

# Export standard data
write.csv(data, "../../results/standard_data/perna2012_standard.csv",
          row.names=FALSE)

# Start here if redoing
#data <- read.csv("../../results/standard_data/perna2012_standard.csv")

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 742817 (lol)

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(0, 1.5),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="pheromone conc.",
                     ref="Perna et al. 2012",
                     file_prefix="perna2012",
                     n_pairs=n_pairs)

