
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
bat1 <- readMat("../../data/firzlaff2006/email_data/m4d_Bat1.mat")
str(bat1)
bat1 <- as.data.frame(t(bat1$result)) #extracts and rotates result, then makes into dataframe
bat1$bat_no <- 1 #new column for bat identity
bat2 <- readMat("../../data/firzlaff2006/email_data/m4d_Bat2.mat")
bat2 <- as.data.frame(t(bat2$result))
bat2$bat_no <- 2
bat3 <- readMat("../../data/firzlaff2006/email_data/m4d_Bat3.mat")
bat3 <- as.data.frame(t(bat3$result))
bat3$bat_no <- 3
bat4 <- readMat("../../data/firzlaff2006/email_data/m4d_Bat4.mat")
bat4 <- as.data.frame(t(bat4$result))
bat4$bat_no <- 4

# Combine matlab files
input_data <- bind_rows(bat1, bat2, bat3, bat4)
names(input_data) <- c("date",
                          "ref_roughness",
                          "delta_roughness",
                          "IR_ID_1",
                          "IR_ID_2",
                          "ref_target",
                          "choice_target",
                          "time_delay",
                          "bat_no")
input_data <- input_data %>%
  filter(date > 0) #gets rid of weird rows

# Export combined data
write.csv(input_data, "../../data/firzlaff2006/firzlaff2006_allbats.csv",
          row.names=FALSE)

# Standardise data
data <- input_data %>%
  mutate(correct = ifelse(ref_target == choice_target, 1, 0)) %>% #correct choice?
  mutate(a = ref_roughness,
         b = ref_roughness + delta_roughness,
         response = correct) %>% #binary data
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b)/2) %>% #normal mean
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/firzlaff2006_standard.csv",
          row.names=FALSE)

# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 51

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-4, -1),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="discrimination probability",
                     stimulus="echo roughness",
                     ref="Firzlaff et al. 2006",
                     file_prefix="firzlaff2006",
                     n_pairs=n_pairs)
