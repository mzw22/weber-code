
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

# Import data with name from original study code
PSexp_comp <- read.csv("../../data/dixit2022/published_data/Experiments_complexity.csv")

# Excluding duplicates (just for this data) - code taken from original
PSexp_comp<-PSexp_comp%>%mutate(Year_Nest = paste(Year, HostNest, sep = "_"))
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2018_PS085"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2018_PS098"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS009"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS043"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS096"),]
PSexp_comp<-PSexp_comp[!(PSexp_comp$Year_Nest=="2020_PS159"),]
PSexp_comp<-PSexp_comp[complete.cases(PSexp_comp[,"HostAv_COMPLEXITY_reduced_ac"]),]

# Rename modified data
input_data <- PSexp_comp

# Standardise data
data <- input_data %>%
  mutate(a = Exp_COMPLEXITY_ac,
         b = HostAv_COMPLEXITY_reduced_ac,
         response = Egg_rejected) %>% #binary datat
  # Get absolute magnitude difference and mean magnitude
  mutate(abs_diff = abs(a - b),
         mean_ab = (a + b*(ClutchSize - 1))/ClutchSize) %>% #mean complexity in nest
  # Keep only the relevant columns
  dplyr::select(c(a, b, abs_diff, mean_ab, response))

# Test plot (k = 1)
plot(response~I(abs_diff/mean_ab), data=data, ylim=c(0, 1))

# Export standardised data
write.csv(data, "../../results/standard_data/dixit2022_standard.csv",
          row.names=FALSE)
  
# 3. Estimate k ----------------------------------------------------------------

# Check number of unique magnitude pairs
data <- data %>%
  mutate(pair = ifelse(a < b,
                       paste(a, b, sep="_"),
                       paste(b, a, sep="_")))
n_pairs <- length(unique(data$pair))
n_pairs
# n = 119

# Find best value of k + 95% conf intervals
k_estimate <- estimate_k(di=data$abs_diff, i=data$mean_ab, k_range=c(-1, 3),
                         response=data$response,
                         family=binomial(link="logit"))
k_estimate

# 4. Plot and summarise the different models -----------------------------------

export_model_results(di=data$abs_diff, i=data$mean_ab, k_estimate=k_estimate,
                     response=data$response,
                     family=binomial(link="logit"),
                     response_lab="egg rejected Y/N",
                     stimulus="egg complexity",
                     ref="Dixit et al. 2022",
                     file_prefix="dixit2022",
                     n_pairs=n_pairs)
