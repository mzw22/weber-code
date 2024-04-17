
# Clean up
rm(list=ls())

# Packages - need to install first using install.packages("tidyr") etc
library(tidyverse)
library(tidyr)
library(dplyr)
    
# Set working directory
setwd("~/Homework/webers_law/data")

# Import data
input_data <- read.csv("./source_data/bee_etal_2012/Bee_et_al_2012_JASA_DATA.csv")

# Standardise data-----------------------------------------------------------------------------------------------------

# Give all input columns the same name
edited_data <- input_data
edited_data$a <- edited_data$Attenuated_Signal_Level
edited_data$b <- edited_data$Unattenuated_Signal_Level
edited_data$response <- edited_data$Choice #binary data
edited_data$N <- 1 #binary data

# Add new columns
edited_data <- edited_data %>% 
  mutate(abs_diff = abs(a - b)) %>%
  mutate(mean_ab = (a + b)/2) #normal mean

# Move important parts of data into new dataframe standard_data, to make it easy to read
standard_data <- edited_data %>%
  dplyr::select(c(abs_diff, mean_ab, response, N)) #dplyr:: avoids clash with MASS select

# Test plot (k = 0)
plot(response~abs_diff, data=standard_data, ylim=c(0, 1))

# Export standard data
write.csv(standard_data, "./standard_data/bee_etal_2012_standard.csv", row.names=FALSE)

