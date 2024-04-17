
# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(tidyverse)

# import data
call_properties <- read.csv("published_data/beebei_call_properties.csv")
habituation_discrimination <- read.csv("published_data/Habituation_discrimination_data_byBlock.csv")

# Add columns for mean pulse duration, interval, period
call_properties$pulse_duration <- NA
call_properties$pulse_interval <- NA
call_properties$pulse_period <- NA
for (row in 1:nrow(call_properties)) {
  pulse_durations <- c(call_properties[row, "Pulse1_Duration"],
                       call_properties[row, "Pulse2_Duration"],
                       call_properties[row, "Pulse3_Duration"],
                       call_properties[row, "Pulse4_Duration"],
                       call_properties[row, "Pulse5_Duration"],
                       call_properties[row, "Pulse6_Duration"])
  row_pulse_duration <- mean(pulse_durations, na.rm=TRUE)
  call_properties[row, "pulse_duration"] <- row_pulse_duration
  
  pulse_intervals <- c(call_properties[row, "Pulse1_Interval"],
                       call_properties[row, "Pulse2_Interval"],
                       call_properties[row, "Pulse3_Interval"],
                       call_properties[row, "Pulse4_Interval"],
                       call_properties[row, "Pulse5_Interval"],
                       call_properties[row, "Pulse6_Interval"])
  row_pulse_interval <- mean(pulse_intervals, na.rm=TRUE)
  call_properties[row, "pulse_interval"] <- row_pulse_interval
  
  pulse_periods <- c(call_properties[row, "Pulse1_Period"],
                     call_properties[row, "Pulse2_Period"],
                     call_properties[row, "Pulse3_Period"],
                     call_properties[row, "Pulse4_Period"],
                     call_properties[row, "Pulse5_Period"],
                     call_properties[row, "Pulse6_Period"])
  row_pulse_period <- mean(pulse_periods, na.rm=TRUE)
  call_properties[row, "pulse_period"] <- row_pulse_period
}

#aggression index - from Tumulty et al. 2022
PCA <- prcomp(~Agg_Calls + Pseudo_Agg_Calls + Approach + Prop_Dist, data=habituation_discrimination, center=TRUE, scale=TRUE)

#Create data frame of PC transformed variables
principal_components <- data.frame(PCA$x)
habituation_discrimination$aggression_index <- principal_components$PC1
habituation_discrimination <- habituation_discrimination %>%
  dplyr::select(c(File:Treatment_dir, Block, aggression_index)) %>%
  pivot_wider(names_from=Block, values_from=aggression_index, names_prefix="aggression_") %>%
  mutate(aggression_change = aggression_Discrimination - aggression_Last_H) %>%
  mutate(percent = ifelse(Treatment_dir == "Control", "0", sub("\\%.*", "", Treatment_dir))) %>%
  mutate(direction = sub(".*%", "", Treatment_dir)) %>%
  mutate(treatment_col = paste("X", percent, "_", direction, sep=""))

write.csv(habituation_discrimination, "aggression_data.csv", row.names=FALSE)
