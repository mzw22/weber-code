
# Setup-------------------------------------------------------------------------

# Clear workspace
#rm(list=ls())

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(DiagrammeR) #for flowchart
  library(DiagrammeRsvg) #for saving
  library(magrittr) #for saving
  library(rsvg) #for saving
library(readxl)
library(litsearchr)
library(tidyverse)

# Extract litsearch numbers ----------------------------------------------------

# Import literature searches
wos <- read.csv("../litsearch/searches/wos_full.csv")
scopus <- read.csv("../litsearch/searches/scopus.csv")
litsearch <- read_xlsx("../litsearch/litsearch_25_11.xlsx", sheet=1) #wos + scopus
aj_papers <- read_xlsx("../litsearch/litsearch_25_11.xlsx", sheet=2)
late_papers <- read_xlsx("../litsearch/litsearch_25_11.xlsx", sheet=3)

# Import final list
final_list <- read.csv("../data/study_list.csv")
k_results <- read.csv("../results/k_results.csv")
# Count how many screened
wos_search <- 0
scopus_search <- 0

n_wos <- nrow(wos)
n_scopus <- nrow(scopus)
n_aj <- nrow(aj_papers)
n_late <- 3

# Correct rows that won't bind
litsearch$Year <- as.numeric(litsearch$Year)
# Bind full search together
full_list <- bind_rows(litsearch, aj_papers, late_papers) %>%
  # Correct NAs in relevant columns
  mutate(rejection_reason = ifelse(is.na(rejection_reason),
                                    "", rejection_reason)) %>%
  mutate(final_decision = ifelse(is.na(final_decision),
                                 "n", final_decision)) %>%
  # Remove duplicates
  filter(rejection_reason != "duplicate")
n_screened <- nrow(full_list)

# Count how many excluded for different reasons
rejected <- full_list %>%
  filter(final_decision != "y")
reason <- rejected$rejection_reason
# Print list of rejection reasons
unique(reason)

n_excluded <- nrow(rejected)
  n_human <- length(reason[reason=="human"])
  n_choice <- length(reason[reason=="not choice"|
                              reason=="no choice"|
                              reason=="not difference"])
  n_nostim <- length(reason[reason=="no axis"|
                            reason=="not quantitative"|
                            reason=="cat input"|
                            reason=="binocular disparity"|
                            reason=="gabor patch"|
                            reason=="acuity"|
                            reason=="detection"|
                            reason=="internal stimulation"|
                            reason=="preference unclear"])
  n_irrelevant <- n_excluded - (n_human+n_choice+n_nostim)
# check total
n_irrelevant+n_human+n_choice+n_nostim

# Remove NAs
k_results <- k_results  %>%
  mutate(exclude = ifelse(is.na(exclude), "", exclude))
# Check how many results were deliberately excluded
k_fitted <- k_results %>%
  filter(exclude != "pathological fit")
# Check how many datasets are in the final table
k_table <- read.csv("../results/meta_analysis/k_results_table.csv")

# Get subset of papers that were selected
selected <- full_list %>%
  filter(final_decision == "y")
n_selected <- nrow(selected)

# Number of studies for which data was unavailable
available <- selected %>%
  filter(availability == "online" |
         availability == "figure" |
         availability == "email") #data obtained
n_unavailable <- n_selected - nrow(available)

# Get number of datasets studied
# Count up data extraction subset
n_attempted <- nrow(final_list)
n_studiesused <- length(unique(final_list$authors))

# Final count
n_final <- nrow(k_table)
# Number of excluded estimates
n_failed <- n_attempted - n_final

# Number of failed k extractions
n_model <- nrow(final_list) - nrow(k_fitted)

# k interval too large
k_fitted$interval <- k_fitted$k_upper_95 - k_fitted$k_lower_95
interval <- k_fitted %>%
  filter(interval > 3)
n_conf <- nrow(interval)

# Pathological fit
n_badfit <- n_failed - (n_model + n_conf)
  
# Flowchart --------------------------------------------------------------------

# Tutorial here:
# https://mikeyharper.uk/flowcharts-in-r-using-diagrammer/

png(filename="../results/figures/flowchart.png")

# Flowchart
flowchart <- 
  'digraph {
    # Create graph
    graph[overlap = true]
    
    # Create box nodes
    node[shape = box, fontname = Arial, fontsize = 14]
    
    databases[label="Database search: \n @@1 \n @@2"]
    other[label="Other sources: \n @@3 \n @@4"]
    screened[label="@@5"]
    excluded[label="@@6 \n @@7 \n @@8 \n @@9 \n @@10"]
    selected[label="@@11"]
    unavailable[label="@@12"]
    attempted[label="@@13"]
    failed[label="@@14 \n @@15 \n @@16 \n @@17"]
    final[label="@@18"]
    
    # Create blank nodes
    node [shape = point, width = 0, height = 0]
    x1; x2; x3
    
    # Make subgraph definition so arrow is horizontal
    subgraph {
      rank = same; x1, excluded
    }
    subgraph {
      rank = same; x2, unavailable
    }
    subgraph {
      rank = same; x3, failed
    }
    
    # edge definitions with the node IDs
    { databases other } -> screened
    screened -> x1  [arrowhead="none"] #middle
    x1 -> excluded
    x1 -> selected
    selected -> x2 [arrowhead="none"] #middle
    x2 -> unavailable
    x2 -> attempted
    attempted -> x3 [arrowhead="none"] #middle
    x3 -> failed
    x3 -> final
  }
  
  [1]:  paste0("WoS (", n_wos, " studies)")
  [2]:  paste0("Scopus (", n_scopus, " studies)")
  [3]:  paste0("Akre & Johnsen 2014 (", n_aj, " studies)")
  [4]:  paste0("Published after search (", n_late, " studies)")
  [5]:  paste0("Studies screened (", n_screened, " studies)")
  [6]:  paste0("Inclusion criteria not met (", n_excluded, " studies)")
    [7]:  paste0("• not relevant (", n_irrelevant, " studies)")
    [8]:  paste0("• human (", n_human, " studies)")
    [9]:  paste0("• not a discrimination task (", n_choice, " studies)")
    [10]:  paste0("• task not suitable (", n_nostim, " studies)")
  [11]: paste0("Studies selected for inclusion (", n_selected, " studies)")
    [12]: paste0("Data unavailable (", n_unavailable, " studies)")
  [13]:  paste0("Datasets analysed (", n_studiesused, " studies, ", n_attempted, " datasets)")
    [14]:  paste0("Datasets excluded (", n_failed, " datasets)")
    [15]:  paste0("• k cannot be estimated (", n_model, " datasets)")
    [16]: paste0("• excluded due to plots (", n_badfit, " estimates)")
    [17]: paste0("• k interval too wide (", n_conf, " estimates)")
  [18]:  paste0("Estimates included in meta-analysis (", n_final, " estimates)")
'
# Visualise flowchart
grViz(flowchart)

grViz(flowchart) %>%
  export_svg %>% charToRaw %>%
  rsvg_png(file="../results/figures/flowchart.png",
           width=600)

