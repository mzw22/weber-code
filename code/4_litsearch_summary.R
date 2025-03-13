
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
library(tiff) #for exporting graph

# Extract litsearch numbers ----------------------------------------------------

# Import literature searches
wos <- read.csv("../litsearch/search-02-12-24/wos_full.csv")
scopus <- read.csv("../litsearch/search-02-12-24/scopus.csv")
databases <- read_xlsx("../litsearch/Supplementary Data 2 02-12-24.xlsx", sheet=1)
aj_papers <- read_xlsx("../litsearch/Supplementary Data 2 02-12-24.xlsx", sheet=2)
# Bind full search together
litsearch <- bind_rows(databases, aj_papers) %>%
  # Remove duplicates
  filter(rejection_reason != "duplicate" | is.na(rejection_reason))
litsearch$final_decision[is.na(litsearch$final_decision)] <- "n"
n_screened <- nrow(litsearch)

# Import final study lists
k_results <- read.csv("../results/k_results.csv")
k_table <- read.csv("../results/meta_analysis/k_table.csv")

n_wos <- nrow(wos)
n_scopus <- nrow(scopus)
n_aj <- nrow(aj_papers)

# Count how many excluded for different reasons
rejected <- litsearch %>%
  filter(final_decision != "y")
reason <- rejected$rejection_reason
# Print list of rejection reasons
unique(reason)

n_studiesexcluded <- nrow(rejected)
  n_human <- length(reason[reason=="human"])
  n_choice <- length(reason[reason=="not choice"|
                              reason=="no choice"|
                              reason=="not difference"|
                              reason == "higher processing"])
  n_nostim <- length(reason[reason=="no axis"|
                            reason=="not quantitative"|
                            reason=="cat input"|
                            reason=="binocular disparity"|
                            reason=="gabor patch"|
                            reason=="acuity"|
                            reason=="detection"|
                            reason=="internal stimulation"|
                            reason=="preference unclear"])
  n_irrelevant <- n_studiesexcluded - (n_human+n_choice+n_nostim)
# check total
n_irrelevant + n_human + n_choice + n_nostim

# Get subset of papers that were selected
n_selected <- litsearch %>%
  filter(final_decision == "y") %>%
  count()

# Number of studies for which data was unavailable
n_unavailable <- litsearch %>%
  filter(final_decision == "y") %>%
  filter(data == "unavailable") %>% count() #data can't be obtained

# Final list for data extraction
n_attempted <- k_table %>% count()
# Remove ones with no sample size
k_results_filtered <- k_results %>%
  filter(is.na(exclude) | exclude != "no sample size")
n_attempted_studies <- litsearch %>%
  filter(final_decision == "y") %>%
  filter(data != "unavailable") %>% count() #data obtained 

# Number of excluded estimates
n_dataexcluded <- k_table %>% filter(Included == "N") %>% count()
# k can't be estimated
n_failed <- k_table %>% filter(Exclusion.reason == "k failed") %>% count()
# k interval too large
n_conf <- k_table %>% filter(Exclusion.reason == "interval") %>% count()
# Pathological fit
n_plot <- k_table %>%
  filter(Exclusion.reason == "response plot" |
           Exclusion.reason == "AIC plot") %>% count()

# Number in final list
n_included <- k_table %>% filter(Included == "Y") %>% count()
  
# Flowchart --------------------------------------------------------------------

# Tutorial here:
# https://mikeyharper.uk/flowcharts-in-r-using-diagrammer/

tiff(filename="../results/figures/flowchart.tif")

# Flowchart
flowchart <- 
  'digraph {
    # Create graph
    graph[overlap = true]
    
    # Create box nodes
    node[shape = box, fontname = Arial, fontsize = 12]
    
    databases[label="Database search \n @@1\\l @@2\\l"]
    other[label="Other sources \n @@3\\l"]
    screened[label="@@5\\l"]
    excluded[label="@@6 \n @@7\\l @@8\\l @@9\\l @@10\\l"]
    selected[label="@@11"]
    unavailable[label="@@12"]
    attempted[label="@@13"]
    failed[label="@@14 \n @@15\\l @@16\\l @@17\\l"]
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
  
  [1]:  paste0("• WoS (", n_wos, " studies)")
  [2]:  paste0("• Scopus (", n_scopus, " studies)")
  [3]:  paste0("• Akre & Johnsen 2014 (", n_aj, " studies)")
  [4]:
  [5]:  paste0("Studies screened (", n_screened, " studies)")
  [6]:  paste0("Inclusion criteria not met (", n_studiesexcluded, " studies)")
    [7]:  paste0("• Not relevant (", n_irrelevant, " studies)")
    [8]:  paste0("• Human subjects (", n_human, " studies)")
    [9]:  paste0("• Not a discrimination task (", n_choice, " studies)")
    [10]:  paste0("• Magnitude axis undefined (", n_nostim, " studies)")
  [11]: paste0("Studies selected for inclusion (", n_selected, " studies)")
    [12]: paste0("Data unavailable (", n_unavailable, " studies)")
  [13]:  paste0("Datasets analysed (", n_attempted_studies, " studies, ", n_attempted, " datasets)")
    [14]:  paste0("Datasets excluded (", n_dataexcluded, " datasets)")
    [15]:  paste0("• k cannot be estimated (", n_failed, " datasets)")
    [16]: paste0("• Excluded due to plots (", n_plot, " estimates)")
    [17]: paste0("• k interval too wide (", n_conf, " estimates)")
  [18]:  paste0("Estimates included in meta-analysis (", n_included, " estimates)")
'
# Visualise flowchart
grViz(flowchart)

# Save the flowchart as a png
grViz(flowchart) %>%
  export_svg %>% charToRaw %>%
  rsvg(width = 566) %>%
  tiff::writeTIFF("../results/figures/flowchart.tiff")
# mm to px at 300dpi:
# px = in*dpi
# px = mm*(in/mm)*dpi
# 150 -> 1771
# 230 -> 2716
