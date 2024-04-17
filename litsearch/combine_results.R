
#load packages---------------------------------------------------------------------------------------

library(litsearchr) #used to extract keywords and make network
library(tidyverse)
library(readxl)

#set wd and clean up----------------------------------------------------------------------------------

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#import data------------------------------------------------------------------------------------------

#import new searches
wos_raw <- read.csv("searches/wos_full.csv")

# Reorganise wos search
wos <- wos_raw
#rename columns
names(wos)[names(wos) == "Article.Title"] <- "Title"
names(wos)[names(wos) == "DOI.Link"] <- "Link"
names(wos)[names(wos) == "Publication.Year"] <- "Year"
names(wos)[names(wos) == "Research.Areas"] <- "WoS.Research.Areas"
names(wos)[names(wos) == "Keywords.Plus"] <- "WoS.Keywords"
#add scopus columns
wos$Scopus.Keywords <- NA

#ordered version
wos2 <- wos[, c("Authors", "Title", "Link", "Year", "Abstract",
                "Addresses", "Email.Addresses",
                "WoS.Categories", "WoS.Research.Areas", "Author.Keywords",
                "WoS.Keywords", "Scopus.Keywords")]

scopus_raw <- read.csv("searches/scopus.csv")

scopus <- scopus_raw

#rename columns
names(scopus)[names(scopus_raw) == "Titles"] <- "Title"
names(scopus)[names(scopus_raw) == "Indexed.Keywords"] <- "Scopus.Keywords"
#add wos columns
scopus$WoS.Keywords <- NA
scopus$WoS.Categories <- NA
scopus$WoS.Research.Areas <- NA
scopus$Email.Addresses <- NA
scopus$Addresses <- NA

#ordered version
scopus2 <- scopus[, c("Authors", "Title", "Link", "Year", "Abstract",
                      "Addresses", "Email.Addresses",
                  "WoS.Categories", "WoS.Research.Areas", "Author.Keywords",
                  "WoS.Keywords", "Scopus.Keywords")]

results_full <- rbind(wos2, scopus2)
import_results <- results_full

results <- remove_duplicates(import_results, field = "Title", method = "exact")
write.csv(results, "combinedresults.csv")
nrow(results)
