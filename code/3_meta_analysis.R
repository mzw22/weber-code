
# Setup-------------------------------------------------------------------------

# Clear workspace
#rm(list=ls())

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages
library(tidyverse) #can't live with it can't live without it
library(metafor) # meta analysis
library(assertive.base) # for bracketing strings
library(ggtree) # for phylogeny
library(rotl) # for matching species names
library(ape) # more phylogeny stuff
library(gtools) #p value stars
library(gridExtra) #for grid plots
library(orchaRd) #for I2
library(cowplot) #nice grids
library(ggtext) #for plot markdown (specifically, italicising species names)
library(rphylopic) #for plot icons
library(cowplot) #for some compound figures
library(phytools) #For Pagel's lambda
#library(ggrepel) #for labels

# Load my functions
source("chisqgrid.R") #Tests for associations between categorical variables

# Import results
k_results_full <- read.csv("../results/k_results.csv")

# Clean up results
k_results_full[k_results_full == ""] <- NA #replace blank strings with NA

# Exclude unusable stuff
nrow(k_results_full) #check number before
k_results_full <- k_results_full %>%
  mutate(interval = k_upper_95 - k_lower_95) #add conf interval size
nrow(k_results_full) #check number after

# Make a results table of k estimates ------------------------------------------

# Organise k results
k_results_full <- k_results_full %>%
  arrange(k_estimate) %>%
  # Add a column for reference
  group_by(reference) %>%
  mutate(sub_no = row_number()) %>%
  mutate(sub_total = n()) %>%
  ungroup() %>%
  mutate(sub_ID = ifelse(sub_total == 1,
                         "",
                         parenthesise(tolower(as.roman(sub_no))))) %>% #lowercase roman numerals
  mutate(reference_lab=paste(reference, sub_ID)) %>%
  arrange(reference_lab) %>%
  mutate(tree_label = paste(common_name, " (", species_short, ")", sep=""))

# Sort results into a table
k_table <- k_results_full
# adjust some labels
# Paper sources
k_table$source[k_table$source == "litsearch"] <- "Database"
k_table$source[k_table$source == "review"] <- "Review"
k_table$source[k_table$source == "both"] <- "Database and review"
k_table$source[k_table$source == "late"] <- "Published post-search"
# Moderator levels
k_table$weber_reported[k_table$weber_reported == "yes"] <- "reported"
k_table$weber_reported[k_table$weber_reported == "no"] <- "not reported"
# Stimulus names
k_table$stimulus <- gsub("No.", "Number of", k_table$stimulus)
k_table$stimulus <- gsub("conc.", "concentration", k_table$stimulus)

k_table <- k_table %>%
  mutate(k_lower_95 = format(round(k_lower_95, 2), nsmall=2),
         k_upper_95 = format(round(k_upper_95, 2), nsmall=2)) %>% #column for formatted k estimate
  # Get reasons for exclusion
  mutate(exclude = ifelse(is.na(k_estimate), "k failed", exclude)) %>%
  mutate(exclude = ifelse(is.na(exclude) & interval > 3, "interval", exclude)) %>%
  mutate(Included = ifelse(is.na(exclude), "Y", "N")) %>%
  filter(exclude != "no sample size" | is.na(exclude)) %>%
  arrange(desc(Included), exclude, reference) %>%
  mutate(k_estimate = format(round(k_estimate, 2), nsmall=2)) %>% #round some values
  mutate(test.statistic = round(test.statistic, 2)) %>%
  # Calculate AIC differences
  # Format table
  mutate(k_p_stars = paste0(ifelse(p.value < 0.001, "<.001", round(p.value, 4)), stars.pval(p.value))) %>%
  dplyr::select(c("reference_lab", "citation", "species_full", "common_name", "stimulus",
                  "modality", "preference", "task", "weber_reported",
                  "k_estimate", "k_lower_95", "k_upper_95",
                  "model", "test.statistic", "k_p_stars",
                  "Included", "exclude", "source", "data_source",
                  "search_date", "filename"))
colnames(k_table) <- c("Reference", "Citation", "Species", "Common name", "Stimulus",
                       "Modality", "Choice", "Task", "Weber's law",
                       "k", "Lower bound", "Upper bound",
                       "Model fitted", "Model test statistic", "Model p value",
                       "Included", "Exclusion reason", "Source", "Data",
                       "Search date", "file prefix")

write.csv(k_table, "../results/meta_analysis/k_table.csv", na="", row.names=FALSE)

# conf interval distribution
hist(k_results_full$k_upper_95-k_results_full$k_lower_95, breaks=20)
hist(log(k_results_full$k_upper_95-k_results_full$k_lower_95), breaks=12)

# Filter results
nrow(k_results_full) #check number before
k_results <- k_results_full %>%
  filter(!is.na(k_estimate)) %>% #filter no. failed studies
  filter(is.na(exclude)) %>% #filter out results that are wrong for some reason
  filter(interval < 3) %>% #95% conf interval smaller than 3
  mutate(reference_lab=fct_reorder(reference_lab, desc(k_estimate))) #reorder
nrow(k_results) #check number after

# Count up results--------------------------------------------------------------

# Total N of k estimates
n_estimates <- c()
n_estimates["k_estimates"] <- nrow(k_results) #total
n_estimates["studies"] <- length(unique(k_results$reference)) #studies
#reference (out of date: change)
n_estimates["species"] <- length(unique(k_results$species_short)) #species

# How many significantly different to 0 and 1? etc
n_estimates["k_negative"] <- sum(k_results$k_upper_95 < 0)
n_estimates["k_abs"] <- sum(k_results$k_lower_95 < 0 &
                              k_results$k_upper_95 > 0 &
                              k_results$k_upper_95 < 1)
n_estimates["k_nearmiss"] <- sum(k_results$k_lower_95 > 0 &
                                   k_results$k_upper_95 < 1)
n_estimates["k_weber"] <- sum(k_results$k_lower_95 > 0 &
                                k_results$k_lower_95 < 1 &
                                k_results$k_upper_95 > 1)
n_estimates["k_oppositemiss"] <- sum(k_results$k_lower_95 > 1)
n_estimates["k_unsure"] <- sum(k_results$k_lower_95 < 0 &
                                 k_results$k_upper_95 > 1)
n_estimates["litsearch"] <- sum(k_results$source == "litsearch")
n_estimates["review"] <- sum(k_results$source == "review")
n_estimates["both"] <- sum(k_results$source == "both")
n_estimates["late"] <- sum(k_results$source == "late")
n_estimates

# Export numbers of things
write.csv(n_estimates, "../results/meta_analysis/n_estimates.csv")

# Data summary -----------------------------------------------------------------

# Plot histogram to visualise distribution of k estimates
k_hist <- ggplot(k_results, aes(x=k_estimate)) +
  geom_histogram(binwidth=0.5)+
  labs(y="no. estimates", x="k")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=11))
k_hist
ggsave(k_hist, file="../results/figures/k_histogram.png",
       width=5, height=4, dpi=300, device="png")

# Look at mean, sd, range
round(mean(k_results$k_estimate), 2)
round(sd(k_results$k_estimate), 2)
round(min(k_results$k_estimate), 2)
round(max(k_results$k_estimate), 2)

# Model with no random effects
model_empty <- rma(yi=k_estimate, sei=standard_error, data=k_results)
model_empty

# Get I2 (heterogeneity) from empty model
model_empty$I2

# Create phylogeny--------------------------------------------------------------

# Summarise k results by species
k_results <- k_results %>%
  mutate(tree_name = species) %>%
  group_by(tree_name) %>%
  mutate(nstudies = n()) %>%
  # Italicise species names
  mutate(tree_label = paste(common_name, " (<i>", species_short, "</i>)", sep="")) %>%
  # meant to be markdown but I cannot get it to work
  ungroup()

# Summarise k results by species
k_names <- k_results %>%
  group_by(tree_name, tree_label) %>%
  summarise(n = n())

# Get vector of matched species names from database and add it to k_results_tree
tnrs_names <- tnrs_match_names(k_names$tree_name) #lookup names in database

# Add column for default names
k_names$default_name <- tnrs_names$unique_name

# Correct the names that are still wonky
# idk why pan troglodytes is being weird but whatever I give up
k_names$default_name <- gsub("_troglodytes", "", k_names$default_name) #fix pan troglogytes for some reason???
k_names$default_name <- gsub("_(species_in_domain_Eukaryota)", "", k_names$default_name) #fix pan troglogytes for some reason???
k_names$default_name <- gsub(" ", "_", k_names$default_name) #replace spaces

# Check that everything is ok with the final list of names
k_names

convert_labels <- function(x, from, to){
  y <- c()
  for (i in x){
    label1.i <- which(from == i)
    label2 <- to[label1.i]
    y <- c(y, label2)
  }
  return(y)
}

# Add default names column to original tree - can then add phylogeny to models
k_results <- k_results %>%
  mutate(phylogeny=convert_labels(tree_label, #convert tree labels to default names
                                  from=k_names$tree_label,
                                  to=k_names$default_name))

# Use list of species to obtain phylogenetic tree-------------------------------

# Construct tree using names list from earlier
phylogenetic_tree <- tol_induced_subtree(ott_ids=tnrs_names$ott_id,
                                         label_format="name")
#plot(phylogenetic_tree, no.margin = TRUE) # Test plot to see the output

# Create matrix of phylogenetic correlations phylo_corr_matrix
phylo_brlen <- compute.brlen(phylogenetic_tree) #branch lengths
phylo_corr_matrix <- vcv(phylo_brlen, corr = TRUE)

# Model with all random effects-------------------------------------------------

# All random effects
model_random <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                          random=list(~1|reference, ~1|phylogeny, ~1|species),
                          R=list(phylogeny=phylo_corr_matrix),
                          data=k_results, method="REML")
model_random

# Heterogeneity
i2_random <- i2_ml(model_random)
i2_random
# Extract total heterogeneity
i2_total <- i2_random["I2_Total"]
i2_total

# Models with 1 random effect removed each
# Remove phylogeny:
model_nophylo <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                        random=list(~1|reference, ~1|species),
                        data=k_results, method="REML")
model_nophylo
# Remove authors:
model_noref <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                          random=list(~1|phylogeny, ~1|species),
                          R=list(phylogeny=phylo_corr_matrix),
                          data=k_results, method="REML")
model_noref
# Remove species:
model_nospecies <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                          random=list(~1|reference, ~1|phylogeny),
                          R=list(phylogeny=phylo_corr_matrix),
                          data=k_results, method="REML")
model_nospecies

# Compare these models to the full model
# Likelihood ratio tests
ref_anova <- anova(model_random, model_noref) #LRT
ref_anova
phylo_anova <- anova(model_random, model_nophylo)
phylo_anova
species_anova <- anova(model_random, model_nospecies)
species_anova
# Phylogeny is not significant, but keep it anyway:
# removing it can cause bias (Cinar et al. 2022)

# Heterogeneity with I2--------------------------------------------------------------------------------
# (Chik et al. 2022)

# wj = 1/variance
wj <- 1/(k_results$standard_error)^2
n_studies <- length(unique(k_results$authors))

sigma2m <- sum(wj)*(n_studies - 1)/((sum(wj))^2 - sum(wj^2))
sigma2m
# typical within-study sampling variance (Nakagawa & Santos, 2012)

# Print model output again
model_random

# Calculate residuals of full model
residual <- residuals(model_random, type="response")
residual_sigma2 <- sum(residual^2) 
residual_sigma2

# Make dataframe with sigma2 values from model
variance_explained <- data.frame(variance_component = unname(model_random$s.names),
                                 sigma2 = model_random$sigma2,
                                 n_levels = model_random$s.nlevels)

variance_explained$LRT <- c(ref_anova$LRT, phylo_anova$LRT, species_anova$LRT)
variance_explained$pval <- c(ref_anova$pval, phylo_anova$pval, species_anova$pval)

# calculate sampling variance and add this as an extra row
variance_sampling <- data.frame(variance_component = "sampling",
                                sigma2 = sigma2m,
                                n_levels = nrow(k_results),
                                LRT=NA, pval=NA)
variance_explained <- rbind(variance_explained, variance_sampling)

# calculate residual variance and add this as an extra row
variance_residual <- data.frame(variance_component = "residual",
                                sigma2 = residual_sigma2,
                                n_levels = nrow(k_results),
                                LRT=NA, pval=NA)
variance_explained <- rbind(variance_explained, variance_residual)

# Check table
variance_explained

# Format for paper
variance_table <- variance_explained %>%
  filter(variance_component != "sampling") %>%
  mutate(prop_explained = sigma2/sum(sigma2)) %>%
  mutate(prop_explained = paste0(round(100*prop_explained, 1), "%")) %>%
  mutate(sigma2 = signif(sigma2, 4)) %>%
  mutate(LRT = signif(LRT, 4)) %>%
  mutate(pval = round(pval, 3))
variance_table

write.csv(variance_table, "../results/meta_analysis/variance_explained.csv",
          row.names=FALSE)

# Now get results of final model------------------------------------------------

# Read effect size, significance:
model_final <- model_random

k_estimate <- model_final$b
k_estimate

# Forest plot
forest_plot <- ggplot(k_results, aes(y=reference_lab, x=k_estimate))+
  labs(y=NULL, x="k")+
  # add mean
  geom_vline(xintercept=model_empty$b, colour="red", linetype="solid",
             linewidth=0.4)+
  #add reference lines for k=0 and k=1
  geom_vline(xintercept=0, colour="black", linetype="solid", linewidth=0.4)+
  geom_vline(xintercept=1, colour="black", linetype="dashed", linewidth=0.4)+
  geom_errorbarh(aes(xmin=k_lower_95, xmax=k_upper_95), #add 95% conf intervals  
                 height=0, col="black", linewidth=0.7)+
  geom_point(size=1.5)+  #add data points
  scale_x_continuous(breaks=seq(-4, 8, 2),
                     minor_breaks=seq(-6, 14, 1))+
  theme_light()+ #theme
  theme(text=element_text(size=12),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(), #remove y axis gridlines
        axis.text=element_text(size=12), #italic y axis text
        axis.title.x=element_text(face="italic", size=12)) #italic k
forest_plot

ggsave(forest_plot, file="../results/figures/forest_plot.png",
       width=6, height=6, dpi=300, device="png")

# Now find significance relative to 1
k_results <- mutate(k_results, k_estimate_relative=(k_estimate - 1))

model_relative <- rma.mv(yi=k_estimate_relative, V=I(standard_error^2),
                         random=list(~1|reference, ~1|species, ~1|phylogeny),
                         R=list(phylogeny=phylo_corr_matrix),
                         data=k_results, method="REML")

# Read effect size, significance:
model_relative

# Plot k by conf intervals -----------------------------------------------------

# Reorder
k_results <- k_results %>%
  mutate(interval = k_upper_95 - k_lower_95) %>%
  arrange(interval) %>%
  mutate(reference_lab=fct_reorder(reference_lab, desc(interval))) #sort by estimate ID

# Test for publication bias in final model--------------------------------------

# Funnel plot: ggplot2 instructions from
# https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/

#store k estimate and SE
estimate <- model_random$b

# Create lines for standard error and conf intervals
se=seq(from=0, to=max(k_results$standard_error), by=0.01)
# 95% conf intervals
lower_95 = estimate-(1.96*se)
upper_95 = estimate+(1.96*se)

# Funnel function (confidence intervals) for using geom_area
invse_fun <- function(x){
  # 95% conf intervals: k_95 = k_estimate + 1.96*se
  # on the funnel plot: x = k_estimate + 1.96*(1/y)
  # therefore: y = 1.96/(x - k_estimate)
  # use abs to make it positive on both sides
  estimate = as.numeric(estimate)
  y <- abs(1.96/(x-estimate)) #1.95: 95% conf intervals
  return(y)
}

se_fun <- function(x){
  # 95% conf intervals: k_95 = k_estimate + 1.96*se
  # on the funnel plot: x = k_estimate + 1.96*y
  # therefore: y = (x - k_estimate)/1.96
  # use abs to make it positive on both sides
  estimate = as.numeric(estimate)
  y <- abs((x-estimate)/1.96) #1.95: 95% conf intervals
  return(y)
}

# Combine these into one dataframe
funnel_lines = data.frame(estimate, se, lower_95, upper_95)

k_results$weber_reported[k_results$weber_reported=="yes"] <- "reported"
k_results$weber_reported[k_results$weber_reported=="no"] <- "not reported"
k_results$weber_reported <- factor(k_results$weber_reported,
                                   levels=c("reported", "not reported"))

# Funnel plot
funnel_plot <- ggplot(k_results, aes(x=k_estimate, y=standard_error)) +
  labs(y="standard error", x="k", shape="Weber's law")+
  #Add confidence intervals
  geom_line(aes(x=lower_95, y=se), data=funnel_lines,
            linetype="dashed", col="black", linewidth=0.3)+ #left line
  geom_line(aes(x=upper_95, y=se), data=funnel_lines,
            linetype="dashed", col="black", linewidth=0.3)+ #right line
  #stat_function(fun=se_fun, geom="area", n=1000,
                #fill="darkgrey", col=NA, alpha=0.5)+ #area
  # Add estimate as a vertical line
  geom_vline(aes(xintercept=estimate),
             col="black", linetype="solid", linewidth=0.3)+
  # Add data
  geom_point(aes(shape=weber_reported), size=2)+
  #geom_text_repel(aes(label=ID), size=4, nudge_y=0.02,
                  #box.padding=0.01, max.overlaps=12)+
  # Set theme
  #xlim(-3, 6)+ #ylim(0, 5)+
  #scale_colour_manual(values=c("black", "grey"))+
  scale_shape_manual(values=c(19, 1))+
  scale_y_reverse()+
  scale_x_continuous(limits=c(-2.5, 3.5),
                     breaks=seq(-2, 3, 1))+
  theme_light()+
  theme(text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position="right",
        axis.title.x=element_text(face="italic")) #italic k
#Call the pretty funnel plot
funnel_plot

ggsave(funnel_plot, file="../results/figures/funnel_plot.png",
       width=6, height=4, dpi=300, device="png")

# Egger's regression tests with standard error
#regtest(model_empty, model="rma", predictor="sei")
# is equivalent to:
#rma(yi=k_estimate~standard_error, vi=I(standard_error^2), data=k_results)
# but we want to include mixed effects for non-independence
# therefore: Egger's regressions manually with rma.mv

# Outcome reporting bias
eggreg_error <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                       mods=~standard_error,
                       random=list(~1|reference, ~1|species, ~1|phylogeny),
                       R=list(phylogeny=phylo_corr_matrix),
                       data=k_results, method="REML")
eggreg_error
# Not significant

# Time lag bias
eggreg_time<- rma.mv(yi=k_estimate, V=I(standard_error^2),
                       mods=~year,
                       random=list(~1|reference, ~1|species, ~1|phylogeny),
                       R=list(phylogeny=phylo_corr_matrix),
                       data=k_results, method="REML")
eggreg_time

# Weber interaction
eggreg_weber <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                       mods=~standard_error*weber_reported,
                       random=list(~1|reference, ~1|species, ~1|phylogeny),
                       R=list(phylogeny=phylo_corr_matrix),
                       data=k_results, method="REML")
eggreg_weber

# All moderators together-------------------------------------------------------

# Complete rows only
k_results_comp <- k_results %>%
  filter(!is.na(preference) &
           !is.na(presentation) &
           !is.na(task) &
           !is.na(modality) &
           !is.na(weber_reported))

# Model containing all moderators
model_mods <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                     mods=~preference+task+modality+weber_reported,
                     random=list(~1|reference, ~1|species, ~1|phylogeny),
                     R=list(phylogeny=phylo_corr_matrix),
                     data=k_results, method="REML")
model_mods

# Calculate variance inflation factor
mods_vif <- metafor::vif(model_mods,
                         btt=c("preference", "task", "modality", "weber_reported"),
                         table=TRUE, sim=TRUE, seed=1)
mods_vif
plot(mods_vif, breaks=seq(1, 10000, by=0.5), xlim=c(1,10))

mods_vif_tab <- as.data.frame(mods_vif)
mods_vif_tab$vif <- round(mods_vif_tab$vif, 2)
mods_vif_tab$sif <- round(mods_vif_tab$sif, 2)
mods_vif_tab$prop <- round(mods_vif_tab$prop, 3)
write.csv(mods_vif_tab, "../results/meta_analysis/mods_vif.csv",
          row.names=FALSE, na="")

# Make a table for the paper
mods_table <- data.frame(moderator=rownames(model_mods$b),
                         k = paste(round(model_mods$b, 2)),
                         se = round(model_mods$se, 2),
                         ci = paste(round(model_mods$ci.lb, 2), ", ",
                                    round(model_mods$ci.ub, 2), sep=""),
                         z.value = round(model_mods$zval, 2),
                         p.value = paste(round(model_mods$pval, 3),
                                      stars.pval(model_mods$pval), sep=""))
mods_table
# Export table
write.csv(mods_table, "../results/meta_analysis/mods_table.csv",
          row.names=FALSE, na="")

mods_nomodality <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                          mods=~preference+task+weber_reported,
                          random=list(~1|reference, ~1|species, ~1|phylogeny),
                          R=list(phylogeny=phylo_corr_matrix),
                          data=k_results, method="REML")
mods_nomodality

# Calculate variance inflation factor
mods_vif_nomodality <- vif(mods_nomodality, table=TRUE)
mods_vif_nomodality

# Chi-square test for co-occurence of moderator variables ----------------------

chi.grid <- chisq.grid(k_results[, c("preference", "modality",
                                     "task", "weber_reported")])
V.table <- chi.grid$V.table
V.table
write.csv(V.table, "../results/meta_analysis/colinearity.csv")

# Correlations
V.matrix <- chi.grid$V
V.matrix[is.na(V.matrix)] <- 1 #correlation = 1
V.matrix[upper.tri(V.matrix)] <- NA #remove top half
corr_V <- data.frame(V.matrix) #convert to df
corr_V$mod1 <- row.names(corr_V)
corr_V <- corr_V %>%
  pivot_longer(cols=-mod1, names_to="mod2", values_to="V")

# P values
p.matrix <- chi.grid$p.value
p.matrix[upper.tri(p.matrix)] <- NA #remove top half
corr_p <- data.frame(p.matrix) #convert to df
corr_p$mod1 <- row.names(corr_p)
corr_p <- corr_p %>%
  pivot_longer(cols=-mod1, names_to="mod2", values_to="p")

corr <- left_join(corr_V, corr_p) %>%
  mutate(stars = stars.pval(p)) %>%
  mutate(stars = ifelse(stars==" "|stars==".", "", stars)) %>% #get rid of whitespace
  mutate(label = ifelse(!is.na(p),
                        # Normal
                        paste(format(round(V, 2), nsmall=2), "\n (p=",
                        format(round(p, 3), nsmall=3),
                        stars, ")", sep=""),
                        # Diagonal
                        ifelse(!is.na(V),
                          (format(round(V, 2), nsmall=2)),
                        NA)))

# Change names
corr[corr=="weber_reported"] <- "Weber's law"
corr[corr=="task"] <- "Task type"
corr[corr=="preference"] <- "Choice"
corr[corr=="modality"] <- "Modality"

corr_plot <- ggplot(data=corr, aes(x=mod1, y=mod2, fill=V))+
  geom_tile()+
  geom_text(aes(label=label), size = 12/.pt)+
  scale_fill_gradient(limits=c(0, 1), breaks=c(0, 0.5, 1),
                      low="white", high="#B2182B",
                      na.value="white")+
  labs(x=NULL, y=NULL, fill="Correlation")+
  theme_minimal()+
  theme(plot.background=element_rect(fill="white", colour=NA),
        text=element_text(size=12),
        axis.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12, vjust=4))
corr_plot

ggsave(plot=corr_plot, file="../results/figures/colinearity.png",
       height=4, width=6.3, dpi=300, device="png")

# More detailed
# Preference and modality
chi_pref_modality <- k_results %>%
  group_by(preference, modality) %>%
  summarise(total=n())
chi_pref_modality
# Preference and task
chi_pref_task <- k_results %>%
  group_by(preference, task) %>%
  summarise(total=n())
chi_pref_task
# Preference and Weber
chi_pref_weber <- k_results %>%
  group_by(preference, weber_reported) %>%
  summarise(total=n())
chi_pref_weber

# From this:
# Preference strongly covaries with basically everything
# Presentation and modality covary: inherent constraints of some modalities?

# Individual moderator models --------------------------------------------------

# Preference: conditioned or innate
model_preference <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                           mods=~preference,
                           random=list(~1|reference, ~1|species, ~1|phylogeny),
                           R=list(phylogeny=phylo_corr_matrix),
                           data=k_results, method="REML")
model_preference

# Presentation: simultaneous or sequential
model_presentation <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                             mods=~presentation,
                             random=list(~1|reference, ~1|species, ~1|phylogeny),
                             R=list(phylogeny=phylo_corr_matrix),
                             data=k_results, method="REML")
model_presentation

# Task type
model_task <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                     mods=~task,
                     random=list(~1|reference, ~1|species, ~1|phylogeny),
                     R=list(phylogeny=phylo_corr_matrix),
                     data=k_results, method="REML")
model_task

# Sensory modality
modality_subset <- k_results %>% #filter unknown modalities
  filter(!is.na(modality))
model_modality <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                         mods=~modality,
                         random=list(~1|reference, ~1|species, ~1|phylogeny),
                         R=list(phylogeny=phylo_corr_matrix),
                         data=k_results, method="REML")
model_modality
# Weber's law reported
model_reported <- rma.mv(yi=k_estimate, V=I(standard_error^2),
                           mods=~weber_reported,
                           random=list(~1|reference, ~1|species, ~1|phylogeny),
                           R=list(phylogeny=phylo_corr_matrix),
                           data=k_results, method="REML")
model_reported

# Plot results of moderator models ---------------------------------------------

# Preference: conditioned or innate
k_preference <- data.frame(estimate=model_preference$b,
                           se=model_preference$se,
                           pval=model_preference$pval,
                           ci.ub=model_preference$ci.ub,
                           ci.lb=model_preference$ci.lb)
# Get mean k
k_preference$k_mean <- k_preference$estimate[1] + c(0, k_preference$estimate[-1])
# Add factor levels
k_preference$levels <- gsub("preference", "", row.names(k_preference))
k_preference$levels[1] <- "conditioned"
# Check
k_preference

# Presentation: simultaneous or sequential
k_presentation <- data.frame(estimate=model_presentation$b,
                             se=model_presentation$se,
                             pval=model_presentation$pval,
                             ci.ub=model_presentation$ci.ub,
                             ci.lb=model_presentation$ci.lb)
k_presentation
# Get mean k
k_presentation$k_mean <- k_presentation$estimate[1] + c(0, k_presentation$estimate[-1])
# Add factor levels
k_presentation$levels <- gsub("presentation", "", row.names(k_presentation))
k_presentation$levels[1] <- "sequential"
# Check
k_presentation

# Task type
k_task <- data.frame(estimate=model_task$b,
                     se=model_task$se,
                     pval=model_task$pval,
                     ci.ub=model_task$ci.ub,
                     ci.lb=model_task$ci.lb)
# Get mean k
k_task$k_mean <- k_task$estimate[1] + c(0, k_task$estimate[-1])
# Add factor levels
k_task$levels <- gsub("task", "", row.names(k_task))
k_task$levels[1] <- "difference"
# Check
k_task

# Sensory modality
k_modality <- data.frame(estimate=model_modality$b,
                         se=model_modality$se,
                         pval=model_modality$pval,
                         ci.ub=model_modality$ci.ub,
                         ci.lb=model_modality$ci.lb)
k_modality
# Get mean k
k_modality$k_mean <- k_modality$estimate[1] + c(0, k_modality$estimate[-1])
# Add factor levels
k_modality$levels <- gsub("modality", "", row.names(k_modality))
k_modality$levels[1] <- "electroreception"
# Check
k_modality

# Weber's law reported
k_reported <- data.frame(estimate=model_reported$b,
                         se=model_reported$se,
                         pval=model_reported$pval,
                         ci.ub=model_reported$ci.ub,
                         ci.lb=model_reported$ci.lb)
k_reported
# Get mean k
k_reported$k_mean <- k_reported$estimate[1] + c(0, k_reported$estimate[-1])
# Add factor levels
#k_reported$levels <- gsub("weber_reported", "", row.names(k_reported))
k_reported$levels <- c("reported", "not reported")
# Check
k_reported

#k_preference$mod <- "stimulus preference"
k_modality$mod <- "modality"
k_task$mod <- "task"
k_reported$mod <- "reported"
k_preference$mod <- "preference"
k_presentation$mod <- "presentation"
# Combine all these into 1 dataframe
k_means <- rbind(k_modality, k_task, k_reported, k_preference, k_presentation)
# correct conf intervals
k_means <- k_means %>%
  mutate(ci.ub = ci.ub + (k_mean - estimate),
         ci.lb = ci.lb + (k_mean - estimate)) %>%
  filter(mod != "presentation")

k_means$mod <- factor(k_means$mod, levels=c("modality", "preference",
                                            "task", "reported"))

mod_labels <- c("(i) Modality", "(ii) Choice",
                "(iii) Task", "(iv) Weber's law")
names(mod_labels) <- c("modality", "preference", "task", "reported")

mean_plot_template <- ggplot(mapping=aes(x=k_mean, y=levels,
                                     xmin=ci.lb, xmax=ci.ub))+
  # Reference lines
  geom_vline(xintercept=0, col="black", linetype="solid", linewidth=0.4)+ #k=0
  geom_vline(xintercept=1, col="black", linetype="dashed", linewidth=0.4)+ #k=1
  #axis limits
  scale_x_continuous(limits=c(-1.8, 3.8), breaks=seq(-1, 3, 1))+
  # Theme
  labs(y=NULL, x="k")+
  theme_bw()+
  theme(text=element_text(size=12),
        axis.text=element_text(size=12),
        plot.title=element_text(size=12),
        axis.title = element_text(size=12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(), #remove y axis gridlines
        axis.title.x=element_text(face="italic")) #italic k
mean_plot_template

modality_plot <- mean_plot_template+
  geom_point(data=filter(k_means, mod=="modality"),
             size=2)+
  geom_errorbarh(data=filter(k_means, mod=="modality"),
                 height=0, linewidth=0.7)+
  labs(title="Sensory modality")
modality_plot

choice_plot <- mean_plot_template+
  geom_point(data=filter(k_means, mod=="preference"),
             size=2)+
  geom_errorbarh(data=filter(k_means, mod=="preference"),
                 height=0, linewidth=0.7)+
  labs(title="Choice type")
choice_plot

task_plot <- mean_plot_template+
  geom_point(data=filter(k_means, mod=="task"),
             size=2)+
  geom_errorbarh(data=filter(k_means, mod=="task"),
                 height=0, linewidth=0.7)+
  labs(title="Task type")
task_plot

weber_plot <- mean_plot_template+
  geom_point(data=filter(k_means, mod=="reported"),
             size=2)+
  geom_errorbarh(data=filter(k_means, mod=="reported"),
                 height=0, linewidth=0.7)+
  labs(title="Weber's Law")
weber_plot

k_means_plot <- plot_grid(plotlist=list(modality_plot+labs(tag="A"),
                                        choice_plot+labs(tag="B"),
                                        task_plot+labs(tag="C"),
                                        weber_plot+labs(tag="D")),
                          ncol=2, align="hv")
k_means_plot

ggsave(k_means_plot, file="../results/figures/k_means.png",
       height=5, width=7, dpi=300, device="png")

# Meta regression to get k for each species-------------------------------------

# Estimate k for each species
model_species <- rma.mv(k_estimate, V = I(standard_error^2),
                        mods=~1+phylogeny,
                        random=list(~1|reference), #still include study
                        R=list(phylogeny=phylo_corr_matrix),
                        data=k_results, method="REML")
model_species

# Extract the species etimates
species_summary <- data.frame(model_species[c("b", "se", "ci.ub", "ci.lb")])
colnames(species_summary)[1] <- "k_species"
rownames(species_summary)[1] <- paste("phylogeny", k_names$default_name[1], sep="")
# Fix model output names
species_summary$default_name <- gsub("phylogeny", "", row.names(species_summary))
species_summary

# Make dataframe of species estimates and save it
k_species <- left_join(k_names, species_summary)
write.csv(k_species, "../results/meta_analysis/k_species.csv",
          row.names=FALSE)

# Plot pretty phylogenetic tree-------------------------------------------------

# Get tip labels from phylogenetic tree and switch in new ones
tip_labels <- phylogenetic_tree$tip.label
new_labels <- convert_labels(phylogenetic_tree$tip.label,
                             from=k_names$default_name,
                             to=k_names$tree_label)

# Now make a copy of the tree with the new tip labels
final_phylo_tree <- phylogenetic_tree
final_phylo_tree$tip.label <- new_labels

# Test plot
#plot(final_phylo_tree, no.margin = TRUE)

# Get rid of unnecessary columns so I can transfer the k estimates to the tree
k_tree_colours <- ungroup(k_species) %>%
  dplyr::select(c("tree_label", "k_species"))

# Adding phylopics, instructions from:
#https://jacintak.github.io/post/2021-08-01-rphylopic/

# Download icons from Phylopic
bat_icon <- get_phylopic(uuid="21180755-3394-40bf-93eb-810954c0f7ba")
rat_icon <- get_phylopic(uuid="8fc30940-a3cd-4ec4-ac6c-0a877f092c71")
bird_icon <- get_phylopic(uuid="cbe76bdb-5a89-4577-8472-6af7c2052d70")
frog_icon <- get_phylopic(uuid="2c4f717a-f5c2-4a90-9069-5bc5984c5506")
swordtail_icon <- get_phylopic(uuid="1b5ad11e-b5a6-4efd-8a21-d035714d9239")
bee_icon <- get_phylopic(uuid="956cf95f-9cbd-459b-a457-c80ba19877a4")
# Unused icons
#primate_icon <- get_phylopic(uuid="d98c79cf-4ca5-4af8-91d2-9a3e51617360")
#spider_icon <- get_phylopic(uuid="6ed46f09-0e46-46cd-9ed4-03b03001de57")
#cricket_icon <- get_phylopic(uuid="e7ba011c-6172-47ca-84bf-0dfc87b47a32")
#sealion_icon <- get_phylopic(uuid="b00e49c6-b365-4924-bf57-878e0a35e7e4")
#goldfish_icon <- get_phylopic(uuid="de187ba5-0498-4d2e-a688-70245c354c5f")
#whale_icon <- get_phylopic(uuid="4533cdfe-04d6-4b76-a06a-ba2d505b67d2")

# Plot pretty coloured phylogenetic tree
icon_size <- 1.2
tree_plot <- ggtree(final_phylo_tree, size=1.5, aes(color=k_species)) %<+%
  k_tree_colours +
  scale_color_gradient2(low="#2166AC", mid="white", high="#B2182B",  #RdBu
                        midpoint=0, name="k")+
  #geom_tiplab(as_ylab=TRUE)+
  # Icons :)))
  add_phylopic(img=bat_icon, x=5, y=24, ysize=0.8*icon_size)+
  add_phylopic(img=rat_icon, x=5, y=20, ysize=0.8*icon_size, horizontal=TRUE)+
  add_phylopic(img=bird_icon, x=4, y=15.8, ysize=1.2*icon_size)+
  add_phylopic(img=frog_icon, x=6, y=13, ysize=0.8*icon_size)+
  add_phylopic(img=swordtail_icon, x=2.5, y=7.5, ysize=0.8*icon_size, horizontal=TRUE)+
  add_phylopic(img=bee_icon, x=3, y=2.5, ysize=icon_size)+
  theme(legend.position="bottom",
        axis.text.y = element_blank(),
        legend.title=element_text(face="italic", size=12),
        legend.text=element_text(size=12))
tree_plot

# Fit tree plot to forest plot
tree_cropped <- tree_plot+
  # For compound figure
  labs(tag="A")+
  # Cut margins
  theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), units="cm"),#adjust margins
        legend.position=c(2.8, 0.6))
tree_cropped

# Get labels used in tree
new_labels <- get_taxa_name(tree_plot)
new_labels

# Fix order of forest plot so it matches tree
k_species$tree_label <- factor(k_species$tree_label,
                               levels = rev(new_labels))

k_species_rel <- k_species
species_intercept <- k_species_rel$k_species[1]
k_species_rel$k_rel <- k_species_rel$k_species #new column for relative mean
k_species_rel$k_rel[-1] <- k_species_rel$k_species[-1] + species_intercept
k_species_rel <- k_species_rel %>%
  mutate(ci.ub_rel = ci.ub + species_intercept,
         ci.lb_rel = ci.lb + species_intercept)

# Names only, no forest plot
tree_plot_names <- ggplot(k_species_rel, aes(y=tree_label))+
  scale_x_continuous(breaks=c(0, seq(-2, 10, 2)))+ #set x axis
  theme_classic()+
  #geom_vline(xintercept=model_empty$b, color="black", linetype="solid")+
  theme(text=element_text(size=12),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_markdown(size=12, hjust=0),
        #axis.text.y = element_blank(), #comment this line out to show tip names
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        # Change margins so it lines up with tree
        plot.margin = unit(c(0.6, 0, 0.3, 0.1), units="cm"))
tree_plot_names

tree_species_plot <- grid.arrange(tree_cropped, tree_plot_names,
                                  ncol=2, widths=c(1, 1.8))
ggsave(tree_species_plot, file="../results/figures/tree_forest.png",
       width=7, height=5.5, dpi=300, device="png")
tree_species_plot

# Estimate phylogenetic signal for k
# With Pagel's lambda
phylosig(tree=phylo_brlen, x=k_species$k_species,
         method="lambda", test=TRUE, se=k_species$se)


# Compound figures -------------------------------------------------------------

# Funnel plot and k means

# Remove previous funnel plot stuff
funnel_plot_simple <- funnel_plot
funnel_plot_simple$layers[[4]] <- NULL
# Combine forest plot and funnel plot
results_overview <- grid.arrange(forest_plot+
                                   labs(tag="A")+
                                   theme(plot.margin = unit(c(0.2,0.5,0.2,0.5), "cm")),
                                 funnel_plot_simple+
                                   labs(tag="B")+
                                   theme(plot.margin = unit(c(0.1,0.5,0.2,0.5), "cm"))+
                                   geom_point(size=1.5),
                               nrow=2, heights=c(1, 0.4))
results_overview

ggsave(results_overview, file="../results/figures/results_overview.png",
       width=129, height=230, unit="mm", dpi=300, device="png")

k_means_b <- plot_grid(plotlist=list(modality_plot, choice_plot,
                                        task_plot, weber_plot),
                          ncol=2, align="hv")
k_means_b

results_detail <- grid.arrange(tree_species_plot,
                               k_means_b+
                                 labs(tag="B")+
                                 theme(plot.tag = element_text(face="plain"),
                                       plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units="cm")),
                               nrow=2, heights=c(1, 0.8))
results_detail

ggsave(results_detail, file="../results/figures/results_detail.png",
       width=150, height=230, unit="mm", dpi=300, device="png")

# mm to px at 300dpi:
# px = in*dpi
# px = mm*(in/mm)*dpi
# 150 -> 1771
# 230 -> 2716
