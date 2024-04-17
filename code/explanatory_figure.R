
# Setup-------------------------------------------------------------------------

# Clear workspace
#rm(list=ls())

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load packages
library(tidyverse)
library(gridExtra)
library(cowplot) #for inserting images

# Explanatory figure for Weber's law, near miss etc. ---------------------------

# Set graph parameters
xmax <- 10
ymax <- 10*1.2

#calculate jnds
jnd_df <- data.frame(magnitude = seq(from=0, to=xmax, by=0.01)) %>%
  mutate(abs = ymax/3,
         weber = magnitude,
         near = magnitude^0.8,
         opp = magnitude^1.2)

jnd_df$weber_ref <- jnd_df$weber #add dotted comparison line
jnds <- jnd_df %>%
  pivot_longer(cols=c(abs, weber, near, opp),
               names_to="percep_fun", values_to="jnd") %>%
  mutate(weber_ref = ifelse(percep_fun == "abs" | percep_fun == "weber",
                            NA, weber_ref)) #%>%
  #mutate(percep_fun = factor(percep_fun, levels=c("abs", "weber", "near", "opp")))

# Change graph labels
jnds$percep_fun[jnds$percep_fun == "abs"] <- "(i) absolute processing"
jnds$percep_fun[jnds$percep_fun == "weber"] <- "(ii) proportional processing"
jnds$percep_fun[jnds$percep_fun == "near"] <- "(iii) near miss"
jnds$percep_fun[jnds$percep_fun == "opp"] <- "(iv) opposite miss"

# Get rid of reference lines
jnds$weber_ref[jnds$percep_fun == "abs"] <- NA
jnds$weber_ref[jnds$percep_fun == "weber"] <- NA

# Import swordtail image
library(png)
img_L <- readPNG("swordtail_L.png") 
img_R <- readPNG("swordtail_R.png") 
# Convert to raster image
library(grid)
swordtail_L <- rasterGrob(img_L, interpolate=TRUE)
swordtail_R <- rasterGrob(img_R, interpolate=TRUE)

insert_swordtails <- function(i_low, i_high, abs_diff, x_low, x_high,
                              y, scale, y_sep_low, y_sep_high){
  width_low1 <- i_low*scale
  width_low2 <- (i_low-abs_diff)*scale
  width_high1 <- i_high*scale
  width_high2 <- (i_high-abs_diff)*scale
  # Labels
  annotations <- list(
  geom_text(aes(x=c(0.25, 0.75), y=c(1, 1),
                label=c("Low magnitude", "High magnitude")),
            hjust=0.5, size=12/.pt),
  geom_text(aes(x=c(0.25, 0.75), y=c(0.1, 0.1),
                label=c(paste("Top fish =", i_low, "cm \n Bottom fish =", i_low-abs_diff, "cm"),
                        paste("Top fish =", i_high, "cm \n Bottom fish =", i_high-abs_diff, "cm"))),
                hjust=0.5, size=12/.pt),
  # Low pair
  annotation_custom(swordtail_L, xmin=x_low-width_low1/2, xmax=x_low+width_low1/2,
                    ymin=y+y_sep_low), 
  annotation_custom(swordtail_R, xmin=x_low-width_low2/2, xmax=x_low+width_low2/2,
                    ymin=y-y_sep_low),
  # High pair
  annotation_custom(swordtail_L, xmin=x_high-width_high1/2, xmax=x_high+width_high1/2,
                    ymin=y+y_sep_high), 
  annotation_custom(swordtail_R, xmin=x_high-width_high2/2, xmax=x_high+width_high2/2,
                    ymin=y-y_sep_high)
  )
  return(annotations)
}

# Plot swordtails
swordtail_plot <- ggplot()+
  labs(tag="(a)")+
  xlim(0, 1)+ylim(0, 1)+
  # Size labels
  insert_swordtails(i_low=8, i_high=15, abs_diff=2,
                    x_low=0.25, x_high=0.75, y=0.08, scale=0.03,
                    y_sep_low=0.2, y_sep_high=0.35)+
  #Theme
  theme_classic()+
  theme(text=element_text(size=12),
        axis.line=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(), axis.ticks=element_blank())
swordtail_plot

weber_plot <- ggplot(jnds, aes(x=magnitude, y=jnd))+
  geom_line()+
  facet_wrap(~percep_fun, ncol=2)+
  xlim(0, xmax)+ ylim(0, ymax)+ #axis limits
  theme_bw()+
  labs(y="just-noticeable difference", x="stimulus magnitude", tag="(b)")+
  geom_line(aes(y=weber_ref), linetype="dashed")+
  theme(text=element_text(size=12), axis.title=element_text(size=12),
        axis.text=element_blank(), axis.ticks=element_blank(),
        panel.grid=element_blank(), strip.background=element_blank(),
        strip.text = element_text(size=12, hjust = 0))
weber_plot

explanatory_figure <- grid.arrange(swordtail_plot,
                                   weber_plot,
                                   nrow=2, heights=c(1, 1.4))
explanatory_figure

# Save
ggsave(explanatory_figure, file="../results/figures/explanatory_figure.png",
       width=120, height=150, units="mm", dpi=300)
