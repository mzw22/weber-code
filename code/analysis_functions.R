
#### Functions for plotting and analysing ####

### Last Updated: 30/09/2023
### Megan Worsley
# These functions would need to be adapted for use in other contexts

# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Plot predictions of stimulus-response model
stimulus_response_plot <- function(di, i, k, response, weights=NULL, family, ylab, xlab, main=""){
  # stimulus contrast di/i^k is calculated using: the following arguments
    # di = a vector of absolute differences in stimulus magnitude
    # i = a vector of absolute stimulus magnitudes
    # k = a number used to weight the importance of i
  # response = the response to the stimulus
  # weights = weights of each measurement, used for binomial models
  # family = model family
  # ylab, xlab, main = graph labels
  
  # model for this value of k
  model_k <- stimulus_model(di=di, i=i, k=k, response=response, weights=weights,
                            family=family)
  # Extract model predictions
  predictions <- as.data.frame(predict(model_k, type="response", se.fit=TRUE))
  
  # Calculate 95% confidence intervals - need to do on the right scale for binomial glm
  inverse_link <- family$linkinv #inverse link function from model
  # Add fit and se.fit on the link scale so that we can calculate confidence intervals
  predictions <- bind_cols(predictions, setNames(as_tibble(predict(model_k, predictions, se.fit = TRUE)[1:2]),
                                                 c('fit_link','se.fit_link')))
  # Calculate confidence intervals +-(2 + se) and backtransform with inverse link
  predictions <- predictions %>%
    mutate(fit_95_high = inverse_link(fit_link + (2 * se.fit_link)))%>%
    mutate(fit_95_low = inverse_link(fit_link - (2 * se.fit_link)))
  
  # Create dataframe (to use ggplot)
  contrast <- stimulus_contrast(di, i, k)
  df <- data.frame(contrast, response)
  
  model_plot <- ggplot(df, aes(x=contrast, y=response))+
    geom_point()+ #response points
    geom_line(aes(y=predictions$fit))+ #model predictions
    geom_ribbon(aes(ymin = predictions$fit_95_low, ymax = predictions$fit_95_high),
                alpha=0.15)+ #prediction uncertainty
    xlim(0, NA)+
    labs(y=ylab, x=xlab, title_lab=main)+
    theme_bw()+
    theme(text=element_text(size=14))
  # This bit doesn't work:
  if(family$family == "binomial"){
    model_plot <- model_plot+
      ylim(0, 1) #set axis limits if binomial
  }
  
  return(model_plot)
}

# Return a nice summary of the model
export_model_results <- function(di, i, k_estimate, response, weights=NULL, family,
                                 response_lab, stimulus, ref, file_prefix, n_pairs){
  ### Generate plots
  # Plot k = fitted value
  k_sol <- k_estimate["k", "estimate"]
  xlab_k <- bquote("weighted "~.(stimulus)~" difference (ΔI/I"^
                     .(round(k_sol, 2))~")")
  modelplot_k <- stimulus_response_plot(di=di, i=i, k=k_sol, response=response,
                                        weights=weights, family=family,
                                        ylab=response_lab, main=ref,
                                        xlab=xlab_k)
  ggsave(plot=modelplot_k, width=5, height=4, dpi=300, device=png,
         filename=paste(file_prefix, "_k_response.png", sep=""),
         path="../../results/data_extraction/plots/k_response")
  
  # Plot k = 0
  modelplot_0 <- stimulus_response_plot(di=di, i=i, k=k_sol, response=response,
                                        weights=weights, family=family,
                                        ylab=response_lab, main=ref,
                                        xlab=paste("absolute", stimulus, "difference"))
  ggsave(plot=modelplot_0, width=5, height=4, dpi=300, device=png,
         filename=paste(file_prefix, "_k0_response.png", sep=""),
         path="../../results/data_extraction/plots/k0_response")
  
  # Plot k = 1
  modelplot_1 <- stimulus_response_plot(di=di, i=i, k=k_sol, response=response,
                                        weights=weights, family=family,
                                        ylab=response_lab,
                                        xlab=paste("relative", stimulus, "difference"))
  ggsave(plot=modelplot_1, width=5, height=4, dpi=300, device=png,
         filename=paste(file_prefix, "_k1_response.png", sep=""),
         path="../../results/data_extraction/plots/k1_response")
  
  ### Generate summaries
  # Summarise k = fitted value
  print(paste("k =", k_sol, "model summary:"))
  model_k <- stimulus_model(di=di, i=i, k=k_sol, response=response,
                            weights=weights, family=family)
  print(summary(model_k))
  results_k <- (summary(model_k)$coefficients)["contrast", ] #extract coefficients
  results_k["R2"] <- NagelkerkeR2(model_k)$R2 #add Nagelkerke's R^2
  results_k
  
  # Summarise k = 0
  print("k = 0 model summary:")
  model_0 <- stimulus_model(di=di, i=i, k=0, response=response,
                            weights=weights, family=family)
  print(summary(model_0))
  results_0 <- (summary(model_0)$coefficients)["contrast", ] #extract coefficients
  results_0["R2"] <- NagelkerkeR2(model_0)$R2 #add Nagelkerke's R^2
  results_0
  
  # Summarise k = 1
  print("k = 1 model summary:")
  model_1 <- stimulus_model(di=di, i=i, k=1, response=response,
                            weights=weights, family=family)
  print(summary(model_1))
  results_1 <- (summary(model_1)$coefficients)["contrast", ] #extract coefficients
  results_1["R2"] <- NagelkerkeR2(model_1)$R2 #add Nagelkerke's R^2
  results_1
  
  # Combine model fit summaries
  model_fits <- rbind(results_0, results_1, results_k)
  rownames(model_fits) <- c("0", "1", "k")
  # Produce a row for the final table
  model_k_results <- t(data.frame(model_fits["k", ]))
  rownames(model_k_results) <- NULL
  
  ### Save results
  k_fit <- data.frame(k_estimate=k_sol,
                      k_lower_95=k_estimate["k", "lower_95"],
                      k_upper_95=k_estimate["k", "upper_95"],
                      AIC_k=k_estimate["AIC", "estimate"],
                      AIC_k_95=k_estimate["AIC", "lower_95"],
                      AIC_0=AIC(model_0),
                      AIC_1=AIC(model_1))
  rownames(k_fit) <- NULL
  k_fit <- round(k_fit, 4) #round to 4 dp
  
  results <- cbind(k_fit, model_k_results)
  
  # Record what type of model it is
  if(identical(family, binomial())){
    reg_type <- "logistic"
  } else if (identical(family, gaussian())){
    reg_type <- "linear"
  }
  results["model"] <- family$family
  results["filename"] <- file_prefix
  results["n_pairs"] <- n_pairs
  # Save results
  write.csv(results, file=paste("../../results/data_extraction/models/",
                                file_prefix, "_models.csv", sep=""),
            row.names=FALSE)
}
