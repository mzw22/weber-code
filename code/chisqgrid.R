
# Takes columns of categorical variables (in dataframe format)
# Produces a grid of chisq p values
chisq.grid <- function(data){
  # make matrix of factors
  nfactors <- ncol(data)
  chi.matrix <- matrix(nrow=nfactors, ncol=nfactors)
  rownames(chi.matrix) <- colnames(data)
  colnames(chi.matrix) <- colnames(data)
  
  # Split into 2 matrices: 1 for test statistics, 1 for p values
  chi.p.matrix <- chi.matrix
  chi.X.matrix <- chi.matrix
  chi.df.matrix <- chi.matrix
  chi.V.matrix <- chi.matrix
  #reporting format: X2(df, N=sample size)=test stat, p=p.value
  chi.str.matrix <- chi.matrix
  chi.Vstr.matrix <- chi.matrix
  
  # Check for a correlation between each pair of factors
  for (col1 in colnames(data)){
    for (col2 in colnames(data)){
      if (col1 != col2){
        # extract columns
        factor1 <- data[, col1]
        factor2 <- data[, col2]
        # Remove NAs
        keep <- !is.na(factor1) & !is.na(factor2)
        factor1 <- factor1[keep]
        factor2 <- factor2[keep]
        # Run chi-square test
        chisq <- chisq.test(x=factor1, y=factor2)
        # Extract important values
        X2 <- as.numeric(chisq$statistic) #extract test statistic
        p.value <- chisq$p.value #extract p value
        df <- as.numeric(chisq$parameter)
        n <- length(factor1)
        # Combine the two in a string
        if (p.value < 0.001){
          p.str <- "p<0.001"
        } else if (p.value > 0.999){
          p.str <- "p=0.999"
        } else {
          p.str <- paste("p=", format(round(p.value, 3), nsmall=3), sep="")
        }
        p.str <- gsub("0.", ".", p.str, fixed=TRUE)
        # Add significance stars
        stars <- stars.pval(p.value)
        if (stars == "."){
          stars <- ""
        } else if (stars == " "){
          stars <- ""
        }
        X.str <- paste("X2(", df, #df
                       ", N=", n, ") = ", #sample size
                       format(round(X2, 2), nsmall=2), #X2 value
                       ", ", p.str, stars, sep="") #p value
        # Calculate Cramer's V
        c <- length(unique(factor1))
        r <- length(unique(factor2))
        V <- sqrt((X2/n)/min(c-1, r-1)) #Cramer's V: measure of correlation
        
        Vstr <- paste("V=", format(round(V, 3), nsmall=3), " ",
                         parenthesise(paste(p.str, stars, sep="")), sep="")
      } else {
        X2 <- NA
        p.value <- NA
        X.str <- NA
        df <- NA
        V <- NA
        Vstr <- NA
      }
      # Add to the grids
      # formatted output
      chi.str.matrix[col1, col2] <- X.str
      chi.str.matrix[col2, col1] <- X.str
      # X2
      chi.X.matrix[col1, col2] <- X2
      chi.X.matrix[col2, col1] <- X2
      # p values
      chi.p.matrix[col1, col2] <- p.value
      chi.p.matrix[col2, col1] <- p.value
      #degrees of freedom
      chi.df.matrix[col1, col2] <- df
      chi.df.matrix[col2, col1] <- df
      # Cramer's V
      chi.V.matrix[col1, col2] <- V
      chi.V.matrix[col2, col1] <- V
      
      chi.Vstr.matrix[col1, col2] <- Vstr
      chi.Vstr.matrix[col2, col1] <- Vstr
    }
  }
  output <- list(X2.table = chi.str.matrix,
                 X2 = chi.X.matrix,
                 p.value = chi.p.matrix,
                 V = chi.V.matrix,
                 V.table = chi.Vstr.matrix)
  return(output)
}
