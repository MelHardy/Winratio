################################################################################################################
########################################Stratification Calculations############################################
################################################################################################################
################################################################################################################

#To be used with the WinRatio package. The win ratio is applied to each strata and then combined weighted win ratio
#can be calculated with the following function. list1_name and list2_name and the respective win ratio lists of each
#strata

# Required libraries 
library(WinRatio)

stratified_win_ratio <- function(list1_name, list2_name) {
  # Getting the lists using their names
  list1 <- get(list1_name)
  list2 <- get(list2_name)
  
  # Extracting elements and combining them into new vectors
  size <- c(list1$n, list2$n)
  wins <- c(list1$total.wins, list2$total.wins)
  loss <- c(list1$total.loss, list2$total.loss)
  se <- c(list1$se, list2$se)
  
  # Calculate the stratum weights
  weights <- 1 / size / sum(1 / size)
  
  # Calculating stratified win ratio
  wrstrat <- sum(wins * weights) / sum(loss * weights)
  
  # Calculating overall standard error
  overall_standard_error <- sqrt(sum((se^2) * weights^2))
  
  # Calculating z
  z <- log(wrstrat) / overall_standard_error
  
  # Calculating p-value
  p <- 2 * (1 - pnorm(abs(z)))
  
  # Calculating lower and upper confidence intervals
  lci <- exp(log(wrstrat) - 1.96 * overall_standard_error)
  uci <- exp(log(wrstrat) + 1.96 * overall_standard_error)
  
  # Printing the results
  cat("Stratified Win Ratio: ", wrstrat, "\n")
  cat("Z-score: ", z, "\n")
  cat("Standard Error: ", overall_standard_error, "\n")
  cat("P-value: ", p, "\n")
  cat("Lower CI: ", lci, "\n")
  cat("Upper CI: ", uci, "\n")
}