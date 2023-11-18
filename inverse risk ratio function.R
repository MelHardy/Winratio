calculate_IRR_stats <- function(RR, Lower_CI, Upper_CI) {
  # Calculate IRR
  IRR <- 1 / RR
  
  # Calculate confidence interval for IRR
  Lower_CI_IRR <- 1 / Upper_CI
  Upper_CI_IRR <- 1 / Lower_CI
  
  # Calculate SE for log-transformed IRR
  SE_ln_IRR <- (log(Upper_CI_IRR) - log(Lower_CI_IRR)) / (2 * 1.96)
  
  # Calculate standard error for IRR
  SE_IRR <- SE_ln_IRR / (abs(IRR)^2)
  
  # Calculate Z-score for IRR
  Z_IRR <- (IRR - 1) / SE_IRR
  
  # Calculate p-value for IRR
  p_value_IRR <- 2 * (1 - pnorm(abs(Z_IRR)))
  
  # Print the results
  cat("Inverse Risk Ratio (IRR):", IRR, "\n")
  cat("Standard Error (SE) for IRR:", SE_IRR, "\n")
  cat("Z-score for IRR:", Z_IRR, "\n")
  cat("p-value for IRR:", p_value_IRR, "\n")
  cat("Standard Error (SE) for log-transformed IRR:", SE_ln_IRR, "\n")
  cat("Lower CI for IRR:", Lower_CI_IRR, "\n")
  cat("Upper CI for IRR:", Upper_CI_IRR, "\n")
  
  # Return results as a list
  results <- list(
    IRR = IRR,
    SE_IRR = SE_IRR,
    Z_IRR = Z_IRR,
    p_value_IRR = p_value_IRR,
    SE_ln_IRR = SE_ln_IRR,
    Lower_CI_IRR = Lower_CI_IRR,
    Upper_CI_IRR = Upper_CI_IRR
  )
  
  return(results)
}
