################################################################################################################
#########################################Pooling with Rubin's Rules#############################################
###############################To be used to pool datasets from multiple imputation############################@
################################################################################################################

# Variables
# n is the sample size in the imputed dataset, k the number of parameters to fit
# est is the dataframe with the winratio estimate from each imputed dataset
# se is the dataframe with the standard error  from each imputed dataset

pooled_rr <- function(est, se, conf.level=0.95, n, k){
  est_log <- log (est) # log transformation of the win ratio
  m <- length(est) # number of imputed datasets
  pe <- mean(est_log) # pooled estimate
  vw <- mean(se^2) # within variance
  vb <- var(est_log) # between variance
  vt <- vw + vb + (vb/m) #total variance
  se_p <- sqrt(vt) #pooled standard error
  final_pe <- exp(pe)
  wald <- log(final_pe)/sqrt(vt)
  lam <- (vb + vb/m)/vt #lambda
  riv <- (vb + vb/m)/vw #relative increase in variance
  df_old <- (m - 1) / lam^2
  df_obs <- (((n - k) + 1) / ((n - k) + 3)) * (n - k) *(1 - lam)
  df_adj <- (df_old * df_obs) / (df_old + df_obs)
  alpha <- 1 - (1 - conf.level)/2
  t <- qt(alpha, df_adj) #t statistic
  lower_ci <- exp(pe - t * se_p)
  upper_ci <- exp(pe + t * se_p)
  p_value <- 2 * (1 - pt(abs(wald), df_adj))
  cat(
    paste(
      "Pooled estimate:", round(final_pe,4), "\n",
      "Pooled standard error: ", round(se_p,4), "\n",
      "p value:", round(p_value,10), "\n",
      "Confidence interval:", round(lower_ci,4), " to ", round(upper_ci,4), "\n",
      sep = ""
    )
  )
}

