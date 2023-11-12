################################################################################################################
#########################################Extended Win Ratio summary#############################################
#####################################To be used with WinRatio R Package#########################################
################################################################################################################

ext_summary <- function(object, ..., digits = 2){
  
  #Calculate the standard error
  se <- sqrt(object$v / object$wr^2 / object$n)
  
  #Add the standard error to the object
  object$se <- se
  
  cat("Number of subjects: N =", object$n, "\n")
  cat(paste0("Number of subjects in group '", object$group1, "': ", object$n1, "\n")) 
  cat(paste0("Number of subjects in group '", object$group0, "': ", object$n0, "\n"))
  cat(paste0("Number of paired comparison: ", object$n1 * object$n0, "\n"))
  cat(paste0("Active group = group '", object$group1, "'\n"))
  
  cat(" ","\n")
  mat <- matrix(data = NA, nrow = length(object$call$outcomes)+1, ncol = 2)
  rownames(mat) <- c(paste0("Outcome ", 1:length(object$call$outcomes)), "Total")
  colnames(mat) <- c("Numbers of 'winners'", "Numbers of 'losers'")
  mat[length(object$call$outcomes)+1,] <- c(object$total.wins, object$total.loss)
  outcomes <- NULL
  for (k in 1:length(object$call$outcomes)){
    if (object$call$outcomes[[k]][2] == "s"){
      outcomes <- c(outcomes, paste0("Outcome ", k, " = ", object$call$outcomes[[k]][1], 
                                     ", ",object$call$outcomes[[k]][3], " (survival event)"))
    } else if (object$call$outcomes[[k]][2] == "r"){
      outcomes <- c(outcomes,paste0("Outcome ", k, " = ", 
                                    paste0(utils::head(object$call$outcomes[[k]][[1]], 1), " ... ", utils::tail(object$call$outcomes[[k]][[1]], 1)), ", ",
                                    paste0(utils::head(object$call$outcomes[[k]][[3]], 1), " ... ", utils::tail(object$call$outcomes[[k]][[3]], 1)),
                                    " (repeated events)"))
    } else if (object$call$outcomes[[k]][2] == "c"){    
      outcomes <- c(outcomes, paste0("Outcome ", k, " = ", object$call$outcomes[[k]][1],
                                     " (continuous event, direction = '", object$call$outcomes[[k]][3], "')"))
    }
    mat[k,] <- c(object$wins[k], object$loss[k])
  }
  paste(object$call$outcomes[[k]][1])
  cat(paste0(outcomes, collapse = "\n"))
  
  cat("\n\n")
  
  print(mat)
  
  signif <- dplyr::case_when(object$p.value < 0.001 ~ "***",
                             object$p.value < 0.01 ~ "**",
                             object$p.value < 0.05 ~ "*",
                             T ~ "")
  pval <- dplyr::if_else(object$p.value < 0.0001, 
                         paste0("p-value < 0.0001", signif), 
                         paste0("p-value = ", sprintf(paste0("%.", 4, "f"), object$p.value), signif))
  
  cat("\n")
  cat(paste0("Win Ratio (CI 95%) = ",sprintf(paste0("%.", digits, "f"), object$wr), " (", 
             sprintf(paste0("%.", digits, "f"), object$wr.lower), " - ",
             sprintf(paste0("%.", digits, "f"), object$wr.upper), "), ", pval,
             "\n\nSignif. codes: '***' p < 0.001, '**' p < 0.01, '*' p < 0.05.", "\n"))
  cat("\n")
  cat(paste0("Variance: ", round (object$v, 3), "\n"))
  cat(paste0("z score:  ", round (object$z, 3), "\n"))
  cat(paste0("Standard error:  ", round (object$se, 3), "\n"))
  {
    wins <- object$wins
    losses <- object$loss
    variance <- object$v
    
    # Calculate ties for each variable
    ties <- vector("integer", length(wins))
    ties[1] <- object$n0[1] * object$n1[1] - wins[1] - losses[1]
    for (i in 2:length(wins)) {
      ties[i] <- ties[i - 1] - wins[i] - losses[i]
    }
    
    total_pairs <- object$n0 * object$n1
    
    proportions_wins <- round(wins / total_pairs * 100, 2)
    proportions_losses <- round(losses / total_pairs *100, 2)
    proportions_ties <- round(ties / total_pairs *100, 2)
    cat("\n") 
    
    # List the results in a table-like format using cat and sprintf
    cat("\n")
    cat("Outcomes\tWins (N)\tWins (%)\tTies (N)\tTies (%)\tLosses (N)\tLosses (%)\n")
    
    for (i in seq_along(wins)) {
      cat(sprintf("Outcome %d\t%d\t\t%.2f\t\t%d\t\t%.2f\t\t%d\t\t%.2f\n",
                  i, wins[i], proportions_wins[i],
                  ties[i], proportions_ties[i],
                  losses[i], proportions_losses[i]))
    }
    
    cat("Total\t\t")
    cat(sprintf("%d\t\t%.2f\t\t%d\t\t%.2f\t\t%d\t\t%.2f\n",
                object$total.wins, 
                round(object$total.wins / (object$n1 * object$n0) * 100, 2),
                object$total.ties, 
                round(object$total.ties / (object$n1 * object$n0) * 100, 2),
                object$total.loss,
                round(object$total.loss / (object$n1 * object$n0) * 100, 2)))
    
    cat("\n")
    cat(paste0("Total number of pairs: N = ", object$n0 * object$n1, "\n"))
    cat(paste0("Total wins: N = ", object$total.wins, "\n"))
    cat(paste0("Proportion of wins: % = ", round(object$total.wins / (object$n1 * object$n0) * 100, 3), "\n"))
    cat(paste0("Total losses: = ", object$total.loss, "\n"))
    cat(paste0("Proportion of losses: % = ", round(object$total.loss / (object$n1 * object$n0) * 100, 3), "\n"))
    cat(paste0("Total number of ties remaining: ", object$total.ties, "\n"))
    cat(paste0("Proportion of ties remaining: % = ", round(object$total.ties / (object$n1 * object$n0) * 100, 3), "\n"))
    cat("\n")
    
    return(object)}
}
