#' Print method for complete_ttest objects
#'
#' @param x Object of class complete_ttest
#' @param ... Additional arguments
#'
#' @export
print.complete_ttest <- function(x, ...) {
  
  cat("\n===== COMPLETE T-TEST ANALYSIS =====\n\n")
  
  # Test information
  cat("Test Type:", x$method, "\n")
  cat("Response Variable:", x$response_var, "\n")
  if(!is.null(x$group_var)) {
    cat("Grouping Variable:", x$group_var, "\n")
  }
  cat("Parametric:", if(x$parametric) "Yes" else "No (non-parametric)\n")
  cat("\n")
  
  # Assumptions
  cat("----- Assumption Checks -----\n")
  assump <- x$assumptions
  if(!is.na(assump$normality_passed)) {
    cat("Normality:", if(assump$normality_passed) "PASSED \u2713" else "FAILED \u2717", "\n")
  }
  if(!is.na(assump$variance_passed) && !is.null(assump$variance_passed)) {
    cat("Homogeneity:", if(assump$variance_passed) "PASSED \u2713" else "FAILED \u2717", "\n")
  }
  
  if(length(assump$recommendations) > 0) {
    cat("\nRecommendations:\n")
    for(rec in assump$recommendations) {
      cat("  \u2022", rec, "\n")
    }
  }
  cat("\n")
  
  # Test results
  cat("----- Test Results -----\n")
  raw <- x$raw
  if(x$test_type == "one_sample") {
    cat(sprintf("t(%.2f) = %.2f, p = %.4f\n", 
                raw$parameter, raw$statistic, raw$p.value))
  } else {
    cat(sprintf("t(%.2f) = %.2f, p = %.4f\n", 
                raw$parameter, raw$statistic, raw$p.value))
  }
  
  # Effect size
  if(!is.null(x$effect_size)) {
    cat(sprintf("\nEffect Size: %s = %.2f (%s)\n",
                x$effect_size$name,
                x$effect_size$value,
                x$effect_size$interpretation))
  }
  
  # Descriptive statistics
  cat("\n----- Descriptive Statistics -----\n")
  print(x$descriptive_stats)
  
  # APA table
  if(!is.null(x$apa_table)) {
    cat("\n----- APA-Formatted Table -----\n")
    print(x$apa_table)
  }
}

#' Summary method for complete_ttest
#'
#' @param object Object of class complete_ttest
#' @param ... Additional arguments
#'
#' @export
summary.complete_ttest <- function(object, ...) {
  
  cat("\n===== SUMMARY OF COMPLETE T-TEST =====\n\n")
  
  # APA-style interpretation
  interpretation <- generate_apa_interpretation(object)
  cat(interpretation, "\n\n")
  
  # Additional details
  cat("Test Details:\n")
  cat("  Method:", object$method, "\n")
  cat("  Sample Size:", sum(object$descriptive_stats$N), "\n")
  cat("  p-value:", format_p_value(object$raw$p.value), "\n")
  
  if(!is.null(object$effect_size)) {
    cat("  Effect Size:", object$effect_size$name, 
        "=", round(object$effect_size$value, 3), 
        "(", object$effect_size$interpretation, ")\n")
  }
}

#' Generate APA-Style Interpretation
#' @noRd
generate_apa_interpretation <- function(result) {
  
  var_name <- result$response_var
  method <- result$method
  
  # Start interpretation
  if(result$test_type == "one_sample") {
    mu <- result$raw$null.value
    mean_val <- result$descriptive_stats$Mean
    
    interpretation <- sprintf(
      "A %s was conducted to compare %s to a hypothesized value of %.2f. 
      The mean was %.2f (SD = %.2f), which was %s significantly different 
      from the hypothesized value, %s = %.2f, p = %.4f, d = %.2f (%s).",
      method,
      var_name,
      mu,
      mean_val,
      result$descriptive_stats$SD,
      if(result$raw$p.value < 0.05) "" else "not",
      if(result$parametric) "t" else "V",
      result$raw$statistic,
      result$raw$p.value,
      result$effect_size$value,
      result$effect_size$interpretation
    )
    
  } else if(result$test_type == "paired") {
    groups <- unique(result$descriptive_stats[[result$group_var]])
    
    interpretation <- sprintf(
      "A %s was conducted to compare %s scores between %s and %s conditions. 
      There was %s significant difference between conditions, 
      t(%.2f) = %.2f, p = %.4f, d = %.2f (%s).",
      method,
      var_name,
      groups[1],
      groups[2],
      if(result$raw$p.value < 0.05) "a statistically" else "no statistically",
      result$raw$parameter,
      result$raw$statistic,
      result$raw$p.value,
      result$effect_size$value,
      result$effect_size$interpretation
    )
    
  } else if(result$test_type == "two_sample") {
    groups <- unique(result$descriptive_stats[[result$group_var]])
    
    #  FIXED: Completed the sprintf with all arguments
    interpretation <- sprintf(
      "A %s was conducted to compare %s between %s and %s groups. 
      There was %s significant difference between groups,
      t(%.2f) = %.2f, p = %.4f, d = %.2f (%s).",
      method,
      var_name,
      groups[1],
      groups[2],
      if(result$raw$p.value < 0.05) "a statistically" else "no statistically",
      result$raw$parameter,
      result$raw$statistic,
      result$raw$p.value,
      result$effect_size$value,
      result$effect_size$interpretation
    )
    
  } else {
    # Fallback for other test types
    interpretation <- sprintf(
      "A %s was conducted. Results were %s significant, 
      test statistic = %.2f, p = %.4f.",
      method,
      if(result$raw$p.value < 0.05) "statistically" else "not statistically",
      result$raw$statistic,
      result$raw$p.value
    )
  }
  
  return(interpretation)
}
