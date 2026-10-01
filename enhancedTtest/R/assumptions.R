#' Complete Assumption Checking for All Test Types
#'
#' @param data Data frame
#' @param response_var Name of response variable
#' @param group_var Name of grouping variable (NULL for one-sample)
#' @param test_type Character, "one_sample", "two_sample", or "paired"
#'
#' @return List with assumption check results
#' @export
check_complete_assumptions <- function(data, response_var, group_var, test_type) {
  
  response <- data[[response_var]]
  
  # Initialize results
  results <- list(
    normality_passed = FALSE,
    variance_passed = NA,
    normality_p_values = NULL,
    variance_p_value = NA,
    warnings = character(),
    recommendations = character()
  )
  
  # Check normality based on test type
  if(test_type == "one_sample") {
    # One-sample: check normality of the single variable
    norm_result <- check_normality(response)
    results$normality_passed <- norm_result$passed
    results$normality_p_values <- norm_result$p_value
    results$normality_details <- norm_result$details
    
    if(!norm_result$passed) {
      results$warnings <- c(results$warnings, 
                            "Normality assumption violated for one-sample test.")
      results$recommendations <- c(results$recommendations,
                                   "Consider Wilcoxon signed-rank test.")
    }
    
  } else if(test_type == "paired") {
    # Paired: check normality of differences
    groups <- unique(data[[group_var]])
    if(length(groups) == 2) {
      group1 <- data[[response_var]][data[[group_var]] == groups[1]]
      group2 <- data[[response_var]][data[[group_var]] == groups[2]]
      differences <- group1 - group2
      
      norm_result <- check_normality(differences)
      results$normality_passed <- norm_result$passed
      results$normality_p_values <- norm_result$p_value
      results$normality_details <- norm_result$details
      
      if(!norm_result$passed) {
        results$warnings <- c(results$warnings,
                              "Normality of differences violated for paired test.")
        results$recommendations <- c(results$recommendations,
                                     "Consider Wilcoxon signed-rank test (paired).")
      }
    }
    
  } else if(test_type == "two_sample") {
    # Two-sample: check normality for each group and homogeneity
    groups <- split(response, data[[group_var]])
    
    # Check normality for each group
    normality_results <- lapply(groups, check_normality)
    norm_passed <- all(sapply(normality_results, function(x) x$passed))
    norm_p_values <- sapply(normality_results, function(x) x$p_value)
    
    results$normality_passed <- norm_passed
    results$normality_p_values <- norm_p_values
    results$normality_details <- normality_results
    
    if(!norm_passed) {
      results$warnings <- c(results$warnings,
                            "Normality assumption violated for one or both groups.")
      results$recommendations <- c(results$recommendations,
                                   "Consider Mann-Whitney U test.")
    }
    
    # Check homogeneity of variance
    var_result <- check_homogeneity(data, response_var, group_var)
    results$variance_passed <- var_result$passed
    results$variance_p_value <- var_result$p_value
    
    if(!var_result$passed) {
      results$warnings <- c(results$warnings,
                            "Variance homogeneity assumption violated.")
      results$recommendations <- c(results$recommendations,
                                   "Consider Welch's t-test.")
    }
  }
  
  return(results)
}

#' Check Normality with Multiple Methods
#'
#' @param x Numeric vector
#' @param method Character, "shapiro", "anderson", or "both"
#' @param alpha Significance level
#'
#' @return List with normality check results
check_normality <- function(x, method = "shapiro", alpha = 0.05) {
  
  x <- x[!is.na(x)]
  n <- length(x)
  
  if(n < 3) {
    return(list(
      passed = NA,
      p_value = NA,
      details = "Sample size too small for normality test"
    ))
  }
  
  if(n > 5000) {
    # For large samples, use visual assessment or Anderson-Darling
    tryCatch({
      if(requireNamespace("nortest", quietly = TRUE)) {
        test_result <- nortest::ad.test(x)
        p_value <- test_result$p.value
        passed <- p_value > alpha
      } else {
        # Use Shapiro but with warning
        test_result <- stats::shapiro.test(x[1:5000])
        p_value <- test_result$p.value
        passed <- p_value > alpha
        warning("Large sample size. Only first 5000 used for Shapiro test.")
      }
    }, error = function(e) {
      return(list(
        passed = NA,
        p_value = NA,
        details = "Normality test failed"
      ))
    })
  } else {
    # Standard Shapiro-Wilk test
    test_result <- stats::shapiro.test(x)
    p_value <- test_result$p.value
    passed <- p_value > alpha
  }
  
  return(list(
    passed = passed,
    p_value = p_value,
    method = if(n > 5000) "Anderson-Darling" else "Shapiro-Wilk",
    n = n
  ))
}

#' Check Homogeneity of Variance
#'
#' @param data Data frame
#' @param response_var Name of response variable
#' @param group_var Name of grouping variable
#' @param method Character, "levene" or "bartlett"
#' @param alpha Significance level
#'
#' @return List with variance homogeneity results
check_homogeneity <- function(data, response_var, group_var, 
                              method = "levene", alpha = 0.05) {
  
  response <- data[[response_var]]
  group <- data[[group_var]]
  
  tryCatch({
    if(method == "levene") {
      # Levene's test (robust to non-normality)
      test_result <- car::leveneTest(response ~ group, data = data, center = median)
      p_value <- test_result$`Pr(>F)`[1]
    } else {
      # Bartlett's test (sensitive to non-normality)
      test_result <- stats::bartlett.test(response ~ group, data = data)
      p_value <- test_result$p.value
    }
    
    return(list(
      passed = p_value > alpha,
      p_value = p_value,
      method = method
    ))
    
  }, error = function(e) {
    return(list(
      passed = NA,
      p_value = NA,
      method = method,
      error = e$message
    ))
  })
}