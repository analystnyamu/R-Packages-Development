# =============================================================================
# AUTOMATIC TEST SELECTION
# File: R/auto_selection.R
# =============================================================================

#' Automatic Test Selection Based on Assumptions
#'
#' Selects the appropriate statistical test (parametric or non-parametric)
#' based on assumption checks, then runs the selected test.
#'
#' @param data Data frame containing the variables
#' @param response_var Character, name of the response variable
#' @param group_var Character, name of the grouping variable
#'   (NULL for one-sample tests)
#' @param test_type Character, one of "one_sample", "two_sample", or "paired"
#' @param paired Logical, whether to use paired test
#' @param alternative Character, alternative hypothesis: "two.sided",
#'   "less", or "greater"
#' @param mu Numeric, hypothesized mean (for one-sample tests)
#'
#' @return A list containing test results with elements:
#'   \itemize{
#'     \item \code{raw}: Raw test result from stats package
#'     \item \code{method}: Name of the test performed
#'     \item \code{parametric}: Logical, whether parametric test was used
#'     \item \code{descriptive_stats}: Descriptive statistics
#'     \item \code{assumptions}: Assumption check results
#'     \item \code{test_type}: Type of test ("one_sample", etc.)
#'     \item \code{response_var}: Name of response variable
#'     \item \code{group_var}: Name of grouping variable
#'     \item \code{paired}: Whether paired test was used
#'   }
#'
#' @keywords internal
#' @noRd
auto_select_ttest <- function(data, response_var, group_var,
                              test_type, paired, alternative, mu) {
  
  # ---- Input validation ----------------------------------------------------
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  
  if (!is.character(response_var) || length(response_var) != 1) {
    stop("'response_var' must be a single character string.", call. = FALSE)
  }
  
  if (!is.null(group_var) &&
      (!is.character(group_var) || length(group_var) != 1)) {
    stop("'group_var' must be NULL or a single character string.",
         call. = FALSE)
  }
  
  if (!test_type %in% c("one_sample", "two_sample", "paired")) {
    stop("'test_type' must be 'one_sample', 'two_sample', or 'paired'.",
         call. = FALSE)
  }
  
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("'alternative' must be 'two.sided', 'less', or 'greater'.",
         call. = FALSE)
  }
  
  # ---- Check assumptions ---------------------------------------------------
  assumptions <- tryCatch({
    check_complete_assumptions(
      data = data,
      response_var = response_var,
      group_var = group_var,
      test_type = test_type
    )
  }, error = function(e) {
    warning("Could not check assumptions: ", e$message,
            "\nProceeding with parametric test.", call. = FALSE)
    list(
      normality_passed = TRUE,
      variance_passed = TRUE,
      normality_p_value = NA_real_,
      variance_p_value = NA_real_,
      warnings = character(0),
      recommendations = character(0)
    )
  })
  
  # ---- Select and run appropriate test -------------------------------------
  
  # -------------------------------------------------------------------------
  # ONE-SAMPLE TESTS
  # -------------------------------------------------------------------------
  if (test_type == "one_sample") {
    
    if (isTRUE(assumptions$normality_passed)) {
      message("\u2713 Using one-sample t-test (normality assumption met)")
      
      result <- perform_one_sample_ttest(
        data = data,
        response_var = response_var,
        alternative = alternative,
        mu = mu
      )
      result$method <- "One-sample t-test"
      result$parametric <- TRUE
      
    } else {
      message("\u2713 Using Wilcoxon signed-rank test (normality violated)")
      
      result <- perform_wilcoxon_signed_rank(
        data = data,
        response_var = response_var,
        alternative = alternative,
        mu = mu
      )
      result$method <- "Wilcoxon signed-rank test"
      result$parametric <- FALSE
    }
  }
  
  # -------------------------------------------------------------------------
  # PAIRED TESTS
  # -------------------------------------------------------------------------
  else if (test_type == "paired") {
    
    if (isTRUE(assumptions$normality_passed)) {
      message("\u2713 Using paired t-test (normality of differences met)")
      
      result <- perform_paired_ttest(
        data = data,
        response_var = response_var,
        group_var = group_var,
        alternative = alternative
      )
      result$method <- "Paired t-test"
      result$parametric <- TRUE
      
    } else {
      message("\u2713 Using Wilcoxon signed-rank test (normality violated)")
      
      result <- perform_paired_wilcoxon(
        data = data,
        response_var = response_var,
        group_var = group_var,
        alternative = alternative
      )
      result$method <- "Wilcoxon signed-rank test (paired)"
      result$parametric <- FALSE
    }
  }
  
  # -------------------------------------------------------------------------
  # TWO-SAMPLE TESTS
  # -------------------------------------------------------------------------
  else if (test_type == "two_sample") {
    
    normality_ok <- isTRUE(assumptions$normality_passed)
    variance_ok <- isTRUE(assumptions$variance_passed)
    
    if (normality_ok && variance_ok) {
      # Case 1: All assumptions met → standard t-test
      message("\u2713 Using two-sample t-test (equal variances)")
      
      result <- perform_two_sample_ttest(
        data = data,
        response_var = response_var,
        group_var = group_var,
        alternative = alternative,
        var_equal = TRUE
      )
      result$method <- "Two-sample t-test (equal variances)"
      result$parametric <- TRUE
      
    } else if (normality_ok && !variance_ok) {
      # Case 2: Normality OK but unequal variances → Welch's t-test
      message("\u2713 Using Welch's t-test (unequal variances)")
      
      result <- perform_two_sample_ttest(
        data = data,
        response_var = response_var,
        group_var = group_var,
        alternative = alternative,
        var_equal = FALSE
      )
      result$method <- "Welch's t-test"
      result$parametric <- TRUE
      
    } else {
      # Case 3: Normality violated → Mann-Whitney U
      message("\u2713 Using Mann-Whitney U test (normality violated)")
      
      result <- perform_mann_whitney(
        data = data,
        response_var = response_var,
        group_var = group_var,
        alternative = alternative
      )
      result$method <- "Mann-Whitney U test"
      result$parametric <- FALSE
    }
  }
  
  # ---- Add metadata to result ----------------------------------------------
  result$assumptions <- assumptions
  result$test_type <- test_type
  result$response_var <- response_var
  result$group_var <- group_var
  result$paired <- paired
  
  return(result)
}