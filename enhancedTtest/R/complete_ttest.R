# =============================================================================
# COMPLETE T-TEST ANALYSIS
# File: R/complete_ttest.R
# =============================================================================

#' Complete T-Test Analysis with Automatic Selection
#'
#' Performs one-sample, two-sample, or paired t-tests with automatic
#' non-parametric fallbacks when assumptions are violated. Handles input
#' validation, assumption checking, effect size calculation, and produces
#' APA-formatted output.
#'
#' @param data A data frame containing the variables.
#' @param formula A formula specifying the model:
#'   \itemize{
#'     \item One-sample: \code{response ~ 1}
#'     \item Two-sample: \code{response ~ group}
#'     \item Paired: \code{response ~ group} with \code{paired = TRUE}
#'   }
#' @param paired Logical, whether to perform a paired t-test
#'   (default \code{FALSE}).
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of \code{"two.sided"} (default), \code{"less"}, or
#'   \code{"greater"}.
#' @param mu Numeric, the hypothesized mean for one-sample tests
#'   (default \code{0}). Ignored for two-sample and paired tests.
#' @param auto_select Logical, if \code{TRUE} (default) automatically selects
#'   a parametric or non-parametric test based on assumption checks
#'   (normality and homogeneity of variance). If \code{FALSE}, always
#'   performs the parametric test.
#' @param apa_table Logical, if \code{TRUE} (default) creates an
#'   APA-formatted \code{flextable} object.
#' @param export_word Logical, if \code{TRUE} exports the APA table to a
#'   Word document (default \code{FALSE}).
#' @param filename Character string, output filename for Word export.
#'   Only used when \code{export_word = TRUE}.
#'
#' @return
#' An object of class \code{complete_ttest} (also inheriting from
#' \code{enhanced_ttest}), which is a named list containing the following
#' components:
#'
#' \describe{
#'   \item{\code{raw}}{The raw test result object returned by
#'     \code{\link[stats]{t.test}} or \code{\link[stats]{wilcox.test}}.
#'     Contains the test statistic, degrees of freedom, p-value, confidence
#'     interval, and estimates.}
#'
#'   \item{\code{method}}{Character string naming the test performed
#'     (e.g., \code{"Welch's t-test"}, \code{"Mann-Whitney U test"}).}
#'
#'   \item{\code{parametric}}{Logical; \code{TRUE} if a parametric test was
#'     used, \code{FALSE} if a non-parametric alternative was selected.}
#'
#'   \item{\code{test_type}}{Character string indicating the test type:
#'     \code{"one_sample"}, \code{"two_sample"}, or \code{"paired"}.}
#'
#'   \item{\code{response_var}}{Character string with the name of the
#'     response variable from the formula.}
#'
#'   \item{\code{group_var}}{Character string with the name of the grouping
#'     variable from the formula, or \code{NULL} for one-sample tests.}
#'
#'   \item{\code{data}}{The processed data frame used in the analysis,
#'     after removing missing values and converting the grouping variable
#'     to a factor (if applicable).}
#'
#'   \item{\code{alternative}}{Character string with the alternative
#'     hypothesis used.}
#'
#'   \item{\code{mu}}{Numeric value of the hypothesized mean
#'     (for one-sample tests).}
#'
#'   \item{\code{descriptive_stats}}{Data frame with descriptive statistics
#'     by group (or overall for one-sample tests). Contains columns:
#'     \code{group}, \code{N}, \code{Mean}, \code{SD}, \code{SE} for
#'     parametric tests; or \code{group}, \code{N}, \code{Median},
#'     \code{IQR} for non-parametric tests.}
#'
#'   \item{\code{effect_size}}{A named list describing the effect size
#'     with elements:
#'     \itemize{
#'       \item \code{value}: Numeric effect size value.
#'       \item \code{name}: Character string, either \code{"Cohen's d"}
#'         (parametric) or \code{"r"} (non-parametric).
#'       \item \code{interpretation}: Character string describing the
#'         magnitude (e.g., \code{"Small"}, \code{"Medium"}, \code{"Large"}).
#'     }}
#'
#'   \item{\code{assumptions}}{A named list with results of assumption
#'     checks, including \code{normality_passed}, \code{normality_p_value},
#'     \code{variance_passed}, \code{variance_p_value}, plus
#'     \code{warnings} and \code{recommendations} character vectors.}
#'
#'   \item{\code{apa_table}}{A \code{flextable} object containing the
#'     publication-ready APA-formatted table, or \code{NULL} if
#'     \code{apa_table = FALSE} or table creation failed.}
#'
#'   \item{\code{recommendation}}{Character string with a plain-language
#'     summary of the analysis, including the test method used, assumption
#'     check results, significance, and effect size interpretation.}
#'
#'   \item{\code{call}}{The matched function call, for reproducibility.}
#' }
#'
#' The object has S3 methods \code{\link{print.complete_ttest}} and
#' \code{\link{summary.complete_ttest}} for displaying results.
#'
#' @examples
#' \donttest{
#' # One-sample t-test: test if mean mpg differs from 20
#' result <- complete_ttest(mtcars, mpg ~ 1, mu = 20)
#' print(result)
#'
#' # Two-sample t-test: compare Sepal.Length between two species
#' iris_subset <- iris[iris$Species != "virginica", ]
#' iris_subset$Species <- droplevels(iris_subset$Species)
#' result <- complete_ttest(iris_subset, Sepal.Length ~ Species)
#' summary(result)
#'
#' # Access components
#' result$method          # Test name
#' result$raw$p.value     # p-value
#' result$effect_size     # Effect size with interpretation
#' result$apa_table       # APA-formatted flextable
#' }
#'
#' @seealso
#' \code{\link{create_complete_apa_table}} for building APA tables,
#' \code{\link{export_apa_to_word}} for Word export,
#' \code{\link{format_p_value}} for formatting p-values.
#'
#' @export
complete_ttest <- function(data,
                           formula,
                           paired = FALSE,
                           alternative = "two.sided",
                           mu = 0,
                           auto_select = TRUE,
                           apa_table = TRUE,
                           export_word = FALSE,
                           filename = "ttest_results.docx") {
  
  # ===========================================================================
  # SECTION 1: INPUT VALIDATION
  # ===========================================================================
  
  if (missing(data)) {
    stop("'data' argument is missing", call. = FALSE)
  }
  
  if (is.null(data)) {
    stop("data must be a data frame (got NULL)", call. = FALSE)
  }
  
  if (!is.data.frame(data)) {
    stop(
      "data must be a data frame (got ",
      paste(class(data), collapse = "/"), ")",
      call. = FALSE
    )
  }
  
  if (ncol(data) == 0) {
    stop("data must have at least one column", call. = FALSE)
  }
  
  if (nrow(data) == 0) {
    stop("data must have at least one row", call. = FALSE)
  }
  
  if (missing(formula)) {
    stop("'formula' argument is missing", call. = FALSE)
  }
  
  if (!inherits(formula, "formula")) {
    stop(
      "formula must be a formula object (e.g., 'response ~ group')",
      call. = FALSE
    )
  }
  
  vars <- all.vars(formula)
  
  if (length(vars) == 0) {
    stop("Invalid formula: no variables found", call. = FALSE)
  }
  
  if (length(vars) > 2) {
    stop(
      "Invalid formula: t-tests require 1 or 2 variables, found ",
      length(vars), ": ", paste(vars, collapse = ", "), ".\n",
      "Use 'response ~ group' or 'response ~ 1'",
      call. = FALSE
    )
  }
  
  response_var <- vars[1]
  
  if (!(response_var %in% names(data))) {
    stop(
      "Response variable '", response_var, "' not found in data.\n",
      "Available variables: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  
  if (!is.numeric(data[[response_var]])) {
    stop(
      "Response variable '", response_var, "' must be numeric (got ",
      paste(class(data[[response_var]]), collapse = "/"), ")",
      call. = FALSE
    )
  }
  
  group_var <- NULL
  
  if (length(vars) == 2) {
    group_var <- vars[2]
    
    if (!(group_var %in% names(data))) {
      stop(
        "Grouping variable '", group_var, "' not found in data.\n",
        "Available variables: ", paste(names(data), collapse = ", "),
        call. = FALSE
      )
    }
  }
  
  if (!is.logical(paired) || length(paired) != 1) {
    stop("'paired' must be a single logical value (TRUE or FALSE)",
         call. = FALSE)
  }
  
  if (!is.character(alternative) || length(alternative) != 1) {
    stop("'alternative' must be a single character string", call. = FALSE)
  }
  
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop(
      "'alternative' must be one of: 'two.sided', 'less', 'greater'",
      call. = FALSE
    )
  }
  
  if (!is.numeric(mu) || length(mu) != 1 || !is.finite(mu)) {
    stop("'mu' must be a single finite numeric value", call. = FALSE)
  }
  
  if (!is.logical(auto_select) || length(auto_select) != 1) {
    stop("'auto_select' must be a single logical value", call. = FALSE)
  }
  
  if (!is.logical(apa_table) || length(apa_table) != 1) {
    stop("'apa_table' must be a single logical value", call. = FALSE)
  }
  
  if (!is.logical(export_word) || length(export_word) != 1) {
    stop("'export_word' must be a single logical value", call. = FALSE)
  }
  
  if (!is.character(filename) || length(filename) != 1) {
    stop("'filename' must be a single character string", call. = FALSE)
  }
  
  # ===========================================================================
  # SECTION 2: DATA PROCESSING
  # ===========================================================================
  
  original_n <- nrow(data)
  
  if (!is.null(group_var)) {
    data <- data[, c(response_var, group_var), drop = FALSE]
  } else {
    data <- data[, response_var, drop = FALSE]
  }
  
  data <- stats::na.omit(data)
  
  if (nrow(data) == 0) {
    stop(
      "No complete cases found after removing missing values.\n",
      "Original rows: ", original_n,
      call. = FALSE
    )
  }
  
  if (!is.null(group_var)) {
    if (!is.factor(data[[group_var]])) {
      data[[group_var]] <- as.factor(data[[group_var]])
    }
    data[[group_var]] <- droplevels(data[[group_var]])
  }
  
  # ===========================================================================
  # SECTION 3: DETERMINE TEST TYPE
  # ===========================================================================
  
  if (is.null(group_var)) {
    test_type <- "one_sample"
    
  } else {
    n_groups <- length(unique(data[[group_var]]))
    
    if (n_groups < 2) {
      test_type <- "one_sample"
      group_var <- NULL
      data <- data[, response_var, drop = FALSE]
      warning(
        "Only one group found in data. Performing one-sample test instead.",
        call. = FALSE
      )
      
    } else if (n_groups == 2) {
      if (paired) {
        test_type <- "paired"
      } else {
        test_type <- "two_sample"
      }
      
    } else {
      stop(
        sprintf(
          "More than 2 groups detected (%d groups: %s).\n",
          n_groups,
          paste(levels(data[[group_var]]), collapse = ", ")
        ),
        "For multiple group comparison, use ANOVA instead.\n",
        "To proceed with t-test, filter your data to exactly 2 groups.",
        call. = FALSE
      )
    }
  }
  
  # ===========================================================================
  # SECTION 4: CHECK ASSUMPTIONS
  # ===========================================================================
  
  assumptions <- check_complete_assumptions(
    data = data,
    response_var = response_var,
    group_var = group_var,
    test_type = test_type
  )
  
  # ===========================================================================
  # SECTION 5: SELECT AND RUN APPROPRIATE TEST
  # ===========================================================================
  
  result <- run_selected_test(
    data = data,
    response_var = response_var,
    group_var = group_var,
    test_type = test_type,
    paired = paired,
    alternative = alternative,
    mu = mu,
    auto_select = auto_select,
    assumptions = assumptions
  )
  
  # ===========================================================================
  # SECTION 6: CALCULATE EFFECT SIZE
  # ===========================================================================
  
  effect_size <- calculate_effect_size_for_result(
    result = result,
    test_type = test_type,
    response_var = response_var
  )
  
  # ===========================================================================
  # SECTION 7: BUILD RESULT OBJECT
  # ===========================================================================
  
  final_result <- list(
    raw = result$raw,
    method = result$method,
    parametric = result$parametric,
    test_type = test_type,
    response_var = response_var,
    group_var = group_var,
    data = data,
    alternative = alternative,
    mu = mu,
    descriptive_stats = result$descriptive_stats,
    effect_size = effect_size,
    assumptions = assumptions,
    apa_table = NULL,
    recommendation = NULL,
    call = match.call()
  )
  
  # ===========================================================================
  # SECTION 8: GENERATE RECOMMENDATION
  # ===========================================================================
  
  final_result$recommendation <- generate_recommendation(
    result = final_result,
    assumptions = assumptions
  )
  
  # ===========================================================================
  # SECTION 9: CREATE APA TABLE
  # ===========================================================================
  
  if (apa_table) {
    final_result$apa_table <- tryCatch(
      {
        create_complete_apa_table(final_result)
      },
      error = function(e) {
        warning(
          "Could not create APA table: ", conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    )
  }
  
  # ===========================================================================
  # SECTION 10: EXPORT TO WORD
  # ===========================================================================
  
  if (export_word && !is.null(final_result$apa_table)) {
    tryCatch(
      {
        export_apa_to_word(final_result$apa_table, filename)
        message("Results exported to: ", filename)
      },
      error = function(e) {
        warning(
          "Could not export to Word: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  
  # ===========================================================================
  # SECTION 11: SET CLASS AND RETURN
  # ===========================================================================
  
  class(final_result) <- c("complete_ttest", "enhanced_ttest")
  
  return(final_result)
}


# =============================================================================
# INTERNAL HELPER: Run Selected Test
# =============================================================================

#' Run the selected statistical test
#'
#' @keywords internal
#' @noRd
run_selected_test <- function(data,
                              response_var,
                              group_var,
                              test_type,
                              paired,
                              alternative,
                              mu,
                              auto_select,
                              assumptions) {
  
  response <- data[[response_var]]
  
  # ---- ONE-SAMPLE TESTS ----------------------------------------------------
  if (test_type == "one_sample") {
    
    use_parametric <- if (auto_select) {
      isTRUE(assumptions$normality_passed)
    } else {
      TRUE
    }
    
    if (use_parametric) {
      raw_result <- stats::t.test(
        response,
        alternative = alternative,
        mu = mu
      )
      
      desc_stats <- data.frame(
        group = "Sample",
        N = length(response),
        Mean = mean(response),
        SD = stats::sd(response),
        SE = stats::sd(response) / sqrt(length(response)),
        stringsAsFactors = FALSE
      )
      
      return(list(
        raw = raw_result,
        method = "One-sample t-test",
        parametric = TRUE,
        descriptive_stats = desc_stats
      ))
      
    } else {
      raw_result <- stats::wilcox.test(
        response,
        alternative = alternative,
        mu = mu
      )
      
      desc_stats <- data.frame(
        group = "Sample",
        N = length(response),
        Median = stats::median(response),
        IQR = stats::IQR(response),
        Q1 = stats::quantile(response, 0.25, names = FALSE),
        Q3 = stats::quantile(response, 0.75, names = FALSE),
        stringsAsFactors = FALSE
      )
      
      return(list(
        raw = raw_result,
        method = "Wilcoxon signed-rank test",
        parametric = FALSE,
        descriptive_stats = desc_stats
      ))
    }
  }
  
  # ---- PAIRED TESTS --------------------------------------------------------
  if (test_type == "paired") {
    
    groups <- levels(data[[group_var]])
    group1_data <- response[data[[group_var]] == groups[1]]
    group2_data <- response[data[[group_var]] == groups[2]]
    
    if (length(group1_data) != length(group2_data)) {
      stop(
        "Paired test requires equal number of observations in each group.\n",
        "Found: ", length(group1_data), " vs ", length(group2_data),
        call. = FALSE
      )
    }
    
    use_parametric <- if (auto_select) {
      isTRUE(assumptions$normality_passed)
    } else {
      TRUE
    }
    
    if (use_parametric) {
      raw_result <- stats::t.test(
        group1_data,
        group2_data,
        paired = TRUE,
        alternative = alternative
      )
      
      desc_stats <- data.frame(
        group = groups,
        N = c(length(group1_data), length(group2_data)),
        Mean = c(mean(group1_data), mean(group2_data)),
        SD = c(stats::sd(group1_data), stats::sd(group2_data)),
        stringsAsFactors = FALSE
      )
      
      return(list(
        raw = raw_result,
        method = "Paired t-test",
        parametric = TRUE,
        descriptive_stats = desc_stats
      ))
      
    } else {
      raw_result <- stats::wilcox.test(
        group1_data,
        group2_data,
        paired = TRUE,
        alternative = alternative
      )
      
      desc_stats <- data.frame(
        group = groups,
        N = c(length(group1_data), length(group2_data)),
        Median = c(stats::median(group1_data), stats::median(group2_data)),
        IQR = c(stats::IQR(group1_data), stats::IQR(group2_data)),
        stringsAsFactors = FALSE
      )
      
      return(list(
        raw = raw_result,
        method = "Wilcoxon signed-rank test (paired)",
        parametric = FALSE,
        descriptive_stats = desc_stats
      ))
    }
  }
  
  # ---- TWO-SAMPLE TESTS ----------------------------------------------------
  if (test_type == "two_sample") {
    
    groups <- levels(data[[group_var]])
    group1_data <- response[data[[group_var]] == groups[1]]
    group2_data <- response[data[[group_var]] == groups[2]]
    
    if (auto_select) {
      
      if (!isTRUE(assumptions$normality_passed)) {
        raw_result <- stats::wilcox.test(
          response ~ data[[group_var]],
          alternative = alternative
        )
        
        desc_stats <- data.frame(
          group = groups,
          N = c(length(group1_data), length(group2_data)),
          Median = c(stats::median(group1_data), stats::median(group2_data)),
          IQR = c(stats::IQR(group1_data), stats::IQR(group2_data)),
          stringsAsFactors = FALSE
        )
        
        return(list(
          raw = raw_result,
          method = "Mann-Whitney U test",
          parametric = FALSE,
          descriptive_stats = desc_stats
        ))
        
      } else if (!isTRUE(assumptions$variance_passed)) {
        raw_result <- stats::t.test(
          response ~ data[[group_var]],
          alternative = alternative,
          var.equal = FALSE
        )
        
        desc_stats <- data.frame(
          group = groups,
          N = c(length(group1_data), length(group2_data)),
          Mean = c(mean(group1_data), mean(group2_data)),
          SD = c(stats::sd(group1_data), stats::sd(group2_data)),
          stringsAsFactors = FALSE
        )
        
        return(list(
          raw = raw_result,
          method = "Welch's t-test",
          parametric = TRUE,
          descriptive_stats = desc_stats
        ))
        
      } else {
        raw_result <- stats::t.test(
          response ~ data[[group_var]],
          alternative = alternative,
          var.equal = TRUE
        )
        
        desc_stats <- data.frame(
          group = groups,
          N = c(length(group1_data), length(group2_data)),
          Mean = c(mean(group1_data), mean(group2_data)),
          SD = c(stats::sd(group1_data), stats::sd(group2_data)),
          stringsAsFactors = FALSE
        )
        
        return(list(
          raw = raw_result,
          method = "Two-sample t-test",
          parametric = TRUE,
          descriptive_stats = desc_stats
        ))
      }
      
    } else {
      raw_result <- stats::t.test(
        response ~ data[[group_var]],
        alternative = alternative,
        var.equal = FALSE
      )
      
      desc_stats <- data.frame(
        group = groups,
        N = c(length(group1_data), length(group2_data)),
        Mean = c(mean(group1_data), mean(group2_data)),
        SD = c(stats::sd(group1_data), stats::sd(group2_data)),
        stringsAsFactors = FALSE
      )
      
      return(list(
        raw = raw_result,
        method = "Welch's t-test",
        parametric = TRUE,
        descriptive_stats = desc_stats
      ))
    }
  }
  
  stop("Unknown test type: ", test_type, call. = FALSE)
}


# =============================================================================
# INTERNAL HELPER: Calculate Effect Size for Result
# =============================================================================

#' Calculate effect size for a test result
#'
#' @keywords internal
#' @noRd
calculate_effect_size_for_result <- function(result, test_type, response_var) {
  
  if (isTRUE(result$parametric)) {
    if (test_type == "one_sample") {
      d <- tryCatch({
        m <- result$descriptive_stats$Mean[1]
        sd_val <- result$descriptive_stats$SD[1]
        mu <- result$raw$null.value
        if (is.null(mu)) mu <- 0
        abs(m - mu) / sd_val
      }, error = function(e) NA_real_)
      
    } else {
      d <- tryCatch({
        stats_df <- result$descriptive_stats
        n1 <- stats_df$N[1]
        n2 <- stats_df$N[2]
        m1 <- stats_df$Mean[1]
        m2 <- stats_df$Mean[2]
        sd1 <- stats_df$SD[1]
        sd2 <- stats_df$SD[2]
        
        pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
        
        if (is.na(pooled_sd) || pooled_sd == 0) {
          NA_real_
        } else {
          abs(m1 - m2) / pooled_sd
        }
      }, error = function(e) NA_real_)
    }
    
    return(list(
      value = d,
      name = "Cohen's d",
      interpretation = interpret_cohens_d(d)
    ))
    
  } else {
    r <- tryCatch({
      p <- result$raw$p.value
      n_total <- sum(result$descriptive_stats$N)
      
      if (is.na(p) || is.na(n_total) || n_total == 0) {
        NA_real_
      } else {
        z_score <- stats::qnorm(p / 2)
        abs(z_score) / sqrt(n_total)
      }
    }, error = function(e) NA_real_)
    
    return(list(
      value = r,
      name = "r",
      interpretation = interpret_r(r)
    ))
  }
}


# =============================================================================
# INTERNAL HELPER: Generate Recommendation
# =============================================================================

#' Generate plain-language recommendation
#'
#' @keywords internal
#' @noRd
generate_recommendation <- function(result, assumptions) {
  
  parts <- character(0)
  
  parts <- c(parts, paste0("Method: ", result$method))
  
  if (!is.null(assumptions$normality_passed) &&
      !is.na(assumptions$normality_passed)) {
    if (assumptions$normality_passed) {
      parts <- c(parts, "Normality assumption: PASSED")
    } else {
      parts <- c(parts, "Normality assumption: FAILED (used non-parametric alternative)")
    }
  }
  
  if (!is.null(assumptions$variance_passed) &&
      !is.na(assumptions$variance_passed)) {
    if (assumptions$variance_passed) {
      parts <- c(parts, "Homogeneity of variance: PASSED")
    } else {
      parts <- c(parts, "Homogeneity of variance: FAILED (used Welch's t-test)")
    }
  }
  
  p_val <- result$raw$p.value
  if (!is.null(p_val) && !is.na(p_val)) {
    if (p_val < 0.05) {
      parts <- c(parts, sprintf("Result: SIGNIFICANT (p = %.4f)", p_val))
    } else {
      parts <- c(parts, sprintf("Result: NOT SIGNIFICANT (p = %.4f)", p_val))
    }
  }
  
  if (!is.null(result$effect_size$value) && !is.na(result$effect_size$value)) {
    parts <- c(parts, sprintf(
      "Effect size: %s = %.3f (%s)",
      result$effect_size$name,
      result$effect_size$value,
      result$effect_size$interpretation
    ))
  }
  
  return(paste(parts, collapse = "\n  "))
}