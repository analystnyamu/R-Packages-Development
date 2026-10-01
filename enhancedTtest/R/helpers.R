# =============================================================================
# HELPER FUNCTIONS
# File: R/helpers.R
# =============================================================================

#' Format p-value for APA style
#'
#' @param p Numeric p-value
#' @param include_stars Logical, include significance stars (default TRUE)
#'
#' @return Character string with formatted p-value
#' @export
#'
#' @examples
#' format_p_value(0.0001)
#' format_p_value(0.03)
#' format_p_value(0.5)
format_p_value <- function(p, include_stars = TRUE) {
  if (is.na(p)) return("NA")
  
  if (p < 0.001) {
    star <- if (include_stars) "***" else ""
    return(paste0("< .001", star))
  } else if (p < 0.01) {
    star <- if (include_stars) "**" else ""
    return(sprintf("%.3f%s", p, star))
  } else if (p < 0.05) {
    star <- if (include_stars) "*" else ""
    return(sprintf("%.3f%s", p, star))
  } else if (p < 0.1) {
    star <- if (include_stars) "\u2020" else ""
    return(sprintf("%.3f%s", p, star))
  } else {
    return(sprintf("%.3f", p))
  }
}


#' Interpret Cohen's d
#'
#' @param d Numeric Cohen's d value
#'
#' @return Character string with interpretation
#' @export
#'
#' @examples
#' interpret_cohens_d(0.3)
interpret_cohens_d <- function(d) {
  if (is.na(d)) return("Unknown")
  d <- abs(d)
  
  if (d < 0.2) {
    return("Negligible")
  } else if (d < 0.5) {
    return("Small")
  } else if (d < 0.8) {
    return("Medium")
  } else if (d < 1.2) {
    return("Large")
  } else {
    return("Very Large")
  }
}


#' Interpret r (for non-parametric tests)
#'
#' @param r Numeric r value
#'
#' @return Character string with interpretation
#' @export
#'
#' @examples
#' interpret_r(0.4)
interpret_r <- function(r) {
  if (is.na(r)) return("Unknown")
  r <- abs(r)
  
  if (r < 0.1) {
    return("Negligible")
  } else if (r < 0.3) {
    return("Small")
  } else if (r < 0.5) {
    return("Medium")
  } else {
    return("Large")
  }
}


#' Generate random test data
#'
#' @param n Sample size per group (default 50)
#' @param mean Mean of first group (default 10)
#' @param sd Standard deviation (default 2)
#' @param group1_name Name of first group (default "A")
#' @param group2_name Name of second group (default "B")
#' @param seed Random seed (default 123)
#'
#' @return A data frame with columns value and group
#' @export
#'
#' @examples
#' df <- generate_test_data()
generate_test_data <- function(n = 50, mean = 10, sd = 2,
                               group1_name = "A", group2_name = "B",
                               seed = 123) {
  set.seed(seed)
  data.frame(
    value = c(
      stats::rnorm(n, mean, sd),
      stats::rnorm(n, mean + 1.5, sd)
    ),
    group = factor(rep(c(group1_name, group2_name), each = n))
  )
}


#' Check if value is numeric and finite
#'
#' @param x Value to check
#' @return Logical
#' @keywords internal
#' @noRd
is_finite_numeric <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x)
}


#' Safely extract information from test results
#'
#' @param raw_result Raw test result object
#' @param element Name of element to extract
#' @param default Default value if not found
#' @return Extracted value or default
#' @keywords internal
#' @noRd
safe_extract <- function(raw_result, element, default = NA) {
  tryCatch({
    if (!is.null(raw_result[[element]])) {
      return(raw_result[[element]])
    } else {
      return(default)
    }
  }, error = function(e) {
    return(default)
  })
}