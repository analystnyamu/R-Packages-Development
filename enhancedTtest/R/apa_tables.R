# =============================================================================
# APA TABLE FUNCTIONS
# File: R/apa_tables.R
# =============================================================================

#' Create APA Table for complete_ttest Object
#'
#' Creates a publication-ready APA-formatted table from a \code{complete_ttest}
#' result object. Automatically selects the correct table format based on
#' the test type (one-sample, two-sample, or paired) and whether the test
#' was parametric or non-parametric.
#'
#' @param result A \code{complete_ttest} result object
#' @return A \code{flextable} object, or \code{NULL} if descriptive
#'   statistics are not available
#' @export
#' @examples
#' \donttest{
#' result <- complete_ttest(
#'   iris[iris$Species != "virginica", ],
#'   Sepal.Length ~ Species
#' )
#' create_complete_apa_table(result)
#' }
create_complete_apa_table <- function(result) {
  
  if (is.null(result$descriptive_stats)) {
    return(NULL)
  }
  
  # ---- One-sample tests ----------------------------------------------------
  if (result$test_type == "one_sample") {
    if (isTRUE(result$parametric)) {
      return(create_one_sample_apa_table(result))
    } else {
      return(create_nonparametric_one_sample_apa_table(result))
    }
  }
  
  # ---- Two-sample / paired tests -------------------------------------------
  if (result$test_type %in% c("two_sample", "paired")) {
    if (isTRUE(result$parametric)) {
      return(create_two_sample_apa_table(result))
    } else {
      return(create_nonparametric_apa_table(result))
    }
  }
  
  return(NULL)
}


#' Create APA Table (Alias)
#'
#' Alias for \code{\link{create_complete_apa_table}}.
#'
#' @param result A \code{complete_ttest} result object
#' @return A \code{flextable} object
#' @export
create_apa_table <- function(result) {
  create_complete_apa_table(result)
}


# =============================================================================
# PARAMETRIC TABLES
# =============================================================================

#' Create APA table for two-sample/paired parametric test
#'
#' @param result A complete_ttest result object
#' @return A flextable object
#' @keywords internal
#' @noRd
create_two_sample_apa_table <- function(result) {
  
  desc <- result$descriptive_stats
  raw <- result$raw
  
  # ---- Validate ------------------------------------------------------------
  if (!all(c("group", "N", "Mean", "SD") %in% names(desc))) {
    stop("Descriptive stats must have 'group', 'N', 'Mean', 'SD' columns")
  }
  
  if (nrow(desc) != 2) {
    stop("Two-sample APA table requires exactly 2 groups. Found: ", nrow(desc))
  }
  
  # ---- Extract values ------------------------------------------------------
  groups <- as.character(desc$group)
  n1 <- as.integer(desc$N[1])
  n2 <- as.integer(desc$N[2])
  m1 <- as.numeric(desc$Mean[1])
  m2 <- as.numeric(desc$Mean[2])
  sd1 <- as.numeric(desc$SD[1])
  sd2 <- as.numeric(desc$SD[2])
  
  mean_diff <- m1 - m2
  t_stat <- as.numeric(raw$statistic)
  df_val <- if (!is.null(raw$parameter)) as.numeric(raw$parameter) else NA
  p_val <- as.numeric(raw$p.value)
  p_formatted <- format_p_value(p_val)
  
  ci_text <- ""
  if (!is.null(raw$conf.int) && length(raw$conf.int) == 2) {
    ci_text <- sprintf("[%.2f, %.2f]", raw$conf.int[1], raw$conf.int[2])
  }
  
  es_value <- ""
  es_interp <- ""
  if (!is.null(result$effect_size$value) && !is.na(result$effect_size$value)) {
    es_value <- sprintf("%.2f", result$effect_size$value)
    es_interp <- result$effect_size$interpretation %||% ""
  }
  
  # ---- Build VERTICAL APA table --------------------------------------------
  apa_df <- data.frame(
    Statistic = c(
      "Sample Size (n)",
      "Mean (M)",
      "Standard Deviation (SD)",
      "Mean Difference",
      "95% Confidence Interval",
      "Test Statistic (t)",
      "Degrees of Freedom (df)",
      "p-value",
      "Effect Size (Cohen's d)",
      "Effect Size Interpretation"
    ),
    Group_1 = c(
      sprintf("%d", n1),
      sprintf("%.2f", m1),
      sprintf("%.2f", sd1),
      "", "", "", "", "", "", ""
    ),
    Group_2 = c(
      sprintf("%d", n2),
      sprintf("%.2f", m2),
      sprintf("%.2f", sd2),
      "", "", "", "", "", "", ""
    ),
    Result = c(
      "", "", "",
      sprintf("%.2f", mean_diff),
      ci_text,
      sprintf("%.2f", t_stat),
      if (is.na(df_val)) "" else sprintf("%.2f", df_val),
      p_formatted,
      es_value,
      es_interp
    ),
    stringsAsFactors = FALSE
  )
  
  # Set column names with group labels
  colnames(apa_df) <- c(
    "Statistic",
    sprintf("%s (n = %d)", groups[1], n1),
    sprintf("%s (n = %d)", groups[2], n2),
    "Result"
  )
  
  # ---- Create flextable ----------------------------------------------------
  ft <- flextable::flextable(apa_df)
  ft <- apply_apa_style(ft)
  
  # ---- Add title -----------------------------------------------------------
  title <- sprintf("Table 1. %s Results for %s",
                   result$method %||% "T-Test",
                   result$response_var %||% "")
  
  ft <- flextable::add_header_lines(ft, values = title, top = TRUE)
  ft <- flextable::fontsize(ft, size = 12, part = "header", i = 1)
  ft <- flextable::italic(ft, italic = TRUE, part = "header", i = 1)
  
  # ---- Add footnote --------------------------------------------------------
  ft <- flextable::add_footer_lines(
    ft,
    values = "Note. * p < .05, ** p < .01, *** p < .001"
  )
  ft <- flextable::fontsize(ft, size = 10, part = "footer")
  
  return(ft)
}


#' Create APA table for one-sample parametric test
#'
#' @param result A complete_ttest result object
#' @return A flextable object
#' @keywords internal
#' @noRd
create_one_sample_apa_table <- function(result) {
  
  desc <- result$descriptive_stats
  raw <- result$raw
  
  # ---- Extract values ------------------------------------------------------
  n <- as.integer(desc$N[1])
  m <- as.numeric(desc$Mean[1])
  sd_val <- as.numeric(desc$SD[1])
  se <- if ("SE" %in% names(desc)) as.numeric(desc$SE[1]) else sd_val / sqrt(n)
  
  t_stat <- as.numeric(raw$statistic)
  df_val <- as.numeric(raw$parameter)
  p_val <- as.numeric(raw$p.value)
  p_formatted <- format_p_value(p_val)
  
  es_value <- ""
  es_interp <- ""
  if (!is.null(result$effect_size$value) && !is.na(result$effect_size$value)) {
    es_value <- sprintf("%.2f", result$effect_size$value)
    es_interp <- result$effect_size$interpretation %||% ""
  }
  
  # ---- Build VERTICAL APA table --------------------------------------------
  apa_df <- data.frame(
    Statistic = c(
      "Sample Size (n)",
      "Mean (M)",
      "Standard Deviation (SD)",
      "Standard Error (SE)",
      "Hypothesized Mean",
      "Test Statistic (t)",
      "Degrees of Freedom (df)",
      "p-value",
      "Effect Size (Cohen's d)",
      "Effect Size Interpretation"
    ),
    Value = c(
      sprintf("%d", n),
      sprintf("%.2f", m),
      sprintf("%.2f", sd_val),
      sprintf("%.2f", se),
      sprintf("%.2f", result$mu %||% 0),
      sprintf("%.2f", t_stat),
      sprintf("%.2f", df_val),
      p_formatted,
      es_value,
      es_interp
    ),
    stringsAsFactors = FALSE
  )
  
  # ---- Create flextable ----------------------------------------------------
  ft <- flextable::flextable(apa_df)
  ft <- apply_apa_style(ft)
  
  # ---- Add title -----------------------------------------------------------
  title <- sprintf("Table 1. One-Sample T-Test Results for %s",
                   result$response_var %||% "")
  
  ft <- flextable::add_header_lines(ft, values = title, top = TRUE)
  ft <- flextable::fontsize(ft, size = 12, part = "header", i = 1)
  ft <- flextable::italic(ft, italic = TRUE, part = "header", i = 1)
  
  # ---- Add footnote --------------------------------------------------------
  ft <- flextable::add_footer_lines(
    ft,
    values = "Note. * p < .05, ** p < .01, *** p < .001"
  )
  ft <- flextable::fontsize(ft, size = 10, part = "footer")
  
  return(ft)
}


# =============================================================================
# NON-PARAMETRIC TABLES
# =============================================================================

#' Create APA table for two-sample/paired non-parametric test
#'
#' @param result A complete_ttest result object
#' @return A flextable object
#' @keywords internal
#' @noRd
create_nonparametric_apa_table <- function(result) {
  
  desc <- result$descriptive_stats
  raw <- result$raw
  
  # ---- Validate ------------------------------------------------------------
  if (!all(c("group", "N", "Median", "IQR") %in% names(desc))) {
    stop("Non-parametric descriptive stats must have ",
         "'group', 'N', 'Median', 'IQR' columns")
  }
  
  if (nrow(desc) != 2) {
    stop("Two-sample APA table requires exactly 2 groups. Found: ", nrow(desc))
  }
  
  # ---- Extract values ------------------------------------------------------
  groups <- as.character(desc$group)
  n1 <- as.integer(desc$N[1])
  n2 <- as.integer(desc$N[2])
  med1 <- as.numeric(desc$Median[1])
  med2 <- as.numeric(desc$Median[2])
  iqr1 <- as.numeric(desc$IQR[1])
  iqr2 <- as.numeric(desc$IQR[2])
  
  # Test statistics
  w_stat <- as.numeric(raw$statistic)
  p_val <- as.numeric(raw$p.value)
  p_formatted <- format_p_value(p_val)
  
  # Effect size
  es_value <- ""
  es_interp <- ""
  if (!is.null(result$effect_size$value) && !is.na(result$effect_size$value)) {
    es_value <- sprintf("%.2f", result$effect_size$value)
    es_interp <- result$effect_size$interpretation %||% ""
  }
  
  # ---- Build VERTICAL APA table --------------------------------------------
  apa_df <- data.frame(
    Statistic = c(
      "Sample Size (n)",
      "Median (Mdn)",
      "Interquartile Range (IQR)",
      "Test Statistic (W)",
      "p-value",
      "Effect Size (r)",
      "Effect Size Interpretation"
    ),
    Group_1 = c(
      sprintf("%d", n1),
      sprintf("%.2f", med1),
      sprintf("%.2f", iqr1),
      "", "", "", ""
    ),
    Group_2 = c(
      sprintf("%d", n2),
      sprintf("%.2f", med2),
      sprintf("%.2f", iqr2),
      "", "", "", ""
    ),
    Result = c(
      "", "", "",
      sprintf("%.2f", w_stat),
      p_formatted,
      es_value,
      es_interp
    ),
    stringsAsFactors = FALSE
  )
  
  # Set column names with group labels
  colnames(apa_df) <- c(
    "Statistic",
    sprintf("%s (n = %d)", groups[1], n1),
    sprintf("%s (n = %d)", groups[2], n2),
    "Result"
  )
  
  # ---- Create flextable ----------------------------------------------------
  ft <- flextable::flextable(apa_df)
  ft <- apply_apa_style(ft)
  
  # ---- Add title -----------------------------------------------------------
  title <- sprintf("Table 1. %s Results for %s",
                   result$method %||% "Non-Parametric Test",
                   result$response_var %||% "")
  
  ft <- flextable::add_header_lines(ft, values = title, top = TRUE)
  ft <- flextable::fontsize(ft, size = 12, part = "header", i = 1)
  ft <- flextable::italic(ft, italic = TRUE, part = "header", i = 1)
  
  # ---- Add footnote --------------------------------------------------------
  ft <- flextable::add_footer_lines(
    ft,
    values = paste0(
      "Note. * p < .05, ** p < .01, *** p < .001. ",
      "Mdn = median; IQR = interquartile range."
    )
  )
  ft <- flextable::fontsize(ft, size = 10, part = "footer")
  
  return(ft)
}


#' Create APA table for one-sample non-parametric test
#'
#' @param result A complete_ttest result object
#' @return A flextable object
#' @keywords internal
#' @noRd
create_nonparametric_one_sample_apa_table <- function(result) {
  
  desc <- result$descriptive_stats
  raw <- result$raw
  
  # ---- Extract values ------------------------------------------------------
  n <- as.integer(desc$N[1])
  med <- as.numeric(desc$Median[1])
  iqr_val <- as.numeric(desc$IQR[1])
  q1 <- if ("Q1" %in% names(desc)) as.numeric(desc$Q1[1]) else NA
  q3 <- if ("Q3" %in% names(desc)) as.numeric(desc$Q3[1]) else NA
  
  w_stat <- as.numeric(raw$statistic)
  p_val <- as.numeric(raw$p.value)
  p_formatted <- format_p_value(p_val)
  
  es_value <- ""
  es_interp <- ""
  if (!is.null(result$effect_size$value) && !is.na(result$effect_size$value)) {
    es_value <- sprintf("%.2f", result$effect_size$value)
    es_interp <- result$effect_size$interpretation %||% ""
  }
  
  # ---- Build VERTICAL APA table --------------------------------------------
  apa_df <- data.frame(
    Statistic = c(
      "Sample Size (n)",
      "Median (Mdn)",
      "Interquartile Range (IQR)",
      "Q1 (25th percentile)",
      "Q3 (75th percentile)",
      "Hypothesized Median",
      "Test Statistic (V)",
      "p-value",
      "Effect Size (r)",
      "Effect Size Interpretation"
    ),
    Value = c(
      sprintf("%d", n),
      sprintf("%.2f", med),
      sprintf("%.2f", iqr_val),
      if (is.na(q1)) "" else sprintf("%.2f", q1),
      if (is.na(q3)) "" else sprintf("%.2f", q3),
      sprintf("%.2f", result$mu %||% 0),
      sprintf("%.2f", w_stat),
      p_formatted,
      es_value,
      es_interp
    ),
    stringsAsFactors = FALSE
  )
  
  # ---- Create flextable ----------------------------------------------------
  ft <- flextable::flextable(apa_df)
  ft <- apply_apa_style(ft)
  
  # ---- Add title -----------------------------------------------------------
  title <- sprintf("Table 1. Wilcoxon Signed-Rank Test Results for %s",
                   result$response_var %||% "")
  
  ft <- flextable::add_header_lines(ft, values = title, top = TRUE)
  ft <- flextable::fontsize(ft, size = 12, part = "header", i = 1)
  ft <- flextable::italic(ft, italic = TRUE, part = "header", i = 1)
  
  # ---- Add footnote --------------------------------------------------------
  ft <- flextable::add_footer_lines(
    ft,
    values = paste0(
      "Note. * p < .05, ** p < .01, *** p < .001. ",
      "Mdn = median; IQR = interquartile range."
    )
  )
  ft <- flextable::fontsize(ft, size = 10, part = "footer")
  
  return(ft)
}


# =============================================================================
# SHARED STYLING HELPER
# =============================================================================

#' Apply APA styling to a flextable
#'
#' @param ft A flextable object
#' @return The styled flextable
#' @keywords internal
#' @noRd
apply_apa_style <- function(ft) {
  ft <- flextable::theme_box(ft)
  ft <- flextable::fontsize(ft, size = 11)
  ft <- flextable::font(ft, fontname = "Times New Roman")
  ft <- flextable::align(ft, align = "center", part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::bold(ft, part = "header")
  return(ft)
}