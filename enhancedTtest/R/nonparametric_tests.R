#' Perform Wilcoxon Signed-Rank Test (One-Sample)
#' @noRd
perform_wilcoxon_signed_rank <- function(data, response_var, alternative, mu) {
  
  response <- data[[response_var]]
  
  result <- list(
    raw = stats::wilcox.test(response, alternative = alternative, mu = mu),
    descriptive_stats = data.frame(
      N = length(response),
      Median = median(response, na.rm = TRUE),
      IQR = IQR(response, na.rm = TRUE),
      Q1 = quantile(response, 0.25, na.rm = TRUE),
      Q3 = quantile(response, 0.75, na.rm = TRUE)
    )
  )
  
  # Add effect size (r = Z / sqrt(N))
  z_score <- qnorm(result$raw$p.value / 2)
  result$effect_size <- abs(z_score) / sqrt(length(response))
  
  return(result)
}

#' Perform Wilcoxon Signed-Rank Test (Paired)
#' @noRd
perform_paired_wilcoxon <- function(data, response_var, group_var, alternative) {
  
  groups <- unique(data[[group_var]])
  group1 <- data[[response_var]][data[[group_var]] == groups[1]]
  group2 <- data[[response_var]][data[[group_var]] == groups[2]]
  
  result <- list(
    raw = stats::wilcox.test(group1, group2, paired = TRUE, alternative = alternative),
    descriptive_stats = data %>%
      dplyr::group_by(!!sym(group_var)) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Median = median(!!sym(response_var), na.rm = TRUE),
        IQR = IQR(!!sym(response_var), na.rm = TRUE),
        Q1 = quantile(!!sym(response_var), 0.25, na.rm = TRUE),
        Q3 = quantile(!!sym(response_var), 0.75, na.rm = TRUE),
        .groups = "drop"
      )
  )
  
  # Effect size for paired Wilcoxon
  z_score <- qnorm(result$raw$p.value / 2)
  result$effect_size <- abs(z_score) / sqrt(length(group1) + length(group2))
  
  return(result)
}

#' Perform Mann-Whitney U Test
#' @noRd
perform_mann_whitney <- function(data, response_var, group_var, alternative) {
  
  response <- data[[response_var]]
  group <- data[[group_var]]
  
  result <- list(
    raw = stats::wilcox.test(response ~ group, alternative = alternative),
    descriptive_stats = data %>%
      dplyr::group_by(!!sym(group_var)) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Median = median(!!sym(response_var), na.rm = TRUE),
        IQR = IQR(!!sym(response_var), na.rm = TRUE),
        Q1 = quantile(!!sym(response_var), 0.25, na.rm = TRUE),
        Q3 = quantile(!!sym(response_var), 0.75, na.rm = TRUE),
        .groups = "drop"
      )
  )
  
  # Effect size for Mann-Whitney
  n1 <- sum(group == unique(group)[1])
  n2 <- sum(group == unique(group)[2])
  z_score <- qnorm(result$raw$p.value / 2)
  result$effect_size <- abs(z_score) / sqrt(n1 + n2)
  
  return(result)
}
