#' Perform One-Sample T-Test
#' @noRd
perform_one_sample_ttest <- function(data, response_var, alternative, mu) {
  
  response <- data[[response_var]]
  
  result <- list(
    raw = stats::t.test(response, alternative = alternative, mu = mu),
    descriptive_stats = data.frame(
      N = length(response),
      Mean = mean(response, na.rm = TRUE),
      SD = sd(response, na.rm = TRUE),
      SE = sd(response, na.rm = TRUE) / sqrt(length(response))
    )
  )
  
  return(result)
}

#' Perform Two-Sample T-Test
#' @noRd
perform_two_sample_ttest <- function(data, response_var, group_var, 
                                     alternative, var_equal) {
  
  response <- data[[response_var]]
  group <- data[[group_var]]
  
  result <- list(
    raw = stats::t.test(response ~ group, 
                        alternative = alternative, 
                        var.equal = var_equal),
    descriptive_stats = data %>%
      dplyr::group_by(!!sym(group_var)) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Mean = mean(!!sym(response_var), na.rm = TRUE),
        SD = sd(!!sym(response_var), na.rm = TRUE),
        SE = sd(!!sym(response_var), na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
  )
  
  return(result)
}

#' Perform Paired T-Test
#' @noRd
perform_paired_ttest <- function(data, response_var, group_var, alternative) {
  
  # For paired test, need to reshape data
  # Assuming data has two groups in long format
  response <- data[[response_var]]
  group <- data[[group_var]]
  
  # Get paired differences
  groups <- unique(group)
  group1 <- response[group == groups[1]]
  group2 <- response[group == groups[2]]
  
  # Ensure equal lengths for pairing
  if(length(group1) != length(group2)) {
    stop("Paired test requires equal number of observations in each group")
  }
  
  result <- list(
    raw = stats::t.test(group1, group2, paired = TRUE, alternative = alternative),
    descriptive_stats = data %>%
      dplyr::group_by(!!sym(group_var)) %>%
      dplyr::summarise(
        N = dplyr::n(),
        Mean = mean(!!sym(response_var), na.rm = TRUE),
        SD = sd(!!sym(response_var), na.rm = TRUE),
        SE = sd(!!sym(response_var), na.rm = TRUE) / sqrt(dplyr::n()),
        .groups = "drop"
      )
  )
  
  return(result)
}
