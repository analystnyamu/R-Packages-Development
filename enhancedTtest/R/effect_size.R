#' Calculate Effect Size for Any Test
#'
#' @param result Test result object
#'
#' @return Effect size with interpretation
calculate_effect_size <- function(result) {
  
  # Extract method
  method <- result$method
  
  # Calculate appropriate effect size
  if(grepl("t-test|Welch", method)) {
    # Cohen's d for t-tests
    if(result$test_type == "one_sample") {
      d <- calculate_cohens_d_one_sample(result)
    } else {
      d <- calculate_cohens_d_two_sample(result)
    }
    return(list(
      value = d,
      name = "Cohen's d",
      interpretation = interpret_cohens_d(d)
    ))
    
  } else if(grepl("Wilcoxon", method)) {
    # r for Wilcoxon tests
    r <- abs(qnorm(result$raw$p.value / 2)) / 
      sqrt(sum(result$descriptive_stats$N))
    return(list(
      value = r,
      name = "r (effect size)",
      interpretation = interpret_r(r)
    ))
    
  } else if(grepl("Mann-Whitney", method)) {
    # r for Mann-Whitney
    n_total <- sum(result$descriptive_stats$N)
    r <- abs(qnorm(result$raw$p.value / 2)) / sqrt(n_total)
    return(list(
      value = r,
      name = "r (effect size)",
      interpretation = interpret_r(r)
    ))
  }
  
  return(NULL)
}

#' Calculate Cohen's d for One-Sample T-Test
#' @noRd
calculate_cohens_d_one_sample <- function(result) {
  m <- result$descriptive_stats$Mean
  mu <- result$raw$null.value
  sd <- result$descriptive_stats$SD
  return(abs(m - mu) / sd)
}

#' Calculate Cohen's d for Two-Sample T-Test
#' @noRd
calculate_cohens_d_two_sample <- function(result) {
  stats <- result$descriptive_stats
  n1 <- stats$N[1]
  n2 <- stats$N[2]
  m1 <- stats$Mean[1]
  m2 <- stats$Mean[2]
  sd1 <- stats$SD[1]
  sd2 <- stats$SD[2]
  
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  return(abs(m1 - m2) / pooled_sd)
}


