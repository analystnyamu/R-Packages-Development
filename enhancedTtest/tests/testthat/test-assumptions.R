test_that("check_normality works correctly", {
  # Normal data
  set.seed(123)
  normal_data <- rnorm(100)
  result <- check_normality(normal_data)
  expect_true(result$passed)
  
  # Non-normal data
  non_normal <- rnorm(100)^2
  result <- check_normality(non_normal)
  expect_false(result$passed)
})

test_that("check_homogeneity works correctly", {
  set.seed(123)
  data <- data.frame(
    value = c(rnorm(30, 10, 2), rnorm(30, 12, 2)),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- check_homogeneity(data, "value", "group")
  expect_true(result$passed)
  
  # Heterogeneous variances
  data <- data.frame(
    value = c(rnorm(30, 10, 1), rnorm(30, 12, 4)),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- check_homogeneity(data, "value", "group")
  expect_false(result$passed)
})
