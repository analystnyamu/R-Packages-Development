test_that("complete_ttest handles one-sample tests correctly", {
  set.seed(123)
  data <- data.frame(value = rnorm(30, 10, 2))
  
  result <- complete_ttest(data, value ~ 1, mu = 10)
  
  expect_s3_class(result, "complete_ttest")
  expect_equal(result$test_type, "one_sample")
  expect_true(result$parametric)
})

test_that("complete_ttest handles two-sample tests correctly", {
  set.seed(123)
  data <- data.frame(
    value = c(rnorm(30, 10, 2), rnorm(30, 12, 2)),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- complete_ttest(data, value ~ group)
  
  expect_s3_class(result, "complete_ttest")
  expect_equal(result$test_type, "two_sample")
  expect_equal(nrow(result$descriptive_stats), 2)
})

test_that("complete_ttest errors with more than 2 groups", {
  expect_error(
    complete_ttest(iris, Sepal.Length ~ Species),
    "More than 2 groups"
  )
})

test_that("complete_ttest works when filtered to 2 groups", {
  iris_subset <- iris[iris$Species %in% c("setosa", "versicolor"), ]
  iris_subset$Species <- droplevels(iris_subset$Species)
  
  result <- complete_ttest(iris_subset, Sepal.Length ~ Species)
  
  expect_s3_class(result, "complete_ttest")
})

test_that("complete_ttest handles paired tests correctly", {
  set.seed(123)
  n <- 30
  paired_data_long <- data.frame(
    value = c(rnorm(n, 10, 2), rnorm(n, 11, 2)),
    time = factor(rep(c("before", "after"), each = n))
  )
  
  result <- complete_ttest(paired_data_long, value ~ time, paired = TRUE)
  
  expect_s3_class(result, "complete_ttest")
  expect_equal(result$test_type, "paired")
})

test_that("complete_ttest handles missing values", {
  set.seed(123)
  group_a <- c(rnorm(29, 10, 2), NA)
  group_b <- c(rnorm(29, 12, 2), NA)
  
  data <- data.frame(
    value = c(group_a, group_b),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- complete_ttest(data, value ~ group)
  
  expect_equal(nrow(result$data), 58)
})

test_that("complete_ttest handles invalid inputs", {
  expect_error(
    complete_ttest(iris, Sepal.Length ~ Species + Petal.Length),
    "Invalid formula"
  )
  
  expect_error(
    complete_ttest(NULL, Sepal.Length ~ Species),
    "data must be a data frame"
  )
  
  expect_error(
    complete_ttest(iris, invalid_col ~ Species),
    "not found"
  )
  
  expect_error(
    complete_ttest(iris, Sepal.Length ~ invalid_group),
    "not found"
  )
})

test_that("complete_ttest returns expected components", {
  set.seed(123)
  data <- data.frame(
    value = c(rnorm(30, 10, 2), rnorm(30, 12, 2)),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- complete_ttest(data, value ~ group)
  
  expected <- c("raw", "method", "parametric", "test_type",
                "response_var", "group_var", "assumptions",
                "descriptive_stats", "data", "alternative",
                "mu", "effect_size")
  
  for (comp in expected) {
    expect_true(comp %in% names(result))
  }
})

test_that("print method works", {
  set.seed(123)
  data <- data.frame(
    value = c(rnorm(30, 10, 2), rnorm(30, 12, 2)),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- complete_ttest(data, value ~ group)
  
  expect_output(print(result), "COMPLETE T-TEST ANALYSIS")
})

test_that("summary method works", {
  set.seed(123)
  data <- data.frame(
    value = c(rnorm(30, 10, 2), rnorm(30, 12, 2)),
    group = factor(rep(c("A", "B"), each = 30))
  )
  
  result <- complete_ttest(data, value ~ group)
  
  expect_output(summary(result), "SUMMARY")
})
