test_that("Basic Functionality", {
  df <- data.frame(
    Trans. = c("Untransformed", "lognormal"),
    Trans.Lower = c(NA, NA),
    Trans.Upper = c(NA, NA),
    lower = c(1, 2),
    est = c(2, 3),
    upper = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformDF(df)
  
  expected_result <- df
  expected_result[1, c("lower", "est", "upper")] <- c(1, 2, 3)
  expected_result[2, c("lower", "est", "upper")] <- c(log(2), log(3), log(4))
  
  expect_equal(result, expected_result)
})


test_that("Missing Required Columns", {
  df <- data.frame(
    Trans. = c("Untransformed", "lognormal"),
    Trans.Lower = c(NA, NA),
    lower = c(1, 2),
    est = c(2, 3),
    upper = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  expect_error(reverseTransformDF(df), "Column Trans.Upper is missing from the input data frame.")
})


test_that("Mixed Transformations", {
  df <- data.frame(
    Trans. = c("Untransformed", "lognormal", "logitNormal"),
    Trans.Lower = c(NA, NA, 0),
    Trans.Upper = c(NA, NA, 1),
    lower = c(1, 2, 0.1),
    est = c(2, 3, 0.5),
    upper = c(3, 4, 0.9),
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformDF(df)
  
  expected_result <- df
  expected_result[1, c("lower", "est", "upper")] <- c(1, 2, 3)
  expected_result[2, c("lower", "est", "upper")] <- c(log(2), log(3), log(4))
  expected_result[3, c("lower", "est", "upper")] <- c(rxode2::logit(0.1, 0, 1), rxode2::logit(0.5, 0, 1), rxode2::logit(0.9, 0, 1))
  
  expect_equal(result, expected_result)
})


test_that("Boundary Conditions for LogitNormal", {
  df <- data.frame(
    Trans. = c("logitNormal"),
    Trans.Lower = c(0),
    Trans.Upper = c(1),
    lower = c(-0.1),
    est = c(0.5),
    upper = c(1.1),
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformDF(df)
  
  expected_result <- df
  expected_result[1, c("lower", "est", "upper")] <- c(-Inf, rxode2::logit(0.5, 0, 1), Inf)
  
  expect_equal(result, expected_result)
})


test_that("Boundary Conditions for ProbitNormal", {
  df <- data.frame(
    Trans. = c("probitNormal"),
    Trans.Lower = c(0),
    Trans.Upper = c(1),
    lower = c(-0.1),
    est = c(0.5),
    upper = c(1.1),
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformDF(df)
  
  expected_result <- df
  expected_result[1, c("lower", "est", "upper")] <- c(-Inf, rxode2::probit(0.5, 0, 1), Inf)
  
  expect_equal(result, expected_result)
})
