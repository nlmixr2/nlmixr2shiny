test_that("Missing Required Columns", {
  df <- data.frame(
    Trans. = c("exp", "expit"),
    Trans.Lower = c(NA, 0),
    lower = c(1, 0.1),
    est = c(2, 0.5),
    upper = c(3, 0.9),
    stringsAsFactors = FALSE
  )
  
  expect_error(transformDF(df), "Column Trans.Upper is missing from the input data frame.")
})


test_that("Valid Input with All Required Columns", {
  df <- data.frame(
    Trans. = c("exp", "expit", "probitInv"),
    Trans.Lower = c(NA, 0, 0),
    Trans.Upper = c(NA, 1, 1),
    lower = c(1, 0.1, 0.1),
    est = c(2, 0.5, 0.5),
    upper = c(3, 0.9, 0.9),
    stringsAsFactors = FALSE
  )
  
  result <- transformDF(df)
  
  expected_result <- df
  expected_result[1, c("lower", "est", "upper")] <- c(exp(1), exp(2), exp(3))
  expected_result[2, c("lower", "est", "upper")] <- c(expit(0.1, 0, 1), expit(0.5, 0, 1), expit(0.9, 0, 1))
  expected_result[3, c("lower", "est", "upper")] <- c(probitInv(0.1, 0, 1), probitInv(0.5, 0, 1), probitInv(0.9, 0, 1))
  
  expect_equal(result$lower, expected_result$lower, tolerance = 1e-8)
  expect_equal(result$est, expected_result$est, tolerance = 1e-8)
  expect_equal(result$upper, expected_result$upper, tolerance = 1e-8)
})


test_that("Different Types of Transformations", {
  df <- data.frame(
    Trans. = c("exp", "expit", "probitInv"),
    Trans.Lower = c(NA, 0, 0),
    Trans.Upper = c(NA, 1, 1),
    lower = c(1, 0.1, 0.1),
    est = c(2, 0.5, 0.5),
    upper = c(3, 0.9, 0.9),
    stringsAsFactors = FALSE
  )
  
  result <- transformDF(df)
  
  expected_result <- df
  expected_result[1, c("lower", "est", "upper")] <- c(exp(1), exp(2), exp(3))
  expected_result[2, c("lower", "est", "upper")] <- c(expit(0.1, 0, 1), expit(0.5, 0, 1), expit(0.9, 0, 1))
  expected_result[3, c("lower", "est", "upper")] <- c(probitInv(0.1, 0, 1), probitInv(0.5, 0, 1), probitInv(0.9, 0, 1))
  
  expect_equal(result$lower, expected_result$lower, tolerance = 1e-8)
  expect_equal(result$est, expected_result$est, tolerance = 1e-8)
  expect_equal(result$upper, expected_result$upper, tolerance = 1e-8)
})


