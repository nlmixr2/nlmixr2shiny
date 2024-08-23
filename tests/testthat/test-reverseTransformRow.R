test_that("Untransformed", {
  row <- data.frame(
    Trans. = "Untransformed",
    Trans.Lower = NA,
    Trans.Upper = NA,
    lower = 1,
    est = 2,
    upper = 3,
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformRow(row)
  expect_equal(result, c(1, 2, 3))
})


test_that("Lognormal Transformation", {
  row <- data.frame(
    Trans. = "lognormal",
    Trans.Lower = NA,
    Trans.Upper = NA,
    lower = 1,
    est = 2,
    upper = 3,
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformRow(row)
  expect_equal(result, c(log(1), log(2), log(3)))
})


test_that("LogitNormal Transformation", {
  row <- data.frame(
    Trans. = "logitNormal",
    Trans.Lower = 0,
    Trans.Upper = 1,
    lower = 0.1,
    est = 0.5,
    upper = 0.9,
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformRow(row)
  expect_equal(result, c(logit(0.1, 0, 1), logit(0.5, 0, 1), logit(0.9, 0, 1)))
})


test_that("ProbitNormal Transformation", {
  row <- data.frame(
    Trans. = "probitNormal",
    Trans.Lower = 0,
    Trans.Upper = 1,
    lower = 0.1,
    est = 0.5,
    upper = 0.9,
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformRow(row)
  expect_equal(result, c(probit(0.1, 0, 1), probit(0.5, 0, 1), probit(0.9, 0, 1)))
})


test_that("Unknown Transformation", {
  row <- data.frame(
    Trans. = "unknown",
    Trans.Lower = NA,
    Trans.Upper = NA,
    lower = 1,
    est = 2,
    upper = 3,
    stringsAsFactors = FALSE
  )
  
  expect_error(reverseTransformRow(row), "Unknown transformation: unknown")
})



test_that("LogitNormal Boundary Conditions", {
  row <- data.frame(
    Trans. = "logitNormal",
    Trans.Lower = 0,
    Trans.Upper = 1,
    lower = -0.1,
    est = 0.5,
    upper = 1.1,
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformRow(row)
  expect_equal(result, c(-Inf, logit(0.5, 0, 1), Inf))
})


test_that("ProbitNormal Boundary Conditions", {
  row <- data.frame(
    Trans. = "probitNormal",
    Trans.Lower = 0,
    Trans.Upper = 1,
    lower = -0.1,
    est = 0.5,
    upper = 1.1,
    stringsAsFactors = FALSE
  )
  
  result <- reverseTransformRow(row)
  expect_equal(result, c(-Inf, probit(0.5, 0, 1), Inf))
})


