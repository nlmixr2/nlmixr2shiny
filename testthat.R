test_that("transformDF handles NA transformations correctly", {
  df <- data.frame(
    name = "no_transform",
    Trans. = NA,
    Trans.Lower = NA,
    Trans.Upper = NA,
    lower = 0,
    est = 1,
    upper = 2
  )
  result <- transformDF(df)
  expect_equal(result$lower, 0)
  expect_equal(result$est, 1)
  expect_equal(result$upper, 2)
})

test_that("transformDF applies exponential transformation correctly", {
  df <- data.frame(
    name = "exp_transform",
    Trans. = "exp",
    Trans.Lower = NA,
    Trans.Upper = NA,
    lower = 0,
    est = 1,
    upper = 2
  )
  result <- transformDF(df)
  expect_equal(result$lower, exp(0))
  expect_equal(result$est, exp(1))
  expect_equal(result$upper, exp(2))
})

test_that("transform_df applies expit transformation correctly", {
  df <- data.frame(
    name = "lvc",
    Trans. = "expit",
    Trans.Lower = 0.1,
    Trans.Upper = 20,
    lower = -Inf,
    est = 0,
    upper = Inf
  )
  result <- transformDF(df)
  expect_equal(result$lower, NaN)
  expect_equal(result$est, 10.05) 
  expect_equal(result$upper, NaN)
})

test_that("transform_df applies probitInv transformation correctly", {
  df <- data.frame(
    name = "probitInv_transform",
    Trans. = "probitInv",
    Trans.Lower = 0.1,
    Trans.Upper = 5,
    lower = -Inf,
    est = 0,
    upper = Inf
  )
  result <- transformDF(df)
  expect_equal(result$lower, rxode2::probitInv(-Inf,0.1,5))
  expect_equal(result$est, rxode2::probitInv(0,0.1,5)) 
  expect_equal(result$upper, rxode2::probitInv(Inf,0.1,5))
})

test_that("transformDF returns thesame table for unknown transformation", {
  df <- data.frame(
    name = "lvc",
    Trans. = "",
    Trans.Lower = NA,
    Trans.Upper = NA,
    lower = 0,
    est = 1,
    upper = 2
  )
  expect_equal(transformDF(df), df)
})



