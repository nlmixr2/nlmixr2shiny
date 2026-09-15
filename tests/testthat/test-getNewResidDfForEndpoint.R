# Helper: build a minimal ri list for one endpoint
.make_ri <- function(endpoint = "cp",
                     resErrorModel = "Proportional",
                     transform = "Untransformed",
                     distribution = "Normal",
                     df = data.frame(prop = 0.2, row.names = endpoint)) {
  ri <- list()
  ri[[endpoint]] <- list(
    resErrorModel = resErrorModel,
    transform = transform,
    distribution = distribution,
    df = df
  )
  ri
}

# Helper: build a mock shiny input list
.make_input <- function(endpoint, df) {
  input <- list()
  input[[paste0("resErrorEst_", endpoint)]] <- df
  input
}

test_that("getNewResidDfForEndpoint: Proportional error returns prop column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Proportional", "Untransformed", "Normal",
                 data.frame(prop = 0.2, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)

  expect_s3_class(result, "data.frame")
  expect_true("prop" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Additive error returns add column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Untransformed", "Normal",
                 data.frame(add = 0.1, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)

  expect_s3_class(result, "data.frame")
  expect_true("add" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Combined1 error returns add and prop columns", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive + Proportional (Combined 1)", "Untransformed", "Normal",
                 data.frame(add = 0.1, prop = 0.2, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)

  expect_s3_class(result, "data.frame")
  expect_true("add" %in% names(result))
  expect_true("prop" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Combined2 error returns add and prop columns", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive + Proportional (Combined 2)", "Untransformed", "Normal",
                 data.frame(add = 0.1, prop = 0.2, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true(all(c("add", "prop") %in% names(result)))
})

test_that("getNewResidDfForEndpoint: Default combined error returns add and prop columns", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive + Proportional (Default)", "Untransformed", "Normal",
                 data.frame(add = 0.1, prop = 0.2, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true(all(c("add", "prop") %in% names(result)))
})

test_that("getNewResidDfForEndpoint: Power error returns pow and exp columns", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Power", "Untransformed", "Normal",
                 data.frame(pow = 0.1, exp = 1.0, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true(all(c("pow", "exp") %in% names(result)))
})

test_that("getNewResidDfForEndpoint: Additive + Power (Combined 1) returns add, pow, exp", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive + Power (Combined 1)", "Untransformed", "Normal",
                 data.frame(add = 0.1, pow = 0.1, exp = 1.0, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true(all(c("add", "pow", "exp") %in% names(result)))
})

test_that("getNewResidDfForEndpoint: Additive + Power (Combined 2) returns add, pow, exp", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive + Power (Combined 2)", "Untransformed", "Normal",
                 data.frame(add = 0.1, pow = 0.1, exp = 1.0, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true(all(c("add", "pow", "exp") %in% names(result)))
})

test_that("getNewResidDfForEndpoint: Additive + Power (Default) returns add, pow, exp", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive + Power (Default)", "Untransformed", "Normal",
                 data.frame(add = 0.1, pow = 0.1, exp = 1.0, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true(all(c("add", "pow", "exp") %in% names(result)))
})

test_that("getNewResidDfForEndpoint: Box-Cox transform adds lambda column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Proportional", "Box-Cox", "Normal",
                 data.frame(prop = 0.2, lambda = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)

  expect_true("prop" %in% names(result))
  expect_true("lambda" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Yeo-Johnson transform adds lambda column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Yeo-Johnson", "Normal",
                 data.frame(add = 0.1, lambda = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true("lambda" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Logit-normal + Box-Cox adds lambda column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Logit-normal + Box-Cox", "Normal",
                 data.frame(add = 0.1, lambda = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true("lambda" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Logit-normal + Yeo-Johnson adds lambda column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Logit-normal + Yeo-Johnson", "Normal",
                 data.frame(add = 0.1, lambda = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true("lambda" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Probit-normal + Box-Cox adds lambda column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Probit-normal + Box-Cox", "Normal",
                 data.frame(add = 0.1, lambda = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true("lambda" %in% names(result))
})

test_that("getNewResidDfForEndpoint: Probit-normal + Yeo-Johnson adds lambda column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Probit-normal + Yeo-Johnson", "Normal",
                 data.frame(add = 0.1, lambda = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true("lambda" %in% names(result))
})

test_that("getNewResidDfForEndpoint: t-distribution adds df column", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Additive", "Untransformed", "t-distribution",
                 data.frame(add = 0.1, df = 5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_true("df" %in% names(result))
})

test_that("getNewResidDfForEndpoint: non-Normal distribution (Poisson) uses distribution cols", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "", "Untransformed", "Poisson",
                 data.frame(lambda = 1.0, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("getNewResidDfForEndpoint: non-Normal distribution (Binomial) single endpoint", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "", "Untransformed", "Binomial",
                 data.frame(n = 1L, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_s3_class(result, "data.frame")
})

test_that("getNewResidDfForEndpoint: multi-endpoint (single=FALSE) uses endpoint-based names", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Proportional", "Untransformed", "Normal",
                 data.frame(row.names = "cp"))  # empty df -> new col created
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", FALSE)
  expect_s3_class(result, "data.frame")
  expect_true("prop" %in% names(result))
})

test_that("getNewResidDfForEndpoint: column reused from existing df when present", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  # df already has 'add' column -> it should be reused as-is
  ri <- .make_ri("cp", "Additive", "Untransformed", "Normal",
                 data.frame(add = 0.5, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_equal(result$add, 0.5)
})

test_that("getNewResidDfForEndpoint: rownames set to endpoint name", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )

  ri <- .make_ri("cp", "Proportional", "Untransformed", "Normal",
                 data.frame(prop = 0.2, row.names = "cp"))
  input <- .make_input("cp", ri$cp$df)

  result <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "cp", TRUE)
  expect_equal(rownames(result), "cp")
})

test_that(".residCurrentDf falls back to the stored names when the table is not rendered", {
  # The `resErrorEst_` handsontable is rendered after the pickers are created,
  # so the picker observers run at least once while `input$resErrorEst_<x>` is
  # still NULL.  When that happened the model's parameter names were thrown
  # away and replaced by freshly generated ones, which both lost the names and
  # left the residual tables perpetually recalculating.
  ri <- .make_ri("Cc", "Proportional", "Untransformed", "Normal",
                 data.frame(prop = "propSd", row.names = "Cc"))

  expect_equal(nlmixr2shiny:::.residCurrentDf(ri, list(), "Cc"),
               data.frame(prop = "propSd", row.names = "Cc"))

  # multi-endpoint models are the ones that used to break, so check that the
  # regenerated names are not substituted there either
  res <- nlmixr2shiny:::getNewResidDfForEndpoint(ri, list(), "Cc", FALSE)
  expect_equal(res$prop, "propSd")
})

test_that(".residCurrentDf prefers the rendered table over the stored names", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) x,
    .package = "rhandsontable"
  )
  ri <- .make_ri("Cc", "Proportional", "Untransformed", "Normal",
                 data.frame(prop = "propSd", row.names = "Cc"))
  input <- .make_input("Cc", data.frame(prop = "myPropSd", row.names = "Cc"))

  expect_equal(nlmixr2shiny:::.residCurrentDf(ri, input, "Cc")$prop, "myPropSd")
  expect_equal(nlmixr2shiny:::getNewResidDfForEndpoint(ri, input, "Cc", FALSE)$prop,
               "myPropSd")
})

test_that(".residCurrentDf ignores an unreadable table", {
  local_mocked_bindings(
    hot_to_r = function(x, ...) stop("not a handsontable"),
    .package = "rhandsontable"
  )
  ri <- .make_ri("Cc", "Proportional", "Untransformed", "Normal",
                 data.frame(prop = "propSd", row.names = "Cc"))
  input <- .make_input("Cc", "garbage")

  expect_equal(nlmixr2shiny:::.residCurrentDf(ri, input, "Cc")$prop, "propSd")
})
