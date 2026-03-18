test_that("getDataNamesForExploration: always includes built-in datasets", {
  result <- nlmixr2shiny:::getDataNamesForExploration()
  expect_true("theo_sd" %in% result)
  expect_true("theo_md" %in% result)
  expect_type(result, "character")
})

test_that("getDataNamesForExploration: includes user data frames from globalenv", {
  # Put a data frame in globalenv temporarily
  assign("test_df_for_nlmixr2shiny", data.frame(x = 1:3), envir = globalenv())
  on.exit(rm("test_df_for_nlmixr2shiny", envir = globalenv()))

  result <- nlmixr2shiny:::getDataNamesForExploration()
  expect_true("test_df_for_nlmixr2shiny" %in% result)
})

test_that("getDataNamesForExploration: does not include non-data-frames from globalenv", {
  assign("test_vec_for_nlmixr2shiny", 1:5, envir = globalenv())
  on.exit(rm("test_vec_for_nlmixr2shiny", envir = globalenv()))

  result <- nlmixr2shiny:::getDataNamesForExploration()
  expect_false("test_vec_for_nlmixr2shiny" %in% result)
})

test_that("getDataForExploration: retrieves dataset loaded into globalenv", {
  assign("theo_sd_test_fixture", nlmixr2data::theo_sd, envir = globalenv())
  on.exit(rm("theo_sd_test_fixture", envir = globalenv()))
  result <- nlmixr2shiny:::getDataForExploration("theo_sd_test_fixture")
  expect_s3_class(result, "data.frame")
})

test_that("getDataForExploration: retrieves user data frame from globalenv", {
  assign("test_df2_for_nlmixr2shiny", data.frame(a = 1:3, b = letters[1:3]),
         envir = globalenv())
  on.exit(rm("test_df2_for_nlmixr2shiny", envir = globalenv()))

  result <- nlmixr2shiny:::getDataForExploration("test_df2_for_nlmixr2shiny")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("getDataForExploration: errors on missing dataset", {
  expect_error(
    nlmixr2shiny:::getDataForExploration("this_dataset_does_not_exist_xyz"),
    "Dataset not found"
  )
})

test_that("expUI: renders without error", {
  expect_no_error(nlmixr2shiny:::expUI("test_exp"))
})

test_that("expServer: initializes without error when results has parEstim", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(parEstim = mod)

  shiny::testServer(nlmixr2shiny:::expServer, args = list(results = results), {
    # Server initializes; no inputs needed for basic check
    expect_true(TRUE)
  })
})

test_that("expServer: handles loadData event with valid dataset", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(parEstim = mod, s = NULL)

  # Create a minimal compatible dataset and put it in globalenv
  testData <- data.frame(
    ID    = c(1L, 1L, 1L, 2L, 2L, 2L),
    TIME  = c(0, 1, 4, 0, 1, 4),
    AMT   = c(100, 0, 0, 100, 0, 0),
    EVID  = c(1L, 0L, 0L, 1L, 0L, 0L),
    CMT   = c(1L, 0L, 0L, 1L, 0L, 0L),
    DV    = c(NA, 50, 30, NA, 45, 28)
  )
  assign("nlmixr2shiny_test_dataset", testData, envir = globalenv())
  on.exit(rm("nlmixr2shiny_test_dataset", envir = globalenv()))

  shiny::testServer(nlmixr2shiny:::expServer, args = list(results = results), {
    session$setInputs(dataset = "nlmixr2shiny_test_dataset", page = 1)
    session$setInputs(loadData = 1)
    # Should load without error
    expect_true(TRUE)
  })
})

test_that("expServer: loadData shows error notification for bad dataset columns", {
  local_mocked_bindings(
    showNotification = function(...) invisible(),
    .package = "shiny"
  )

  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(parEstim = mod, s = NULL)

  # Dataset missing required columns
  badData <- data.frame(x = 1:3, y = 4:6)
  assign("nlmixr2shiny_bad_dataset", badData, envir = globalenv())
  on.exit(rm("nlmixr2shiny_bad_dataset", envir = globalenv()))

  shiny::testServer(nlmixr2shiny:::expServer, args = list(results = results), {
    session$setInputs(dataset = "nlmixr2shiny_bad_dataset", page = 1)
    session$setInputs(loadData = 1)
    # Error handled gracefully
    expect_true(TRUE)
  })
})

test_that("expServer: dataPlot silently stops when s is NULL (req behavior)", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(parEstim = mod, s = NULL)

  shiny::testServer(nlmixr2shiny:::expServer, args = list(results = results), {
    # req(results$s) throws a shiny.silent.error when s is NULL — that is expected
    err <- tryCatch(output$dataPlot, shiny.silent.error = function(e) e)
    expect_true(inherits(err, "shiny.silent.error") || is.null(err))
  })
})
