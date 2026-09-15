test_that("importUI renders without error", {
  expect_no_error(nlmixr2shiny:::importUI("test_import"))
})

test_that("importServer initializes without error", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, parEstim = NULL,
    modelModified = FALSE, modelTypeSwitch = "Model Builder"
  )
  shiny::testServer(nlmixr2shiny:::importServer, args = list(results = results), {
    expect_true(TRUE)
  })
})

test_that("importServer: NONMEM import sets results$pkpdm on success (mocked)", {
  skip_if_not_installed("nonmem2rx")

  # Use the real Theopd.ctl from nonmem2rx inst
  ctl_path <- system.file("Theopd.ctl", package = "nonmem2rx")
  skip_if(!nzchar(ctl_path), "nonmem2rx Theopd.ctl not found")

  results <- shiny::reactiveValues(
    pkpdm = NULL, parEstim = NULL,
    modelModified = FALSE, modelTypeSwitch = "Model Builder"
  )

  # Build a fake shinyFiles selection structure
  fake_file <- list(
    root = "nonmem2rx",
    files = list(
      list(
        root = "nonmem2rx",
        path = list("Theopd.ctl")
      )
    )
  )

  local_mocked_bindings(
    .package = "shinyFiles",
    parseFilePaths = function(roots, selection) {
      data.frame(datapath = ctl_path, stringsAsFactors = FALSE)
    }
  )

  shiny::testServer(nlmixr2shiny:::importServer, args = list(results = results), {
    session$setInputs(importType = "NONMEM", importFile = fake_file)
    expect_false(is.null(results$pkpdm))
    expect_true(isTRUE(results$modelModified))
    expect_equal(results$modelTypeSwitch, "Current Model")
  })
})

test_that("importServer: Monolix import sets results$pkpdm on success (mocked)", {
  skip_if_not_installed("monolix2rx")

  mlx_path <- system.file("theo/theophylline_project.mlxtran", package = "monolix2rx")
  skip_if(!nzchar(mlx_path), "monolix2rx theophylline_project.mlxtran not found")

  results <- shiny::reactiveValues(
    pkpdm = NULL, parEstim = NULL,
    modelModified = FALSE, modelTypeSwitch = "Model Builder"
  )

  fake_file <- list(
    root = "monolix2rx",
    files = list(
      list(
        root = "monolix2rx",
        path = list("theo", "theophylline_project.mlxtran")
      )
    )
  )

  local_mocked_bindings(
    .package = "shinyFiles",
    parseFilePaths = function(roots, selection) {
      data.frame(datapath = mlx_path, stringsAsFactors = FALSE)
    }
  )

  shiny::testServer(nlmixr2shiny:::importServer, args = list(results = results), {
    session$setInputs(importType = "Monolix", importFile = fake_file)
    expect_false(is.null(results$pkpdm))
    expect_true(isTRUE(results$modelModified))
    expect_equal(results$modelTypeSwitch, "Current Model")
  })
})

test_that("importServer: import error leaves results unchanged", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, parEstim = NULL,
    modelModified = FALSE, modelTypeSwitch = "Model Builder"
  )

  fake_file <- list(
    root = "wd",
    files = list(list(root = "wd", path = list("bad.ctl")))
  )

  local_mocked_bindings(
    .package = "shinyFiles",
    parseFilePaths = function(roots, selection) {
      data.frame(datapath = "/nonexistent/bad.ctl", stringsAsFactors = FALSE)
    }
  )

  local_mocked_bindings(
    .package = "nonmem2rx",
    nonmem2rx = function(...) stop("parse error")
  )

  shiny::testServer(nlmixr2shiny:::importServer, args = list(results = results), {
    session$setInputs(importType = "NONMEM", importFile = fake_file)
    expect_null(results$pkpdm)
    expect_false(isTRUE(results$modelModified))
  })
})

# Integration tests using real packages (skipped if packages absent)

test_that("nonmem2rx converts Theopd.ctl to an rxUi object", {
  skip_if_not_installed("nonmem2rx")
  ctl_path <- system.file("Theopd.ctl", package = "nonmem2rx")
  skip_if(!nzchar(ctl_path), "Theopd.ctl not found in nonmem2rx inst")
  mod <- nonmem2rx::nonmem2rx(ctl_path)
  expect_s3_class(mod, "rxUi")
})

test_that("monolix2rx converts theophylline_project.mlxtran to an rxUi object", {
  skip_if_not_installed("monolix2rx")
  mlx_path <- system.file("theo/theophylline_project.mlxtran", package = "monolix2rx")
  skip_if(!nzchar(mlx_path), "theophylline_project.mlxtran not found in monolix2rx inst")
  mod <- monolix2rx::monolix2rx(mlx_path)
  expect_s3_class(mod, "rxUi")
})
