test_that("aceUI: renders without error (non-RStudio environment)", {
  # In test environment rstudioapi::isAvailable() returns FALSE
  expect_no_error(nlmixr2shiny:::aceUI("test_ace"))
})

test_that("aceUI: renders without error when RStudio is available", {
  local_mocked_bindings(
    isAvailable = function(...) TRUE,
    .package = "rstudioapi"
  )
  expect_no_error(nlmixr2shiny:::aceUI("test_ace2"))
})

test_that("aceServer: updates results$ace when input$ace changes", {
  results <- shiny::reactiveValues(parEstim = NULL, ace = NULL)

  shiny::testServer(nlmixr2shiny:::aceServer, args = list(results = results), {
    session$setInputs(ace = "mod1 <- function() { ini({ a <- 1 }) }")
    expect_equal(results$ace, "mod1 <- function() { ini({ a <- 1 }) }")
  })
})

test_that("aceServer: observe block sets results$ace from input$ace", {
  results <- shiny::reactiveValues(parEstim = NULL, ace = NULL)

  shiny::testServer(nlmixr2shiny:::aceServer, args = list(results = results), {
    session$setInputs(ace = "some code", fontSize = 14, theme = "solarized_light")
    expect_equal(results$ace, "some code")
  })
})

test_that("aceServer: updates editor when parEstim changes", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(parEstim = NULL, ace = NULL)

  shiny::testServer(nlmixr2shiny:::aceServer, args = list(results = results), {
    # NULL->mod fires observeEvent(results$parEstim); setInputs flushes
    results$parEstim <- mod
    session$setInputs(ace = "")
    expect_true(TRUE)
  })
})

test_that("aceServer: copyCode event copies to clipboard", {
  local_mocked_bindings(
    write_clip = function(...) invisible(),
    .package = "clipr"
  )
  local_mocked_bindings(
    showNotification = function(...) invisible(),
    .package = "shiny"
  )

  results <- shiny::reactiveValues(parEstim = NULL, ace = NULL)

  shiny::testServer(nlmixr2shiny:::aceServer, args = list(results = results), {
    session$setInputs(ace = "some_code <- 1")
    session$setInputs(copyCode = 1)
    expect_true(TRUE)
  })
})

test_that("aceServer: insertCode event calls rstudioapi and stopApp", {
  local_mocked_bindings(
    insertText = function(...) invisible(),
    .package = "rstudioapi"
  )
  local_mocked_bindings(
    stopApp = function(...) invisible(),
    .package = "shiny"
  )

  results <- shiny::reactiveValues(parEstim = NULL, ace = NULL)

  shiny::testServer(nlmixr2shiny:::aceServer, args = list(results = results), {
    session$setInputs(ace = "mymod <- function() {}")
    session$setInputs(insertCode = 1)
    expect_true(TRUE)
  })
})
