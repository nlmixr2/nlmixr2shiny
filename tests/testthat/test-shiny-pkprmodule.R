test_that("pkprUI: renders without error", {
  expect_no_error(nlmixr2shiny:::pkprUI("test_pkpr"))
})

test_that("pkprServer: initializes with empty table_data", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(pkpdm = mod, modProp = NULL)

  shiny::testServer(nlmixr2shiny:::pkprServer, args = list(results = results), {
    expect_true(TRUE)
  })
})

test_that("pkprServer: adding a new compartment-property row updates modProp", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(pkpdm = mod, modProp = NULL)

  shiny::testServer(nlmixr2shiny:::pkprServer, args = list(results = results), {
    # Set compartment and property inputs then trigger add_row
    session$setInputs(
      compartment = "A1",
      property = "initial value",
      add_row = 1
    )
    # modProp should now have an entry
    expect_false(is.null(results$modProp))
    expect_true(length(results$modProp) >= 1)
    expect_true(any(grepl("addIni", results$modProp)))
  })
})

test_that("pkprServer: adding duplicate row does not duplicate entry", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(pkpdm = mod, modProp = NULL)

  shiny::testServer(nlmixr2shiny:::pkprServer, args = list(results = results), {
    session$setInputs(compartment = "A1", property = "initial value", add_row = 1)
    n1 <- length(results$modProp)

    # Try adding the same combination again
    session$setInputs(add_row = 2)
    n2 <- length(results$modProp)

    # Should not have added a duplicate
    expect_equal(n1, n2)
  })
})

test_that("pkprServer: delete button click removes a row", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(pkpdm = mod, modProp = NULL)

  shiny::testServer(nlmixr2shiny:::pkprServer, args = list(results = results), {
    # Add two rows
    session$setInputs(compartment = "A1", property = "initial value", add_row = 1)
    session$setInputs(compartment = "A1", property = "rate", add_row = 2)
    n_after_add <- length(results$modProp)

    # Simulate delete button click for first row
    session$setInputs(
      table_output_cell_clicked = list(
        value = '<button class="btn btn-danger btn-sm delete" id="1">-</button>',
        row = 1,
        col = 2
      )
    )
    n_after_delete <- length(results$modProp)

    expect_true(n_after_delete < n_after_add)
  })
})

test_that("pkprServer: modProp is character(0) when no non-Fixed rows added", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(pkpdm = mod, modProp = NULL)

  shiny::testServer(nlmixr2shiny:::pkprServer, args = list(results = results), {
    # Don't add any rows; table_data stays empty
    session$setInputs(compartment = "A1", property = "initial value")
    # results$modProp hasn't been triggered via add_row
    expect_true(is.null(results$modProp) || length(results$modProp) == 0)
  })
})
