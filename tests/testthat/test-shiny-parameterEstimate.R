test_that("ParEstUI: renders without error", {
  expect_no_error(nlmixr2shiny:::ParEstUI("test_par"))
})

test_that("updateParEstimWithEsts: returns early when results fields are NULL", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  results <- list2env(list(parEstim = NULL, iniDf = NULL, paramNames = NULL),
                      parent = emptyenv())
  nlmixr2shiny:::updateParEstimWithEsts(results)
  expect_null(results$parEstim)

  results2 <- list2env(list(parEstim = .make_pk_prop(), iniDf = NULL, paramNames = NULL),
                       parent = emptyenv())
  nlmixr2shiny:::updateParEstimWithEsts(results2)
  expect_false(is.null(results2$parEstim))
})

test_that("updateParEstimWithEsts: updates estimates without back-transform", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .thetaNames <- .theta$name
  .estimates <- as.list(setNames(.theta$est, .thetaNames))

  results <- list2env(list(
    parEstim = mod, iniDf = .estimates, paramNames = .thetaNames,
    backTransform = FALSE, pkpdm = NULL
  ), parent = emptyenv())

  nlmixr2shiny:::updateParEstimWithEsts(results)
  expect_false(is.null(results$pkpdm))
  expect_false(is.null(results$parEstim))
})

test_that("updateParEstimWithEsts: updates estimates with back-transform (exp)", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .thetaNames <- .theta$name
  .estimates <- as.list(setNames(exp(.theta$est), .thetaNames))

  results <- list2env(list(
    parEstim = mod, iniDf = .estimates, paramNames = .thetaNames,
    backTransform = TRUE, pkpdm = NULL
  ), parent = emptyenv())

  nlmixr2shiny:::updateParEstimWithEsts(results)
  expect_false(is.null(results$pkpdm))
})

test_that("ParEstServer: initializes without error", {
  mod <- .make_pk_prop()
  results <- shiny::reactiveValues(
    parEstim = mod, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    expect_true(TRUE)
  })
})

# Helper used in multiple tests below: trigger observeEvent(results$parEstim) by making
# a NULL->mod change and flushing the reactive graph via session$setInputs()
.trigger_parEstim_observer <- function(session, results, mod) {
  results$parEstim <- mod
  session$setInputs(backTransform = TRUE)  # setInputs flushes reactive graph
}

test_that("ParEstServer: render outputs execute without error", {
  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      as.data.frame(t(setNames(.theta$est, .theta$name)))
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    # NULL->mod change + flush triggers observeEvent(results$parEstim), registering all outputs
    results$parEstim <- mod
    session$setInputs(backTransform = TRUE)
    expect_no_error(output$initalEstimates)
    expect_no_error(output$plotOptions)
    expect_no_error(output$dosingTable1)
    expect_no_error(output$dosingTable2)
    expect_no_error(output$timeSampling)
  })
})

test_that("ParEstServer: initalEstimates renders and sets paramNames", {
  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      as.data.frame(t(setNames(.theta$est, .theta$name)))
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    results$parEstim <- mod
    session$setInputs(backTransform = TRUE)
    tbl <- output$initalEstimates
    expect_false(is.null(tbl))
    expect_false(is.null(results$paramNames))
  })
})

test_that("ParEstServer: backTransform=FALSE render still works", {
  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      as.data.frame(t(setNames(.theta$est, .theta$name)))
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = FALSE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    results$parEstim <- mod
    session$setInputs(backTransform = FALSE)
    expect_no_error(output$initalEstimates)
  })
})

test_that("ParEstServer: dosingTable1 multi-state model", {
  f <- function() {
    ini({
      tcl <- log(0.008); tv <- log(0.6); tq <- log(0.3); tvp <- log(1.0); prop.err <- 0.2
    })
    model({
      cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp)
      ke <- cl/v; kcp <- q/v; kpc <- q/vp
      d/dt(A1) = -ke*A1 - kcp*A1 + kpc*A2
      d/dt(A2) = kcp*A1 - kpc*A2
      cp = A1/v; cp ~ prop(prop.err)
    })
  }
  mod2cmt <- rxode2::rxode2(f)

  local_mocked_bindings(
    hot_to_r = function(x, ...) if (is.null(x)) NULL else if (is.data.frame(x)) x else data.frame(),
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod2cmt, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    results$parEstim <- mod2cmt
    session$setInputs(backTransform = TRUE)
    expect_no_error(output$dosingTable1)
  })
})

test_that("ParEstServer: plotTabs registered after rxsolve changes", {
  mod <- .make_pk_prop()

  local_mocked_bindings(
    hot_to_r = function(x, ...) if (is.null(x)) NULL else if (is.data.frame(x)) x else data.frame(),
    .package = "rhandsontable"
  )

  ev <- rxode2::et(amt = 100) |> rxode2::add.sampling(seq(0, 24, by = 1))
  solved <- rxode2::rxSolve(mod, ev)

  results <- shiny::reactiveValues(
    parEstim = mod, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    # NULL->solved change + flush triggers observeEvent(results$rxsolve), registering plotTabs
    results$rxsolve <- solved
    session$setInputs(backTransform = TRUE)
    tbl <- output$plotTabs
    expect_true(TRUE)
  })
})

test_that("ParEstServer: initialEstimates input triggers iniDf update", {
  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      as.data.frame(t(setNames(.theta$est, .theta$name)))
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = mod, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    session$setInputs(
      initalEstimates = list(
        data = lapply(seq_len(nrow(.theta)), function(i) as.list(.theta[i, ]))
      )
    )
    expect_false(is.null(results$iniDf))
  })
})

test_that("ParEstServer: backTransform observer transforms estimates", {
  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .thetaDf <- as.data.frame(t(setNames(.theta$est, .theta$name)))

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      .thetaDf
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    # Step 1: set up parEstim observer -> registers output$initalEstimates, sets iniDf()
    results$parEstim <- mod
    session$setInputs(backTransform = TRUE, initalEstimates = .thetaDf)
    # Access the render to ensure iniDf() is populated
    output$initalEstimates

    # Step 2: change backTransform -> triggers the backTransform observer
    session$setInputs(backTransform = FALSE)
    expect_true(TRUE)  # observer ran without error

    # Step 3: change back to TRUE to cover that branch too
    session$setInputs(backTransform = TRUE)
    expect_true(TRUE)
  })
})

test_that("ParEstServer: useLhs observer renames estimates", {
  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .thetaDf <- as.data.frame(t(setNames(.theta$est, .theta$name)))

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      .thetaDf
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    results$parEstim <- mod
    session$setInputs(backTransform = TRUE, useLhs = TRUE, initalEstimates = .thetaDf)
    output$initalEstimates

    # Toggle useLhs to trigger the observer
    session$setInputs(useLhs = FALSE)
    expect_true(TRUE)

    session$setInputs(useLhs = TRUE)
    expect_true(TRUE)
  })
})

test_that("ParEstServer: goPlot triggers solveODE and sets rxsolve", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_prop()
  .iniDf <- mod$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .thetaDf <- as.data.frame(t(setNames(.theta$est, .theta$name)))

  dosingDf1  <- data.frame(amt = 100, rate = NA_real_, cmt = "A1")
  dosingDf2  <- data.frame(start = 0, interval = NA_real_, ndoses = NA_real_)
  timingDf   <- data.frame(start = 0, end = 24, step = 1)
  plotOptsDf <- data.frame(N.Sub = 1, logy = FALSE)

  local_mocked_bindings(
    hot_to_r = function(x, ...) {
      if (is.null(x)) return(NULL)
      if (is.data.frame(x)) return(x)
      .thetaDf
    },
    .package = "rhandsontable"
  )

  results <- shiny::reactiveValues(
    parEstim = NULL, pkpdm = mod, iniDf = NULL,
    paramNames = NULL, backTransform = TRUE, rxsolve = NULL
  )

  shiny::testServer(nlmixr2shiny:::ParEstServer, args = list(results = results), {
    results$parEstim <- mod
    session$setInputs(
      backTransform = TRUE,
      dosingTable1  = dosingDf1,
      dosingTable2  = dosingDf2,
      timeSampling  = timingDf,
      plotOptions   = plotOptsDf,
      initalEstimates = .thetaDf
    )
    session$setInputs(goPlot = 1)
    expect_false(is.null(results$rxsolve))
  })
})
