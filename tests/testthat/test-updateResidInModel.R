# Helper to create a minimal rinfo-like structure for updateResidInModel
.make_rinfo <- function(endpoint, resErrorModel, transform, distribution, df) {
  ret <- list()
  ret[[endpoint]] <- list(
    df = df,
    resErrorModel = resErrorModel,
    transform = transform,
    distribution = distribution
  )
  ret[["_modelPars"]] <- character(0)
  ret
}

test_that("updateResidInModel: returns early when rinfo is NULL", {
  results <- list2env(list(
    rinfo = NULL,
    parEstim = .make_pk_add()
  ), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  # Should not have crashed and rinfo stays NULL
  expect_null(results$rinfo)
})

test_that("updateResidInModel: additive error, Normal, Untransformed", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_add()
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
  expect_false(is.null(results$parEstim))
  expect_false(is.null(results$pkpdm))
})

test_that("updateResidInModel: proportional error, Normal, Untransformed", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_prop()
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: additive + proportional error", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_addprop()
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: Box-Cox transform", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_boxcox()
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: Yeo-Johnson transform", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      add.err <- 0.1
      prop.err <- 0.2
      lambda <- 0.5
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(prop.err) + prop(add.err) + yeoJohnson(lambda)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: t-distribution", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      add.err <- 0.1
      df.err <- 5
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(add.err) + dt(df.err)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  # dt() is not a valid residual distribution; updateResidInModel catches the
  # parse error internally and emits a warning before clearing rinfo
  suppressWarnings(nlmixr2shiny:::updateResidInModel(results))

  expect_null(results$rinfo)
})

test_that("updateResidInModel: Cauchy distribution", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      add.err <- 0.1
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(add.err) + dcauchy()
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: Log-normal transform", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      add.err <- 0.1
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ lnorm(add.err)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: Poisson (non-normal) distribution", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ dpois(cp)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: clears rinfo after successful update", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_add()
  ri <- residInfo(mod)
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
  expect_false(is.null(results$pkpdm))
})

test_that("updateResidInModel: manually constructed additive rinfo updates model", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_add()
  # Manually construct rinfo for an additive error
  ri <- .make_rinfo(
    endpoint = "cp",
    resErrorModel = "Additive",
    transform = "Untransformed",
    distribution = "Normal",
    df = list(add = "add.err")
  )
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: manually constructed proportional rinfo", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_prop()
  ri <- .make_rinfo(
    endpoint = "cp",
    resErrorModel = "Proportional",
    transform = "Untransformed",
    distribution = "Normal",
    df = list(prop = "prop.err")
  )
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: combined error (add + prop) rinfo", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_addprop()
  ri <- .make_rinfo(
    endpoint = "cp",
    resErrorModel = "Additive + Proportional (Default)",
    transform = "Untransformed",
    distribution = "Normal",
    df = list(add = "add.err", prop = "prop.err")
  )
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})

test_that("updateResidInModel: combined error Combined 1 variant", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_addprop()
  ri <- .make_rinfo(
    endpoint = "cp",
    resErrorModel = "Additive + Proportional (Combined 1)",
    transform = "Untransformed",
    distribution = "Normal",
    df = list(add = "add.err", prop = "prop.err")
  )
  results <- list2env(list(rinfo = ri, parEstim = mod), parent = emptyenv())

  nlmixr2shiny:::updateResidInModel(results)

  expect_null(results$rinfo)
})
