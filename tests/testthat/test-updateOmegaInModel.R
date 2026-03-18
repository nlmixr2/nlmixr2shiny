test_that("updateOmegaInModel: returns early when betweenSubjectVaribility is NULL", {
  results <- list2env(list(
    betweenSubjectVaribility = NULL,
    triangleTable = NULL,
    parEstim = .make_pk_prop()
  ), parent = emptyenv())

  nlmixr2shiny:::updateOmegaInModel(results)

  # No crash, nothing changed
  expect_null(results$betweenSubjectVaribility)
})

test_that("updateOmegaInModel: returns early when triangleTable is NULL", {
  results <- list2env(list(
    betweenSubjectVaribility = c("eta.cl", "eta.v"),
    triangleTable = NULL,
    parEstim = .make_pk_prop()
  ), parent = emptyenv())

  nlmixr2shiny:::updateOmegaInModel(results)

  # triangleTable stays NULL, no crash
  expect_null(results$triangleTable)
})

test_that("updateOmegaInModel: keeps both etas and updates omega matrix", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_eta_boxcox()
  .omeNames <- dimnames(mod$omega)[[1]]

  # Keep both etas, update with identity-ish matrix
  triMat <- matrix(
    c(0.5, 0.01, 0, 0.3),
    nrow = 2, ncol = 2,
    dimnames = list(.omeNames, .omeNames)
  )
  triDf <- as.data.frame(triMat)

  results <- list2env(list(
    betweenSubjectVaribility = .omeNames,
    triangleTable = triDf,
    parEstim = mod,
    pkpdm = NULL
  ), parent = emptyenv())

  nlmixr2shiny:::updateOmegaInModel(results)

  expect_null(results$betweenSubjectVaribility)
  expect_null(results$triangleTable)
  expect_false(is.null(results$parEstim))
  expect_false(is.null(results$pkpdm))
})

test_that("updateOmegaInModel: removes an eta when not in betweenSubjectVaribility", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_eta_boxcox()
  .omeNames <- dimnames(mod$omega)[[1]]
  # Keep only first eta
  keepEta <- .omeNames[1]

  triDf <- data.frame(
    row.names = keepEta,
    check.names = FALSE
  )
  triDf[[keepEta]] <- 0.5

  results <- list2env(list(
    betweenSubjectVaribility = keepEta,
    triangleTable = triDf,
    parEstim = mod,
    pkpdm = NULL
  ), parent = emptyenv())

  nlmixr2shiny:::updateOmegaInModel(results)

  expect_null(results$betweenSubjectVaribility)
  expect_null(results$triangleTable)
  # Model updated - the removed eta should not be in new omega
  .newOme <- dimnames(results$parEstim$omega)[[1]]
  expect_false(.omeNames[2] %in% .newOme)
})

test_that("updateOmegaInModel: clears betweenSubjectVaribility and triangleTable after update", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_eta_boxcox()
  .omeNames <- dimnames(mod$omega)[[1]]

  triMat <- diag(length(.omeNames))
  dimnames(triMat) <- list(.omeNames, .omeNames)
  triDf <- as.data.frame(triMat)

  results <- list2env(list(
    betweenSubjectVaribility = .omeNames,
    triangleTable = triDf,
    parEstim = mod,
    pkpdm = NULL
  ), parent = emptyenv())

  nlmixr2shiny:::updateOmegaInModel(results)

  expect_null(results$betweenSubjectVaribility)
  expect_null(results$triangleTable)
})

test_that("updateOmegaInModel: matrix symmetrized correctly", {
  local_mocked_bindings(
    waiter_show = function(...) invisible(),
    waiter_hide = function(...) invisible(),
    .package = "waiter"
  )

  mod <- .make_pk_eta_boxcox()
  .omeNames <- dimnames(mod$omega)[[1]]

  # Upper triangular matrix (only upper triangle populated)
  triMat <- matrix(0, nrow = 2, ncol = 2,
                   dimnames = list(.omeNames, .omeNames))
  triMat[1, 1] <- 0.5
  triMat[1, 2] <- 0.05
  triMat[2, 2] <- 0.3
  triDf <- as.data.frame(triMat)

  results <- list2env(list(
    betweenSubjectVaribility = .omeNames,
    triangleTable = triDf,
    parEstim = mod,
    pkpdm = NULL
  ), parent = emptyenv())

  nlmixr2shiny:::updateOmegaInModel(results)

  # Check new omega is symmetric
  .newOmega <- results$parEstim$omega
  expect_equal(.newOmega[1, 2], .newOmega[2, 1])
})
