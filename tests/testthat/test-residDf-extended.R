# Tests for residInfo() on various model types to improve rxUiResidDf.R coverage

test_that("residInfo: boxCox transform model adds lambda column", {
  mod <- .make_pk_boxcox()
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_true("lambda" %in% names(ri$cp$df))
})

test_that("residInfo: add+prop model returns add and prop columns", {
  mod <- .make_pk_addprop()
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_true("add" %in% names(ri$cp$df))
  expect_true("prop" %in% names(ri$cp$df))
})

test_that("residInfo: proportional error model", {
  mod <- .make_pk_prop()
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_type(ri, "list")
  expect_true("cp" %in% names(ri))
  expect_equal(ri$cp$resErrorModel, "Proportional")
})

test_that("residInfo: additive error model", {
  mod <- .make_pk_add()
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_equal(ri$cp$resErrorModel, "Additive")
  expect_true("add" %in% names(ri$cp$df))
})

test_that("residInfo: t-distribution error model adds df column", {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv  <- log(0.6)
      add.err <- 0.1
      df.t <- 10
    })
    model({
      cl <- exp(tcl)
      v  <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(add.err) + t(df.t)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_true("df" %in% names(ri$cp$df))
})

test_that("residInfo: cauchy distribution model", {
  f <- function() {
    ini({
      tcl    <- log(0.008)
      tv     <- log(0.6)
      add.err <- 0.1
    })
    model({
      cl <- exp(tcl)
      v  <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(add.err) + cauchy()
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_type(ri, "list")
  expect_true("cp" %in% names(ri))
})

test_that("residInfo: power error model (pow)", {
  f <- function() {
    ini({
      tcl     <- log(0.008)
      tv      <- log(0.6)
      pow.err <- 0.1
      pow.exp <- 2
    })
    model({
      cl <- exp(tcl)
      v  <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ pow(pow.err, pow.exp)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_true("pow" %in% names(ri$cp$df))
  expect_true("exp" %in% names(ri$cp$df))
})

test_that("residInfo: add + power error model", {
  f <- function() {
    ini({
      tcl     <- log(0.008)
      tv      <- log(0.6)
      add.err <- 0.05
      pow.err <- 0.1
      pow.exp <- 2
    })
    model({
      cl <- exp(tcl)
      v  <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(add.err) + pow(pow.err, pow.exp)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_true("add" %in% names(ri$cp$df))
  expect_true("pow" %in% names(ri$cp$df))
})

test_that("residInfo: Poisson distribution model (residInfo.default pois branch)", {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv  <- log(0.6)
      pois.lam <- 1
    })
    model({
      cl <- exp(tcl)
      v  <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ pois(pois.lam)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_type(ri, "list")
})

test_that("residInfo: Binomial distribution model", {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv  <- log(0.6)
      binom.n <- 10
      binom.p <- 0.5
    })
    model({
      cl <- exp(tcl)
      v  <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ binom(binom.n, binom.p)
    })
  }
  mod <- rxode2::rxode2(f)
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_type(ri, "list")
})

test_that("residInfo: eta+boxcox combined model", {
  mod <- .make_pk_eta_boxcox()
  ri <- nlmixr2shiny:::residInfo(mod)
  expect_true("lambda" %in% names(ri$cp$df))
  expect_equal(ri$cp$distribution, "Normal")
})

test_that(".toResidName: maps all distribution display names", {
  fn <- nlmixr2shiny:::.toResidName
  expect_equal(fn("Normal"),              "norm")
  expect_equal(fn("Poisson"),             "pois")
  expect_equal(fn("Binomial"),            "binom")
  expect_equal(fn("Beta"),                "beta")
  expect_equal(fn("T"),                   "t")
  expect_equal(fn("Chi-Squared"),         "chisq")
  expect_equal(fn("Exponential"),         "dexp")
  expect_equal(fn("F"),                   "f")
  expect_equal(fn("Geometric"),           "geom")
  expect_equal(fn("Hypergeometric"),      "hyper")
  expect_equal(fn("Uniform"),             "unif")
  expect_equal(fn("Weibull"),             "weibull")
  expect_equal(fn("Cauchy"),              "cauchy")
  expect_equal(fn("Gamma"),               "dgamma")
  expect_equal(fn("Ordinal"),             "ord")
  expect_equal(fn("Log-likelihood"),      "ll")
  expect_equal(fn("Normal (AD)"),         "dnorm")
  expect_equal(fn("Negative Binomial"),   "nbinom")
  expect_equal(fn("Negative Binomial (mu)"), "nbinomMu")
})
