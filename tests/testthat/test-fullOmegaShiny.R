test_that("fullOmegaShiny", {

  mod1 <- function () {
    ini({
      lcl <- 1
      lvc <- 3.45
      propSd <- c(0, 0.5)
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      kel <- cl/vc
      d/dt(central) <- -kel * central
      Cc <- central/vc
      Cc ~ prop(propSd)
    })
  }

  mod1 <- mod1()

  expect_equal(mod1$fullOmegaShiny,
               lotri::lotri(etaCl ~ 0.1,
                            etaVc ~ 0.1))

  mod2 <- function () {
    ini({
      lcl <- 1
      lvc <- 3.45
      propSd <- c(0, 0.5)
      eta.cl ~ 0.1
      eta.vc ~ 0.2
    })
    model({
      cl <- exp(lcl+eta.cl)
      vc <- exp(lvc+eta.vc)
      kel <- cl/vc
      d/dt(central) <- -kel * central
      Cc <- central/vc
      Cc ~ prop(propSd)
    })
  }

  mod2 <- mod2()

  expect_equal(mod2$fullOmegaShiny,
               lotri::lotri(eta.cl ~ 0.1,
                            eta.vc ~ 0.2))

  mod2 <- function () {
    ini({
      lcl <- 1
      lvc <- 3.45
      propSd <- c(0, 0.5)
      eta.cl ~ 0.1
    })
    model({
      cl <- exp(lcl+eta.cl)
      vc <- exp(lvc)
      kel <- cl/vc
      d/dt(central) <- -kel * central
      Cc <- central/vc
      Cc ~ prop(propSd)
    })
  }

  mod2 <- mod2()

  expect_equal(mod2$fullOmegaShiny,
               lotri::lotri(eta.cl ~ 0.1,
                            etaVc ~ 0.1))


  mod2 <- function () {
    ini({
      lcl <- 1
      lvc <- 3.45
      propSd <- c(0, 0.5)
      eta.vc ~ 0.2
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc+eta.vc)
      kel <- cl/vc
      d/dt(central) <- -kel * central
      Cc <- central/vc
      Cc ~ prop(propSd)
    })
  }

  mod2 <- mod2()

  expect_equal(mod2$fullOmegaShiny,
               lotri::lotri(eta.vc ~ 0.2,
                            etaCl ~ 0.1))

})


test_that("$fullEtaAddExpr", {

  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <-  log(0.6)
      eta.cl + eta.v ~ c(1,
                         0.01, 1)
      add.err <- 0.1
      prop.err <- 0.2
      lambda <- 0.5
    })
    model({
      cl <- exp(tcl + eta.cl) # individual value of clearance
      v <- exp(tv + eta.v)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(prop.err) + prop(add.err) +
        boxCox(lambda) # define error model
    })
  }

  tmp <- rxode2::rxode2(f)

  expect_equal(tmp$fullEtaAddExpr,
               character(0))


  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <-  log(0.6)
      add.err <- 0.1
      prop.err <- 0.2
      lambda <- 0.5
    })
    model({
      cl <- exp(tcl) # individual value of clearance
      v <- exp(tv)    # individual value of volume
      ke <- cl / v            # elimination rate constant
      d/dt(A1) = - ke * A1    # model differential equation
      cp = A1 / v             # concentration in plasma
      cp ~ add(prop.err) + prop(add.err) +
        boxCox(lambda) # define error model
    })
  }

  tmp <- rxode2::rxode2(f)

  expect_equal(tmp$fullEtaAddExpr,
               c(etaCl = "results$parEstim <- nlmixr2lib::addEta(results$parEstim, \"cl\")",
                 etaV = "results$parEstim <- nlmixr2lib::addEta(results$parEstim, \"v\")"))

})

test_that("$fullEtaAddExpr quotes parameter names that shadow visible functions", {
  # `alpha` is exported by scales and re-exported through ggplot2, which
  # nlmixr2shiny imports wholesale; a bare (unquoted) `alpha` in the
  # generated call would resolve to that function instead of being treated
  # as the parameter name, and addEta() would then fail its numeric
  # assertion on a closure. See updateOmegaInModel().
  f <- function() {
    ini({
      lalpha <- 1
      lv <- log(0.6)
      add.err <- 0.1
    })
    model({
      alpha <- exp(lalpha)
      v <- exp(lv)
      d/dt(A1) = -alpha * A1
      cp = A1 / v
      cp ~ add(add.err)
    })
  }

  tmp <- rxode2::rxode2(f)

  expect_equal(tmp$fullEtaAddExpr,
               c(etaAlpha = "results$parEstim <- nlmixr2lib::addEta(results$parEstim, \"alpha\")",
                 etaV = "results$parEstim <- nlmixr2lib::addEta(results$parEstim, \"v\")"))

  # the generated expression must actually be safe to eval(), not merely look
  # quoted: bind a same-named function in the calling scope the way
  # `ggplot2::alpha` is visible from nlmixr2shiny's namespace, and confirm the
  # eta is still added rather than raising the closure-typed assertion error.
  alpha <- function(colour, alpha = NA) colour
  results <- new.env()
  results$parEstim <- tmp
  for (expr in tmp$fullEtaAddExpr) {
    eval(str2lang(expr))
  }
  expect_true(all(c("etaAlpha", "etaV") %in% dimnames(results$parEstim$omega)[[1]]))
})
