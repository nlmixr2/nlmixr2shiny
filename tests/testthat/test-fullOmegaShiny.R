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
