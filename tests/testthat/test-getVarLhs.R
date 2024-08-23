f <- function() {
  ini({
    lcl <- 1
    label("Clearance (CL)")
    lvp <- 5
    label("Peripheral volume of distribution (Vp)")
    lq <- 0.1
    label("Intercompartmental clearance (Q)")
    propSd <- c(0, 0.5)
    label("Proportional residual error (fraction)")
    lkin <- 0.1
    lvc <- 1
    label("zero order response production(kin)")
    lkout <- 0.1
    label("first order rate response loss (kout)")
    lEk <- 0.1
    label("linear effect constant (Ek)")
    effectSd <- c(0, 0.1)
    label("additive error for effect")
  })
  model({
    kin <- probitInv(lkin, 0.1, 5)
    kout <- exp(lkout)
    Ek <- expit(lEk, 0.1, 20)
    cl <- exp(lcl)
    vc <- lvc
    vp <- exp(lvp)
    q <- exp(lq)
    kel <- cl/vc
    k12 <- q/vc
    k21 <- q/vp
    d/dt(central) <- -kel * central - k12 * central + k21 *
      peripheral1
    d/dt(peripheral1) <- k12 * central - k21 * peripheral1
    Cc <- central/vc
    Cc ~ prop(propSd)
    R(0) <- kin/kout
    d/dt(R) <- kin * (1 + Ek * Cc) - kout * R
    effect <- R
    effect ~ add(effectSd)
  })
}

test_that("Model that is not rxUi Class", {
  model <- f
  
  expected <- nlmixr2(f)$varLhs
  result <- .getVarLhs(f)
  
  expect_equal(result, expected)
})


test_that("Model that is rxUi Class", {
  model <- f
  
  expected <- nlmixr2(f)$varLhs
  result <- .getVarLhs(nlmixr2(f))
  
  expect_equal(result, expected)
})


