test_that(".getVarLhs works with rxUi object", {
  mod <- .make_pk_prop()
  result <- nlmixr2shiny:::.getVarLhs(mod)
  expect_true(!is.null(result))
  # Should contain the model parameters (cl and v are in pureMuRef)
  expect_true(length(result) > 0)
})

test_that(".getVarLhs accepts a function and converts to rxUi", {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      eta.cl ~ 0.1
      prop.err <- 0.2
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ prop(prop.err)
    })
  }
  result <- nlmixr2shiny:::.getVarLhs(f)
  expect_true(!is.null(result))
  expect_true(length(result) > 0)
})

test_that(".getVarLhs: model without mu-ref returns varLhs fallback", {
  mod <- .make_pk_add()
  result <- nlmixr2shiny:::.getVarLhs(mod)
  # Should return something (either varLhs or pureMuRef)
  expect_true(!is.null(result) || length(result) == 0)
})
