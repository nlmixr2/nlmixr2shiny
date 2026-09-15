# Shared model fixtures for nlmixr2shiny tests

# 1-cmt PK with etas (eta.cl + eta.v) and proportional error
.make_pk_prop <- function() {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      eta.cl + eta.v ~ c(1, 0.01, 1)
      prop.err <- 0.2
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ prop(prop.err)
    })
  }
  rxode2::rxode2(f)
}

# 1-cmt PK with additive error, no etas
.make_pk_add <- function() {
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
      cp ~ add(add.err)
    })
  }
  rxode2::rxode2(f)
}

# 1-cmt PK with additive + proportional error, no etas
.make_pk_addprop <- function() {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      add.err <- 0.1
      prop.err <- 0.1
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(add.err) + prop(prop.err)
    })
  }
  rxode2::rxode2(f)
}

# 1-cmt PK with box-cox transform
.make_pk_boxcox <- function() {
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
      cp ~ add(prop.err) + prop(add.err) + boxCox(lambda)
    })
  }
  rxode2::rxode2(f)
}

# 1-cmt with both etas and box-cox (for combined tests)
.make_pk_eta_boxcox <- function() {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      eta.cl + eta.v ~ c(1, 0.01, 1)
      add.err <- 0.1
      prop.err <- 0.2
      lambda <- 0.5
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      cp = A1 / v
      cp ~ add(prop.err) + prop(add.err) + boxCox(lambda)
    })
  }
  rxode2::rxode2(f)
}

# PK/PD model with two endpoints (Cc, effect); the two-endpoint case is what
# used to wedge the "Statistical Model" tab
.make_pkpd_two_endpoints <- function() {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      tec50 <- log(1)
      eta.cl ~ 0.1
      prop.err <- 0.2
      effect.sd <- 0.1
    })
    model({
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      ec50 <- exp(tec50)
      ke <- cl / v
      d/dt(A1) = -ke * A1
      Cc = A1 / v
      effect <- Cc / (ec50 + Cc)
      Cc ~ prop(prop.err)
      effect ~ add(effect.sd)
    })
  }
  rxode2::rxode2(f)
}

# error model the "Distribution" picker cannot represent
.make_pk_gamma <- function() {
  f <- function() {
    ini({
      tcl <- log(0.008)
      tv <- log(0.6)
      shape <- 1
      scale <- 1
    })
    model({
      cl <- exp(tcl)
      v <- exp(tv)
      d/dt(A1) = -(cl / v) * A1
      cp = A1 / v
      cp ~ dgamma(shape, scale)
    })
  }
  rxode2::rxode2(f)
}

# error model with a distribution the picker does offer
.make_pk_pois <- function() {
  f <- function() {
    ini({
      tlam <- 1
    })
    model({
      lam <- exp(tlam)
      d/dt(A1) = -0.1 * A1
      cp = A1 + lam
      cp ~ pois(lam)
    })
  }
  rxode2::rxode2(f)
}
