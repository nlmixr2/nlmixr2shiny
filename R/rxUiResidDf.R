#' This creates the residual data frame for
#'
#' @param line line to parse
#'
#' @return error lines for the model
#'
#' @export
#'
#' @keywords internal
#'
#' @author Matthew L. Fidler
residDf <- function(line) {
  UseMethod("residDf")
}

residDfObject <- function(x, line) {
  predDf <- x$predDf
  if (line > nrow(predDf)) {
    return(NULL)
  }
  predLine <- predDf[line, ]
  ret <- list(x, predLine, line)
  class(ret) <- c(paste(predLine$distribution), "residDf")
  ret
}


#' @rdname residDf
#' @export
residDf.rxUi <- function(line) {
  predDf <- line$predDf
  lapply(seq_along(pred_df$cond), function(c) {
    mod <- residDfObject(line, c)
    residDf(mod)
  })
}

#' Get the additive transformation
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The quoted symbolic name of the additive standard deviation
#' @author Matthew Fidler
#' @noRd
.residForErrorAdd <- function(env, pred1) {
  if (!is.na(pred1$a)) {
    .p1 <- str2lang(pred1$a)
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("add", "lnorm", "logitNorm", "probitNorm") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- env$iniDf$name[.w]
    } else {
      stop("cannot find additive standard deviation for '", .cnd, "'",
           ifelse(length(env$predDf$condition) == 1L, "", "; this parameter could be estimated by another endpoint, to fix move outside of error expression."), call.=FALSE)
    }
  }
  data.frame("add"=.p1)
}

#' Get Variance for proportional error
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The quoted proportional error
#' @author Matthew Fidler
#' @noRd
.residForErrorProp <- function(env, pred1) {
  if (!is.na(pred1$b)) {
    .p1 <- pred1$b
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("prop", "propF", "propT") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- env$iniDf$name[.w]
    } else {
      stop("cannot find proportional standard deviation", call.=FALSE)
    }
  }
  data.frame("prop"=.p1)
}

#' Get the Variance for pow error model
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return The quoted additive + proportional expression
#' @author Matthew Fidler
#' @noRd
.residForErrorPow <- function(env, pred1) {
  .cnd <- pred1$cond
  if (!is.na(pred1$b)) {
    .p1 <- pred1$b
  } else {
    .w <- which(env$iniDf$err %in% c("pow", "powF", "powT") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- env$iniDf$name[.w]
    } else {
      stop("cannot find power standard deviation", call.=FALSE)
    }
  }
  if (!is.na(pred1$c)) {
    .p2 <- pred1$c
  } else {
    .w <- which(env$iniDf$err %in% c("pow2", "powF2", "powT2") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p2 <- env$iniDf$name[.w]
    } else {
      stop("cannot find exponent of power expression", call.=FALSE)
    }
  }
  data.frame("pow"=.p1, "exp"=.p2)
}

.residForErrorAddProp <- function(env, pred1) {
  if (!is.na(pred1$a)) {
    .p1 <- pred1$a
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("add", "lnorm", "probitNorm", "logitNorm") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- env$iniDf$name[.w]
    } else {
      stop("cannot find additive standard deviation", call.=FALSE)
    }
  }
  if (!is.na(pred1$b)) {
    .p2 <- pred1$b
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("prop", "propT", "propF") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p2 <- env$iniDf$name[.w]
    } else {
      stop("cannot find proportional standard deviation", call.=FALSE)
    }
  }
  data.frame("add"=.p1, "prop"=.p2)
}

#' Additive + Power
#'
#' @param env Environment for the parsed model
#' @param pred1 The `data.frame` of the current error
#' @return additive + power
#' @author Matthew Fidler
#' @noRd
.residForErrorAddPow <- function(env, pred1) {
  if (!is.na(pred1$a)) {
    .p1 <- pred1$a
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("add", "lnorm", "logitNorm", "probitNorm") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p1 <- env$iniDf$name[.w]
    } else {
      stop("cannot find additive standard deviation", call.=FALSE)
    }
  }
  if (!is.na(pred1$b)) {
    .p2 <- pred1$b
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("pow", "powF", "powT") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p2 <- env$iniDf$name[.w]
    } else {
      stop("cannot find pow standard deviation", call.=FALSE)
    }
  }
  if (!is.na(pred1$c)) {
    .p3 <- pred1$c
  } else {
    .cnd <- pred1$cond
    .w <- which(env$iniDf$err %in% c("pow2", "powF2", "powT2") & env$iniDf$condition == .cnd)
    if (length(.w) == 1L) {
      .p3 <- env$iniDf$name[.w]
    } else {
      stop("cannot find pow exponent", call.=FALSE)
    }
  }
  data.frame("add"=.p1, "pow"=.p2, "c"=.p3)
}
#' This gets the data frame for an error type
#'
#'
#' @param env rxode2 ui environment
#' @param pred1 prediction for current endpoint
#' @return data frame with the estimate of the error
#' @noRd
#' @author Matthew L. Fidler
.residGetVarianceForErrorType <- function(env, pred1) {
  switch(as.character(pred1$errType),
         add = .residForErrorAdd(env, pred1),
         prop = .residForErrorProp(env, pred1),
         pow =  .residForErrorPow(env, pred1),
         `add + prop` = .residForErrorAddProp(env, pred1),
         `add + pow` = .residForErrorAddPow(env, pred1))
}
#' Add lambda parameter to the data frame for box-cox and yeo-Johnosn models
#'
#' @param df data frame to add a lambda to.
#' @param env rxode2 ui environment
#' @param pred1 prediction for current endpoint
#' @returndata frame with lambda added if needed
#' @noRd
#' @author Matthew L. Fidler
.residAddLambda <- function(df, env, pred1) {
  if (pred1$transform %in%
        c("boxCox", "yeoJohnson", "logit + yeoJohnson",
          "probit + yeoJohnson", "logit + boxCox", "probit + boxCox")) {
    cbind(df, data.frame(lambda=rxode2::.rxGetLambdaFromPred1AndIni(env, pred1),
                         row.names=pred1$var))
  } else {
    df
  }
}

#' Add degrees of freedom for t-distribution
#'
#'
#' @param df data frame to add a df to
#' @param env rxode2 ui environment
#' @param pred1 prediction for current endpoint
#' @return data frame with df added if needed
#' @noRd
#' @author Matthew L. Fidler
.residAddDf <- function(df, env, pred1) {
  if (pred1$distribution == "t") {
    .iniDf <- env$iniDf
    .cnd <- pred1$cond
    .w <- which(.iniDf$err == "t" & .iniDf$condition == .cnd)
    if (length(.w) == 1) {
      .nu <- paste(.iniDf$name[.w])
    } else {
      if (is.na(pred1$d)) {
        stop("t distribution needs a proper degrees of freedom specified",
             call. = FALSE)
      }
      .nu <- pred1$d
    }
    cbind(df, data.frame(df=.nu))
  } else {
    df
  }
}

#' Expand the limits for logit and probit models
#'
#' @param df data frame
#' @param env rxode2 ui environment
#' @param pred1 prediction for current endpoint
#' @return data frame with low and high limits added if needed
#' @noRd
#' @author Matthew L. Fidler
.residExpandLimits <- function(df, env, pred1) {
  if (pred1$transform %in%
        c("logit",  "logit + yeoJohnson", "probit", "probit + yeoJohnson", "logit + boxCox", "probit + boxCox")) {
    cbind(df, data.frame(low=rxode2::.rxGetLowBoundaryPred1AndIni(env, pred1),
                         hi=rxode2::.rxGetHiBoundaryPred1AndIni(env, pred1),
                         row.names=pred1$var))
  } else {
    df
  }
}

#' @rdname residDf
#' @export
residDf.norm <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .ret <- .residGetVarianceForErrorType(env, pred1) |>
    .residAddDf(env, pred1) |>
    .residAddLambda(env, pred1) |>
    .residExpandLimits(env, pred1)
  row.names(.ret) <- pred1$var
  .ret
}

# Special cases for t and cauchy, simply use norm

#' @rdname residDf
#' @export
residDf.t <-  residDf.norm

#' @rdname residDf
#' @export
residDf.cauchy <-  residDf.norm

#' @rdname residDf
#' @export
residDf.dnorm <- residDf.norm
.residDistributionNames <-
  list(
    pois = c("lambda"),
    binom = c("size", "prob"),
    beta = c("shape1", "shape2"),
    chisq = c("df"),
    dexp = c("rate"),
    f = c("df1", "df2"),
    geom = c("prob"),
    hyper = c("m", "n", "k"),
    unif = c("min", "max"),
    weibull = c("shape", "scale"),
    dgamma = c("shape", "scale"),
    nbinom = c("size", "prob"),
    nbinomMu = c("size", "mu")
  )

#' @rdname residDf
#' @export
residDf.default <- function(line) {
  env <- line[[1]]
  pred1 <- line[[2]]
  .dist <- as.character(pred1$distribution)
  if (.dist == "LL") {
    return(data.frame(LL=deparse1(env$lstExpr[[pred1$line]][[3]]),
                      row.names=pred1$var))
  } else if (.dist == "ordinal") {
    return(data.frame(line=deparse1(env$lstExpr[[pred1$line]]),
                      row.names=pred1$var))
  }
  .nargs <- max(.errDist[[.dist]])
  .cnd <- pred1$cond
  .argName <- .namedArgumentsToPredDf[[.dist]]
  .args <- vapply(seq(1:.nargs), function(.i) {
    .curDist <- .argName[.i]
    if (!is.na(pred1[[.curDist]])) {
      return(pred1[[.curDist]])
    } else {
      .curDist <- paste0(.dist, ifelse(.i == 1, "", .i))
      .w <- which(env$iniDf$err == .curDist & env$iniDf$condition == .cnd)
      if (length(.w) == 1) {
        return(env$iniDf$name[.w])
      } else {
        return("")
      }
    }
  }, character(1))

  stats::setNames(as.data.frame(t(.args),  stringsAsFactors = FALSE,
                                row.names=pred1$var),
                  .residDistributionNames[[.dist]])
}
