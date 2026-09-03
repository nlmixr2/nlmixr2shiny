#' Update residuals in model
#'
#' @param results list containing results including models
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
updateResidInModel <- function(results) {
  if (is.null(results$rinfo)) {
    return()
  }
  waiter::waiter_show(html = tagList(
    waiter::spin_fading_circles(),  # A nice spinning loading indicator
    h4("updating Residual Specification in model...")
  ))
  on.exit(waiter::waiter_hide(), add = TRUE)
  .ri <- results$rinfo
  .nri <- .residEndpoints(.ri)
  .var <- vapply(.nri,
                 function(x) {
                   .resErrorModel <- .ri[[x]]$resErrorModel
                   .transform <- .ri[[x]]$transform
                   .dist <- .ri[[x]]$distribution
                   .df <- .ri[[x]]$df
                   .ret <- NULL
                   if (.dist %in% c("Normal", "t-distribution", "Cauchy")) {
                     .add <- switch(.transform,
                                    "Untransformed"="",
                                    "Log-normal"="lnorm",
                                    "Logit-normal"="logitNorm",
                                    "Box-Cox"="",
                                    "Yeo-Johnson"="",
                                    "Logit-normal + Box-Cox"="logitNorm",
                                    "Logit-normal + Yeo-Johnson"="logitNorm",
                                    "Probit-normal"="probitNorm",
                                    "Probit-normal + Box-Cox"="probitNorm",
                                    "Probit-normal + Yeo-Johnson"="probitNorm")

                     .lambda <- switch(.transform,
                                       "Untransformed"="",
                                       "Log-normal"="",
                                       "Logit-normal"="",
                                       "Box-Cox"="boxCox",
                                       "Yeo-Johnson"="yeoJohnson",
                                       "Logit-normal + Box-Cox"="boxCox",
                                       "Logit-normal + Yeo-Johnson"="yeoJohnson",
                                       "Probit-normal"="",
                                       "Probit-normal + Box-Cox"="boxCox",
                                       "Probit-normal + Yeo-Johnson"="boxCox")

                     .comb <- switch(.resErrorModel,
                                     "Additive"="",
                                     "Proportional"="",
                                     "Power"="",
                                     "Additive + Proportional (Combined 1)"="comb1()",
                                     "Additive + Proportional (Combined 2)"="comb2()",
                                     "Additive + Proportional (Default)"="",
                                     "Additive + Power (Combined 1)"="comb1()",
                                     "Additive + Power (Combined 2)"="comb2()",
                                     "Additive + Power (Default)"="",
                                     "")
                     if (is.null(.df$add) && .add != "") {
                       # lnorm(NA) + ...
                       .ret <- c(.ret, paste0(.add, "(NA)"))
                     } else if (!is.null(.df$add)) {
                       if (.add == "") .add <- "add"
                       .ret <- c(.ret, paste0(.add, "(", .df$add, ")"))
                     }
                     if (!is.null(.df$prop)) {
                       .ret <- c(.ret, paste0("prop(", .df$prop, ")"))
                     }
                     if (!is.null(.df$pow) && !is.null(.df$exp)) {
                       .ret <- c(.ret, paste0("pow(", .df$pow, ", ", .df$exp, ")"))
                     }
                     if (!is.null(.df$lambda) && .lambda != "") {
                       .ret <- c(.ret, paste0(.lambda, "(", .df$lambda, ")"))
                     }
                     # Now t/cauchy
                     if (.dist == "t-distribution" && !is.null(.df$df)) {
                       .ret <- c(.ret, paste0("dt(", .df$df, ")"))
                     } else if (.dist == "Cauchy" ) {
                       .ret <- c(.ret, "dcauchy()")
                     }
                     if (.comb != "") {
                       .ret <- c(.ret, .comb)
                     }
                     return(paste0(x, "~", paste(.ret, collapse = "+")))
                   } else {
                     .dist2 <- .toResidName(.dist)
                     .cols <- .residDistributionNames[[.dist2]]
                     .ret <- NULL
                     for (n in .cols) {
                       if (!is.null(.df[[n]])) {
                         .ret <- c(.ret, .df[[n]])
                       }
                     }
                     return(paste0(x, "~", paste0("d", .dist2, "(",
                                                  paste(.ret, collapse = ", "), ")")))
                   }
                 }, character(1), USE.NAMES=FALSE)
  # Now apply the residual model pipe
  for (p in .var) {
    tmp <- try(eval(bquote(rxode2::model(results$parEstim, .(str2lang(p))))), silent=TRUE)
    if (inherits(tmp, "try-error")) {
      warning("Error updating residual model for ", p, ": ", tmp)
    } else {
      results$parEstim <- tmp
    }
  }
  results$pkpdm <- results$parEstim
  results$rinfo <- NULL
}
