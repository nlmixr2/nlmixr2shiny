#' Get the new residual error data frame for an endpoint
#'
#'
#' @param ri residual info from the model
#' @param input input so that the table can be read
#' @param x the endpoint
#' @param single is this a single endpoint model?
#' @return New dataset to be updated
#' @noRd
#' @author Matthew L. Fidler
getNewResidDfForEndpoint <- function(ri, input, x, single) {
  .newRes <- ri[[x]]$resErrorModel
  .df <- rhandsontable::hot_to_r(input[[paste0("resErrorEst_", x)]])
  .transform <- ri[[x]]$transform
  .dist <- ri[[x]]$distribution

  if (.dist %in% c("Normal", "t-distribution", "Cauchy")) {
    .cols <- switch(.newRes,
                    "Additive" = c("add"),
                    "Proportional" = c("prop"),
                    "Power" = c("pow", "exp"),
                    "Additive + Proportional (Combined 1)" = c("add", "prop"),
                    "Additive + Proportional (Combined 2)" = c("add", "prop"),
                    "Additive + Proportional (Default)" = c("add", "prop"),
                    "Additive + Power (Combined 1)" = c("add", "pow", "exp"),
                    "Additive + Power (Combined 2)" = c("add", "pow", "exp"),
                    "Additive + Power (Default)" = c("add", "pow", "exp"),
                    character(0))

    if (.transform %in% c("Box-Cox",
                          "Yeo-Johnson",
                          "Logit-normal + Box-Cox",
                          "Logit-normal + Yeo-Johnson",
                          "Probit-normal + Box-Cox",
                          "Probit-normal + Yeo-Johnson")) {
      .cols <- c(.cols, "lambda")
    }
    if (.dist == "t-distribution") {
      .cols <- c(.cols, "df")
    }
    .dfNew <- lapply(.cols, function(n) {
      if (n %in% colnames(.df)) {
        return(.df[[n]])
      } else {
        .n <- ifelse(n == "exp", "c", n)
        if (single) {
          return(nlmixr2lib::defaultCombine(.n,
                                            ifelse(n %in% c("add", "prop", "pow"), "sd", "")))
        } else {
          return(nlmixr2lib::defaultCombine(x, .n,
                                            ifelse(n %in% c("add", "prop", "pow"), "sd", "")))
        }
      }
    })
    .dfNew <- as.data.frame(.dfNew)
    names(.dfNew) <- .cols
    rownames(.dfNew) <- x
    .dfNew
  } else {
    .dist2 <- .toResidName(.dist)
    .cols <- .residDistributionNames[[.dist2]]
    .dfNew <- lapply(.cols, function(n) {
      if (n %in% colnames(.df)) {
        return(.df[[n]])
      } else {
        if (single) {
          return(n)
        } else {
          return(nlmixr2lib::defaultCombine(x, n))
        }
      }
    })
    .dfNew <- as.data.frame(.dfNew)
    names(.dfNew) <- .cols
    rownames(.dfNew) <- x
    .dfNew
  }
}
