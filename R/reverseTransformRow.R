reverseTransformRow <- function(row) {
  trans <- row["Trans."]
  tranUower <- as.numeric(row["Trans.Lower"])
  transUpper <- as.numeric(row["Trans.Upper"])

  lower <- as.numeric(row["lower"])
  est <- as.numeric(row["est"])
  upper <- as.numeric(row["upper"])

  if (trans == "Untransformed") {
    return(c(lower, est, upper))
  } else if (trans == "lognormal") {
    return(c(log(lower), log(est), log(upper)))
  } else if (trans == "logitNormal") {
    if (lower > transLower) {
      lower <- rxode2::logit(lower, transLower, transUpper)
    } else {
      lower <- -Inf
    }
    est <- rxode2::logit(est, transLower, transUpper)
    if (upper < transUpper) {
      upper <- rxode2::logit(upper, transLower, transUpper)
    } else {
      upper <- Inf
    }
    return(c(lower, est, upper))
  } else if (trans == "probitNormal") {
    if (lower > transLower) {
      lower <- rxode2::probit(lower, transLower, transUpper)
    } else {
      lower <- -Inf
    }
    est <- rxode2::probit(est, transLower, transUpper)
    if (upper < transUpper) {
      upper <- rxode2::probit(upper, transLower, transUpper)
    } else {
      upper <- Inf
    }
    return(c(lower, est, upper))
  } else {
    stop(paste("Unknown transformation:", trans))
  }
}
