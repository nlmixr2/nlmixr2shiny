reverseTransformRow <- function(row) {
  trans <- row["Trans."]
  trans_lower <- as.numeric(row["Trans.Lower"])
  trans_upper <- as.numeric(row["Trans.Upper"])
  
  lower <- as.numeric(row["lower"])
  est <- as.numeric(row["est"])
  upper <- as.numeric(row["upper"])
  
  if (trans == "Untransformed") {
    return(c(lower, est, upper))
  } else if (trans == "lognormal") {
    return(c(log(lower), log(est), log(upper)))
  } else if (trans == "logitNormal") {
    if (lower > trans_lower) {
      lower <- rxode2::logit(lower, trans_lower, trans_upper)
    } else {
      lower <- -Inf
    }
    est <- rxode2::logit(est, trans_lower, trans_upper)
    if (upper < trans_upper) {
      upper <- rxode2::logit(upper, trans_lower, trans_upper)
    } else {
      upper <- Inf
    }
    return(c(lower, est, upper))
  } else if (trans == "probitNormal") {
    if (lower > trans_lower) {
      lower <- rxode2::probit(lower, trans_lower, trans_upper)
    } else {
      lower <- -Inf
    }
    est <- rxode2::probit(est, trans_lower, trans_upper)
    if (upper < trans_upper) {
      upper <- rxode2::probit(upper, trans_lower, trans_upper)
    } else {
      upper <- Inf
    }
    return(c(lower, est, upper))
  } else {
    stop(paste("Unknown transformation:", trans))
  }
}