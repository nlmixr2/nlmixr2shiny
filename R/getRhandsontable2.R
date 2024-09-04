transformDF <- function(df) {
  assertthat::assert_that(is.data.frame(df), msg = "Input must be a data frame.")

  transformRow <- function(row) {
    trans <- row["Trans."]
    trans_lower <- as.numeric(row["Trans.Lower"])
    trans_upper <- as.numeric(row["Trans.Upper"])

    lower <- as.numeric(row["lower"])
    est <- as.numeric(row["est"])
    upper <- as.numeric(row["upper"])

    # Apply transformation to 'est' regardless of the status of 'lower' and 'upper'
    transformed_est <- if (is.na(trans) || trans == "") {
      est
    } else if (trans == "exp") {
      exp(est)
    } else if (trans == "expit") {
      assertthat::assert_that(!is.na(trans_lower) && !is.na(trans_upper),
                              msg = "trans_lower and trans_upper must not be NA for expit transformation.")
      expit(est, trans_lower, trans_upper)
    } else if (trans == "probitInv") {
      assertthat::assert_that(!is.na(trans_lower) && !is.na(trans_upper),
                              msg = "trans_lower and trans_upper must not be NA for probitInv transformation.")
      probitInv(est, trans_lower, trans_upper)
    } else {
      est  # Default to no transformation if the trans value is unrecognized
    }

    # Handle 'lower' and 'upper' transformations, if they exist
    transformed_lower <- if (!is.na(lower)) {
      if (is.na(trans) || trans == "") {
        lower
      } else if (trans == "exp") {
        exp(lower)
      } else if (trans == "expit") {
        assertthat::assert_that(!is.na(trans_lower) && !is.na(trans_upper),
                                msg = "trans_lower and trans_upper must not be NA for expit transformation.")
        if (is.infinite(lower) || lower < trans_lower) {
          trans_lower
        } else {
          expit(lower, trans_lower, trans_upper)
        }
      } else if (trans == "probitInv") {
        assertthat::assert_that(!is.na(trans_lower) && !is.na(trans_upper),
                                msg = "trans_lower and trans_upper must not be NA for probitInv transformation.")
        if (is.infinite(lower) || lower < trans_lower) {
          trans_lower
        } else {
          probitInv(lower, trans_lower, trans_upper)
        }
      } else {
        lower  # Default to no transformation if the trans value is unrecognized
      }
    } else {
      NA  # Leave lower as NA if it is originally NA
    }

    transformed_upper <- if (!is.na(upper)) {
      if (is.na(trans) || trans == "") {
        upper
      } else if (trans == "exp") {
        exp(upper)
      } else if (trans == "expit") {
        assertthat::assert_that(!is.na(trans_lower) && !is.na(trans_upper),
                                msg = "trans_lower and trans_upper must not be NA for expit transformation.")
        if (is.infinite(upper) || upper > trans_upper) {
          trans_upper
        } else {
          expit(upper, trans_lower, trans_upper)
        }
      } else if (trans == "probitInv") {
        assertthat::assert_that(!is.na(trans_lower) && !is.na(trans_upper),
                                msg = "trans_lower and trans_upper must not be NA for probitInv transformation.")
        if (is.infinite(upper) || upper > trans_upper) {
          trans_upper
        } else {
          probitInv(upper, trans_lower, trans_upper)
        }
      } else {
        upper  # Default to no transformation if the trans value is unrecognized
      }
    } else {
      NA  # Leave upper as NA if it is originally NA
    }

    return(c(transformed_lower, transformed_est, transformed_upper))
  }

  transformed <- t(apply(df, 1, transformRow))
  df.trans <- df
  df.trans[, c("lower", "est", "upper")] <- transformed
  return(df.trans)
}
