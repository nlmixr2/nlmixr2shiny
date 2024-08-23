reverseTransformDF <- function(df) {
  required_columns <- c("Trans.", "Trans.Lower", "Trans.Upper", "lower", "est", "upper")
  for (col in required_columns) {
    if (!col %in% names(df)) {
      stop(paste("Column", col, "is missing from the input data frame."))
    }
  }
  reversed <- t(apply(df, 1, reverseTransformRow))
  df.rev <- df
  df.rev[, c("lower", "est", "upper")] <- reversed
  return(df.rev)
}

