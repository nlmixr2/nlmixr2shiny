changeTransColumnValues <- function(df) {
  required_columns <- c("Trans.")
  for (col in required_columns) {
    if (!col %in% names(df)) {
      stop(paste("Column", col, "is missing from the input data frame."))
    }
  }
  
  df$Trans. <- vapply(df$Trans., function(trans) {
    if (is.na(trans) || trans == "") {
      return("Untransformed")
    } else if (trans == "exp") {
      return("lognormal")
    } else if (trans == "expit") {
      return("logitNormal")
    } else if (trans == "probitInv") {
      return("probitNormal")
    } else {
      return(trans)  
    }
  },character(1))
  
  return(df)
}