

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
transformDF <- function(df=getRhandsontable(f)) {
  assertthat::assert_that(is.data.frame(df), msg = "Input must be a data frame.")
  
  required_columns <- c("Trans.", "Trans.Lower", "Trans.Upper", "lower", "est", "upper")
  for (col in required_columns) {
    assertthat::assert_that(assertthat::has_name(df, col), msg = paste("Column", col, "is missing from the input data frame."))
  }
  
  transformRow <- function(row) {
    trans <- row["Trans."]
    trans_lower <- as.numeric(row["Trans.Lower"])
    trans_upper <- as.numeric(row["Trans.Upper"])
    
    lower <- as.numeric(row["lower"])
    est <- as.numeric(row["est"])
    upper <- as.numeric(row["upper"])
    
    assertthat::assert_that(!is.na(lower), !is.na(est), !is.na(upper), msg = "lower, est, and upper values must not be NA.")
    
    if (is.na(trans) || trans == "") {
      return(c(lower, est, upper))
    } else if (trans == "exp") {
      return(c(exp(lower), exp(est), exp(upper)))
    } else if (trans == "expit") {
      assertthat::assert_that(!is.na(trans_lower), !is.na(trans_upper), msg = "trans_lower and trans_upper must not be NA for expit transformation.")
      expp <- c(expit(lower, trans_lower, trans_upper),
               expit(est, trans_lower, trans_upper),
               expit(upper, trans_lower, trans_upper))
      if(is.infinite(lower)||lower<trans_lower){
        expp[1] <- trans_lower
      }
      if(is.infinite(upper)||lower>trans_lower){
        expp[3] <- trans_upper
      }
      
      return(expp)
      
    } else if (trans == "probitInv") {
      assertthat::assert_that(!is.na(trans_lower), !is.na(trans_upper), msg = "trans_lower and trans_upper must not be NA for probitInv transformation.")
      expp2 <- c(probitInv(lower, trans_lower, trans_upper), 
               probitInv(est, trans_lower, trans_upper), 
               probitInv(upper, trans_lower, trans_upper))
      if(is.infinite(lower)||lower<trans_lower){
        expp2[1] <- trans_lower
      }
      if(is.infinite(upper)||lower>trans_lower){
        expp2[3] <- trans_upper
      }
      
      return(expp2)
    } 
  }
  
  transformed <- t(apply(df, 1, transformRow))
  df.trans <- df
  df.trans[, c("lower", "est", "upper")] <- transformed
  return(df.trans)
}





