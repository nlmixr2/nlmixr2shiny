#' Get the data for Exploring the model
#'
#' @return A character vector of the names of the data sets.
#' @noRd
#' @author Dyani Peterson 
#' @examples
#' getDataNamesForExploration()
getDataNamesForExploration <- function() {
  v <- vapply(ls(envir=globalenv()),
              function(v) {
                if (inherits(get(v, envir=globalenv()), "data.frame")) {
                  v
                } else {
                  ""
                }
              },
              character(1),
              USE.NAMES = FALSE)
  v <- v[v != ""]
# Add nlmixr2 datasets 
c(v, "theo_sd", "theo_md")
}
getDataForExploration <- function(d) {
  get(d, envir = globalenv())
}
