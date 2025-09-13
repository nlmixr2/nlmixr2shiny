#' Update Omega Matrix in Model
#'
#' @param results results list or matrix
#' @return nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
updateOmegaInModel <- function(results) {
  if (#is.null(results$parEstim) ||
    is.null(results$betweenSubjectVaribility) ||
      is.null(results$triangleTable)) {
    return()
  }
  waiter::waiter_show(html = tagList(
    waiter::spin_fading_circles(),  # A nice spinning loading indicator
    h4("updating Omega Matrix in model...")
  ))
  on.exit(waiter::waiter_hide(), add = TRUE)
  .bsv <- results$betweenSubjectVaribility

  .oldOme <- dimnames(results$parEstim$omega)[[1]]
  .rm <- setdiff(.oldOme, .bsv)
  if (length(.rm) > 0) {
    results$parEstim <- nlmixr2est::rmEta(results$parEstim, .rm)
  }
  # Now add anything that was requested
  .add <- results$parEstim$fullEtaAddExpr
  for (b in .bsv) {
    .cur <- .add[b]
    if (!is.na(.cur)) {
      eval(str2lang(.cur))
    }
  }
  #Now update the omega matrix
  .mat <- as.matrix(results$triangleTable)
  .tmat <- t(.mat)
  diag(.tmat) <- 0
  .mat <- .mat + .tmat
  results$parEstim <- ini(results$parEstim, .mat)
  results$pkpdm <- results$parEstim
  results$betweenSubjectVaribility <- NULL
  results$triangleTable <- NULL
}
