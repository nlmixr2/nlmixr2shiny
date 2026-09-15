rxUiGet.fullEtasShiny <- function(x, ...) {
  .ui <- x[[1]]
  .omega <- .ui$omega
  .ome <- dimnames(.omega)[[1]]
  .getSplitMuModel <- .ui$getSplitMuModel
  .pureMuRef <- .getSplitMuModel$pureMuRef
  .muRefDataFrame <- .ui$muRefDataFrame
  .etas <- NULL
  if (length(.muRefDataFrame$theta) > 0) {
    .w <- which(names(.getSplitMuModel$pureMuRef) %in%
                  .muRefDataFrame$theta)
    if (length(.w) > 0) {
      .etas <- .getSplitMuModel$pureMuRef[-.w]
    }
  }
  if (is.null(.etas)) {
    .etas <- .getSplitMuModel$pureMuRef
  }
  .etas
}

rxUiGet.fullEtaAddExpr <- function(x, ...) {
  .etas <- rxUiGet.fullEtasShiny(x, ...)
  if (length(.etas) == 0) {
    return(character(0))
  } else {
    .ret <- vapply(.etas,
           function(v) {
             nlmixr2lib::defaultCombine("eta", v)
           }, character(1), USE.NAMES = FALSE)
  }
  # Quote the parameter name as a string literal rather than splicing it in as
  # a bare symbol: `addEta()` accepts either, but a bare symbol is looked up
  # in the caller's scope before falling back to being treated as a name, so
  # a parameter that happens to share a name with any function visible there
  # (e.g. `alpha`, re-exported from scales via `import(ggplot2)`) resolves to
  # that function instead and fails `addEta()`'s numeric assertion.
  .q <- vapply(.etas, deparse1, character(1), USE.NAMES = FALSE)
  setNames(paste0("results$parEstim <- nlmixr2lib::addEta(results$parEstim, ", .q, ")"),
           .ret)
}

rxUiGet.fullOmegaShiny <- function(x, ...) {
  .etas <- rxUiGet.fullEtasShiny(x, ...)
  .ui <- x[[1]]
  .omega <- .ui$omega
  if (length(.etas) == 0) {
    .omega
  } else {
    .pureMuRef <- vapply(.etas,
                         function(v) {
                           paste0(nlmixr2lib::defaultCombine("eta", v), "~ 0.1")
                         }, character(1), USE.NAMES = FALSE)
    .pureMuRef <- str2lang(paste0("lotri::lotri(",
                                  paste(.pureMuRef, collapse=", "),
                                  ")"))
    if (length(.omega) == 0) {
      eval(.pureMuRef)
    } else {
      lotri::lotriMat(list(.omega, eval(.pureMuRef)))
    }
  }
}
