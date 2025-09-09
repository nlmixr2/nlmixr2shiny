rxUiGet.fullOmegaShiny <- function(x, ...) {
  .ui <- x[[1]]
  .omega <- .ui$omega
  .ome <- dimnames(.omega)[[1]]
  .getSplitMuModel <- .ui$getSplitMuModel
  .pureMuRef <- .getSplitMuModel$pureMuRef
  .muRefDataFrame <- .ui$muRefDataFrame
  if (length(.muRefDataFrame$theta) > 0) {
    .w <- which(names(.getSplitMuModel$pureMuRef) %in% .muRefDataFrame$theta)
    if (length(.w) > 0) {
      .etas <- .getSplitMuModel$pureMuRef[-.w]
    } else {
      .etas <- .getSplitMuModel$pureMuRef
    }
  } else {
    .etas <- .getSplitMuModel$pureMuRef
  }
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
