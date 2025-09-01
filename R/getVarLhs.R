.getVarLhs <- function(model) {
  if (!inherits(model, "rxUi")) {
    .ui <- rxode2::rxode2(model)
  } else {
    .ui <- model
  }
  .varLhs <- .ui$varLhs
  if (is.null(.varLhs)) .varLhs <- .ui$getSplitMuModel$pureMuRef
  .varLhs
}
