.onLoad <- function(libname, pkgname) {
  rxode2::.s3register("rxode2::rxUiGet", "fullOmegaShiny")
  rxode2::.s3register("rxode2::rxUiGet", "fullEtaAddExpr")
}
