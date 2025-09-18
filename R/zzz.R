.onLoad <- function(libname, pkgname) {
  nlmixr2est::.iniS3()
  rxode2::.s3register("rxode2::rxUiGet", "fullOmegaShiny")
  rxode2::.s3register("rxode2::rxUiGet", "fullEtaAddExpr")
}
