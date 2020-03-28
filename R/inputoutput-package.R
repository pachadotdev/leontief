#' @useDynLib inputoutput
#' @importFrom Rcpp evalCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("inputoutput", libpath)
}
