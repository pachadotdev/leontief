#' @useDynLib inputoutput
#' @importFrom Rcpp evalCpp
NULL

#' Demand matrix (rev. 2008)
#' This matrix contains the production of the chilean economy divided into
#' 12 industries. The intermediate and total demand is also included to support
#' the vignettes. The measuring unit is CLP million of the year 2008.
#' @name demand_matrix_12x12
#' @docType data
#' @author Central Bank of Chile
#' @usage demand_matrix_12x12
#' @format A matrix with 12 rows and 14 columns
#' @references \url{https://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/excel.html}
#' @keywords data
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("inputoutput", libpath)
}
