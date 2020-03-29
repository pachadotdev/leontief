#' @useDynLib inputoutput
#' @importFrom Rcpp evalCpp
NULL

#' Transaction matrix (rev. 2013)
#' This matrix contains the production of the chilean economy divided into
#' 12 industries. The measuring unit is CLP million of the year 2013
#' @name transaction_matrix
#' @docType data
#' @author Central Bank of Chile
#' @usage demand_matrix_12x12
#' @format A matrix with 12 rows and 12 columns
#' @references \url{https://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/excel.html}
#' @keywords data
NULL

#' Wage and demand matrix (rev. 2008)
#' This matrix contains the wage, intermediate demand and disaggregated final
#' demand of the  chilean economy divided into 12 industries. The final demand
#' is given by components (household consumption, government consumption, etc.)
#' and aggregated. The measuring unit is CLP million of the year 2013.
#' @name wage_demand_matrix
#' @docType data
#' @author Central Bank of Chile
#' @usage wage_demand_matrix
#' @format A matrix with 12 rows and 9 columns
#' @references \url{https://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/excel.html}
#' @keywords data
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("inputoutput", libpath)
}
