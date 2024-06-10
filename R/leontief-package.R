#' Transaction matrix (2013 data)
#' This matrix contains the production of the chilean economy divided into
#' 12 industries. The measuring unit is CLP million of the year 2013
#' @name transaction_matrix
#' @docType data
#' @author Central Bank of Chile
#' @usage transaction_matrix
#' @format A matrix with 12 rows and 12 columns
#' @references \url{https://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/excel.html}
#' @keywords data
NULL

#' Wage and demand matrix (2013 data)
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

#' Employment matrix (2013 data)
#' This matrix contains the employed people by industry and the employment coefficient
#' that is the number of people divided by the total final demand from the wage and
#' demand matrix.
#' @name employment_matrix
#' @docType data
#' @author University of Bio-Bio, based on data from the National Bureau of Statistics
#' @usage wage_demand_matrix
#' @format A matrix with 12 rows and 2 columns
#' @references \url{https://revistas.ubiobio.cl/index.php/HHEE/article/download/3441/3473/}
#' @keywords data
NULL
