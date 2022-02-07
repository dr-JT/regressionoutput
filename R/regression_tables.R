#' Regression Model Tables
#'
#' Display all tables relevant for Regression models. You can include up to
#' three models (x, y, and z) for hierarchical regression results.
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param ci display unstandardized confidence intervals? default = TRUE
#' @param se display standard errors? default = TRUE
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

regression_tables <- function(x, y = NULL, z = NULL,
                              ci = TRUE, se = TRUE,
                              print = TRUE) {
  table_rsquared <- regression_rsquared(x, y, z, print = TRUE)
  table_modelsig <- regression_modelsig(x, y, z, print = TRUE)
  table_coeff <- regression_coeff(x, y, z, ci = ci, se = se)

  print(table_rsquared)
  print(table_modelsig)
  print(table_coeff)
}
