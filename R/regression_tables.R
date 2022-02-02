#' Regression Model Tables
#'
#' Display all tables relevant for Regression models. You can include up to
#' three models (x, y, and z) for hierarchical regression results.
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param coeff logical. Display the sjPlot coefficient table? You might want
#'     to generate your own custom sjPlot::tab_model() coefficient table and
#'     supress this one.
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

regression_tables <- function(x, y = NULL, z = NULL, coeff = TRUE, print = TRUE) {
  table_modelsig <- regression_modelsig(x, y, z, print = TRUE)
  table_rsquared <- regression_rsquared(x, y, z, print = print)
  table_coeff <- regression_coeff(x, y, z)

  print(table_modelsig)
  print(table_rsquared)
  if (coeff = TRUE) print(table_coeff)
}
