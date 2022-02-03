#' Standardized Regression Model
#'
#' Convert an unstandardized lm model object into a standardized lm model object.
#'     This standardizes all variables in the model and reruns the lm() on the
#'     standardized values.
#' @param x a model object
#' @export
#'

get_standardized <- function(x) {
  x_data <- insight::get_data(x)
  x_data <- dplyr::mutate_if(x_data, is.numeric, scale)
  x_formula <- insight::find_formula(x)$conditional
  model_standardized <- lm(x_formula, data = x_data)
  return(model_standardized)
}
