#' Create Coefficients Data Frame
#'
#' @param x an lm model object
#' @param model The model number (e.g., "H1")
#' @export
#'

get_coeff <- function(x, model = "H1") {
  table <- broom::tidy(x, conf.int = TRUE)
  table <- dplyr::mutate(table,
                         Model = ifelse(term == "(Intercept)", model, ""))

  table <- dplyr::select(table, Model, Term = term, b = estimate,
                         SE = std.error, Lower.CI_unstd = conf.low,
                         Upper.CI_unstd = conf.high,
                         t = statistic, p = p.value)

  x_standardized <- regression_standardized(x)
  table_std <- broom::tidy(x_standardized, conf.int = TRUE)
  table_std <- dplyr::select(table_std, B = estimate, SE_B = std.error,
                             Lower.CI = conf.low, Upper.CI = conf.high)

  table <- dplyr::bind_cols(table, table_std)
  table <- dplyr::relocate(table, t, p, .after = Upper.CI)
  return(table)
}
