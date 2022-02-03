#' Create Coefficients Data Frame
#'
#' @param x an lm model object
#' @export
#'

get_coeff <- function(x) {
  table <- broom::tidy(x, conf.int = TRUE)
  table <- dplyr::mutate(table)

  table <- dplyr::select(table, Term = term, b = estimate,
                         SE = std.error, Lower.CI_unstd = conf.low,
                         Upper.CI_unstd = conf.high,
                         t = statistic, p = p.value)

  x_standardized <- get_standardized(x)
  table_std <- broom::tidy(x_standardized, conf.int = TRUE)
  table_std <- dplyr::select(table_std, B = estimate, SE_B = std.error,
                             Lower.CI = conf.low, Upper.CI = conf.high)

  table <- dplyr::bind_cols(table, table_std)
  table <- dplyr::relocate(table, t, p, .after = Upper.CI)
  return(table)
}
