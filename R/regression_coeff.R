#' Regression Coefficients
#'
#' Displays a table for regression coefficients.
#'      Multiple models can be added (x, y, and z).
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param ci display unstandardized confidence intervals? default = FALSE
#' @param se display standard errors? default = FALSE
#' @export
#'

regression_coeff <- function(x, y = NULL, z = NULL,
                             ci = FALSE, se = FALSE) {
  table <- get_coeff(x, model = "H1")
  x_formula <- insight::find_formula(x)$conditional
  dv <- insight::find_response(x)

  if (!is.null(y)) {
    y_table <- get_coeff(y, model = "H2")
    y_formula <- insight::find_formula(y)$conditional
    table <- dplyr::bind_rows(table, y_table)
    }
  if (!is.null(z)) {
    z_table <- get_coeff(z, model = "H3")
    z_formula <- insight::find_formula(z)$conditional
    table <- dplyr::bind_rows(table, z_table)
  }

  table <- dplyr::mutate(table,
                         Lower.CI_unstd = round(Lower.CI_unstd, 3),
                         Upper.CI_unstd = round(Upper.CI_unstd, 3),
                         Lower.CI = round(Lower.CI, 3),
                         Upper.CI = round(Upper.CI, 3))

  table <- tidyr::unite(table, "CI_unstd",
                        c("Lower.CI_unstd", "Upper.CI_unstd"), sep = " - ")

  table <- tidyr::unite(table, "CI",
                        c("Lower.CI", "Upper.CI"), sep = " - ")

  if (ci == FALSE) {
    table <- dplyr::select(table, -Lower.CI_unstd, -Upper.CI_unstd)
    if (se == FALSE) {
      header_names <- c("Model", "Term", "b", "B",
                        "95% CI", "t", "p")
      table <- dplyr::select(table, -SE, -SE_B)
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names)
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left",
                                         bootstrap_options = "striped")
      table <- kableExtra::add_header_above(table, c(" ", " ", " ",
                                                     "Standardized" = 3,
                                                     " ", " "))
    } else {
      header_names <- c("Model", "Term", "b", "SE", "B", "SE",
                        "95% CI", "t", "p")
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names)
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left",
                                         bootstrap_options = "striped")
      table <- kableExtra::add_header_above(table, c(" ", " ", " ", " ",
                                                     "Standardized" = 4,
                                                     " ", " "))
    }
  } else {
    if (se == FALSE) {
      header_names <- c("Model", "Term", "b", "95% CI", "B",
                        "95% CI", "t", "p")
      table <- dplyr::select(table, -SE, -SE_B)
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names)
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left",
                                         bootstrap_options = "striped")
      table <- kableExtra::add_header_above(table, c(" ", " ", " ", " ", " ",
                                                     "Standardized" = 3,
                                                     " ", " "))
    } else {
      header_names <- c("Model", "Term", "b", "SE", "95% CI",
                        "B", "SE", "95% CI", "t", "p")
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = paste("Regression Coefficients: ",
                                            dv, sep = ""),
                            row.names = FALSE,
                            col.names = header_names)
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left",
                                         bootstrap_options = "striped")
      table <- kableExtra::add_header_above(table, c(" ", " ", " ", " ", " ", " ",
                                                     "Standardized" = 4,
                                                     " ", " "))
    }
  }

  table <- kableExtra::add_footnote(table,
                                    label = paste("H1: ", x_formula, sep = ""),
                                    notation = "none")
  if (!is.null(y)) {
    table <- kableExtra::add_footnote(table,
                                      label = paste("H2: ", y_formula, sep = ""),
                                      notation = "none")
  }
  if (!is.null(z)) {
    table <- kableExtra::add_footnote(table,
                                      label = paste("H3: ", z_formula, sep = ""),
                                      notation = "none")
  }

  return(table)
}
