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

  if (!is.null(y)) {
    y_table <- get_coeff(y, model = "H2")
    table <- dplyr::bind_rows(table, y_table)
  }
  if (!is.null(z)) {
    z_table <- get_coeff(z, model = "H3")
    table <- dplyr::bind_rows(table, z_table)
  }

  if (ci == FALSE) {
    table <- dplyr::select(table, -Lower.CI_unstd, -Upper.CI_unstd)
    if (se == FALSE) {
      header_names <- c("Model", "Term", "b", "B",
                        "Lower CI", "Upper CI", "t", "p")
      table <- dplyr::select(table, -SE, -SE_B)
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Regression Coefficients",
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
                        "Lower CI", "Upper CI", "t", "p")
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Regression Coefficients",
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
      header_names <- c("Model", "Term", "b", "Lower CI", "Upper CI", "B",
                        "Lower CI", "Upper CI", "t", "p")
      table <- dplyr::select(table, -SE, -SE_B)
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Regression Coefficients",
                            row.names = FALSE,
                            col.names = header_names)
      table <- kableExtra::kable_styling(table, full_width = FALSE,
                                         position = "left",
                                         bootstrap_options = "striped")
      table <- kableExtra::add_header_above(table, c(" ", " ", " ", " ", " ",
                                                     "Standardized" = 3,
                                                     " ", " "))
    } else {
      header_names <- c("Model", "Term", "b", "SE", "Lower CI", "Upper CI",
                        "B", "SE", "Lower CI", "Upper CI", "t", "p")
      table <- knitr::kable(table, digits = 3, format = "html",
                            caption = "Regression Coefficients",
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

  return(table)
}
