#' Regression Model Fit
#'
#' Displays a table for the fit of regression models. Multiple models can
#' be added (x, y, and z).
#' @param x a model object
#' @param y a model object
#' @param z a model object
#' @param print Create a knitr table for displaying as html table?
#'     (default = TRUE)
#' @export
#'

regression_interaction <- function(x, y = NULL, z = NULL, print = TRUE) {
  x_formula <- insight::find_formula(x)$conditional
  x_n <- insight::model_info(x)$n_obs
  dv <- insight::find_response(x)

  x_table <- anova(x)
  x_table <- dplyr::mutate(x_table,
                           Term = rownames(x_table),
                           Model = "H1")

  x_table <- dplyr::select(x_table,
                           Model, Term, `Sum Sq`, Df, `Mean Sq`,
                           statistic, p.value)

  if (!is.null(y)) {
    y_formula <- insight::find_formula(y)$conditional
    y_n <- insight::model_info(y)$n_obs
    y_table <- anova(y)
    y_table <- dplyr::mutate(y_table,
                             Term = rownames(y_table),
                             Model = "H2")

    y_table <- dplyr::select(y_table,
                             Model, Term, `Sum Sq`, Df, `Mean Sq`,
                             statistic, p.value)
  } else {
    y_table <- data.frame()
    y_top <- data.frame()
  }
  if (!is.null(z)) {
    z_formula <- insight::find_formula(z)$conditional
    z_n <- insight::model_info(z)$n_obs
    z_table <- anova(z)
    z_table <- dplyr::mutate(z_table,
                             Term = rownames(z_table),
                             Model = "H3")

    z_table <- dplyr::select(z_table,
                             Model, Term, `Sum Sq`, Df, `Mean Sq`,
                             statistic, p.value)
  } else {
    z_table <- data.frame()
  }

  table <- dplyr::bind_rows(x_table, y_table, z_table)
  table <- dplyr::mutate(table, statistic = round(statistic, 3),
                         p.value = round(p.value, 3))
  table[is.na(table)] <- " "
  colnames(table) <- c("Model", "Term", "Sum of Squares", "df", "Mean Square",
                       "F-value", "p")

  if (print == TRUE){
    table <- knitr::kable(table, digits = 3, format = "html",
                          caption = paste("Model Summary Fit: ", dv, sep = ""),
                          row.names = FALSE,
                          align = c("l", "l", rep("c", 5)))
    table <- kableExtra::kable_classic(table, position = "left")
    table <- kableExtra::kable_styling(table, full_width = FALSE,
                                       position = "left")
    table <- kableExtra::collapse_rows(table, columns = 1, valign = "top")
    table <- kableExtra::row_spec(table, 0, bold = TRUE)
    if (is.null(y) & is.null(z)) {
      table <- kableExtra::footnote(table,
                                    number = paste("<small>", "H1: ", deparse(x_formula),
                                                   "; N = ", x_n, "</small>", sep = ""),
                                    escape = FALSE)
    } else if (!is.null(y) & is.null(z)) {
      table <- kableExtra::footnote(table,
                                    number = c(paste("<small>", "H1: ", deparse(x_formula),
                                                     "; N = ", x_n, "</small>", sep = ""),
                                               paste("<small>", "H2: ", deparse(y_formula),
                                                     "; N = ", y_n, "</small>", sep = "")),
                                    escape = FALSE)
    } else {
      table <- kableExtra::footnote(table,
                                    number = c(paste("<small>", "H1: ", deparse(x_formula),
                                                     "; N = ", x_n, "</small>", sep = ""),
                                               paste("<small>", "H2: ", deparse(y_formula),
                                                     "; N = ", y_n, "</small>", sep = ""),
                                               paste("<small>", "H3: ", deparse(z_formula),
                                                     "; N = ", z_n, "</small>", sep = "")),
                                    escape = FALSE)
    }
  } else if (print == FALSE){
    table <- as.data.frame(table)
  }

  return(table)
}
