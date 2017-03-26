#' Does evaluation of text give an error?
#'
#' Can a character vector (where each line is treated as a line of R code) be
#' evaluated as an R expression (or several R expressions) in the current
#' environment without giving an error?
#'
#' @param text_expr The expression to be evaluated, as a character vector.
#'
#' @return `TRUE` if the code gives an error and `FALSE` otherwise.
#' @examples
#' text_eval_error("a <- 1")
#' text_eval_error("a <- ")
#' @export
text_eval_error <- function(text_expr) {
  stopifnot(is.character(text_expr))
  try(source(textConnection(text_expr), local = parent.env(environment())),
      silent = TRUE) %>%
    inherits("try-error")
}

#' Evaluate a text string
#'
#' @param text_expr The expression to be evaluated, as a character vector.
#'
#' @examples
#' TextEval("3 + 4")
#' to.be.evaluated <- "var(c(1, 6, 8))"
#' TextEval(to.be.evaluated)
#'
#' @export
text_eval <- function(text_expr) {
  stopifnot(is.character(text_expr))
  source(textConnection(text_expr), local = parent.env(environment()))
}
