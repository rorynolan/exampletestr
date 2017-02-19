#' Does evaluation of text give an error?
#'
#' Can a character vector (where each line is treated as a line of R code) be
#' evaluated as an R expression (or several R expressions) without giving an
#' error?
#'
#' @param text_expr The expression to be evaluated, as a character vector.
#'
#' @return `TRUE` if the code gives an error and `FALSE` otherwise.
#' @examples
#' text_eval_error("a <- 1")
#' text_eval_error("a <- ")
#' @export
text_eval_error <- function(text_expr) {
  try(parse(text = text_expr), silent = TRUE) %>% inherits("try-error")
}

#' Text expression groups.
#'
#' Given a character vector of R expressions, break the vector up into groups of
#' lines, where each group of lines is a valid R expression.
#'
#' @param text_expr A character vector.
#'
#' @return A list of character vectors, each of which can be evaluated as a
#'   valid R expression.
#' @examples
#' text_expr <- c("a <- 1",
#' "fx <- function(x) {",
#' "paste('f', x)",
#' "}")
#' extract_expressions(text_expr)
#' @export
extract_expressions <- function(text_expr) {
  expr_groups <- list()
  i <- 1
  while (i <= length(text_expr)) {
    j <- 0
    expr <- text_expr[i]
    while(text_eval_error(expr)) {
      j <- j + 1
      expr <- text_expr[i:(i + j)]
    }
    expr_groups <- append(expr_groups, list(expr))
    i <- i + j + 1
  }
  expr_groups
}

#' Evaluate a text string
#'
#' @param string The string to evaluate (as if it were a command).
#'
#' @examples
#' TextEval("3 + 4")
#' to.be.evaluated <- "var(c(1, 6, 8))"
#' TextEval(to.be.evaluated)
#'
#' @export
TextEval <- function(string) {
  stopifnot(is.character(string) && length(string) == 1)
  eval(parse(text = string))
}

#' Construct an `expect_equal` expression
#'
#' Construct an `expect_equal` expression from a character vector
#' containing an expression to be evaluated.
#'
#' @param text_expr A character vector of lines that, when executed produce a
#'   single output.
#'
#' @return A character vector. The lines of text containing the
#'   `expect_equal` code corresponding to the input, which will help to
#'   write the test file based on an example detailed with roxgen. Remember that
#'   this is something that you're intended to fill the gaps in later.
#'
#' @examples
#' text_expr <- c("sum(1, ", "2)")
#' cat(paste(text_expr, collapse = "\n"))
#' construct_expect_equal(text_expr)
#' cat(paste(construct_expect_equal(text_expr), collapse = "\n"))
#' @export
construct_expect_equal <- function(text_expr) {
  text_expr[1] <- paste0("expect_equal(", text_expr[1])
  l <- length(text_expr)
  text_expr[l] <- paste0(text_expr[l], ", )")
  text_expr
}
