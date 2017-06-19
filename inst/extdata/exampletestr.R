#' @importFrom magrittr "%>%"
#' @importFrom roxygen2 roclet
#' @importFrom devtools use_travis
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}
