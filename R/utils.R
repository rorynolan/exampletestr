#' Does parsing of text give an error?
#'
#' Can a character vector (where each line is treated as a line of R code) be
#' parsed as an R expression (or several R expressions) without giving an
#' error?
#'
#' @param text_expr The expression to be evaluated, as a character vector.
#'
#' @return `TRUE` if the code gives an error and `FALSE` otherwise.
#' @examples
#' text_parse_error("a <- 1")
#' text_parse_error("a <- ")
#' @noRd
text_parse_error <- function(text_expr) {
  checkmate::assert_character(text_expr)
  try_res <- try(parse(text = text_expr), silent = TRUE)
  error <- inherits(try_res, "try-error")
  if (error) attr(error, "message") <- attr(try_res, "message")
  error
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
#' text_expr <- c(
#'   "a <- 1",
#'   "fx <- function(x) {",
#'   "  x + 1",
#'   "}  # this comment will disappear"
#' )
#' extract_expressions(text_expr)
#' @noRd
extract_expressions <- function(text_expr) {
  checkmate::assert_character(text_expr)
  text_expr %>%
    parse(text = .) %>%
    as.list() %>%
    purrr::map(deparse) %>%
    purrr::map(styler::style_text)
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
#'   write the test file based on documentation examples. Remember that
#'   this is something that you're intended to fill the gaps in later.
#'
#' @examples
#' text_expr <- c("sum(1, ", "2)")
#' cat(paste(text_expr, collapse = "\n"))
#' construct_expect_equal(text_expr)
#' cat(paste(construct_expect_equal(text_expr), collapse = "\n"))
#' @noRd
construct_expect_equal <- function(text_expr) {
  checkmate::assert_character(text_expr)
  text_expr[1] <- paste0("expect_equal(", text_expr[1])
  l <- length(text_expr)
  text_expr[l] <- paste0(text_expr[l], ", )")
  text_expr
}

#' Extract examples from a `.Rd` file as a character vector.
#'
#' This is a convenient wrapper to [tools::Rd2ex] which actually returns a
#' character vector of the examples in the `.Rd` file.
#'
#' @param rd_file_path The path to the `.Rd` file.
#'
#' @return A character vector.
#'
#' @examples
#' str_detect_rd <- system.file("extdata", "str_detect.Rd",
#'   package = "exampletestr"
#' )
#' extract_examples_rd(str_detect_rd)
#' @noRd
extract_examples_rd <- function(rd_file_path) {
  checkmate::assert_file_exists(rd_file_path)
  tc <- textConnection(" ", "w")
  tools::Rd2ex(rd_file_path, tc)
  examples_lines <- textConnectionValue(tc)
  close(tc)
  examples_lines
}

#' Is a function documented in a `.Rd` file?
#'
#' Scan the `.Rd` files in a package and look for the specified function.
#'
#' @param fun A string. The name of the function without parentheses or
#'   arguments.
#'
#' @return A boolean.
#' @noRd
is_documented <- function(fun) {
  checkmate::assert_string(fun)
  if (!fs::dir_exists(usethis::proj_path("man"))) {
    return(FALSE)
  }
  rd_files <- fs::dir_ls(usethis::proj_path("man"), regexp = "\\.Rd$")
  if (!length(rd_files)) {
    return(FALSE)
  }
  rd_contents <- purrr::map(rd_files, readr::read_lines, lazy = FALSE) %>%
    purrr::map_chr(stringr::str_c, collapse = "") %>%
    magrittr::set_names(rd_files)
  fun_pattern <- stringr::str_c(
    "(",
    stringr::str_escape("\\alias{"),
    fun,
    stringr::str_escape("}"),
    "|",
    stringr::str_escape("\\name{"),
    fun,
    stringr::str_escape("}"),
    ")"
  )
  for (i in seq_along(rd_contents)) {
    if (stringr::str_detect(rd_contents[i], fun_pattern)) {
      return(TRUE)
    }
  }
  FALSE
}

exampletestr_document <- function(usethis_quiet) {
  withr::with_options(list(usethis.quiet = usethis_quiet), {
    usethis::ui_info(paste(
      "Running",
      "{usethis::ui_code('roxygen2::roxygenize')}",
      ". . ."
    ))
    proj_location <- withr::with_options(
      list(usethis.quiet = TRUE),
      usethis::proj_get()
    )
    invisible(utils::capture.output(roxygen2::roxygenize(proj_location)))
    usethis::ui_done("Roxygenized :-)")
  })
}

make_available_test_file_name <- function(test_file_name) {
  checkmate::assert_string(test_file_name)
  withr::with_options(
    list(usethis.quiet = TRUE),
    {
      testthat_dir <- usethis::proj_path("tests", "testthat")
      checkmate::assert_directory_exists(testthat_dir)
    }
  )
  test_files <- fs::dir_ls(testthat_dir)
  if (test_file_name %in% test_files) {
    if (!stringr::str_detect(test_file_name, stringr::coll("-examples.R$"))) {
      test_file_name %<>%
        fs::path_ext_remove() %>%
        paste0("-examples") %>%
        strex::str_give_ext("R")
    }
    i <- 1
    while (test_file_name %in% test_files) {
      test_file_name %<>%
        strex::str_before_last("examples") %>%
        paste0("examples", "--", i) %>%
        strex::str_give_ext("R")
      i <- i + 1
    }
  }
  test_file_name
}

check_for_DESCRIPTION <- function() {
  if (!fs::file_exists(usethis::proj_path("DESCRIPTION"))) {
    rlang::abort(
      c(
        "Your package has no 'DESCRIPTION' file.",
        "i" = "Every R package must have a 'DESCRIPTION' in the root dir.",
        "i" = stringr::str_glue(
          "You specified the package '{usethis::proj_path()}'."
        )
      )
    )
  }
  invisible(TRUE)
}

check_for_man <- function() {
  if (!fs::dir_exists(usethis::proj_path("man"))) {
    rlang::abort(
      c(
        "Your package has no 'man/' folder.",
        "i" = "'exampletestr' looks for examples in the .Rd files in 'man/'.",
        "i" = stringr::str_glue("Package path used: '{usethis::proj_path()}'.")
      )
    )
  } else if (!length(fs::dir_ls(usethis::proj_path("man")))) {
    rlang::abort(
      c(
        "Your package has no .Rd files in the 'man/' folder.",
        "i" = "'exampletestr' looks for examples in the .Rd files in 'man/'.",
        "i" = stringr::str_glue("Package path used: '{usethis::proj_path()}'.")
      )
    )
  }
  invisible(TRUE)
}

#' Ensure that the package has testthat set up.
#'
#' Checks if testthat is setup. If not, runs [usethis::use_testthat()].
#'
#' @return `TRUE` invisiby.
#'
#' @noRd
ensure_testthat <- function() {
  if (!fs::dir_exists(usethis::proj_path("tests", "testthat"))) {
    usethis::use_testthat()
  }
  invisible(TRUE)
}
