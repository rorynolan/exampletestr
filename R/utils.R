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
#' This is a convenient wrapper to [tools::Rd2ex] which actually returns a character vector of the examples in the `.Rd` file.
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
#' @param fun A string. The name of the function without parentheses or arguments.
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
  rd_contents <- purrr::map(rd_files, readr::read_lines) %>%
    purrr::map_chr(stringr::str_c, collapse = "") %>%
    magrittr::set_names(rd_files)
  fun_pattern <- stringr::str_c(
    "(",
    ore::ore.escape("\\alias{"),
    fun,
    ore::ore.escape("}"),
    "|",
    ore::ore.escape("\\name{"),
    fun,
    ore::ore.escape("}"),
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
    usethis::ui_info(paste("Running",
                           "{usethis::ui_code('roxygen2::roxygenize')}",
                           ". . ."))
    proj_location <- withr::with_options(list(usethis.quiet = TRUE),
                                         usethis::proj_get())
    invisible(utils::capture.output(roxygen2::roxygenize(proj_location)))
    usethis::ui_done("Roxygenized :-)")
  })
}

make_available_test_file_name <- function(test_file_name) {
  checkmate::assert_string(test_file_name)
  withr::with_options(
    list(usethis.quiet = TRUE), {
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
        filesstrings::give_ext("R")
    }
    i <- 1
    while (test_file_name %in% test_files) {
      test_file_name %<>%
        filesstrings::before_last("examples") %>%
        paste0("examples", "--", i) %>%
        filesstrings::give_ext("R")
      i <- i + 1
    }
  }
  test_file_name
}

#' Construct the bullet point bits for `custom_stop()`.
#'
#' @param string The message for the bullet point.
#'
#' @return A string with the bullet-pointed message nicely formatted for the
#'   console.
#'
#' @noRd
custom_stop_bullet <- function(string) {
  checkmate::assert_string(string)
  string %>%
    stringr::str_replace_all("\\s+", " ") %>% {
    glue::glue("    * {.}")
  }
}

#' Nicely formatted error message.
#'
#' Format an error message with bullet-pointed sub-messages with nice
#' line-breaks.
#'
#' Arguments should be entered as `glue`-style strings.
#'
#' @param main_message The main error message.
#' @param ... Bullet-pointed sub-messages.
#'
#' @noRd
custom_stop <- function(main_message, ..., .envir = parent.frame()) {
  checkmate::assert_string(main_message)
  main_message %<>%
    stringr::str_replace_all("\\s+", " ") %>%
    glue::glue(.envir = .envir)
  out <- main_message
  dots <- unlist(list(...))
  if (length(dots)) {
    if (!is.character(dots)) {
      stop("\nThe arguments in ... must all be of character type.")
    }
    dots %<>%
      purrr::map_chr(glue::glue, .envir = .envir) %>%
      purrr::map_chr(custom_stop_bullet)
    out %<>% {
      glue::glue_collapse(c(., dots), sep = "\n")
    }
  }
  rlang::abort(glue::glue_collapse(out, sep = "\n"))
}

check_for_DESCRIPTION <- function() {
  if (!fs::file_exists(usethis::proj_path("DESCRIPTION"))) {
    pkgdireq <- paste0("pkg_dir = \"", usethis::proj_path(), "\"")
    custom_stop(
      "Your package has no {usethis::ui_path('DESCRIPTION')} file.",
      "
      Every R package must have a {usethis::ui_path('DESCRIPTION')} file in the
      root directory.
      ",
      "Perhaps you specified the wrong {usethis::ui_code('pkg_dir')}?",
      "You specified {usethis::ui_code(pkgdireq)}."
    )
  }
  invisible(TRUE)
}

check_for_man <- function() {
  pkgdireq <- paste0("pkg_dir = \"", usethis::proj_path(), "\"")
  if (!fs::dir_exists(usethis::proj_path("man"))) {
    custom_stop(
      "
      Your package has no
      {usethis::ui_path(usethis::proj_path('man/'),
                        usethis::proj_path())} folder.",
      "
      {usethis::ui_code('exampletestr')} looks for examples in the
      {usethis::ui_path('*.Rd')} files in the
      {usethis::ui_path(usethis::proj_path('man/'), usethis::proj_path())}
      folder of a package and cannot function without them.
      "
    )
  } else if (!length(fs::dir_ls(usethis::proj_path("man")))) {
    custom_stop(
      "Your package has no {usethis::ui_path('*.Rd')} files in its
      {usethis::ui_path(usethis::proj_path('man'), usethis::proj_path())}
      folder.",
      "
      exampletestr looks for examples in the {usethis::ui_path('*.Rd')}
      files in the {usethis::ui_path(usethis::proj_path('man'),
                    usethis::proj_path())} folder of a package and cannot
      function if there are no {usethis::ui_path('*.Rd')} files there.
      "
    )
  }
  invisible(TRUE)
}
