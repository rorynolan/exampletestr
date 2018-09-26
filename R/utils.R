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
#' text_expr <- c("a <- 1",
#'                "fx <- function(x) {",
#'                "  x + 1",
#'                "}  # this comment will disappear")
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
#' this_function_rd <- system.file("extdata", "str_detect.Rd",
#'                                 package = "exampletestr")
#' extract_examples_rd(this_function_rd)
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
  init_wd <- getwd()
  on.exit(setwd(getwd()))
  setwd(rprojroot::find_package_root_file())
  if (!dir.exists(rprojroot::find_package_root_file("man"))) {
    return(FALSE)
  }
  setwd(rprojroot::find_package_root_file("man"))
  rd_files <- dir(pattern = "\\.Rd$")
  if (!length(rd_files)) return(FALSE)
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

cat_line <- function(...) cat(..., "\n", sep = "")

todo_bullet <- function() crayon::red(clisymbols::symbol$bullet)
done_bullet <- function() crayon::green(clisymbols::symbol$tick)

bulletize <- function(line, bullet = "*", .envir = parent.frame()) {
  line %<>% glue::glue(.envir = .envir)
  paste0(bullet, " ", line)
}

code <- function(...) {
  x <- paste0(...)
  crayon::make_style("darkgrey")(encodeString(x, quote = "`"))
}

empty_dir <- function(path) {
  checkmate::assert_directory(path)
  init_wd <- setwd(path)
  on.exit(setwd(init_wd))
  stuff <- dir()
  for (s in stuff) {
    if (isTRUE(checkmate::check_directory_exists(s))) {
      filesstrings::dir.remove(s)
    } else {
      file.remove(s)
    }
  }
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
  string %<>% strwrap(width = 57)
  string[1] %<>% {
    glue::glue("    * {.}")
  }
  if (length(string) > 1) {
    string[-1] %<>% {
      glue::glue("      {.}")
    }
  }
  glue::glue_collapse(string, sep = "\n")
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
  main_message %<>% glue::glue(.envir = .envir)
  out <- strwrap(main_message, width = 63)
  dots <- unlist(list(...))
  if (length(dots)) {
    if (!is.character(dots)) {
      stop("\nThe arguments in ... must all be of character type.")
    }
    dots %<>% purrr::map_chr(glue::glue, .envir = .envir) %>%
      purrr::map_chr(custom_stop_bullet)
    out %<>% {
      glue::glue_collapse(c(., dots), sep = "\n")
    }
  }
  rlang::abort(glue::glue_collapse(out, sep = "\n"))
}

#' Wrap messages to make them prettier.
#'
#' Format messages with line breaks so that single words don't appear on multiple lines.
#'
#' @param ... Bits of the message to be pasted together.
#'
#' @noRd
pretty_msg <- function(...) {
  dots <- unlist(list(...))
  checkmate::assert_character(dots)
  glue::glue_collapse(dots) %>%
    strwrap(width = 63) %>%
    glue::glue_collapse(sep = "\n") %>%
    message()
}
