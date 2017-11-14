#' Extract examples lines from the functions in a .R file of a package.
#'
#' In each `.R` file in the `R/` folder of a package project, for the functions
#' defined therein, there can corresponding examples in the `.Rd` files of the
#' man/` folder. This function extracts those examples into a list of character
#' vectors, one list element for each documented function.
#'
#' Anything found within a `\\dontrun\{...\}` block is ignored.
#'
#' @param r_file_name The name of the `.R` file within `R/`. There's no need to
#'   specify the file path (as `R/x.R`, but you can do this if you want), you
#'   can just use `x.R` for whichever file `x` it is. You can also omit the `.R`
#'   for convenience, however using the wrong case (e.g. `.r`) will produce an
#'   error.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). This is the parent directory of `R/` and `man/`.
#'
#' @examples
#' setwd(tempdir())
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", c("exemplar.R", "exampletestr.R"),
#'                       package = "exampletestr"), "R")
#' devtools::document()
#' exampletestr::extract_examples("exemplar")
#' exampletestr::extract_examples("exemplar")
#' setwd("..")
#' filesstrings::dir.remove("tempkg")
#' \dontrun{
#' extract_examples("non_existent_file")}
#'
#' @return A list of character vectors.
#' @export
extract_examples <- function(r_file_name, pkg_dir = ".") {
  if (stringr::str_detect(r_file_name, "/")) {
    r_file_name <- filesstrings::str_after_nth(r_file_name, "/", -1)
  }
  r_file_name <- stringr::str_c(pkg_dir, "/R/", r_file_name) %>%
    filesstrings::give_ext("R")
  r_file_lines_quotes_gone <- readLines(r_file_name) %>%
    formatR::tidy_source(text = ., comment = FALSE, arrow = TRUE,
                         output = FALSE, width.cutoff = 500) %>%
    getElement("text.tidy") %>%
    textConnection() %>%
    readLines() %>%
    filesstrings::remove_quoted()
  r_file_funs <- stringr::str_match(r_file_lines_quotes_gone,
                                    "(^[^ ]*) <- function\\(")[, 2] %>%
    stats::na.omit()
  rd_file_paths <- list.files(paste0(pkg_dir, "/man"), pattern = "\\.Rd$") %>%
    paste0(pkg_dir, "/man/", .)
  rd_file_lines <- lapply(rd_file_paths, readLines)
  rd_file_short_names <- rd_file_paths %>% filesstrings::before_last_dot() %>%
    filesstrings::str_after_nth("/", -1)
  names(rd_file_lines) <- rd_file_short_names
  documented_funs_in_alias_tags <- unlist(rd_file_lines) %>%
    stringr::str_extract("\\\\alias\\{.*\\}") %>%
    stats::na.omit()
  documented_funs <- stringr::str_sub(documented_funs_in_alias_tags, 8, -2) %>%
    stringr::str_trim()
  file_documented_funs <- intersect(documented_funs, r_file_funs)
  if (length(file_documented_funs) == 0) return(list(character(0)))
  documented_where <- paste0("\\alias{", file_documented_funs, "}") %>%
    purrr::map_int(function(x) {
      i <- 1
      while (!any(stringr::str_detect(rd_file_lines[[i]], stringr::coll(x)))) {
        i <- i + 1
      }
      as.integer(i)
    }) %>%
    purrr::map_chr(~ rd_file_short_names[.])
  wanted_rds <- unique(documented_where)
  wanted_rd_paths <- paste0(pkg_dir, "/man/",
                            filesstrings::give_ext(wanted_rds, "Rd"))
  examples <- lapply(wanted_rd_paths, extract_examples_rd)
  names(examples) <- wanted_rds
  ls_exs <- lengths(examples)
  if (filesstrings::all_equal(ls_exs, 0)) return(list(character(0)))
  examples[as.logical(lengths(examples))]
}

#' Make the shell of a `test_that` test.
#'
#' Given a character vector of the examples from a function, create the shell of
#' a [testthat::test_that()] code block (to be filled in by the user) based upon
#' those examples.
#'
#' Assignment lines (lines with `<-`, or even an `=` assignment (naughty, I
#' know)) and lines starting with `print(`, `stop(`, `warning(`, `setwd(`,
#' `plot(`, `ggplot(`, `set.seed` or `library(` are left alone, others are put
#' in the shell of an `expect_equal()` statement. To prevent anything from being
#' put in the shell of an `expect_equal()` statement, set `e_e = FALSE`.
#' Anything found within a `\\dontrun\{...\}` block is ignored.
#'
#' @param example_block A character vector of the lines in the examples of a
#'   function's documentation.
#' @param desc To be the `desc` argument of the [testthat::test_that()] call.
#' @param e_e Set this to `FALSE` to prevent anything from being put in the
#'   shell of an `expect_equal()` statement.
#'
#' @return A character vector giving the shell of a `test_that` function call
#'   testing all of the calls in the example block.
#'
#' @examples
#' setwd(tempdir())
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", c("exemplar.R", "exampletestr.R"),
#'                       package = "exampletestr"), "R")
#' devtools::document()
#' exampletestr::make_test_shell(exampletestr::extract_examples("exemplar")[[1]])
#' exampletestr::make_test_shell(exampletestr::extract_examples("exemplar")[[1]],
#'                               desc = "xyz", e_e = FALSE)
#' setwd("..")
#' filesstrings::dir.remove("tempkg")
#'
#' @export
make_test_shell <- function(example_block, desc = "", e_e = TRUE) {
  stopifnot(is.character(example_block))
  if (filesstrings::all_equal(example_block, character(0))) return(character(0))
  expressions <- extract_expressions(example_block)
  if (e_e) {
    for_checking <- expressions %>%
      lapply(filesstrings::remove_quoted) %>%
      lapply(stringr::str_replace_all, " ", "")
    leave_alone <- vapply(for_checking, function(x) {
      any(stringr::str_detect(x,
            paste0("(?:<-|^stop\\(|^warning\\(|^#|^setwd\\(|^library\\(|",
                   "^plot\\(|^ggplot\\(|^print\\(|^set\\.seed\\()")))
    }, logical(1))
    inside_test_that <- mapply(function(x, y) {
      if (x) {
        y
      } else {
        construct_expect_equal(y)
      }
    }, leave_alone, expressions, SIMPLIFY = FALSE) %>%
      unlist
  } else {
    inside_test_that <- unlist(expressions)
  }
  c(paste0("test_that(\"", desc, "\", {"),
    paste(" ", inside_test_that),  # this will prepend two spaces
    "})")
}

#' Create the shell of a test file.
#'
#' For a given file `x.R` in the `R/` directory of a package, for each function
#' defined in that `.R` file, `make_tests_shells_file` checks if there are
#' examples for that function detailed in the `man/` directory (in a `.Rd` file)
#' and if so creates a shell (skeleton) of a [testthat::test_that()] test based
#' on those examples via [make_test_shell()]. The created shells are then
#' written to a file `test_x.R` in `tests/testthat`.
#'
#' @param r_file_name The name of the `.R` file within `R/`. There's no need to
#'   specify the file path (as `R/x.R`, but you can do this if you want), you
#'   can just use `x.R` for whichever file `x` it is. You can also omit the `.R`
#'   for convenience, however using the wrong case (e.g. `.r` when the file
#'   actually has the extension `.R`) will produce an error.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of R/.
#' @param overwrite Overwrite if the test file you're trying to create already
#'   exists?
#' @param e_e Set this to `FALSE` to prevent anything from being put in the
#'   shell of an `expect_equal()` statement.
#'
#' @return The shell of the test file is written into tests/testthat. It has the
#'   same name as the .R file it was created from except it has "test_" tacked
#'   onto the front.
#' @examples
#' setwd(tempdir())
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", c("exemplar.R", "exampletestr.R"),
#'                       package = "exampletestr"), "R")
#' devtools::document()
#' exampletestr::make_tests_shells_file("exemplar")
#' devtools::document()
#' exampletestr::make_tests_shells_pkg(overwrite = TRUE)
#' setwd("..")
#' filesstrings::dir.remove("tempkg")
#'
#' @export
make_tests_shells_file <- function(r_file_name, pkg_dir = ".",
                                   overwrite = FALSE, e_e = TRUE) {
  checkmate::check_string(r_file_name)
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(pkg_dir)
  if (!dir.exists("tests/testthat")) devtools::use_testthat()
  if (stringr::str_detect(r_file_name, "/")) {
    r_file_name <- filesstrings::str_after_nth(r_file_name, "/", -1)
  }
  r_file_name <- filesstrings::give_ext(r_file_name, "R")
  exampless <- extract_examples(r_file_name, pkg_dir = ".")
  not_making_message <- paste0("No examples found for file \"",
                               r_file_name, "\", ",
                              "so no corresponding test file will be made ",
                              "for this file.")
  if (!length(exampless)) {
    message(not_making_message)
    return(invisible(character(0)))
  }
  test_shells <- mapply(make_test_shell, SIMPLIFY = FALSE,
                        exampless, paste(names(exampless), "works"), e_e = e_e)
  if (filesstrings::all_equal(unique(test_shells), list(character(0)))) {
    message(not_making_message)
    return(invisible(character(0)))
  }
  context <- r_file_name %>%
    filesstrings::before_last_dot() %>%
    filesstrings::str_split_camel_case() %>%
    unlist() %>%
    paste() %>%
    stringr::str_replace_all("[-_]", " ") %T>% {
      if (nchar(.) > 0) {
        . <- paste0(toupper(filesstrings::str_elem(., 1)),
                    stringr::str_sub(., 2, -1))
      }
    }
  combined <- c(paste0("context(\"", context, "\")"), "",
                purrr::reduce(test_shells, ~ c(.x, "", .y)))
  test_file_name <- paste0("tests/testthat/test_", r_file_name)
  if (!overwrite && file.exists(test_file_name)) {
    stop ("Stopping as to proceed would be to overwrite an existing test file:",
          " '", paste0("test_", r_file_name), "'. ",
          "To proceed, rerun with overwrite = TRUE.")
  }
  writeLines(combined, test_file_name)
  invisible(combined)
}

#' @rdname make_tests_shells_file
#' @export
make_tests_shells_pkg <- function(pkg_dir = ".", overwrite = FALSE,
                                  e_e = TRUE) {
  current_wd <- getwd()
  setwd(pkg_dir)
  on.exit(setwd(current_wd))
  list.files(path = "R") %>%
    lapply(make_tests_shells_file, overwrite = overwrite, e_e = e_e) %>%
    invisible
}
