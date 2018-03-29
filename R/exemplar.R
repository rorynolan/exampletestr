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
#' @param document Run [devtools::document()] to update package documentation
#'   before starting?
#'
#' @examples
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", "detect.R", package = "exampletestr"), "R")
#' devtools::document()
#' exampletestr::extract_examples("detect")
#' setwd("..")
#' filesstrings::dir.remove("tempkg")
#'
#' @return A list of character vectors.
#' @export
extract_examples <- function(r_file_name, pkg_dir = ".", document = TRUE) {
  checkmate::assert_directory_exists(pkg_dir)
  if (stringr::str_detect(r_file_name, "/"))
    r_file_name <- filesstrings::str_after_last(r_file_name, "/")
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(pkg_dir)
  if (document) {
    if (file.exists(rprojroot::find_package_root_file("DESCRIPTION"))) {
      message("Running devtools::document() . . .")
      devtools::document()
    } else {
      stop("Your package has no DESCRIPTION file.",
           "    * Every R package must have a DESCRIPTION file ",
           "in the root directory.")
    }
  }
  r_file_name <- stringr::str_c(rprojroot::find_package_root_file("R"),
                                "/", r_file_name) %>%
    filesstrings::give_ext("R")
  checkmate::assert_file_exists(r_file_name)
  r_file_lines_quotes_gone <- readr::read_lines(r_file_name) %>%
    parse(text = .) %>%
    purrr::map(deparse) %>%
    unlist() %>%
    paste0("\n") %>%
    purrr::map(readr::read_lines) %>%
    unlist() %>%
    filesstrings::remove_quoted()
  r_file_funs <- stringr::str_match(r_file_lines_quotes_gone,
                                    "(^[^ ]*) <- function\\(")[, 2] %>%
    stats::na.omit()
  rd_file_paths <- list.files(rprojroot::find_package_root_file("man"),
                              pattern = "\\.Rd$") %>%
    paste0(rprojroot::find_package_root_file("man/"), .)
  rd_file_lines <- purrr::map(rd_file_paths, readr::read_lines)
  rd_file_short_names <- rd_file_paths %>% filesstrings::before_last_dot() %>%
    filesstrings::str_after_last("/")
  names(rd_file_lines) <- rd_file_short_names
  documented_funs_in_alias_tags <- unlist(rd_file_lines) %>%
    stringr::str_extract("\\\\alias\\{.*\\}") %>%
    stats::na.omit()
  documented_funs <- stringr::str_sub(documented_funs_in_alias_tags, 8, -2) %>%
    stringr::str_trim()
  file_documented_funs <- intersect(documented_funs, r_file_funs)
  if (!length(file_documented_funs)) return(list())
  documented_where <- paste0("\\alias{", file_documented_funs, "}") %>%
    purrr::map_int(function(x) {
      i <- 1
      while (!any(stringr::str_detect(rd_file_lines[[i]], stringr::coll(x))))
        i <- i + 1
      as.integer(i)
    }) %>%
    purrr::map_chr(~ rd_file_short_names[.])
  wanted_rds <- unique(documented_where)
  wanted_rd_paths <- paste0(rprojroot::find_package_root_file("man/"),
                            filesstrings::give_ext(wanted_rds, "Rd"))
  examples <- purrr::map(wanted_rd_paths, extract_examples_rd)
  names(examples) <- wanted_rds
  ls_exs <- lengths(examples)
  if (filesstrings::all_equal(ls_exs, 0)) return(list())
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
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", "detect.R", package = "exampletestr"), "R")
#' devtools::document()
#' make_test_shell(extract_examples("detect")[[1]])
#' make_test_shell(extract_examples("detect")[[1]],
#'                 desc = "xyz", e_e = FALSE)
#' setwd("..")
#' filesstrings::dir.remove("tempkg")
#'
#' @export
make_test_shell <- function(example_block, desc = "", e_e = TRUE) {
  checkmate::assert_character(example_block)
  if (!length(example_block)) return(character(0))
  expressions <- extract_expressions(example_block)
  if (e_e) {
    for_checking <- expressions %>%
      purrr::map(filesstrings::remove_quoted) %>%
      purrr::map(stringr::str_replace_all, " ", "")
    leave_alone <- purrr::map_lgl(for_checking,
      ~ any(stringr::str_detect(., paste0("(?:<-|^stop\\(|^warning",
                                          "\\(|^#|^setwd\\(|^library\\(|",
                                          "^plot\\(|^ggplot\\(|^print",
                                          "\\(|^set\\.seed\\()"))))
    inside_test_that <- purrr::map2(leave_alone, expressions,
      ~ if (.x) {
          .y
        } else {
          construct_expect_equal(.y)
        }
    ) %>%
      unlist()
  } else {
    inside_test_that <- unlist(expressions)
  }
  c(paste0("test_that(\"", desc, "\", {"),
    paste(" ", inside_test_that),  # this will prepend two spaces
    "})")
}

#' Create test shells.
#'
#' \itemize{\item For a given function `fun()` in a package,
#' [make_test_shell_fun()][test-shells] checks if there are examples for that
#' function detailed in the `man/` directory (in a `.Rd` file) and if so creates
#' a shell (skeleton) of a [testthat::test_that()] test based on those examples
#' via [make_test_shell()][test-shells]. The created shell is then written to a
#' corresponding file `test-fun-examples.R` in `tests/testthat`. \item For a
#' given file `x.R` in the `R/` directory of a package, for each function
#' defined in that `.R` file, [make_tests_shells_file()][test-shells] checks if
#' there are examples for that function detailed in the `man/` directory (in a
#' `.Rd` file) and if so creates a shell (skeleton) of a [testthat::test_that()]
#' test based on those examples via [make_test_shell()][test-shells]. The
#' created shells are then written to a corresponding file `test-x-examples.R`
#' in `tests/testthat`. \item [make_test_shells_pkg()][test-shells] runs
#' [make_test_shells_file()][test-shells] on every `.R` file in the `R/`
#' directory of a package.}
#'
#' @param r_file_name The name of the `.R` file within `R/`. There's no need to
#'   specify the file path (as `R/x.R`, but you can do this if you want), you
#'   can just use `x.R` for whichever file `x` it is. You can also omit the `.R`
#'   for convenience, however using the wrong case (e.g. `.r` when the file
#'   actually has the extension `.R`) will produce an error.
#' @param fun The name of the function to make a test shell for.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of R/.
#' @param overwrite Overwrite if the test file you're trying to create already
#'   exists?
#' @param e_e Set this to `FALSE` to prevent anything from being put in the
#'   shell of an `expect_equal()` statement.
#' @param open Open the created test file in your editor after it is created?
#' @inheritParams extract_examples
#'
#' @return The shell of the test file is written into tests/testthat. It has the
#'   same name as the .R file it was created from except it has "test_" tacked
#'   onto the front.
#' @examples
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", "detect.R", package = "exampletestr"), "R")
#' make_test_shell_fun("str_detect()", document = TRUE, open = FALSE)
#' make_tests_shells_file("detect", document = FALSE, open = FALSE)
#' make_tests_shells_pkg(overwrite = TRUE, document = FALSE)
#' setwd("..")
#' filesstrings::dir.remove("tempkg")
#'
#' @name test-shells
NULL

#' @rdname test-shells
#' @export
make_test_shell_fun <- function(fun, pkg_dir = ".",
                                overwrite = FALSE, e_e = TRUE,
                                open = TRUE, document = TRUE) {
  checkmate::assert_string(fun)
  checkmate::assert_directory_exists(pkg_dir)
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(pkg_dir)
  if (stringr::str_detect(fun, stringr::coll("("))) {
    if (filesstrings::str_elem(fun, 1) == "(") {
      stop("The function `fun` cannot start with a parenthesis.")
    }
    fun %<>% stringr::str_extract("^.*\\(") %>%
      stringr::str_sub(end = -2)
  }
  if (document) {
    if (file.exists(rprojroot::find_package_root_file("DESCRIPTION"))) {
      message("Running devtools::document() . . .")
      devtools::document()
    } else {
      stop("Your package has no DESCRIPTION file.",
           "    * Every R package must have a DESCRIPTION file ",
           "in the root directory.")
    }
  }
  examples <- list.files(rprojroot::find_package_root_file("R")) %>%
    purrr::map(extract_examples, document = FALSE) %>%
    purrr::reduce(c)
  available_funs <- names(examples)
  fun_index <- fun
  if (! fun %in% available_funs) {
    fun_found <- FALSE
    escaped_fun <- ore::ore.escape(fun)
    for (i in seq_along(examples)) {
      if (any(stringr::str_detect(examples[[i]],
                                  paste0("### Aliases: ", ".*",
                                         escaped_fun)))) {
        fun_index <- i
        fun_found <- TRUE
        break
      }
    }
    if (!fun_found) {
      stop("Could not find documented function `", fun, "()`.", "\n", "    ",
           "* Make sure it's documented in the man/ folder of your package.")
    }
  }
  examples %<>% {.[[fun_index]]}
  test_shell <- make_test_shell(examples, paste0(fun, "() works"), e_e = e_e)
  context <- paste0("context(\"", fun, "()\")")
  combined <- c(context, "", test_shell)
  test_file_name <- paste0(rprojroot::find_package_root_file("tests"),
                           "/testthat/test-", fun, "-examples") %>%
    filesstrings::give_ext("R")
  if (!overwrite && file.exists(test_file_name)) {
    stop ("Stopping as to proceed would be to overwrite an existing test file:",
          " '", paste0("test_", test_file_name), "'. ", "\n",
          "    * To proceed, rerun with overwrite = TRUE.")
  }
  if (!dir.exists(rprojroot::find_package_root_file("tests/testthat")))
    devtools::use_testthat()
  readr::write_lines(combined, test_file_name)
  if (open) file.edit(test_file_name)
  invisible(combined)
}

#' @rdname test-shells
#' @export
make_tests_shells_file <- function(r_file_name, pkg_dir = ".",
                                   overwrite = FALSE, e_e = TRUE,
                                   open = TRUE, document = TRUE) {
  checkmate::assert_string(r_file_name)
  checkmate::assert_directory_exists(pkg_dir)
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(pkg_dir)
  if (document) {
    if (file.exists(rprojroot::find_package_root_file("DESCRIPTION"))) {
      message("Running devtools::document() . . .")
      devtools::document()
    } else {
      stop("Your package has no DESCRIPTION file.", "\n",
           "    * Every R package must have a DESCRIPTION file ",
           "in the root directory.")
    }
  }
  if (stringr::str_detect(r_file_name, "/"))
    r_file_name <- filesstrings::str_after_last(r_file_name, "/")
  r_file_name <- filesstrings::give_ext(r_file_name, "R")
  exampless <- extract_examples(r_file_name, pkg_dir = ".", document = FALSE)
  not_making_message <- paste0("No examples found for file \"",
                               r_file_name, "\", ",
                              "so no corresponding test file will be made ",
                              "for this file.")
  if (!length(exampless)) {
    message(not_making_message)
    return(invisible(character(0)))
  }
  test_shells <- mapply(make_test_shell, SIMPLIFY = FALSE,
                        exampless, paste0(names(exampless), "() works"),
                        e_e = e_e)
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
  test_file_name <- paste0(rprojroot::find_package_root_file("tests"),
                           "/testthat/test-",
                           filesstrings::before_last_dot(r_file_name),
                           "-examples") %>%
    filesstrings::give_ext("R")
  if (!overwrite && file.exists(test_file_name)) {
    stop ("Stopping as to proceed would be to overwrite an existing test file:",
          " '", paste0("test_", r_file_name), "'. ", "\n",
          "    * To proceed, rerun with overwrite = TRUE.")
  }
  if (!dir.exists(rprojroot::find_package_root_file("tests/testthat")))
    devtools::use_testthat()
  readr::write_lines(combined, test_file_name)
  if (open) file.edit(test_file_name)
  invisible(combined)
}

#' @rdname test-shells
#' @export
make_tests_shells_pkg <- function(pkg_dir = ".", overwrite = FALSE,
                                  e_e = TRUE, open = FALSE, document = TRUE) {
  current_wd <- getwd()
  setwd(pkg_dir)
  on.exit(setwd(current_wd))
  if (document) {
    if (file.exists(rprojroot::find_package_root_file("DESCRIPTION"))) {
      message("Running devtools::document() . . .")
      devtools::document()
    } else {
      stop("Your package has no DESCRIPTION file.", "\n",
           "    * Every R package must have a DESCRIPTION file ",
           "in the root directory.")
    }
  }
  list.files(path = rprojroot::find_package_root_file("R")) %>%
    purrr::map(make_tests_shells_file, overwrite = overwrite, e_e = e_e,
               open = open, document = FALSE) %>%
    invisible()
}
