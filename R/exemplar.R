#' Create test shells.
#'
#' \itemize{\item For a given function `fun()` in a package,
#' [make_test_shell_fun()][make-test-shells] checks if there are examples for
#' that function detailed in the 'man/' directory (in a '.Rd' file) and if so
#' creates a shell (skeleton) of a [testthat::test_that()] test based on those
#' examples. The created shell is then written to a corresponding file
#' 'test-fun.R' (or if that already exists, 'test-fun-examples.R') in
#' 'tests/testthat'. \item For a given file 'x.R' in the 'R/' directory of a
#' package, for each function defined in that '.R' file,
#' [make_tests_shells_file()][make-test-shells] checks if there are examples for
#' that function detailed in the 'man/' directory (in a '.Rd' file) and if so
#' creates a shell (skeleton) of a [testthat::test_that()] test based on those
#' examples via [make_test_shell()][make-test-shells]. The created shells are
#' then written to a corresponding file 'test-x.R' (or  'test-x-examples.R' if
#' 'test-x.R' is already taken) in 'tests/testthat'. \item
#' [make_test_shells_pkg()][make-test-shells] runs
#' [make_test_shells_file()][make-test-shells] on every '.R' file in the 'R/'
#' directory of a package.}
#'
#' @param r_file_name The name of the '.R' file within 'R/'. There's no need to
#'   specify the file path (as 'R/x.R', but you can do this if you want), you
#'   can just use 'x.R' for whichever file 'x' it is. You can also omit the '.R'
#'   for convenience, however using the wrong case (e.g. '.r' when the file
#'   actually has the extension '.R') will produce an error.
#' @param fun The name of the function to make a test shell for.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of R/.
#' @param overwrite Overwrite if the test file you're trying to create already
#'   exists?
#' @param e_e Set this to `FALSE` to prevent anything from being put in the
#'   shell of a [testthat::expect_equal()] statement.
#' @param open Open the created test file in your editor after it is created?
#' @param document Run [roxygen2::roxygenize()] to update package documentation
#'   before starting?
#'
#' @return The shell of the test file is written into tests/testthat. It has the
#'   same name as the .R file it was created from except it has "test_" tacked
#'   onto the front.
#'
#' @examples
#' pkg_dir <- paste0(tempdir(check = TRUE), "/tmpkg")
#' usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
#' fs::file_copy(
#'   system.file("extdata", c("detect.R", "match.R"),
#'     package = "exampletestr"
#'   ),
#'   paste0(pkg_dir, "/R")
#' )
#' make_test_shell_fun("str_detect()", pkg_dir,
#'   document = TRUE, open = FALSE
#' )
#' make_tests_shells_file("detect", pkg_dir,
#'   document = FALSE, open = FALSE
#' )
#' make_tests_shells_pkg(pkg_dir,
#'   overwrite = TRUE, document = FALSE
#' )
#' fs::dir_delete(pkg_dir)
#'
#' @name make-test-shells
NULL

#' @rdname make-test-shells
#' @export
make_test_shell_fun <- function(fun, pkg_dir = ".",
                                overwrite = FALSE, e_e = TRUE,
                                open = TRUE, document = TRUE) {
  checkmate::assert_string(fun)
  checkmate::assert_directory_exists(pkg_dir)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(e_e)
  checkmate::assert_flag(open)
  checkmate::assert_flag(document)
  usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
  usethis::local_project(pkg_dir, quiet = TRUE)
  check_for_DESCRIPTION()
  if (document) exampletestr_document(usethis_quiet_init)
  check_for_man()
  fun %<>% stringr::str_trim()
  if (stringr::str_detect(fun, stringr::coll("("))) {
    if (filesstrings::str_elem(fun, 1) %in% c("(", ")")) {
      custom_stop(
        "The function 'fun' cannot start with a parenthesis.",
        "Your 'fun' is '{fun}'."
      )
    }
    fun %<>% stringr::str_extract("^.*\\(") %>%
      stringr::str_sub(end = -2)
  }
  examples <- fs::dir_ls(usethis::proj_path("R")) %>%
    purrr::map(extract_examples, pkg_dir = pkg_dir, document = FALSE) %>%
    purrr::reduce(c)
  available_funs <- names(examples)
  fun_index <- fun
  if (!fun %in% available_funs) {
    fun_found <- FALSE
    escaped_fun <- ore::ore.escape(fun)
    for (i in seq_along(examples)) {
      if (any(stringr::str_detect(
        examples[[i]],
        paste0(
          "### Aliases: ", ".*",
          escaped_fun
        )
      ))) {
        fun_index <- i
        fun_found <- TRUE
        break
      }
    }
    if (!fun_found) {
      if (is_documented(fun)) {
        custom_stop(
          "
          The function {usethis::ui_code(stringr::str_c(fun, '()'))}
          is documented but has no accompanying examples.
          ", "
          {usethis::ui_code('make_test_shell_fun()')} only works on functions
          with examples.
          "
        )
      }
      custom_stop(
        "
        Could not find a documented function called
        {usethis::ui_code(stringr::str_c(fun, '()'))}.
        ", "
        Make sure it's documented in the {usethis::ui_code('man/')} folder of
        your package.
        "
      )
    }
  }
  examples %<>% {
    .[[fun_index]]
  }
  test_shell <- make_test_shell(examples, paste0("`", fun, "()` works"),
    e_e = e_e
  )
  test_file_name <- usethis::proj_path("tests", "testthat",
                                       paste0("test-", fun, "-examples"),
                                       ext = "R")
  if (!fs::dir_exists(usethis::proj_path("tests", "testthat")))
    usethis::use_testthat()
  if (!overwrite) test_file_name %<>% make_available_test_file_name()
  readr::write_lines(test_shell, test_file_name)
  withr::with_options(list(usethis.quiet = usethis_quiet_init), {
    usethis::ui_info(
      "Wrote {usethis::ui_path(usethis::proj_path(test_file_name),
                               usethis::proj_path())}."
      )
    if (open) file.edit(test_file_name)
    usethis::ui_todo(
      glue::glue("Complete the unit tests in ",
                 "{usethis::ui_path(usethis::proj_path(test_file_name),
                                    usethis::proj_path())}."
      )
    )
  })
  invisible(test_shell)
}

#' @rdname make-test-shells
#' @export
make_tests_shells_file <- function(r_file_name, pkg_dir = ".",
                                   overwrite = FALSE, e_e = TRUE,
                                   open = TRUE, document = TRUE) {
  checkmate::assert_string(r_file_name)
  checkmate::assert_directory_exists(pkg_dir)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(e_e)
  checkmate::assert_flag(open)
  checkmate::assert_flag(document)
  usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
  usethis::local_project(pkg_dir, quiet = TRUE)
  check_for_DESCRIPTION()
  if (document) exampletestr_document(usethis_quiet_init)
  check_for_man()
  if (stringr::str_detect(r_file_name, "/"))
    r_file_name %<>% filesstrings::after_last("/")
  r_file_name %<>% filesstrings::give_ext("R")
  exampless <- extract_examples(r_file_name,
                                pkg_dir = pkg_dir, document = FALSE)
  if (!length(exampless)) {
    usethis::ui_info(glue::glue(
      "No examples found for file '{r_file_name}', ",
      "so no corresponding test file will be made for this file."
    ))
    return(invisible(character(0)))
  }
  test_shells <- purrr::pmap(
    list(
      example_block = exampless,
      desc = glue::glue("`{names(exampless)}()` works")
    ),
    make_test_shell,
    e_e = e_e
  )
  combined <- purrr::reduce(test_shells, ~ c(.x, "", .y))
  if (!fs::dir_exists(usethis::proj_path("tests", "testthat")))
    usethis::use_testthat()
  test_file_name <- usethis::proj_path(
    "tests", "testthat", paste0("test-", r_file_name)
  )
  if (!overwrite) test_file_name %<>% make_available_test_file_name()
  readr::write_lines(combined, test_file_name)
  withr::with_options(list(usethis.quiet = usethis_quiet_init), {
    usethis::ui_info(
      "Wrote {usethis::ui_path(usethis::proj_path(test_file_name),
                               usethis::proj_path())}."
      )
    if (open) file.edit(test_file_name)
    usethis::ui_todo(
      glue::glue("Complete the unit tests in ",
                 "{usethis::ui_path(usethis::proj_path(test_file_name),
                                    usethis::proj_path())}."
      )
    )
  })
  invisible(combined)
}

#' @rdname make-test-shells
#' @export
make_tests_shells_pkg <- function(pkg_dir = ".", overwrite = FALSE,
                                  e_e = TRUE, open = FALSE, document = TRUE) {
  checkmate::assert_directory_exists(pkg_dir)
  checkmate::assert_flag(overwrite)
  checkmate::assert_flag(e_e)
  checkmate::assert_flag(open)
  checkmate::assert_flag(document)
  usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
  usethis::local_project(pkg_dir, quiet = TRUE)
  check_for_DESCRIPTION()
  if (document) exampletestr_document(usethis_quiet_init)
  check_for_man()
  if (!fs::dir_exists(usethis::proj_path("R")) ||
      length(fs::dir_ls(usethis::proj_path("R"))) == 0) {
    withr::with_options(list(usethis.quiet = usethis_quiet_init), {
      usethis::ui_info(
        glue::glue("No files found in the ",
                   "{usethis::ui_path(usethis::proj_path('R'),
                                      usethis::proj_path())} directory ",
                   "of the package so no test shells created.")
      )
    })
    return(invisible(list()))
  } else {
    r_files <- fs::dir_ls(usethis::proj_path("R"))
  }
  withr::with_options(list(usethis.quiet = usethis_quiet_init), {
    out <- purrr::map(r_files, make_tests_shells_file,
                      pkg_dir = pkg_dir, overwrite = overwrite, e_e = e_e,
                      open = open, document = FALSE
    )
    usethis::ui_done("Finished creating test shells for your package.")
    invisible(out)
  })
}
