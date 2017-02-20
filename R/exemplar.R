#' Extract examples lines from a roxygen-documented .R file.
#'
#' In each .R file in the R/ folder of a pcakage project, there can be examples
#' within documented via the [roxygen2][roxygen2::roxygen2] under the
#' `@examples` roxygen tag.
#'
#' Anything examples found within or after a \\dontrun\{\} block are ignored.
#'
#' @param r_file_name The name of the .R file within R/. Don't specify this as
#'   "R/x.R", just use "x.R" for whichever file x it is. You can also omit the
#'   .R for convenience, however using the wrong case (e.g. .r) will produce an
#'   error. If instead, you wish to set the full path to the file here, set
#'   \code{pkg_dir} to \code{NULL}.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of R/. If you
#'   want to specify the full file path in the \code{r_file_name} argument, set
#'   \code{pkg_dir} to \code{NULL}.
#'
#' @examples
#' if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
#' "'tempkg' folder.")
#' dir.create("tempkg")
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", "exemplar.R", package = "exampletestr"), "R")
#' extract_examples("exemplar")
#' extract_examples("exemplar")
#' setwd("..")
#' filesstrings::RemoveDirs("tempkg")
#' \dontrun{
#' extract_examples("non_existent_file")}
#'
#' @return A charachter vector.
#' @export
extract_examples <- function(r_file_name, pkg_dir = ".") {
  r_file_lines <- ifelse(is.null(pkg_dir), r_file_name,
                         stringr::str_c(pkg_dir, "/R/", r_file_name)) %>%
    filesstrings::MakeExtName("R") %>%
    readLines %>%
    stringr::str_trim() %>% {
      .[as.logical(nchar(.))]
    }
  roxygen_line_indices <- r_file_lines %>%
    {stringr::str_locate(., "#'")[, "start"] == 1} %>%
    which
  roxygen_index_groups <- filesstrings::GroupClose(roxygen_line_indices)
  roxygen_line_groups <- lapply(roxygen_index_groups,
                                function(x) r_file_lines[x]) %>%
    lapply(stringr::str_sub, 3, -1) %>%  # remove roxygen tags
    lapply(stringr::str_trim) %>%  # remove whitespace padding
    {.[as.logical(nchar(.))]}  # remove empty lines
  atexs <- "@examples"
  startwith_atexs <- roxygen_line_groups %>%
    lapply(stringr::str_locate, atexs) %>%
    vapply(function(x) any(x == 1, na.rm = TRUE), logical(1))
  roxygen_index_groups <- roxygen_index_groups[startwith_atexs]
  roxygen_line_groups <- roxygen_line_groups[startwith_atexs]  # restrict both
    # of these to what we're interested in (examples)
  line_indices_after_roxygens <- roxygen_index_groups %>%
    vapply(BBmisc::getLast, integer(1)) + 1
  lines_after_roxygens <- r_file_lines[line_indices_after_roxygens]
  are_all_functions <- lines_after_roxygens %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_detect(stringr::coll("<-function(")) %>%
    all
  if (!are_all_functions) {
    stop("Lines in the .R file which follow roxygen blocks which contain an ",
         "@examples tag must be the start of a function definition and hence ",
         "contain ' <- function(' or something like that. ",
         "The equals assignment '= function(' is not allowed.")
  }
  function_names <- lines_after_roxygens %>%
    stringr::str_split_fixed("<-", 2) %>% {
      stringr::str_trim(.[, 1])
    }
  at_tag_line_indices <- roxygen_line_groups %>%
    lapply(function(x) filesstrings::StrElem(x, 1) == "@") %>%
    lapply(which)
  atex_line_indices <- roxygen_line_groups %>%
    lapply(function(x) stringr::str_sub(x, 1, nchar(atexs)) == atexs) %>%
    lapply(which)
  if (!length(atex_line_indices)) {
    return(list())
  }
  if (unique(vapply(atex_line_indices, length, integer(1))) != 1) {
    stop("Each roxygen block which documents a function ",
         "should contain at most 1 @examples tag.")
  } else {
    atex_line_indices <- unlist(atex_line_indices)
  }
  at_tags_after_exs <- mapply(function(x, y) x[x > y], SIMPLIFY = FALSE,
                              at_tag_line_indices, atex_line_indices)
  exs_lines_index_ends <- mapply(function(x, y) ifelse(length(x),
                                                       x - 1, length(y)),
                           at_tags_after_exs, roxygen_line_groups)
  exs_lines <- mapply(function(x, y, z) x[y:z], SIMPLIFY = FALSE,
    roxygen_line_groups, atex_line_indices, exs_lines_index_ends) %>%
    lapply(function(x) {
      x[1] <- stringr::str_sub(x[1], nchar(atexs) + 1, -1)
      x
    }) %>% lapply(function(x) x[as.logical(nchar(stringr::str_trim(x)))])
  for (i in seq_along(exs_lines)) {
    if (any(stringr::str_detect(exs_lines[[i]], "^\\\\dontrun\\{"))) {
      exs_lines[[i]] <- exs_lines[[i]] %>% {
        .[seq_len(match(T, stringr::str_detect(., "^\\\\dontrun\\{")) - 1)]
      }
    }
  }
  names(exs_lines) <- function_names
  exs_lines
}

#' Make the shell of a `test_that` test.
#'
#' Given a character vector of the examples from a function, create the shell of
#' a `test_that` code block (to be filled in by the user) based upon those
#' examples.
#'
#' @param example_block A character vector of the lines in the examples of a
#'   function's documentation.
#' @param desc To be the `desc` argument of the
#'   [testthat::test_that()] call.
#'
#' @return A character vector giving the shell of a test_that function call
#'   testing all of the calls in the example block.
#'
#' @examples
#' if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
#' "'tempkg' folder.")
#' dir.create("tempkg")
#' devtools::create("tempkg")
#' setwd("tempkg")
#' file.copy(system.file("extdata", "exemplar.R", package = "exampletestr"),
#' "R", overwrite = TRUE)
#' make_test_shell(extract_examples("exemplar")[[1]])
#' setwd("..")
#' filesstrings::RemoveDirs("tempkg")
#'
#' @export
make_test_shell <- function(example_block, desc = "") {
  stopifnot(is.character(example_block))
  expressions <- extract_expressions(example_block)
  for_checking <- expressions %>%
    lapply(filesstrings::RemoveQuoted) %>%
    lapply(stringr::str_replace_all, " ", "")
  leave_alone <- vapply(for_checking, function(x) {
      any(stringr::str_detect(x, "(?:<-|setwd\\(|stop\\(|warning\\(|^#)"))
    }, logical(1))
  plots <- vapply(for_checking, function(x) {
    any(stringr::str_detect(x, "(?:plot\\()"))  # this will catch ggplot( too
  }, logical(1))
  inside_test_that <- mapply(function(x, y) {
    if (x) {
      y
    } else {
      construct_expect_equal(y)
    }
  }, leave_alone, expressions, SIMPLIFY = FALSE) %>% {
    .[!plots]
    } %>% unlist
  c(paste0("test_that(\"", desc, "\", {"),
    paste(" ", inside_test_that),  # this will prepend two spaces
    "})")
}

#' Create the shell of a test file based on roxygen examples.
#'
#' Based on the roxygen-specified examples in a .R file in the R/ directory of
#' the package, \code{make_tests_shells_file} create the shell of a test file,
#' to be completed by the user. \code{make_tests_shells_pkg} does this for every
#' file in R/.
#'
#' @param r_file_name The name of the .R file within R/. Don't specify this as
#'   "R/x.R", just use "x.R" for whichever file x it is. You can also omit the
#'   .R for convenience, however using the wrong case (e.g. .r) will produce an
#'   error. If instead, you wish to set the full path to the file here, set
#'   \code{pkg_dir} to \code{NULL}.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of R/.
#' @param overwrite Overwrite if the test file you're trying to create already
#'   exists?
#'
#' @return The shell of the test file is written into tests/testthat. It has the
#'   same name as the .R file it was created from except it has "test_" tacked
#'   onto the front.
#' @examples
#' if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
#' "'tempkg' folder.")
#' dir.create("tempkg")
#' devtools::create("tempkg")
#' setwd("tempkg")
#' devtools::use_testthat()
#' file.copy(system.file("extdata", "exemplar.R", package = "exampletestr"),
#' "R", overwrite = TRUE)
#' make_tests_shells_file("exemplar")
#' file.copy(system.file("extdata", "exampletestr.R", package = "exampletestr"),
#' "R", overwrite = TRUE)
#' make_tests_shells_pkg(overwrite = TRUE)
#' # Now check your tempkg/tests/testthat directory to see what they look like
#' # The next two lines clean up
#' setwd("..")
#' filesstrings::RemoveDirs("tempkg")
#'
#' @export
make_tests_shells_file <- function(r_file_name, pkg_dir = ".",
                                   overwrite = FALSE) {
  current_wd <- getwd()
  on.exit(setwd(current_wd))
  setwd(pkg_dir)
  if (!dir.exists("tests/testthat")) {
    stop ("To use this function, your project directory must have a tests ",
          "directory containing a testthat directory i.e. 'tests/testthat'. ",
          "To start using testthat, run devtools::use_testthat().")
  }
  r_file_name <- filesstrings::MakeExtName(r_file_name, "R")
  exampless <- extract_examples(r_file_name, pkg_dir = ".")
  if (!length(exampless)) {
    message("No examples found in ", r_file_name, " so not making a ",
            "corresponding test file.")
    return(invisible(character(0)))
  }
  test_shells <- mapply(make_test_shell, SIMPLIFY = FALSE,
                        exampless, paste(names(exampless), "works"))
  combined <- Reduce(function(x, y) c(x, "", y), test_shells)
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
make_tests_shells_pkg <- function(pkg_dir = ".", overwrite = FALSE) {
  current_wd <- getwd()
  setwd(pkg_dir)
  on.exit(setwd(current_wd))
  list.files(path = "R") %>%
    lapply(make_tests_shells_file, overwrite = overwrite) %>%
    invisible
}
