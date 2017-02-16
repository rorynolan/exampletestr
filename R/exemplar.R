#' Extract examples lines from a roxygen-documented .R file.
#'
#' In each .R file in the R/ folder of a pcakage project, there can be examples
#' within documented via the [roxygen2][roxygen2::roxygen2] under the
#' `@examples` roxygen tag..
#'
#' @param r_file_name The name of the .R file within R/. Don't specify this as
#'   "R/x.R", just use "x.R" for whichever file x it is. You can also omit the
#'   .R for convenience, however using the wrong case (e.g. .r) will produce an
#'   error.
#' @param proj_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of man/.
#'
#' @examples
#' extract_examples("exemplar")
#' extract_examples("exemplar")
#' extract_examples("exemplar")
#'
#' @return A charachter vector.
#' @export
extract_examples <- function(r_file_name, proj_dir = ".") {
  r_file_lines <- stringr::str_c(proj_dir, "/R/", r_file_name) %>%
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
  roxygen_line_groups <- roxygen_line_groups[startwith_atexs]  # restrict both of these to what we're interested in (examples)
  line_indices_after_roxygens <- roxygen_index_groups %>%
    sapply(BBmisc::getLast) + 1
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
  if (unique(sapply(atex_line_indices, length)) != 1) {
    stop("Each roxygen block should contain at most 1 @examples tag.")
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
  names(exs_lines) <- function_names
  exs_lines
}

braces <- rbind(curly = c("{", "}"), square = c("[", "]"), round = c("(", ")"))
colnames(braces) <- c("opening", "closing")

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
#' @export
make_test_shell <- function(example_block, desc = "") {
  expressions <- extract_expressions(example_block)

  assignments <- sapply(expressions, function(x) {
    any(stringr::str_detect(x, "<-"))
  })
  inside_test_that <- mapply(function(x, y) {
    if (x) {
      y
    } else {
      construct_expect_equal(y)
    }
  }, assignments, expressions, SIMPLIFY = FALSE) %>% unlist
  c(paste0("test_that(\"", desc, "\", {"),
    inside_test_that,
    "})")
}

#' Create the shell of a test file based on roxygen examples.
#'
#' Based on the roxygen-specified examples in a .R file in the R/ directory of
#' the package, create the shell of a test file, to be completed by the user.
#'
#' @inheritParams extract_examples
#'
#' @return The shell of the test file is written into tests/testthat. It has the
#'   same name as the .R file it was created from except it has "test_" tacked
#'   onto the front.
#' @export
make_tests_shells_file <- function(r_file_name, proj_dir = ".") {
  r_file_name <- filesstrings::MakeExtName(r_file_name, "R")
  exampless <- extract_examples(r_file_name, proj_dir = proj_dir)
  test_shells <- mapply(make_test_shell, SIMPLIFY = FALSE,
                        exampless, paste(names(exampless), "works"))
  combined <- Reduce(function(x, y) c(x, "", y), test_shells)
  testthat_dir <- paste0(proj_dir, "/tests/testthat")
  test_file_name <- paste0(testthat_dir, "/test_", r_file_name)
  writeLines(combined, test_file_name)
  invisible(combined)
}
