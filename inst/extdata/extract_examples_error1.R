#' Extract examples lines from a roxygen-documented .R file.
#'
#' In each .R file in the R/ folder of a pcakage project, there can be examples
#' within documented via the [roxygen2][roxygen2::roxygen2] under the
#' `@examples` roxygen tag..
#'
#' @param r_file_name The name of the .R file within R/. Don't specify this as
#'   "R/x.R", just use "x.R" for whichever file x it is. You can also omit the
#'   .R for convenience, however using the wrong case (e.g. .r) will produce an
#'   error. If instead, you wish to set the full path to the file here, set
#'   \code{proj_dir} to \code{NULL}.
#' @param proj_dir The directory of the R project for this package (defaults to
#'   current directory). Note that this is the parent directory of man/. If you
#'   want to specify the full file path in the \code{r_file_name} argument, set
#'   \code{proj_dir} to \code{NULL}.
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
#'
#' @return A charachter vector.
#' @export
extract_examples = function(r_file_name, proj_dir = ".") {
  r_file_lines <- ifelse(is.null(proj_dir), r_file_name,
                         stringr::str_c(proj_dir, "/R/", r_file_name)) %>%
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
  names(exs_lines) <- function_names
  exs_lines
}
