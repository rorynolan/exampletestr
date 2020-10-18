#' Extract examples lines from the functions in a .R file of a package.
#'
#' In each '.R' file in the 'R/' folder of a package project, for the functions
#' defined therein, there can corresponding examples in the '.Rd' files of the
#' 'man/' folder. This function extracts those examples into a list of character
#' vectors, one list element for each documented function.
#'
#' Anything found within a `\\dontrun\{...\}` block is ignored.
#'
#' @param r_file_name The name of the '.R' file within 'R/'. There's no need to
#'   specify the file path (as 'R/x.R', but you can do this if you want), you
#'   can just use 'x.R' for whichever file 'x' it is. You can also omit the '.R'
#'   for convenience, however using the wrong case (e.g. '.r') will produce an
#'   error.
#' @param pkg_dir The directory of the R project for this package (defaults to
#'   current directory). This is the parent directory of 'R/' and 'man/'. In
#'   reality, this specification is somewhat lenient and even if you are in any
#'   sub-directory of the root, it will work (e.g. it will work if you are in
#'   'R/' or 'man/'). Beware that this behaviour can cause a problem if you have
#'   an R package inside an R package (but really, you have yourself to blame if
#'   that's the case).
#' @param document Run [roxygen2::roxygenize()] to update package documentation
#'   before starting?
#'
#' @examples
#' pkg_dir <- paste0(tempdir(check = TRUE), "/tmpkg")
#' usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
#' fs::file_copy(
#'   system.file("extdata", "detect.R", package = "exampletestr"),
#'   paste0(pkg_dir, "/R")
#' )
#' exampletestr:::extract_examples("detect", pkg_dir)
#' fs::dir_delete(pkg_dir)
#' @return A list of character vectors.
#' @noRd
extract_examples <- function(r_file_name, pkg_dir = ".", document = TRUE) {
  checkmate::assert_flag(document)
  checkmate::assert_directory_exists(pkg_dir)
  checkmate::assert_string(r_file_name, min.chars = 1)
  usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
  usethis::local_project(path = pkg_dir, quiet = TRUE)
  check_for_DESCRIPTION()
  if (document) exampletestr_document(usethis_quiet_init)
  check_for_man()
  if (strex::str_elem(r_file_name, -1) == "/") {
    r_file_name %<>% strex::str_before_last("/")
  }
  if (stringr::str_detect(r_file_name, "/")) {
    r_file_name <- strex::str_after_last(r_file_name, "/")
  }
  r_file_name %<>%
    usethis::proj_path("R", .) %>%
    strex::str_give_ext("R")
  checkmate::assert_file_exists(r_file_name)
  r_file_lines_quotes_gone <- readr::read_lines(r_file_name) %>%
    parse(text = .) %>%
    purrr::map(deparse) %>%
    unlist() %>%
    paste0("\n") %>%
    purrr::map(readr::read_lines) %>%
    unlist() %>%
    strex::str_remove_quoted()
  r_file_funs <- stringr::str_match(
    r_file_lines_quotes_gone,
    "(^[^ ]*)\\s*(<-|=)\\s*function\\("
  )[, 2] %>%
    stats::na.omit()
  rd_file_paths <- fs::dir_ls(usethis::proj_path("man"),
    regexp = "\\.Rd$"
  )
  rd_file_short_names <- rd_file_paths %>%
    fs::path_file() %>%
    fs::path_ext_remove()
  rd_file_lines <- purrr::map(rd_file_paths, readr::read_lines)
  names(rd_file_lines) <- rd_file_short_names
  documented_funs_in_alias_tags <- unlist(rd_file_lines) %>%
    stringr::str_extract("\\\\alias\\{.*\\}") %>%
    stats::na.omit()
  documented_funs <- stringr::str_sub(
    documented_funs_in_alias_tags,
    8, -2
  ) %>%
    stringr::str_trim()
  file_documented_funs <- intersect(documented_funs, r_file_funs)
  if (!length(file_documented_funs)) {
    return(list())
  }
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
  wanted_rd_paths <- usethis::proj_path("man", wanted_rds, ext = "Rd")
  examples <- purrr::map(wanted_rd_paths, extract_examples_rd)
  names(examples) <- wanted_rds
  ls_exs <- lengths(examples)
  if (isTRUE(unique(ls_exs) == 0)) {
    return(list())
  }
  examples[as.logical(ls_exs)]
}
