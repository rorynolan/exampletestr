## ----knitr setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = "#>")
knitr::opts_knit$set(root.dir = tempdir())

## ----setup---------------------------------------------------------------
library(exampletestr)
library(testthat)
devtools::create("tempkg")
devtools::use_testthat("tempkg")
file.copy(system.file("extdata", c("match.R", "detect.R"), package = "exampletestr"), 
          "tempkg/R")
devtools::document("tempkg")

## ----Look at match.R file, eval=FALSE------------------------------------
#  #' Extract matched groups from a string.
#  #'
#  #' Vectorised over `string` and `pattern`.
#  #'
#  #' @inheritParams str_detect
#  #' @param pattern Pattern to look for, as defined by an ICU regular
#  #'   expression. See [stringi::stringi-search-regex] for more details.
#  #' @return For `str_match`, a character matrix. First column is the
#  #'   complete match, followed by one column for each capture group.
#  #'   For `str_match_all`, a list of character matrices.
#  #'
#  #' @seealso [str_extract()] to extract the complete match,
#  #'   [stringi::stri_match()] for the underlying
#  #'   implementation.
#  #' @export
#  #' @examples
#  #' strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
#  #'   "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
#  #'   "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
#  #'   "Home: 543.355.3679")
#  #' phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
#  #'
#  #' str_extract(strings, phone)
#  #' str_match(strings, phone)
#  #'
#  #' # Extract/match all
#  #' str_extract_all(strings, phone)
#  #' str_match_all(strings, phone)
#  #'
#  #' x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
#  #' str_match(x, "<(.*?)> <(.*?)>")
#  #' str_match_all(x, "<(.*?)>")
#  #'
#  #' str_extract(x, "<.*?>")
#  #' str_extract_all(x, "<.*?>")
#  str_match <- function(string, pattern) {
#    if (type(pattern) != "regex") {
#      stop("Can only match regular expressions", call. = FALSE)
#    }
#  
#    stri_match_first_regex(string,
#      pattern,
#      opts_regex = opts(pattern)
#    )
#  }
#  
#  #' @rdname str_match
#  #' @export
#  str_match_all <- function(string, pattern) {
#    if (type(pattern) != "regex") {
#      stop("Can only match regular expressions", call. = FALSE)
#    }
#  
#    stri_match_all_regex(string,
#      pattern,
#      omit_no_match = TRUE,
#      opts_regex = opts(pattern)
#    )
#  }

## ----Demonstrate extract_examples----------------------------------------
extract_examples("match", pkg_dir = "tempkg")

## ----Demonstrate make_test_shell-----------------------------------------
lapply(extract_examples("match", pkg_dir = "tempkg"), make_test_shell, "whatevs")

## ----make_tests_shells_file----------------------------------------------
make_tests_shells_file("match", pkg_dir = "tempkg", open = FALSE)

## ----load stringr, include=FALSE-----------------------------------------
library(stringr)

## ----test-utils.R contents, eval=FALSE-----------------------------------
#  context("Match")
#  
#  test_that("str_match() works", {
#    strings <- c(
#      " 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
#      "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
#      "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
#      "Home: 543.355.3679"
#    )
#    phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
#    expect_equal(str_extract(strings, phone), )
#    expect_equal(str_match(strings, phone), )
#    expect_equal(str_extract_all(strings, phone), )
#    expect_equal(str_match_all(strings, phone), )
#    x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
#    expect_equal(str_match(x, "<(.*?)> <(.*?)>"), )
#    expect_equal(str_match_all(x, "<(.*?)>"), )
#    expect_equal(str_extract(x, "<.*?>"), )
#    expect_equal(str_extract_all(x, "<.*?>"), )
#  })

## ----fill in test shell--------------------------------------------------
context("Match")

test_that("str_match() works", {
  strings <- c(
    " 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
    "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
    "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
    "Home: 543.355.3679"
  )
  phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
  expect_equal(str_extract(strings, phone), 
               c("219 733 8965", "329-293-8753", NA, "595 794 7569", 
                 "387 287 6718", NA, "233.398.9187", "482 952 3315", 
                 "239 923 8115", "579-499-7527", NA, "543.355.3679"))
  expect_equal(str_match(strings, phone), 
               matrix(c("219 733 8965", "219", "733", "8965",
                        "329-293-8753", "329", "293", "8753",
                        NA,             NA,    NA,    NA,    
                        "595 794 7569", "595", "794", "7569",
                        "387 287 6718", "387", "287", "6718",
                        NA,             NA,    NA,    NA,    
                        "233.398.9187", "233", "398", "9187",
                        "482 952 3315", "482", "952", "3315",
                        "239 923 8115", "239", "923", "8115",
                        "579-499-7527", "579", "499", "7527",
                        NA,             NA,    NA,    NA,    
                        "543.355.3679", "543", "355", "3679"), 
                      ncol = 4, byrow = TRUE))
  expect_equal(str_extract_all(strings, phone), 
               list("219 733 8965", "329-293-8753", character(0), 
                    "595 794 7569", "387 287 6718", character(0),
                    "233.398.9187", "482 952 3315", 
                    c("239 923 8115", "842 566 4692"), "579-499-7527",
                    character(0), "543.355.3679"))
  expect_equal(str_match_all(strings, phone), 
               list(t(c("219 733 8965", "219", "733", "8965")),
                    t(c("329-293-8753", "329", "293", "8753")),
                    matrix(character(0), ncol = 4),
                    t(c("595 794 7569", "595", "794", "7569")),
                    t(c("387 287 6718", "387", "287", "6718")),
                    matrix(character(0), ncol = 4),
                    t(c("233.398.9187", "233", "398", "9187")),
                    t(c("482 952 3315", "482", "952", "3315")),
                    matrix(c("239 923 8115", "239", "923", "8115",
                             "842 566 4692", "842", "566", "4692"),
                           ncol = 4, byrow = TRUE),
                    t(c("579-499-7527", "579", "499", "7527")),
                    matrix(character(0), ncol = 4),
                    t(c("543.355.3679", "543", "355", "3679"))))
  x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
  expect_equal(str_match(x, "<(.*?)> <(.*?)>"),
               matrix(c("<a> <b>", "a",  "b", 
                        "<a> <>",  "a",  "",
                        NA,        NA,   NA,  
                        NA,        NA,   NA,  
                        NA,        NA,   NA), ncol = 3, byrow = TRUE))
  expect_equal(str_match_all(x, "<(.*?)>"), 
               list(matrix(c("<a>", "a", 
                             "<b>", "b"), ncol = 2, byrow = TRUE),
                    matrix(c("<a>", "a", 
                             "<>", ""), ncol = 2, byrow = TRUE),
                    t(c("<a>", "a")),
                    matrix(character(0), ncol = 2),
                    t(rep(NA_character_, 2))))
  expect_equal(str_extract(x, "<.*?>"), c("<a>", "<a>", "<a>", NA, NA))
  expect_equal(str_extract_all(x, "<.*?>"), 
               list(c("<a>", "<b>"), c("<a>", "<>"), "<a>", character(0),
                    NA_character_))
})

## ----setdown, include=FALSE----------------------------------------------
filesstrings::dir.remove("tempkg")

