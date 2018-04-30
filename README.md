
# `exampletestr` <img src="junk/sticker.png" height="200" align="right">

An R package to help developers create unit tests (designed for use with
the testthat package) for their package, based on the examples in their
package documentation.

[![Travis-CI Build
Status](https://travis-ci.org/rorynolan/exampletestr.svg?branch=master)](https://travis-ci.org/rorynolan/exampletestr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/rorynolan/exampletestr?branch=master&svg=true)](https://ci.appveyor.com/project/rorynolan/exampletestr)
[![codecov](https://codecov.io/gh/rorynolan/exampletestr/branch/master/graph/badge.svg)](https://codecov.io/gh/rorynolan/exampletestr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/exampletestr)](https://cran.r-project.org/package=exampletestr)
![RStudio CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/exampletestr)
[![RStudio CRAN monthly
downloads](http://cranlogs.r-pkg.org/badges/exampletestr)](https://cran.r-project.org/package=exampletestr)
[![Rdocumentation](http://www.rdocumentation.org/badges/version/exampletestr)](http://www.rdocumentation.org/packages/exampletestr)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/82205896.svg)](https://zenodo.org/badge/latestdoi/82205896)

## Installation

In R, enter

``` r
install.packages("exampletestr")
```

and you’re done\!

## Use

First, let’s set up a dummy package directory with just the `match.R`
and `detect.R` files from the source code of the `stringr` package.

``` r
library(exampletestr)
library(testthat)
devtools::create("tempkg")
```

    #> Creating package 'tempkg' in '/Users/rnolan/Dropbox/DPhil/Misc/RStuff/exampletestr'

    #> No DESCRIPTION found. Creating with values:

    #> Package: tempkg
    #> Title: What the Package Does (one line, title case)
    #> Version: 0.0.0.9000
    #> Authors@R: person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))
    #> Description: What the package does (one paragraph).
    #> Depends: R (>= 3.5.0)
    #> License: What license is it under?
    #> Encoding: UTF-8
    #> LazyData: true

    #> * Creating `tempkg.Rproj` from template.

    #> * Adding `.Rproj.user`, `.Rhistory`, `.RData` to ./.gitignore

``` r
devtools::use_testthat("tempkg")
```

    #> * Adding testthat to Suggests

    #> * Creating `tests/testthat`.

    #> * Creating `tests/testthat.R` from template.

``` r
file.copy(system.file("extdata", c("match.R", "detect.R"), package = "exampletestr"), 
          "tempkg/R")
```

    #> [1] TRUE TRUE

``` r
devtools::document("tempkg")
```

    #> Updating tempkg documentation

    #> Loading tempkg

    #> Updating roxygen version in /Users/rnolan/Dropbox/DPhil/Misc/RStuff/exampletestr/tempkg/DESCRIPTION

    #> Writing NAMESPACE
    #> Writing str_detect.Rd
    #> Writing str_match.Rd

The `match.R` file looks like this:

``` r
#' Extract matched groups from a string.
#'
#' Vectorised over `string` and `pattern`.
#'
#' @inheritParams str_detect
#' @param pattern Pattern to look for, as defined by an ICU regular
#'   expression. See [stringi::stringi-search-regex] for more details.
#' @return For `str_match`, a character matrix. First column is the
#'   complete match, followed by one column for each capture group.
#'   For `str_match_all`, a list of character matrices.
#'
#' @seealso [str_extract()] to extract the complete match,
#'   [stringi::stri_match()] for the underlying
#'   implementation.
#' @export
#' @examples
#' strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
#'   "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
#'   "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
#'   "Home: 543.355.3679")
#' phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
#'
#' str_extract(strings, phone)
#' str_match(strings, phone)
#'
#' # Extract/match all
#' str_extract_all(strings, phone)
#' str_match_all(strings, phone)
#'
#' x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
#' str_match(x, "<(.*?)> <(.*?)>")
#' str_match_all(x, "<(.*?)>")
#'
#' str_extract(x, "<.*?>")
#' str_extract_all(x, "<.*?>")
str_match <- function(string, pattern) {
  if (type(pattern) != "regex") {
    stop("Can only match regular expressions", call. = FALSE)
  }

  stri_match_first_regex(string,
    pattern,
    opts_regex = opts(pattern)
  )
}

#' @rdname str_match
#' @export
str_match_all <- function(string, pattern) {
  if (type(pattern) != "regex") {
    stop("Can only match regular expressions", call. = FALSE)
  }

  stri_match_all_regex(string,
    pattern,
    omit_no_match = TRUE,
    opts_regex = opts(pattern)
  )
}
```

So let’s demonstrate `extract_examples`:

``` r
extract_examples("match", pkg_dir = "tempkg")
```

    #> Running devtools::document() . . .

    #> Updating tempkg documentation

    #> Loading tempkg

    #> $str_match
    #>  [1] "### Name: str_match"                                                             
    #>  [2] "### Title: Extract matched groups from a string."                                
    #>  [3] "### Aliases: str_match str_match_all"                                            
    #>  [4] ""                                                                                
    #>  [5] "### ** Examples"                                                                 
    #>  [6] ""                                                                                
    #>  [7] "strings <- c(\" 219 733 8965\", \"329-293-8753 \", \"banana\", \"595 794 7569\","
    #>  [8] "  \"387 287 6718\", \"apple\", \"233.398.9187  \", \"482 952 3315\","            
    #>  [9] "  \"239 923 8115 and 842 566 4692\", \"Work: 579-499-7527\", \"$1000\","         
    #> [10] "  \"Home: 543.355.3679\")"                                                       
    #> [11] "phone <- \"([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})\""                      
    #> [12] ""                                                                                
    #> [13] "str_extract(strings, phone)"                                                     
    #> [14] "str_match(strings, phone)"                                                       
    #> [15] ""                                                                                
    #> [16] "# Extract/match all"                                                             
    #> [17] "str_extract_all(strings, phone)"                                                 
    #> [18] "str_match_all(strings, phone)"                                                   
    #> [19] ""                                                                                
    #> [20] "x <- c(\"<a> <b>\", \"<a> <>\", \"<a>\", \"\", NA)"                              
    #> [21] "str_match(x, \"<(.*?)> <(.*?)>\")"                                               
    #> [22] "str_match_all(x, \"<(.*?)>\")"                                                   
    #> [23] ""                                                                                
    #> [24] "str_extract(x, \"<.*?>\")"                                                       
    #> [25] "str_extract_all(x, \"<.*?>\")"                                                   
    #> [26] ""                                                                                
    #> [27] ""                                                                                
    #> [28] ""

Indeed we get all of the lines of the documentation examples. Now with
`make_test_shell`, we turn it into something usable with `testthat`:

``` r
lapply(extract_examples("match", pkg_dir = "tempkg"), make_test_shell, 
       "whatevs")
```

    #> Running devtools::document() . . .

    #> Updating tempkg documentation

    #> Loading tempkg

    #> $str_match
    #>  [1] "test_that(\"whatevs\", {"                                                 
    #>  [2] "  strings <- c("                                                          
    #>  [3] "    \" 219 733 8965\", \"329-293-8753 \", \"banana\", \"595 794 7569\","  
    #>  [4] "    \"387 287 6718\", \"apple\", \"233.398.9187  \", \"482 952 3315\","   
    #>  [5] "    \"239 923 8115 and 842 566 4692\", \"Work: 579-499-7527\", \"$1000\","
    #>  [6] "    \"Home: 543.355.3679\""                                               
    #>  [7] "  )"                                                                      
    #>  [8] "  phone <- \"([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})\""             
    #>  [9] "  expect_equal(str_extract(strings, phone), )"                            
    #> [10] "  expect_equal(str_match(strings, phone), )"                              
    #> [11] "  expect_equal(str_extract_all(strings, phone), )"                        
    #> [12] "  expect_equal(str_match_all(strings, phone), )"                          
    #> [13] "  x <- c(\"<a> <b>\", \"<a> <>\", \"<a>\", \"\", NA)"                     
    #> [14] "  expect_equal(str_match(x, \"<(.*?)> <(.*?)>\"), )"                      
    #> [15] "  expect_equal(str_match_all(x, \"<(.*?)>\"), )"                          
    #> [16] "  expect_equal(str_extract(x, \"<.*?>\"), )"                              
    #> [17] "  expect_equal(str_extract_all(x, \"<.*?>\"), )"                          
    #> [18] "})"

We can make the unit tests *shell* file (*shell* because it needs to be
filled in) via `make_tests_shell_file`. Running

``` r
make_tests_shells_file("match", pkg_dir = "tempkg", open = FALSE)
```

    #> Running devtools::document() . . .

    #> Updating tempkg documentation

    #> Loading tempkg

outputs a `test-utils.R` file in the `tests/testthat` folder with
contents

``` r
context("Match")

test_that("str_match() works", {
  strings <- c(
    " 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
    "387 287 6718", "apple", "233.398.9187  ", "482 952 3315",
    "239 923 8115 and 842 566 4692", "Work: 579-499-7527", "$1000",
    "Home: 543.355.3679"
  )
  phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
  expect_equal(str_extract(strings, phone), )
  expect_equal(str_match(strings, phone), )
  expect_equal(str_extract_all(strings, phone), )
  expect_equal(str_match_all(strings, phone), )
  x <- c("<a> <b>", "<a> <>", "<a>", "", NA)
  expect_equal(str_match(x, "<(.*?)> <(.*?)>"), )
  expect_equal(str_match_all(x, "<(.*?)>"), )
  expect_equal(str_extract(x, "<.*?>"), )
  expect_equal(str_extract_all(x, "<.*?>"), )
})
```

which can be sensibly completed as

``` r
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
```

To create these test shell files for each file in the `R/` directory of
your package, run `make_tests_shells_pkg()`.

### The Goal is NOT Fully Automated Unit Test Creation

I would like to stress that whilst unit testing should be automatic, the
creation of these tests is a manual process, a manual check. This
package is supposed to help you *start* making those tests. It is not
supposed to create fully functioning tests automatically, nor can it
help you to write every type of test you might want.

# Contribution

Contributions to this package are welcome. The preferred method of
contribution is through a github pull request. Feel free to contact me
by creating an issue. Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this
project you agree to abide by its terms.
