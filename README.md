exampletestr
================

An R package to help developers create unit tests (designed for use with the testthat package) for their package, based on the examples in their package documentation.

[![Travis-CI Build Status](https://travis-ci.org/rorynolan/exampletestr.svg?branch=master)](https://travis-ci.org/rorynolan/exampletestr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rorynolan/exampletestr?branch=master&svg=true)](https://ci.appveyor.com/project/rorynolan/exampletestr) [![codecov](https://codecov.io/gh/rorynolan/exampletestr/branch/master/graph/badge.svg)](https://codecov.io/gh/rorynolan/exampletestr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/exampletestr)](https://cran.r-project.org/package=exampletestr) ![RStudio CRAN downloads](http://cranlogs.r-pkg.org/badges/grand-total/exampletestr) [![RStudio CRAN monthly downloads](http://cranlogs.r-pkg.org/badges/exampletestr)](https://cran.rstudio.com/web/packages/exampletestr/index.html) [![Rdocumentation](http://www.rdocumentation.org/badges/version/exampletestr)](http://www.rdocumentation.org/packages/exampletestr) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

Installation
------------

In R, enter

``` r
install.packages("exampletestr")
```

and you're done!

Use
---

First, let's set up a dummy package directory with just the `utils.R` file from the source code of the `exampletestr` package.

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
    #> Depends: R (>= 3.4.2)
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
file.copy(system.file("extdata", "utils.R", package = "exampletestr"), 
          "tempkg/R")
```

    #> [1] TRUE

``` r
devtools::document("tempkg")
```

    #> Updating tempkg documentation

    #> Loading tempkg

    #> Updating roxygen version in /Users/rnolan/Dropbox/DPhil/Misc/RStuff/exampletestr/tempkg/DESCRIPTION

    #> Writing NAMESPACE
    #> Writing text_parse_error.Rd
    #> Writing extract_expressions.Rd
    #> Writing construct_expect_equal.Rd
    #> Writing extract_examples_rd.Rd

The `utils.R` file looks like this:

``` r
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
#' @export
text_parse_error <- function(text_expr) {
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
#' @param remove_comments Should comments be removed?
#'
#' @return A list of character vectors, each of which can be evaluated as a
#'   valid R expression.
#' @examples
#' text_expr <- c("a <- 1",
#' "fx <- function(x) {",
#' "  x + 1",
#' "}  # this comment should disappear")
#' extract_expressions(text_expr)
#' @export
extract_expressions <- function(text_expr, remove_comments = TRUE) {
  stopifnot(length(text_expr) > 0)
  expr_groups <- list()
  i <- 1
  while (i <= length(text_expr)) {
    j <- 0
    expr <- text_expr[i]
    while(text_parse_error(expr)) {
      j <- j + 1
      expr <- text_expr[i:(i + j)]
    }
    expr_groups <- append(expr_groups, list(expr))
    i <- i + j + 1
  }
  if (remove_comments) {
    expr_groups <- purrr::map(expr_groups, ~ formatR::tidy_source(text = .,
        comment = !remove_comments, arrow = TRUE, indent = 2, output = FALSE,
        width.cutoff = 50)) %>%
      purrr::map(getElement, "text.tidy") %>%
      purrr::map(paste0, "\n") %>%
      purrr::map(readr::read_lines)
    for (i in seq_along(expr_groups)) {
      if (filesstrings::all_equal(expr_groups[[i]], character(0))) {
        expr_groups[[i]] <- ""
      }
    }
  }
  empties <- purrr::map_lgl(expr_groups, ~ isTRUE(all.equal(., "")))
  expr_groups <- expr_groups[!empties]
  lapply(expr_groups, stringr::str_trim, side = "right")
  # str_trim because sometimes formatR leaves unnecessary trailing whitespace
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
#' @export
construct_expect_equal <- function(text_expr) {
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
#' this_function_rd <- system.file("extdata", "extract_examples_rd.Rd",
#'                                 package = "exampletestr")
#' extract_examples_rd(this_function_rd)
#' @export
extract_examples_rd <- function(rd_file_path) {
  tc <- textConnection(" ", "w")
  tools::Rd2ex(rd_file_path, tc)
  examples_lines <- textConnectionValue(tc)
  close(tc)
  examples_lines
}
```

So let's demonstrate `extract_examples`:

``` r
extract_examples("utils", pkg_dir = "tempkg")
```

    #> $construct_expect_equal
    #>  [1] "### Name: construct_expect_equal"                                 
    #>  [2] "### Title: Construct an 'expect_equal' expression"                
    #>  [3] "### Aliases: construct_expect_equal"                              
    #>  [4] ""                                                                 
    #>  [5] "### ** Examples"                                                  
    #>  [6] ""                                                                 
    #>  [7] "text_expr <- c(\"sum(1, \", \"2)\")"                              
    #>  [8] "cat(paste(text_expr, collapse = \"\\n\"))"                        
    #>  [9] "construct_expect_equal(text_expr)"                                
    #> [10] "cat(paste(construct_expect_equal(text_expr), collapse = \"\\n\"))"
    #> [11] ""                                                                 
    #> [12] ""                                                                 
    #> [13] ""                                                                 
    #> 
    #> $extract_examples_rd
    #>  [1] "### Name: extract_examples_rd"                                           
    #>  [2] "### Title: Extract examples from a '.Rd' file as a character vector."    
    #>  [3] "### Aliases: extract_examples_rd"                                        
    #>  [4] ""                                                                        
    #>  [5] "### ** Examples"                                                         
    #>  [6] ""                                                                        
    #>  [7] "this_function_rd <- system.file(\"extdata\", \"extract_examples_rd.Rd\","
    #>  [8] "                                package = \"exampletestr\")"             
    #>  [9] "extract_examples_rd(this_function_rd)"                                   
    #> [10] ""                                                                        
    #> [11] ""                                                                        
    #> [12] ""                                                                        
    #> 
    #> $extract_expressions
    #>  [1] "### Name: extract_expressions"          
    #>  [2] "### Title: Text expression groups."     
    #>  [3] "### Aliases: extract_expressions"       
    #>  [4] ""                                       
    #>  [5] "### ** Examples"                        
    #>  [6] ""                                       
    #>  [7] "text_expr <- c(\"a <- 1\","             
    #>  [8] "\"fx <- function(x) {\","               
    #>  [9] "\"  x + 1\","                           
    #> [10] "\"}  # this comment should disappear\")"
    #> [11] "extract_expressions(text_expr)"         
    #> [12] ""                                       
    #> [13] ""                                       
    #> [14] ""                                       
    #> 
    #> $text_parse_error
    #>  [1] "### Name: text_parse_error"                    
    #>  [2] "### Title: Does parsing of text give an error?"
    #>  [3] "### Aliases: text_parse_error"                 
    #>  [4] ""                                              
    #>  [5] "### ** Examples"                               
    #>  [6] ""                                              
    #>  [7] "text_parse_error(\"a <- 1\")"                  
    #>  [8] "text_parse_error(\"a <- \")"                   
    #>  [9] ""                                              
    #> [10] ""                                              
    #> [11] ""

Indeed we get all of the lines of the documentation examples. Now with `make_test_shell`, we turn it into something usable with `testthat`:

``` r
lapply(extract_examples("utils", pkg_dir = "tempkg"), make_test_shell, "whatevs")
```

    #> $construct_expect_equal
    #> [1] "test_that(\"whatevs\", {"                                                           
    #> [2] "  text_expr <- c(\"sum(1, \", \"2)\")"                                              
    #> [3] "  expect_equal(cat(paste(text_expr, collapse = \"\\n\")), )"                        
    #> [4] "  expect_equal(construct_expect_equal(text_expr), )"                                
    #> [5] "  expect_equal(cat(paste(construct_expect_equal(text_expr), collapse = \"\\n\")), )"
    #> [6] "})"                                                                                 
    #> 
    #> $extract_examples_rd
    #> [1] "test_that(\"whatevs\", {"                                                  
    #> [2] "  this_function_rd <- system.file(\"extdata\", \"extract_examples_rd.Rd\","
    #> [3] "    package = \"exampletestr\")"                                           
    #> [4] "  expect_equal(extract_examples_rd(this_function_rd), )"                   
    #> [5] "})"                                                                        
    #> 
    #> $extract_expressions
    #> [1] "test_that(\"whatevs\", {"                                          
    #> [2] "  text_expr <- c(\"a <- 1\", \"fx <- function(x) {\", \"  x + 1\","
    #> [3] "    \"}  # this comment should disappear\")"                       
    #> [4] "  expect_equal(extract_expressions(text_expr), )"                  
    #> [5] "})"                                                                
    #> 
    #> $text_parse_error
    #> [1] "test_that(\"whatevs\", {"                      
    #> [2] "  expect_equal(text_parse_error(\"a <- 1\"), )"
    #> [3] "  expect_equal(text_parse_error(\"a <- \"), )" 
    #> [4] "})"

This might look a little weird in the output but it's really just

``` r
test_that("whatevs", {
  expect_equal(text_eval_error("a <- 1"), )
  expect_equal(text_eval_error("a <- "), )
})
```

and so on, which is what we would want. Now we have something we can fill in ourselves to create a real unit test.

We can make the unit tests *shell* file (*shell* because it needs to be filled in) via `make_tests_shell_file`. Running

``` r
make_tests_shells_file("utils", pkg_dir = "tempkg")
```

outputs a `test_utils.R` file in the `tests/testthat` folder with contents

``` r
context("Utils")

test_that("construct_expect_equal works", {
  text_expr <- c("sum(1, ", "2)")
  expect_equal(cat(paste(text_expr, collapse = "\n")), )
  expect_equal(construct_expect_equal(text_expr), )
  expect_equal(cat(paste(construct_expect_equal(text_expr), collapse = "\n")), )
})

test_that("extract_examples_rd works", {
  this_function_rd <- system.file("extdata", "extract_examples_rd.Rd",
    package = "exampletestr")
  expect_equal(extract_examples_rd(this_function_rd), )
})

test_that("extract_expressions works", {
  text_expr <- c("a <- 1", "fx <- function(x) {", "  x + 1",
    "}  # this comment should disappear")
  expect_equal(extract_expressions(text_expr), )
})

test_that("text_parse_error works", {
  expect_equal(text_parse_error("a <- 1"), )
  expect_equal(text_parse_error("a <- "), )
})
```

which, for my purposes, I complete as

``` r
context("Utils")

test_that("text_parse_error works", {
  expect_false(text_parse_error("a <- 1"))
  expect_true(text_parse_error("a <- "))
})

test_that("extract_expressions works", {
  text_expr <- c("a <- 1",
                 "fx <- function(x) {",
                 "  x + 1",
                 "}")
  expect_equal(extract_expressions(text_expr), list(
    "a <- 1",
    c("fx <- function(x) {",
      "  x + 1",
      "}")
  ))
})

test_that("construct_expect_equal works", {
  text_expr <- c("sum(1, ", "2)")
  expect_equal(construct_expect_equal(text_expr), c("expect_equal(sum(1, ",
                                                    "2), )"))
})
```

To create these test shell files for each file in the `R/` directory of your package, run `make_tests_shells_pkg()`.

### The Goal is NOT Fully Automated Unit Test Creation

I would like to stress that whilst unit testing should be automatic, the creation of these tests is a manual process, a manual check. This package is supposed to help you *start* making those tests. It is not supposed to create fully functioning tests automatically, nor can it help you to write every type of test you might want.

Contribution
============

Contributions to this package are welcome. The preferred method of contribution is through a github pull request. Feel free to contact me by creating an issue. Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
