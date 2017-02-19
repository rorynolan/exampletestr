exampletestr
================

An R package to help developers create unit tests (designed for use with the testthat package) for their package, based on the examples in their package documentation. The documentation must be done with roxygen.

[![Travis-CI Build Status](https://travis-ci.org/rorynolan/exampletestr.svg?branch=master)](https://travis-ci.org/rorynolan/exampletestr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rorynolan/exampletestr?branch=master&svg=true)](https://ci.appveyor.com/project/rorynolan/exampletestr) [![Coverage Status](https://img.shields.io/codecov/c/github/rorynolan/exampletestr/master.svg)](https://codecov.io/github/rorynolan/exampletestr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/exampletestr)](https://cran.r-project.org/package=exampletestr)

Installation
------------

In R, enter

``` r
install.packages("devtools")
devtools::install_github("rorynolan/filesstrings", "rorynolan/exampletestr")
```

and you're done!

Use
---

First, let's set up a dummy package directory with just the `utils.R` file from the source code of the `exampletestr` package. Take a look at the `utils.R` file:

``` r
library(exampletestr)
library(testthat)
if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
"'tempkg' folder.")
dir.create("tempkg")
devtools::create("tempkg")
```

    #> Creating package 'tempkg' in '/Users/rnolan/Dropbox/DPhil/Misc/RStuff/exampletestr'

    #> No DESCRIPTION found. Creating with values:

    #> Package: tempkg
    #> Title: What the Package Does (one line, title case)
    #> Version: 0.0.0.9000
    #> Authors@R: person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))
    #> Description: What the package does (one paragraph).
    #> Depends: R (>= 3.3.2)
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

The `utils.R` file looks like this:

``` r
#' Does evaluation of text give an error?
#'
#' Can a character vector (where each line is treated as a line of R code) be evaluated as an R expression (or several R expressions) without giving an error?
#'
#' @param text_expr The expression to be evaluated, as a character vector.
#'
#' @return `TRUE` if the code gives an error and `FALSE` otherwise.
#' @examples
#' text_eval_error("a <- 1")
#' text_eval_error("a <- ")
#' @export
text_eval_error <- function(text_expr) {
  try(parse(text = text_expr), silent = TRUE) %>% inherits("try-error")
}

#' Text expression groups.
#'
#' Given a character vector of R expressions, break the vector up into groups of lines, where each group of lines is a valid R expression.
#'
#' @param text_expr A character vector.
#'
#' @return A list of character vectors, each of which can be evaluated as a valid R expression.
#' @examples
#' text_expr <- c("a <- 1",
#' "fx <- function(x) {",
#' "paste('f', x)",
#' "}")
#' extract_expressions(text_expr)
#' @export
extract_expressions <- function(text_expr) {
  expr_groups <- list()
  i <- 1
  while (i <= length(text_expr)) {
    j <- 0
    expr <- text_expr[i]
    while(text_eval_error(expr)) {
      j <- j + 1
      expr <- text_expr[i:(i + j)]
    }
    expr_groups <- append(expr_groups, list(expr))
    i <- i + j + 1
  }
  expr_groups
}

#' Evaluate a text string
#'
#' @param string The string to evaluate (as if it were a command).
#'
#' @examples
#' TextEval("3 + 4")
#' to.be.evaluated <- "var(c(1, 6, 8))"
#' TextEval(to.be.evaluated)
#'
#' @export
TextEval <- function(string) {
  stopifnot(is.character(string) && length(string) == 1)
  eval(parse(text = string))
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
#'   write the test file based on an example detailed with roxgen. Remember that
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
```

So let's demonstrate `extract_examples`:

``` r
extract_examples("utils")
```

    #> $text_eval_error
    #> [1] "text_eval_error(\"a <- 1\")" "text_eval_error(\"a <- \")" 
    #> 
    #> $extract_expressions
    #> [1] "text_expr <- c(\"a <- 1\","     "\"fx <- function(x) {\","      
    #> [3] "\"paste('f', x)\","             "\"}\")"                        
    #> [5] "extract_expressions(text_expr)"
    #> 
    #> $TextEval
    #> [1] "TextEval(\"3 + 4\")"                   
    #> [2] "to.be.evaluated <- \"var(c(1, 6, 8))\""
    #> [3] "TextEval(to.be.evaluated)"             
    #> 
    #> $construct_expect_equal
    #> [1] "text_expr <- c(\"sum(1, \", \"2)\")"                              
    #> [2] "cat(paste(text_expr, collapse = \"\\n\"))"                        
    #> [3] "construct_expect_equal(text_expr)"                                
    #> [4] "cat(paste(construct_expect_equal(text_expr), collapse = \"\\n\"))"

Indeed we get all of the lines of the documentation examples. Now with `make_test_shell`, we turn it into something usable with `testthat`:

``` r
make_test_shell(extract_examples("utils")[[1]], "whatevs")
```

    #> [1] "test_that(\"whatevs\", {"                     
    #> [2] "  expect_equal(text_eval_error(\"a <- 1\"), )"
    #> [3] "  expect_equal(text_eval_error(\"a <- \"), )" 
    #> [4] "})"

This might look a little weird in the output but it's really just

``` r
test_that("whatevs", {
  expect_equal(text_eval_error("a <- 1"), )
  expect_equal(text_eval_error("a <- "), )
})
```

which is what we would want. Now we have something we can fill in ourselves to create a real unit test.

We can make the unit tests file ( because it needs to be filled in) via `make_tests_shell_file`. Running

``` r
make_tests_shells_file("utils", proj_dir = "tempkg")
```

outputs a `test_utils.R` file in the `tests/testthat` folder with contents

``` r
test_that("text_eval_error works", {
  expect_equal(text_eval_error("a <- 1"), )
  expect_equal(text_eval_error("a <- "), )
})

test_that("extract_expressions works", {
  text_expr <- c("a <- 1",
  "fx <- function(x) {",
  "paste('f', x)",
  "}")
  expect_equal(extract_expressions(text_expr), )
})

test_that("TextEval works", {
  expect_equal(TextEval("3 + 4"), )
  to.be.evaluated <- "var(c(1, 6, 8))"
  expect_equal(TextEval(to.be.evaluated), )
})

test_that("construct_expect_equal works", {
  text_expr <- c("sum(1, ", "2)")
  expect_equal(cat(paste(text_expr, collapse = "\n")), )
  expect_equal(construct_expect_equal(text_expr), )
  expect_equal(cat(paste(construct_expect_equal(text_expr), collapse = "\n")), )
})
```

which, for my purposes, I complete as

``` r
test_that("text_eval_error works", {
  expect_false(text_eval_error("a <- 1"))
  expect_true(text_eval_error("a <- "))
})

test_that("extract_expressions works", {
  text_expr <- c("a <- 1",
  "fx <- function(x) {",
  "paste('f', x)",
  "}")
  expect_equal(extract_expressions(text_expr), list(
    "a <- 1",
    c("fx <- function(x) {",
      "paste('f', x)",
      "}")
  ))
})

test_that("TextEval works", {
  expect_equal(TextEval("3 + 4"), 7)
  to.be.evaluated <- "var(c(1, 6, 8))"
  expect_equal(TextEval(to.be.evaluated), var(c(1, 6, 8)))
})

test_that("construct_expect_equal works", {
  text_expr <- c("sum(1, ", "2)")
  expect_equal(construct_expect_equal(text_expr), c("expect_equal(sum(1, ",
                                                    "2), )"))
})
```
