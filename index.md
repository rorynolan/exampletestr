
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `exampletestr` <img src="man/figures/logo.png" align="right" height=140/>

`exampletestr` is for creating shells (skeletons) of `testthat` unit
test files based on the examples in a package’s documentation. When
these tests are completed, they will ensure that the examples function
correctly and form a valuable part of your unit test suite. I have often
(but not always) found that I test my functions manually by making sure
my examples work as intended, so why not use these as a basis for proper
unit testing?

## Installation

You can install the released version of exampletestr from
[CRAN](https://CRAN.R-project.org/package=exampletestr) with:

``` r
install.packages("exampletestr")
```

You can install the released version of exampletestr from
[GitHub](https://github.com/rorynolan/exampletestr) with:

``` r
remotes::install_github("rorynolan/exampletestr")
```

## How to use `exampletestr`

There are three ways to use `exampletestr` to make unit test shells
based on function examples:

1. [One function at a time](https://rorynolan.github.io/exampletestr/articles/one-function-at-a-time.html) (this article also explains the integration with [`roxytest`](https://github.com/mikldk/roxytest)).
2. [One `.R` file at a     time](https://rorynolan.github.io/exampletestr/articles/one-file-at-a-time.html).
3. [A whole package all at once](https://rorynolan.github.io/exampletestr/articles/whole-package.html).

These three articles should be read in order to learn how the package
works and how to use it.

## The Goal is NOT Fully Automated Unit Test Creation

I would like to stress that whilst unit testing should be automatic, the
creation of these tests is a manual process, a manual check. This
package is supposed to help you to *start* making those tests. It is not
supposed to create fully functioning tests automatically, nor can it
help you to write every type of test you might want.
