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
