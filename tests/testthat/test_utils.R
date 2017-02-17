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
