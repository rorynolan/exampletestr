usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
pkg_dir <- paste0(tempdir(check = TRUE), "/tmpkg")
setup({
  options(usethis.quiet = TRUE)
  usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
})
teardown({
  options(usethis.quiet = usethis_quiet_init)
  fs::dir_delete(pkg_dir)
})

test_that("text_parse_error works", {
  expect_false(text_parse_error("a <- 1"))
  expect_true(text_parse_error("a <- "))
})

test_that("extract_expressions works", {
  text_expr <- c(
    "a <- 1",
    "fx <- function(x) {",
    "  x + 1",
    "}"
  )
  expect_equal(purrr::map(extract_expressions(text_expr), as.character), list(
    "a <- 1",
    c(
      "fx <- function(x) {",
      "  x + 1",
      "}"
    )
  ))
})

test_that("construct_expect_equal works", {
  text_expr <- c("sum(1, ", "2)")
  expect_equal(construct_expect_equal(text_expr), c(
    "expect_equal(sum(1, ",
    "2), )"
  ))
})

test_that("`check_for_DESCRIPTION()` works", {
  skip_if_not_installed("crayon")
  description <- readr::read_lines(paste0(pkg_dir, "/DESCRIPTION"))
  fs::file_delete(paste0(pkg_dir, "/DESCRIPTION"))
  fs::file_create(paste0(pkg_dir, "/.here"))
  no_DESCRIPTION_err_msg <- rlang::catch_cnd(make_tests_shells_pkg(pkg_dir),
                                             classes = "error")$message
  expect_match(
    crayon::strip_style(no_DESCRIPTION_err_msg),
    paste0("Your package has no 'DESCRIPTION' file.\n",
           "    * Every R package must have a 'DESCRIPTIO",
           "N' file in the root directory.\n    * Perhaps ",
           "you specified the wrong `pkg_dir`?\n    *",
           " You specified `pkg_dir = "),
    fixed = TRUE)
  readr::write_lines(description, paste0(pkg_dir, "/DESCRIPTION"))
})

test_that("custom_stop works", {
  expect_error(custom_stop("1", 2, 3),
               "arguments .+ must all be of character type")
})


