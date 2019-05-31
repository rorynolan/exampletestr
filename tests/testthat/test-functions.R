usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
tmp_dir <- tempdir(check = TRUE)
pkg_dir <- paste0(tmp_dir, "/tmpkg")
pkg_dir2 <- paste0(tmp_dir, "/tmpkg2")
setup({
  options(usethis.quiet = TRUE)
  fs::file_delete(fs::dir_ls(tmp_dir))
  usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
  usethis::create_package(pkg_dir2, rstudio = FALSE, open = FALSE)
})
teardown({
  options(usethis.quiet = usethis_quiet_init)
  fs::dir_delete(tmp_dir)
})

test_that("`make_tests_shell_fun()` works", {
  fs::file_copy(system.file("extdata", c("detect.R", "match.R"),
    package = "exampletestr"
  ), paste0(pkg_dir, "/R"))
  usethis::local_project(pkg_dir)
  expect_false(is_documented("str_detect"))
  fs::dir_create(paste0(pkg_dir, "/man"))
  expect_false(is_documented("str_detect"))
  make_test_shell_fun("str_detect()", open = FALSE, pkg_dir = pkg_dir)
  expect_equal(
    readr::read_lines(
      usethis::proj_path("/tests/testthat/test-str_detect-examples.R")
    ),
    c(
      "test_that(\"`str_detect()` works\", {",
      "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
      "  expect_equal(str_detect(fruit, \"a\"), )",
      "  expect_equal(str_detect(fruit, \"^a\"), )",
      "  expect_equal(str_detect(fruit, \"a$\"), )",
      "  expect_equal(str_detect(fruit, \"b\"), )",
      "  expect_equal(str_detect(fruit, \"[aeiou]\"), )",
      "  expect_equal(str_detect(\"aecfg\", letters), )",
      "})"
    )
  )
  make_test_shell_fun("str_match_all", open = FALSE, pkg_dir = pkg_dir)
  expect_equal(
    readr::read_lines(
      usethis::proj_path("tests/testthat/test-str_match_all-examples.R")
    ),
    c(
      "test_that(\"`str_match_all()` works\", {",
      "  strings <- c(",
      "    \" 219 733 8965\", \"329-293-8753 \", \"banana\", \"595 794 7569\",",
      "    \"387 287 6718\", \"apple\", \"233.398.9187  \", \"482 952 3315\",",
      paste(
        "    \"239 923 8115 and 842 566 4692\",",
        "\"Work: 579-499-7527\", \"$1000\","
      ),
      "    \"Home: 543.355.3679\"",
      "  )",
      "  phone <- \"([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})\"",
      "  expect_equal(str_extract(strings, phone), )",
      "  expect_equal(str_match(strings, phone), )",
      "  expect_equal(str_extract_all(strings, phone), )",
      "  expect_equal(str_match_all(strings, phone), )",
      "  x <- c(\"<a> <b>\", \"<a> <>\", \"<a>\", \"\", NA)",
      "  expect_equal(str_match(x, \"<(.*?)> <(.*?)>\"), )",
      "  expect_equal(str_match_all(x, \"<(.*?)>\"), )",
      "  expect_equal(str_extract(x, \"<.*?>\"), )",
      "  expect_equal(str_extract_all(x, \"<.*?>\"), )",
      "})"
    )
  )
  expect_error(
    make_test_shell_fun("()fun", pkg_dir = pkg_dir),
    "The function .+ cannot start with a.+parenthesis."
  )
  expect_error(
    make_test_shell_fun("non_fun", pkg_dir = pkg_dir),
    "Could not find.*non_fun\\(\\)"
  )
  fs::file_copy(system.file("extdata", c("hello.R", "goodbye.R"),
    package = "exampletestr"
  ), paste0(pkg_dir2, "/R"))
  skip_if_not_installed("crayon")
  usethis::with_project(pkg_dir2, {
    no_man_err_msg <- rlang::catch_cnd(
      make_test_shell_fun("hello",
                          pkg_dir = pkg_dir2,
                          document = FALSE
      )
    )$message
    expect_match(crayon::strip_style(no_man_err_msg),
                 paste("Your package has no 'man' folder.\n    *",
                       "`exampletestr` looks for examples in",
                       "the '*.Rd' files in",
                       "the 'man' folder of a package and",
                       "cannot function without them."),
      fixed = TRUE
    )
    fs::dir_create(paste0(pkg_dir2, "/man"))
    no_rd_err_msg <- rlang::catch_cnd(
      make_test_shell_fun("hello",
        pkg_dir = pkg_dir2,
        document = FALSE
      )
    )$message
    expect_match(crayon::strip_style(no_rd_err_msg),
                 paste("Your package has no '*.Rd' files in",
                       "its 'man/' folder.\n    * exampletestr",
                       "looks for examples in the '*.Rd' files",
                       "in the 'man/' folder of a package and",
                       "cannot function if there are no '*.Rd'",
                       "files there."),
      fixed = TRUE
    )
    no_examples_err_msg <- rlang::catch_cnd(
      make_test_shell_fun("hello",
                          pkg_dir = pkg_dir2,
                          document = TRUE
      ),
      classes = "error"
    )$message
    expect_match(
      crayon::strip_style(no_examples_err_msg),
      paste0(
        "The function .+ is documented but has.+",
        "no.+accompanying.+examples.+",
        "only works on.+functions.+with.+examples."
      )
    )
  })
})
