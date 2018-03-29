context("Exemplar")

test_that("extract_examples() works", {
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "detect.R",
                                    package = "exampletestr"), "R"))
  expect_equal(exampletestr::extract_examples("detect"),
               list(str_detect = c(
                 "### Name: str_detect",
                 "### Title: Detect the presence or absence of a pattern in a string.",
                 "### Aliases: str_detect",
                 "",
                 "### ** Examples",
                 "",
                 "fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "str_detect(fruit, \"a\")",
                 "str_detect(fruit, \"^a\")",
                 "str_detect(fruit, \"a$\")",
                 "str_detect(fruit, \"b\")",
                 "str_detect(fruit, \"[aeiou]\")",
                 "",
                 "# Also vectorised over pattern",
                 "str_detect(\"aecfg\", letters)",
                 "",
                 "",
                 ""
               )))
  setwd("..")
  expect_true(filesstrings::dir.remove("tempkg"))
})

test_that("make_test_shell() works", {
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "detect.R",
                                    package = "exampletestr"), "R"))
  expect_equal(make_test_shell(extract_examples("R/detect.R")[[1]]),
               c("test_that(\"\", {",
                 "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "  expect_equal(str_detect(fruit, \"a\"), )",
                 "  expect_equal(str_detect(fruit, \"^a\"), )",
                 "  expect_equal(str_detect(fruit, \"a$\"), )",
                 "  expect_equal(str_detect(fruit, \"b\"), )",
                 "  expect_equal(str_detect(fruit, \"[aeiou]\"), )",
                 "  expect_equal(str_detect(\"aecfg\", letters), )",
                 "})" ))
  expect_equal(make_test_shell(extract_examples("detect")[[1]], desc = "xyz",
                               e_e = FALSE),
               c("test_that(\"xyz\", {",
                 "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "  str_detect(fruit, \"a\")",
                 "  str_detect(fruit, \"^a\")",
                 "  str_detect(fruit, \"a$\")",
                 "  str_detect(fruit, \"b\")",
                 "  str_detect(fruit, \"[aeiou]\")",
                 "  str_detect(\"aecfg\", letters)",
                 "})"))
  setwd("..")
  expect_true(filesstrings::dir.remove("tempkg"))
})

test_that("make_tests_shells_file() works", {
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(all(file.copy(system.file("extdata", c("detect.R", "match.R"),
                                        package = "exampletestr"), "R")))
  make_tests_shells_file("R/detect.R", open = FALSE)
  expect_equal(readr::read_lines("tests/testthat/test-detect-examples.R"),
               c("context(\"Detect\")",
                 "",
                 "test_that(\"str_detect() works\", {",
                 "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "  expect_equal(str_detect(fruit, \"a\"), )",
                 "  expect_equal(str_detect(fruit, \"^a\"), )",
                 "  expect_equal(str_detect(fruit, \"a$\"), )",
                 "  expect_equal(str_detect(fruit, \"b\"), )",
                 "  expect_equal(str_detect(fruit, \"[aeiou]\"), )",
                 "  expect_equal(str_detect(\"aecfg\", letters), )",
                 "})"  ))
  empty_lines <- character(2)
  readr::write_lines(empty_lines, "R/empty.R")
  make_tests_shells_pkg(overwrite = TRUE)
  expect_error(make_tests_shells_pkg(overwrite = FALSE),
               "Stopping as to proceed would be to overwrite")
  expect_equal(readr::read_lines("tests/testthat/test-detect-examples.R"),
               c("context(\"Detect\")",
                 "",
                 "test_that(\"str_detect() works\", {",
                 "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "  expect_equal(str_detect(fruit, \"a\"), )",
                 "  expect_equal(str_detect(fruit, \"^a\"), )",
                 "  expect_equal(str_detect(fruit, \"a$\"), )",
                 "  expect_equal(str_detect(fruit, \"b\"), )",
                 "  expect_equal(str_detect(fruit, \"[aeiou]\"), )",
                 "  expect_equal(str_detect(\"aecfg\", letters), )",
                 "})"  ))
  setwd("..")
  expect_true(filesstrings::dir.remove("tempkg"))
})

test_that("make_tests_shell_fun() works", {
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(all(file.copy(system.file("extdata", c("detect.R", "match.R"),
                                        package = "exampletestr"), "R")))
  make_test_shell_fun("str_detect", open = FALSE)
  expect_equal(readr::read_lines("tests/testthat/test-str_detect-examples.R"),
               c("context(\"str_detect()\")",
                 "",
                 "test_that(\"str_detect() works\", {",
                 "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "  expect_equal(str_detect(fruit, \"a\"), )",
                 "  expect_equal(str_detect(fruit, \"^a\"), )",
                 "  expect_equal(str_detect(fruit, \"a$\"), )",
                 "  expect_equal(str_detect(fruit, \"b\"), )",
                 "  expect_equal(str_detect(fruit, \"[aeiou]\"), )",
                 "  expect_equal(str_detect(\"aecfg\", letters), )",
                 "})"  ))
  setwd("..")
  expect_true(filesstrings::dir.remove("tempkg"))
})
