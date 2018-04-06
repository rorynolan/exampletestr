context("Example extraction")
test_that("extract_examples() works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_true((devtools::create("tempkg", quiet = TRUE)))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "detect.R",
                                    package = "exampletestr"), "R"))
  extracted_examples <- suppressMessages(
                          exampletestr::extract_examples("detect"))
  expect_equal(extracted_examples,
               list(str_detect = c(
                 "### Name: str_detect",
                 paste("### Title:",
                       "Detect the presence or absence of a pattern",
                       "in a string."),
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
  expect_error(exampletestr::extract_examples("detect"),
               "no DESCRIPTION file")
  setwd(cwd)
})

context("Test shells")
test_that("make_test_shell() works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_true(devtools::create("tempkg", quiet = TRUE))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "detect.R",
                                    package = "exampletestr"), "R"))
  expect_equal(suppressMessages(
                 make_test_shell(extract_examples("R/detect.R")[[1]])),
               c("test_that(\"\", {",
                 "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
                 "  expect_equal(str_detect(fruit, \"a\"), )",
                 "  expect_equal(str_detect(fruit, \"^a\"), )",
                 "  expect_equal(str_detect(fruit, \"a$\"), )",
                 "  expect_equal(str_detect(fruit, \"b\"), )",
                 "  expect_equal(str_detect(fruit, \"[aeiou]\"), )",
                 "  expect_equal(str_detect(\"aecfg\", letters), )",
                 "})" ))
  expect_equal(suppressMessages(
                 make_test_shell(extract_examples("detect")[[1]], desc = "xyz",
                                                  e_e = FALSE)),
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
  setwd(cwd)
})

context("Functions")
test_that("make_tests_shell_fun() works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_true(devtools::create("tempkg", quiet = TRUE))
  setwd("tempkg")
  expect_true(all(file.copy(system.file("extdata", c("detect.R", "match.R"),
                                        package = "exampletestr"), "R")))
  make_test_shell_fun("str_detect()", open = FALSE)
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
  make_test_shell_fun("str_match_all", open = FALSE)
  expect_equal(
    readr::read_lines("tests/testthat/test-str_match_all-examples.R"),
    c("context(\"str_match_all()\")",
      "",
      "test_that(\"str_match_all() works\", {",
      "  strings <- c(",
      "    \" 219 733 8965\", \"329-293-8753 \", \"banana\", \"595 794 7569\",",
      "    \"387 287 6718\", \"apple\", \"233.398.9187  \", \"482 952 3315\",",
      paste("    \"239 923 8115 and 842 566 4692\",",
            "\"Work: 579-499-7527\", \"$1000\","),
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
      "})"             ))
  expect_error(make_test_shell_fun("()fun"), "cannot start with a parenthesis")
  expect_error(make_test_shell_fun("non_fun"), "Could not find.*non_fun\\(\\)")
  expect_error(make_test_shell_fun("str_detect"),
               "overwrite.*existing.*test-str_detect-examples.R")
  setwd("..")
  expect_true(filesstrings::dir.remove("tempkg"))
  expect_error(make_test_shell_fun("str_detect", open = FALSE),
               "no DESCRIPTION file")
  setwd(cwd)
})


context("Files")
test_that("make_tests_shells_file() works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_true(devtools::create("tempkg", quiet = TRUE))
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
  context("Packages")
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
  expect_error(make_tests_shells_file("detect", open = FALSE),
               "no DESCRIPTION file")
  expect_error(make_tests_shells_pkg(), "no DESCRIPTION file")
  setwd(cwd)
})
