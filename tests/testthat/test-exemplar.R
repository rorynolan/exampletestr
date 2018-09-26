context("Example extraction")
test_that("`extract_examples()` works", {
  expect_error(
    extract_examples("detect", tempdir()),
    "no DESCRIPTION file"
  )
  utils::capture.output(usethis::create_package(tempdir(), open = FALSE)) %>%
    invisible()
  expect_true(file.copy(system.file("extdata", "detect.R",
    package = "exampletestr"
  ), paste0(tempdir(), "/R")))
  extracted_examples <- suppressMessages(
    extract_examples("detect", pkg_dir = tempdir())
  )
  expect_equal(
    extracted_examples,
    list(str_detect = c(
      "### Name: str_detect",
      paste(
        "### Title:",
        "Detect the presence or absence of a pattern",
        "in a string."
      ),
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
    ))
  )
  suppressMessages(empty_dir(tempdir()))
})

context("Test shells")
test_that("`make_test_shell()` works", {
  utils::capture.output(usethis::create_package(tempdir(), open = FALSE)) %>%
    invisible()
  expect_true(file.copy(system.file("extdata", "detect.R",
    package = "exampletestr"
  ), paste0(tempdir(), "/R")))
  expect_equal(
    suppressMessages(
      make_test_shell(extract_examples("R/detect.R", pkg_dir = tempdir())[[1]])
    ),
    c(
      "test_that(\"\", {",
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
  expect_equal(
    suppressMessages(
      make_test_shell(extract_examples("detect", pkg_dir = tempdir())[[1]],
        desc = "xyz",
        e_e = FALSE
      )
    ),
    c(
      "test_that(\"xyz\", {",
      "  fruit <- c(\"apple\", \"banana\", \"pear\", \"pinapple\")",
      "  str_detect(fruit, \"a\")",
      "  str_detect(fruit, \"^a\")",
      "  str_detect(fruit, \"a$\")",
      "  str_detect(fruit, \"b\")",
      "  str_detect(fruit, \"[aeiou]\")",
      "  str_detect(\"aecfg\", letters)",
      "})"
    )
  )
  suppressMessages(empty_dir(tempdir()))
})

context("Functions")
test_that("`make_tests_shell_fun()` works", {
  expect_error(
    make_test_shell_fun("str_detect", open = FALSE, pkg_dir = tempdir()),
    "no DESCRIPTION file"
  )
  utils::capture.output(usethis::create_package(tempdir(), open = FALSE)) %>%
    invisible()
  expect_true(all(file.copy(system.file("extdata", c("detect.R", "match.R"),
    package = "exampletestr"
  ), paste0(tempdir(), "/R"))))
  utils::capture.output(
    make_test_shell_fun("str_detect()", open = FALSE, pkg_dir = tempdir())
  ) %>%
    invisible()
  expect_equal(
    readr::read_lines(paste0(tempdir(),
                             "/tests/testthat/test-str_detect-examples.R")),
    c(
      "context(\"`str_detect()`\")",
      "",
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
  utils::capture.output(
    make_test_shell_fun("str_match_all", open = FALSE, pkg_dir = tempdir())
  ) %>%
    invisible()
  expect_equal(
    readr::read_lines(
      rprojroot::find_package_root_file(
        "tests/testthat/test-str_match_all-examples.R",
        path = tempdir()
      )
    ),
    c(
      "context(\"`str_match_all()`\")",
      "",
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
  expect_error(make_test_shell_fun("()fun"),
               "The function .+ cannot start with a.+parenthesis.")
  expect_error(make_test_shell_fun("non_fun", pkg_dir = tempdir()),
               "Could not find.*non_fun\\(\\)")
  expect_error(
    make_test_shell_fun("str_detect", pkg_dir = tempdir()),
    "overwrite.*existing.*test-str_detect-examples.R"
  )
  suppressMessages(empty_dir(tempdir()))
  utils::capture.output(usethis::create_package(tempdir(), open = FALSE)) %>%
    invisible()
  expect_true(all(file.copy(system.file("extdata", c("hello.R", "goodbye.R"),
                                        package = "exampletestr"
  ), paste0(tempdir(), "/R"))))
  suppressMessages(filesstrings::dir.remove(paste0(tempdir(), "/man")))
  expect_error(make_test_shell_fun("hello", pkg_dir = tempdir(),
                                   document = FALSE),
               paste0("The package has no.+folder.+",
                      "looks in the.+folder for.+",
                      "examples and.+cannot function without.+a.+folder.+",
                      "Try running your test shell creating function again.+",
                      "with.+; this will create a.+",
                      "folder"))
  dir.create(paste0(tempdir(), "/man"))
  expect_error(make_test_shell_fun("hello", pkg_dir = tempdir(),
                                   document = FALSE),
               paste0("The package.+ folder has no.+",
                      "files.+",
                      "looks in the.+",
                      "folder for .+",
                      "files and cannot function.+without them."))

  expect_error(make_test_shell_fun("hello", pkg_dir = tempdir(),
                                   document = TRUE),
               paste0("The function .+ is documented but has.+",
                      "no.+accompanying examples.+",
                      "only works on.+functions with examples.")
  )
  suppressMessages(empty_dir(tempdir()))
})


context("Files")
test_that("`make_tests_shells_file()` works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  utils::capture.output(usethis::create_package("tempkg", open = FALSE)) %>%
    invisible()
  setwd("tempkg")
  expect_true(all(file.copy(system.file("extdata", c("detect.R", "match.R"),
    package = "exampletestr"
  ), "R")))
  utils::capture.output(make_tests_shells_file("R/detect.R", open = FALSE)) %>%
    invisible()
  expect_equal(
    readr::read_lines("tests/testthat/test-detect-examples.R"),
    c(
      "context(\"Detect\")",
      "",
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
  empty_lines <- character(2)
  readr::write_lines(empty_lines, "R/empty.R")
  utils::capture.output(make_tests_shells_pkg(overwrite = TRUE)) %>%
    invisible()
  context("Packages")
  expect_error(
    make_tests_shells_pkg(overwrite = FALSE),
    "Stopping as to proceed would be to overwrite"
  )
  expect_equal(
    readr::read_lines("tests/testthat/test-detect-examples.R"),
    c(
      "context(\"Detect\")",
      "",
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
  setwd("..")
  expect_true(filesstrings::dir.remove("tempkg"))
  expect_error(
    make_tests_shells_file("detect", open = FALSE),
    "no DESCRIPTION file"
  )
  expect_error(make_tests_shells_pkg(), "no DESCRIPTION file")
  setwd(cwd)
})
