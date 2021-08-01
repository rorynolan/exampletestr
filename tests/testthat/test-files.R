usethis_quiet_init <- getOption("usethis.quiet", default = FALSE)
tmp_dir <- tempdir(check = TRUE)
pkg_dir <- paste0(tmp_dir, "/tmpkg")
setup({
  options(usethis.quiet = TRUE)
  fs::file_delete(fs::dir_ls(tmp_dir))
  usethis::create_package(pkg_dir, rstudio = FALSE, open = FALSE)
})
teardown({
  options(usethis.quiet = usethis_quiet_init)
  fs::dir_delete(tmp_dir)
})

test_that("`make_tests_shells_file()` and `make_tests_shells_pkg()` work", {
  fs::file_copy(
    system.file("extdata", c("detect.R", "match.R"),
      package = "exampletestr"
    ),
    paste0(pkg_dir, "/R")
  )
  usethis::local_project(pkg_dir)
  replicate(4, make_tests_shells_file("R/detect.R", pkg_dir, open = FALSE),
    simplify = FALSE
  )
  test_detect_file_paths <- usethis::proj_path("tests/testthat",
    paste0(
      "test-detect",
      c(
        "",
        paste0(
          "-examples",
          c(
            "",
            paste0("--", 1:2)
          )
        )
      )
    ),
    ext = "R"
  )
  expect_true(all(fs::file_exists(test_detect_file_paths)))
  expect_equal(
    readr::read_lines(
      usethis::proj_path("tests/testthat/test-detect.R"),
      lazy = FALSE
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
  expect_equal(
    length(unique(purrr::map(test_detect_file_paths, readr::read_lines))),
    1
  )
  empty_lines <- character(2)
  readr::write_lines(empty_lines, usethis::proj_path("R/empty.R"))
  make_tests_shells_pkg(pkg_dir, overwrite = TRUE)
  expect_equal(
    readr::read_lines(
      usethis::proj_path("tests/testthat/test-detect.R"),
      lazy = FALSE
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
  withr::with_options(list(usethis.quiet = FALSE), {
    fs::dir_delete(paste0(pkg_dir, "/R"))
    if (packageVersion("usethis") > "1.5.1") {
      expect_message(
        make_tests_shells_pkg(
          pkg_dir,
          document = FALSE
        ),
        regexp = "No files found in the.+R.+directory.+no test.+created"
      )
    }
  })
})
