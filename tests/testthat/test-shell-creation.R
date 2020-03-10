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

test_that("`make_test_shell()` works", {
  expect_true(file.copy(system.file("extdata", "detect.R",
    package = "exampletestr"
  ), paste0(pkg_dir, "/R")))
  expect_equal(
    make_test_shell(
      extract_examples("R/detect.R", pkg_dir = pkg_dir)[[1]]
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
    make_test_shell(extract_examples("detect", pkg_dir = pkg_dir)[[1]],
      desc = "xyz", e_e = FALSE
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
  expect_equal(make_test_shell(character()), character())
})
