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

test_that("`extract_examples()` works", {
  expect_error(
    extract_examples("detect", tempdir(check = TRUE)),
    "Path .+ does not appear to be inside a project or package.",
    class = "usethis_error"
  )
  fs::file_copy(system.file("extdata", "detect.R",
    package = "exampletestr"
  ), paste0(pkg_dir, "/R"))
  invisible(capture.output(roxygen2::roxygenize(pkg_dir)))
  detect_rd <- paste0(pkg_dir, "/man/str_detect.Rd") %>%
    readr::read_lines(lazy = FALSE) %>%
    stringr::str_trim()
  detect_rd_ex_lines <- match("\\examples{", detect_rd) %>%
    list(c(match("}", detect_rd[seq(., length(detect_rd))]) + . - 1)) %>%
    do.call(seq, .)
  readr::write_lines(
    detect_rd[-detect_rd_ex_lines],
    paste0(pkg_dir, "/man/str_detect.Rd")
  )
  expect_equal(
    extract_examples("R/detect.R/",
      pkg_dir = pkg_dir,
      document = FALSE
    ),
    list()
  )
  extracted_examples <- extract_examples("detect", pkg_dir = pkg_dir)
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
})
