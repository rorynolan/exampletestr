test_that("extract_examples works", {
  setwd(tempdir())
  if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
  "'tempkg' folder.")
  expect_true(dir.create("tempkg"))
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "exemplar.R",
                                    package = "exampletestr"),
                        "R"))
  expect_true(file.copy(system.file("extdata", "extract_examples_error1.R",
                                    package = "exampletestr"),
                        "R"))
  expect_true(file.copy(system.file("extdata", "extract_examples_error2.R",
                                    package = "exampletestr"),
                        "R"))
  expect_true(file.copy(system.file("extdata", "extract_examples_error3.R",
                                    package = "exampletestr"),
                        "R"))
  expect_equal(extract_examples("exemplar")$extract_examples, c(
    paste("if (dir.exists(\"tempkg\"))",
          "warning(\"Do not proceed, you'll mess with your \","),
    "\"'tempkg' folder.\")",
    "dir.create(\"tempkg\")",
    "devtools::create(\"tempkg\")",
    "setwd(\"tempkg\")",
    paste("file.copy(system.file(\"extdata\",",
          "\"exemplar.R\", package = \"exampletestr\"), \"R\")"),
    "extract_examples(\"exemplar\")",
    "extract_examples(\"exemplar\")",
    "setwd(\"..\")",
    "filesstrings::RemoveDirs(\"tempkg\")"
  ))
  expect_equal(extract_examples("exemplar")$make_test_shell, c(
    paste("if (dir.exists(\"tempkg\"))",
          "warning(\"Do not proceed, you'll mess with your \","),
    "\"'tempkg' folder.\")",
    "dir.create(\"tempkg\")",
    "devtools::create(\"tempkg\")",
    "setwd(\"tempkg\")",
    paste("file.copy(system.file(\"extdata\",",
          "\"exemplar.R\", package = \"exampletestr\"),"),
    "\"R\", overwrite = TRUE)",
    "make_test_shell(extract_examples(\"exemplar\")[[1]])",
    "setwd(\"..\")",
    "filesstrings::RemoveDirs(\"tempkg\")"
  ))
  expect_equal(extract_examples("exemplar")$make_tests_shells_file, c(
    paste("if (dir.exists(\"tempkg\"))",
          "warning(\"Do not proceed, you'll mess with your \","),
    "\"'tempkg' folder.\")",
    "dir.create(\"tempkg\")",
    "devtools::create(\"tempkg\")",
    "setwd(\"tempkg\")",
    "devtools::use_testthat()",
    paste("file.copy(system.file(\"extdata\",",
          "\"exemplar.R\", package = \"exampletestr\"),"),
    "\"R\", overwrite = TRUE)",
    "make_tests_shells_file(\"exemplar\")",
    paste("file.copy(system.file(\"extdata\",",
          "\"exampletestr.R\", package = \"exampletestr\"),"),
    "\"R\", overwrite = TRUE)",
    "make_tests_shells_pkg(overwrite = TRUE)",
    paste("# Now check your tempkg/tests/testthat",
          "directory to see what they look like"),
    "# The next two lines clean up",
    "setwd(\"..\")",
    "filesstrings::RemoveDirs(\"tempkg\")"
  ))
  expect_error(extract_examples("extract_examples_error1.R"))
  expect_error(extract_examples("extract_examples_error2.R"))
  expect_error(extract_examples("extract_examples_error3.R"))
  setwd("..")
  expect_true(filesstrings::RemoveDirs("tempkg"))
})

test_that("make_test_shell works", {
  setwd(tempdir())
  if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
                                    "'tempkg' folder.")
  expect_true(dir.create("tempkg"))
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "exemplar.R",
                                    package = "exampletestr"),
                        "R"))
  expect_equal(make_test_shell(extract_examples("exemplar")[[1]], "abc"), c(
    "test_that(\"abc\", {",
    paste("  if (dir.exists(\"tempkg\"))",
          "warning(\"Do not proceed, you'll mess with your \","),
    "  \"'tempkg' folder.\")",
    "  expect_equal(dir.create(\"tempkg\"), )",
    "  expect_equal(devtools::create(\"tempkg\"), )",
    "  expect_equal(setwd(\"tempkg\"), )",
    paste("  expect_equal(file.copy(system.file(\"extdata\",",
          "\"exemplar.R\", package = \"exampletestr\"), \"R\"), )"),
    "  expect_equal(extract_examples(\"exemplar\"), )",
    "  expect_equal(extract_examples(\"exemplar\"), )",
    "  expect_equal(setwd(\"..\"), )",
    "  expect_equal(filesstrings::RemoveDirs(\"tempkg\"), )",
    "})"
  ))
  setwd("..")
  expect_true(filesstrings::RemoveDirs("tempkg"))
})

test_that("make_tests_shells_file works", {
  setwd(tempdir())
  if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
                                    "'tempkg' folder.")
  expect_true(dir.create("tempkg"))
  expect_error(make_tests_shells_file("exemplar", pkg_dir = "tempkg"))
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "exemplar.R",
                                    package = "exampletestr"),
                        "R"))
  expect_true(devtools::use_testthat())
  expect_equal(make_tests_shells_file("exemplar"),
               readLines(system.file("extdata", "test_exemplar.R",
                                     package = "exampletestr")))
  expect_equal(make_tests_shells_pkg(overwrite = TRUE)[[1]],
               readLines(system.file("extdata", "test_exemplar.R",
                                     package = "exampletestr")))
  file.copy(system.file("extdata", "exampletestr.R", package = "exampletestr"),
            "R", overwrite = TRUE)
  expect_equal(make_tests_shells_pkg(overwrite = TRUE),
               lapply(c("exampletestr", "exemplar"), make_tests_shells_file,
                      overwrite = TRUE))
  expect_error(make_tests_shells_file("exemplar"))
  # Now check your tempkg/tests/testthat directory to see what they look like
  # The next two lines clean up
  setwd("..")
  expect_true(filesstrings::RemoveDirs("tempkg"))
})
