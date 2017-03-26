test_that("extract_examples works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "exemplar.R",
                                    package = "exampletestr"),
                        "R"))
  suppressWarnings(devtools::document())
  expect_equal(extract_examples("exemplar")$extract_examples, c(
    "### Name: extract_examples",
    "### Title: Extract examples lines from the functions in a .R file of a",
    "###   package.",
    "### Aliases: extract_examples",
    "",
    "### ** Examples",
    "",
    "setwd(tempdir())",
    "devtools::create(\"tempkg\")",
    "setwd(\"tempkg\")",
    "file.copy(system.file(\"extdata\", c(\"exemplar.R\", \"exampletestr.R\"),",
    "                      package = \"exampletestr\"), \"R\")",
    "devtools::document()",
    "exampletestr::extract_examples(\"exemplar\")",
    "exampletestr::extract_examples(\"exemplar\")",
    "setwd(\"..\")",
    "filesstrings::RemoveDirs(\"tempkg\")",
    "## Not run: ",
    "##D extract_examples(\"non_existent_file\")",
    "## End(Not run)",
    "",
    "",
    "",
    ""
  ))
  expect_equal(extract_examples("exemplar")$make_test_shell, c(
    "### Name: make_test_shell",
    "### Title: Make the shell of a 'test_that' test.",
    "### Aliases: make_test_shell",
    "",
    "### ** Examples",
    "",
    "setwd(tempdir())",
    "devtools::create(\"tempkg\")",
    "setwd(\"tempkg\")",
    "file.copy(system.file(\"extdata\", c(\"exemplar.R\", \"exampletestr.R\"),",
    "                      package = \"exampletestr\"), \"R\")",
    "devtools::document()",
    "exampletestr::make_test_shell(exampletestr::extract_examples(\"exemplar\")[[1]])",
    "exampletestr::make_test_shell(exampletestr::extract_examples(\"exemplar\")[[1]],",
    "                              desc = \"xyz\", e_e = FALSE)",
    "setwd(\"..\")",
    "filesstrings::RemoveDirs(\"tempkg\")",
    "",
    "",
    "",
    ""
  ))
  expect_equal(extract_examples("exemplar")$make_tests_shells_file, c(
    "### Name: make_tests_shells_file",
    "### Title: Create the shell of a test file.",
    "### Aliases: make_tests_shells_file make_tests_shells_pkg",
    "",
    "### ** Examples",
    "",
    "setwd(tempdir())",
    "devtools::create(\"tempkg\")",
    "setwd(\"tempkg\")",
    "devtools::use_testthat()",
    "file.copy(system.file(\"extdata\", c(\"exemplar.R\", \"exampletestr.R\"),",
    "                      package = \"exampletestr\"), \"R\")",
    "devtools::document()",
    "exampletestr::make_tests_shells_file(\"exemplar\")",
    "devtools::document()",
    "exampletestr::make_tests_shells_pkg(overwrite = TRUE)",
    "setwd(\"..\")",
    "filesstrings::RemoveDirs(\"tempkg\")",
    "",
    "",
    "",
    ""
  ))
  expect_equal(extract_examples("exemplar")$extract_examples,
               extract_examples("R/exemplar")$extract_examples)
  setwd("..")
  expect_true(filesstrings::RemoveDirs("tempkg"))
})

test_that("make_test_shell works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_true(dir.create("tempkg"))
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "exemplar.R",
                                    package = "exampletestr"),
                        "R"))
  devtools::document()
  expect_equal(make_test_shell(extract_examples("exemplar")[[1]], "abc"), c(
    "test_that(\"abc\", {",
    "  setwd(tempdir())",
    "  expect_equal(devtools::create(\"tempkg\"), )",
    "  setwd(\"tempkg\")",
    "  expect_equal(file.copy(system.file(\"extdata\", c(\"exemplar.R\", \"exampletestr.R\"),",
    "    package = \"exampletestr\"), \"R\"), )",
    "  expect_equal(devtools::document(), )",
    "  expect_equal(exampletestr::extract_examples(\"exemplar\"), )",
    "  expect_equal(exampletestr::extract_examples(\"exemplar\"), )",
    "  setwd(\"..\")",
    "  expect_equal(filesstrings::RemoveDirs(\"tempkg\"), )",
    "})"
  ))
  expect_equal(make_test_shell(extract_examples("exemplar")[[1]], "abc",
                               e_e = FALSE),
               c(
    "test_that(\"abc\", {",
    "  setwd(tempdir())",
    "  devtools::create(\"tempkg\")",
    "  setwd(\"tempkg\")",
    "  file.copy(system.file(\"extdata\", c(\"exemplar.R\", \"exampletestr.R\"),",
    "    package = \"exampletestr\"), \"R\")",
    "  devtools::document()",
    "  exampletestr::extract_examples(\"exemplar\")",
    "  exampletestr::extract_examples(\"exemplar\")",
    "  setwd(\"..\")",
    "  filesstrings::RemoveDirs(\"tempkg\")",
    "})"
  ))
  setwd("..")
  expect_true(filesstrings::RemoveDirs("tempkg"))
})

test_that("make_tests_shells_file works", {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(tempdir())
  expect_error(make_tests_shells_file("exemplar", pkg_dir = "tempkg"))
  expect_true(devtools::create("tempkg"))
  setwd("tempkg")
  expect_true(file.copy(system.file("extdata", "exemplar.R",
                                    package = "exampletestr"),
                        "R"))
  expect_true(file.copy(system.file("extdata", "non_documented_fun.R",
                                    package = "exampletestr"),
                        "R"))
  expect_error(make_tests_shells_file("exemplar"))
  expect_true(devtools::use_testthat())
  devtools::document()
  expect_equal(make_tests_shells_file("exemplar"),
               readLines(system.file("extdata", "test_exemplar_uncompleted.R",
                                     package = "exampletestr")))
  expect_equal(make_tests_shells_pkg(overwrite = TRUE)[[1]],
               readLines(system.file("extdata", "test_exemplar_uncompleted.R",
                                     package = "exampletestr")))
  file.copy(system.file("extdata", "exampletestr.R", package = "exampletestr"),
            "R", overwrite = TRUE)
  expect_equal(make_tests_shells_pkg(overwrite = TRUE),
               lapply(c("exampletestr", "exemplar",
                        "non_documented_fun"), make_tests_shells_file,
                      overwrite = TRUE))
  expect_error(make_tests_shells_file("exemplar"))
  # Now check your tempkg/tests/testthat directory to see what they look like
  # The next two lines clean up
  setwd("..")
  expect_true(filesstrings::RemoveDirs("tempkg"))
})
