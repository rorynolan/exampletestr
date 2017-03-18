test_that("extract_examples works", {
  if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
  "'tempkg' folder.")
  expect_equal(dir.create("tempkg"), )
  expect_equal(devtools::create("tempkg"), )
  expect_equal(setwd("tempkg"), )
  expect_equal(file.copy(system.file("extdata", "exemplar.R", package = "exampletestr"), "R"), )
  expect_equal(extract_examples("exemplar"), )
  expect_equal(extract_examples("exemplar"), )
  expect_equal(setwd(".."), )
  expect_equal(filesstrings::RemoveDirs("tempkg"), )
})

test_that("make_test_shell works", {
  if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
  "'tempkg' folder.")
  expect_equal(dir.create("tempkg"), )
  expect_equal(devtools::create("tempkg"), )
  expect_equal(setwd("tempkg"), )
  expect_equal(file.copy(system.file("extdata", "exemplar.R", package = "exampletestr"),
  "R", overwrite = TRUE), )
  expect_equal(make_test_shell(extract_examples("exemplar")[[1]]), )
  expect_equal(setwd(".."), )
  expect_equal(filesstrings::RemoveDirs("tempkg"), )
})

test_that("make_tests_shells_file works", {
  if (dir.exists("tempkg")) warning("Do not proceed, you'll mess with your ",
  "'tempkg' folder.")
  expect_equal(dir.create("tempkg"), )
  expect_equal(devtools::create("tempkg"), )
  expect_equal(setwd("tempkg"), )
  expect_equal(devtools::use_testthat(), )
  expect_equal(file.copy(system.file("extdata", "exemplar.R", package = "exampletestr"),
  "R", overwrite = TRUE), )
  expect_equal(make_tests_shells_file("exemplar"), )
  expect_equal(file.copy(system.file("extdata", "exampletestr.R", package = "exampletestr"),
  "R", overwrite = TRUE), )
  expect_equal(make_tests_shells_pkg(overwrite = TRUE), )
  # Now check your tempkg/tests/testthat directory to see what they look like
  # The next two lines clean up
  expect_equal(setwd(".."), )
  expect_equal(filesstrings::RemoveDirs("tempkg"), )
})
