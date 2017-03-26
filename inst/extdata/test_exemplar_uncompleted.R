test_that("extract_examples works", {
  setwd(tempdir())
  expect_equal(devtools::create("tempkg"), )
  setwd("tempkg")
  expect_equal(file.copy(system.file("extdata", c("exemplar.R", "exampletestr.R"),
    package = "exampletestr"), "R"), )
  expect_equal(devtools::document(), )
  expect_equal(exampletestr::extract_examples("exemplar"), )
  expect_equal(exampletestr::extract_examples("exemplar"), )
  setwd("..")
  expect_equal(filesstrings::RemoveDirs("tempkg"), )
})

test_that("make_test_shell works", {
  setwd(tempdir())
  expect_equal(devtools::create("tempkg"), )
  setwd("tempkg")
  expect_equal(file.copy(system.file("extdata", c("exemplar.R", "exampletestr.R"),
    package = "exampletestr"), "R"), )
  expect_equal(devtools::document(), )
  expect_equal(exampletestr::make_test_shell(exampletestr::extract_examples("exemplar")[[1]]), )
  expect_equal(exampletestr::make_test_shell(exampletestr::extract_examples("exemplar")[[1]],
    desc = "xyz", e_e = FALSE), )
  setwd("..")
  expect_equal(filesstrings::RemoveDirs("tempkg"), )
})

test_that("make_tests_shells_file works", {
  setwd(tempdir())
  expect_equal(devtools::create("tempkg"), )
  setwd("tempkg")
  expect_equal(devtools::use_testthat(), )
  expect_equal(file.copy(system.file("extdata", c("exemplar.R", "exampletestr.R"),
    package = "exampletestr"), "R"), )
  expect_equal(devtools::document(), )
  expect_equal(exampletestr::make_tests_shells_file("exemplar"), )
  expect_equal(devtools::document(), )
  expect_equal(exampletestr::make_tests_shells_pkg(overwrite = TRUE), )
  setwd("..")
  expect_equal(filesstrings::RemoveDirs("tempkg"), )
})
