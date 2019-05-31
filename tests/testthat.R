library(testthat)
library(exampletestr)

get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    }
    if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    }
  }
  if (os == "osx") os <- "mac"
  tolower(os)
}

if (get_os() != "mac" || Sys.getenv("NOT_CRAN") == "true")
  test_check("exampletestr")
