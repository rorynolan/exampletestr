required_pkgs <- c("tidyverse", "stringr", "lubridate", "rvest", "RCurl")
lapply(required_pkgs, library, character.only = TRUE)

download.file("http://cran.R-project.org/web/packages/packages.rds",
              "packages.rds", mode = "wb")
cran_packages_info <- readRDS("packages.rds")
cran_packages <- cran_packages_info[, "Package"]
length(cran_packages)
post2015 <- ymd(cran_packages_info[, "Published"]) >= ymd("2016-01-01")
new_packages <- cran_packages[post2015]
length(new_packages)

check_for_tests <- function(pkg_name) {
  rdrr_address <- paste0("https://rdrr.io/cran/", pkg_name)
  if (url.exists(rdrr_address)) {
    rdrr_page_text <- rdrr_address %>% read_html %>% html_text
    trimmed_lines <- str_split(rdrr_page_text, "\n") %>% unlist %>% str_trim()
    trimmed_lines %>% str_detect("^tests/") %>% any
  } else {
    NA
  }
}

are_tested <- sapply(cran_packages, check_for_tests)
sum(are_tested, na.rm = TRUE)
sum(are_tested[new_packages], na.rm = TRUE)

unit_test_packages <- c("testthat", "RUnit", "svUnit", "unitizer")
reverse_deps <- tools:::package_dependencies(packages = unit_test_packages,
                  cran_packages_info, recursive=FALSE, reverse=TRUE,
                  which = c("Depends","Imports","LinkingTo", "Suggests"))
map_int(reverse_deps, length)
