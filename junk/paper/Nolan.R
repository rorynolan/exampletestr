required_pkgs <- c("tidyverse", "stringr", "lubridate", "rvest", "RCurl")
lapply(required_pkgs, library, character.only = TRUE)
check_for_tests <- function(pkg_name) {
  rdrr_address <- paste0("https://rdrr.io/cran/", pkg_name)
  if (url.exists(rdrr_address)) {
    rdrr_page_text <- rdrr_address %>% read_html %>% html_text
    trimmed_lines <- str_split(rdrr_page_text, "\n") %>% unlist %>% str_trim()
    trimmed_lines %>% str_detect("^tests/.+") %>% any
  } else {
    NA  # allow for not found
  }
}

download.file("http://cran.R-project.org/web/packages/packages.rds",
              "packages.rds", mode = "wb")
cran_packages_info <- readRDS("packages.rds")
cran_packages <- cran_packages_info[, "Package"]
post2015 <- ymd(cran_packages_info[, "Published"]) >= ymd("2016-01-01")
new_packages <- cran_packages[post2015]

tested_check <- cran_packages %>%
  map_lgl(check_for_tests) %>%
  set_names(cran_packages) %>%
  na.omit  # allow for not found on rdrr
length(tested_check)
#> [1] 10464
sum(tested_check)
#> [1] 2622
new_packages <- intersect(new_packages,
                          names(tested_check))  # allow for not found on rdrr
length(new_packages)
#> [1] 6013
sum(tested_check[new_packages])
#> [1] 2024

unit_test_packages <- c("testthat", "RUnit", "svUnit", "unitizer")
reverse_deps <- tools:::package_dependencies(packages = unit_test_packages,
                                             cran_packages_info, recursive=FALSE, reverse=TRUE,
                                             which = c("Depends","Imports","LinkingTo", "Suggests"))
map_int(reverse_deps, length)
#> testthat    RUnit   svUnit unitizer
#>     1938      133       11        0

str_detect_examples <- c('fruit <- c("apple", "banana", "pear", "pinapple")',
                         'str_detect(fruit, "a")',
                         'str_detect(fruit, "^a")',
                         'str_detect(fruit, "a$")',
                         'str_detect(fruit, "b")',
                         'str_detect(fruit, "[aeiou]")',
                         '# Also vectorised over pattern',
                         'str_detect("aecfg", letters)')
exampletestr::make_test_shell(str_detect_examples, "str_detect works") %>%
  cat(sep = "\n")
#> test_that("str_detect works", {
#>   fruit <- c("apple", "banana", "pear", "pinapple")
#>   expect_equal(str_detect(fruit, "a"), )
#>   expect_equal(str_detect(fruit, "^a"), )
#>   expect_equal(str_detect(fruit, "a$"), )
#>   expect_equal(str_detect(fruit, "b"), )
#>   expect_equal(str_detect(fruit, "[aeiou]"), )
#>   expect_equal(str_detect("aecfg", letters), )
#> })