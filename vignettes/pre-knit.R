rmd_orig_files <- fs::dir_ls(
  path = usethis::proj_path("vignettes"),
  glob = "*.Rmd.orig"
)
rmd_out_files <- strex::str_before_last(rmd_orig_files, stringr::fixed(".orig"))
purrr::map2(
  rmd_orig_files, rmd_out_files,
  ~ knitr::knit(.x, output = .y)
)
