roxytest_addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  make_test_shell_fun(text, roxytest = TRUE)
}
