## `exampletestr` 1.5.0

## REFACTORING
* Move away from `rprojroot` to `usethis_proj_*`.


## `exampletestr` 1.4.2

#### BUG FIXES
* Require necessary version of glue.


## `exampletestr` 1.4.1

#### BUG FIXES
* Some calls to `tempdir()` needed to be `tempdir(check = TRUE)`.
    - This necessitates a dependency on R >= 3.5.0 (this is when the `check` option appeared in `tempdir()`).


## `exampletestr` 1.4.0

#### NEW FEATURES
* A `pkgdown` site at https://rorynolan.github.io/exampletestr/index.html.
* More vignettes.

#### MINOR IMPROVEMENTS
* Better messages and warnings, in the style of the `usethis` package.

#### BUG FIXES
* Now depending appropriately on `usethis` instead of relying on `devtools` for things that are now implemented in `usethis`.


## `exampletestr` 1.3.1

#### BUG FIXES
* Enforce bug-fixed new versions of `styler` and `filesstrings`.


## `exampletestr` 1.3.0

#### NEW FEATURES
* The naming of created files is now less likely to cause conflicts. Now each file created ends with "examples".
* There's a new function `make_test_shell_fun()` for making test shells one function at a time. This is thanks to a suggestion by Lorenz Walthert: <https://github.com/rorynolan/exampletestr/issues/6>.
* Created files are now optionally opened in the editor when they are created.
* The use of file paths is now more stable thanks to the `rprojroot` package.


## `exampletestr` 1.2.0

#### NEW FEATURES
* `exampletestr` now names its test files in the same way as `usethis::use_test()`.


## `exampletestr` 1.1.1

#### BUG FIXES
* The new R doesn't like it when the working directory is changed by running examples. This required a fix which this patch provides.


## `exampletestr` 1.1.0

#### MINOR IMPROVEMENTS
* Add `testthat::context()` to test shells.
* Update for `filesstrings` v2.0.0.

#### BUG FIXES
* Fix issues with open text connections.


## `exampletestr` 1.0.1

#### BUG FIXES
* The CITATION is now correct.


## `exampletestr` 1.0.0

* The package has now passed peer review.


## `exampletestr` 0.5.0

#### MINOR IMPROVEMENTS
* Minor documentation improvement.
* There's no longer an error telling you to run `usethis::use_testthat()`, now this is just done for you.


## `exampletestr` 0.4.2

#### MINOR IMPROVEMENTS
* Add explicit LICENSE file to github.
* Publish to Wellcome Open Research.


## `exampletestr` 0.4.1

#### BUG FIXES
* Fix to an error in DESCRIPTION.

#### MINOR IMPROVEMENTS
* Minor documentation improvement.


## `exampletestr` 0.4.0

#### NEW FEATURES
* There is now the option to not put stuff in `expect_equal()` statements by default.

#### MINOR IMPROVEMENTS
* The package now uses .Rd files to read the examples rather than `roxygen2` tags.
* Comments in examples are handled better.


## `exampletestr` 0.3.1

#### BUG FIXES
* Minor fix to vignette.

## `exampletestr` 0.3.0
* First CRAN-worthy version.
