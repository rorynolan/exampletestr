## Test environments
* local OS X install, R 3.4.3
* ubuntu 12.04 (on travis-ci), R 3.4.2
* Windows Server 2012 (on appveyor), R 3.4.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Reverse Dependencies 
* There are no reverse dependencies.

## Fix
* This package was submitted to CRAN recently, but experienced warnings when the new R-devel came out (because the new devel doesn't like examples to change the working directory). This is a fix to address this new requirement in R-devel.

