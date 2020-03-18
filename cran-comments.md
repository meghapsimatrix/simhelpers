## Test environments
* local OS X install, R 3.6.3
* ubuntu 14.04.5 LTS (on travis-ci), R 3.6.2, devel
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* Found the following hidden files and directories:
  .nojekyll.R
  These were most likely included in error. See section ‘Package
  structure’ in the ‘Writing R Extensions’ manual.

  I added that file on purpose to avoid conflicts with jekyll commands when building the pkgdown website for the package. 
