## Resubmission

This is a resubmission. This is a maintenance release which removes import of a defunct function from the furrr package. 

## Test environments

* local Windows 11 Pro, R 4.2.0
* local OS Big Sur, R 4.1.2
* ubuntu 20.04.3 LTS (on Github), R devel, release, oldrelease
* macOS-latest (on Github), R release
* windows-latest (on Github), R release
* win-builder (devel, release, oldrelease)
* macOS builder:  macOS 11.5.2, Apple M1 (release)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE:

* Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1002/jrsm.5
    From: inst/doc/visualization.html
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1037/met0000011
    From: inst/doc/visualization.html
    Status: 400
    Message: Bad Request
  URL: https://doi.org/10.3102/1076998615606099
    From: inst/doc/visualization.html
    Status: 503
    Message: Service Unavailable
  URL: https://journals.sagepub.com/doi/10.3102/1076998615606099
    From: man/Tipton_Pusto.Rd
    Status: 503
    Message: Service Unavailable

  The flagged URLs are correct.
