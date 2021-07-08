## Test environments

* Mac OS X (local) - R 4.0.3
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (r-hub) - R 4.1.0
* Fedora Linux, R-devel, clang, gfortran (r-hub) - R Under development (unstable)
* Windows development (win-builder) - R Under development (unstable)
* Windows release (win-builder) - R 4.1.0
* Windows previous major release R (win-builder) - R 4.0.5

## R CMD check results

0 ERRORs, O WARNINGs, 2 NOTEs:

> New submission

> Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1136/bmj.2.6051.1525
    From: inst/doc/af-impact.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.1136/jech.56.8.606
    From: inst/doc/af-impact.html
    Status: 403
    Message: Forbidden

Both of those URLs resolved when tested manually.

## Downstream dependencies

None.

