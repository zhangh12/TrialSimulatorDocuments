# Documents of `TrialSimulator` for Verification

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

`TrialSimulator` is an R package for conducting patient-level simulation of randomized clinical trial. 
Unit test is usually not sufficient to verify its correctness. Instead, we run patient-level simulation 
of massive replicates to compare empirical power, analysis time, expected number of events, etc. with 
their analytic values. This approach is computationally intensive and is infeasible to be included 
in the R package. We use this repository to host all testing reports, which are re-generated using 
the `TrialSimulator` package whenever a major update is available. 

## Usage

To preview a compiled report (`/docs/*.html`) as a rendered page, you can enter the URL of the 
HTML file on [this website](https://htmlpreview.github.io/). 

## Installation `TrialSimulator`

You can install the development version of `TrialSimulator` from [GitHub](https://github.com/) with:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github(
  "zhangh12/TrialSimulator", 
  build_manual = TRUE, 
  build_vignettes = TRUE, 
  force = TRUE
)
```
