# Rt visualization in D3

[![Travis build status](https://travis-ci.com/hamishgibbs/RtD3.svg?branch=master)](https://travis-ci.com/hamishgibbs/RtD3)
[![Codecov test coverage](https://codecov.io/gh/hamishgibbs/RtD3/branch/master/graph/badge.svg)](https://codecov.io/gh/hamishgibbs/RtD3?branch=master)
[![Documentation](https://img.shields.io/badge/Package-documentation-lightgrey.svg?style=flat)](https://hamishgibbs.github.io/RtD3/)

An R interface for generating interactive visualisations of rt estimates. Provides an interface for the JavaScript library [rt_vis](https://github.com/hamishgibbs/rt_vis). This package was designed using rt estimates generated by the [EpiNow2](https://epiforecasts.io/EpiNow2/) package, an open source resource for creating rt estimates.

## Installation

Install the development version of this package with:

``` r
remotes::install_github("hamishgibbs/RtD3")
```

## Quickstart

Estimates are available from [epiforecasts](https://epiforecasts.io/) [covid-rt-estimates](https://github.com/epiforecasts/covid-rt-estimates) in the format expected by this package. Use the helper function `readInEpiNow2` to generate the summary widget with existing estimates.

``` r

rtData <- list("Cases" = readInEpiNow2(
    path = "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary",
    region_var = "country"))

summaryWidget(rtData = rtData)

```

Optionally, a map can be included using data from the `rnaturalearth` package.

``` r

geoData = rnaturalearth::ne_countries(returnclass = 'sf')

summaryWidget(geoData = geoData, rtData = rtData)

```

## Development

Comments and contributions to this package are welcome. To record a problem with the package, please [create an issue](https://github.com/hamishgibbs/RtD3/issues/new) on [Github](https://github.com/hamishgibbs/RtD3). 

