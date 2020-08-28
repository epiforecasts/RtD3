# RtD3
Rt visualisation in D3

[![Travis build status](https://travis-ci.com/hamishgibbs/RtD3.svg?branch=master)](https://travis-ci.com/hamishgibbs/RtD3)
[![Codecov test coverage](https://codecov.io/gh/hamishgibbs/RtD3/branch/master/graph/badge.svg)](https://codecov.io/gh/hamishgibbs/RtD3?branch=master)

An R interface for the js library [rt_vis](https://github.com/hamishgibbs/rt_vis).

*Please note: this package is in the early stages of development.*

## Example usage

``` {r}
geoData = sf::st_read('https://raw.githubusercontent.com/hamishgibbs/rt_interactive_vis/master/geo_data/world.geojson')
summaryData = readr::read_csv('https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/summary_table.csv')
r0Data = readr::read_csv('https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/rt.csv')
casesInfectionData = readr::read_csv('https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/cases_by_infection.csv')
casesReportData = readr::read_csv('https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/cases_by_report.csv')
obsCasesData = readr::read_csv('https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/national/cases/summary/reported_cases.csv')

RtD3::RtD3(
  geoData = geoData,
  summaryData = summaryData,
  r0Data = r0Data,
  casesInfectionData = casesInfectionData,
  casesReportData = casesReportData,
  obsCasesData = obsCasesData
)
```
