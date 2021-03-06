% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legend_sequential.R
\name{legend_sequential}
\alias{legend_sequential}
\title{legend_sequential}
\usage{
legend_sequential(
  variable_name,
  legend_scale = "scaleLinear",
  color_low = "white",
  color_high = "green",
  color_no_data = "lightgrey"
)
}
\arguments{
\item{variable_name}{string, name of the variable of this legend.}

\item{legend_scale}{string, type of legend scale, must be the name of a d3 scale. Default: "scaleLinear".}

\item{color_low}{string, color for the lowest value of the legend.}

\item{color_high}{string, color for the highest value of the legend.}

\item{color_no_data}{string, color for no data entries.}
}
\description{
Create a qualitative legend. `legend_scale`` accepts scales listed in \href{https://github.com/d3/d3-scale#continuous-scales}{d3-scale}.
}
