% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math_stat_analysis.R
\name{wbt_raster_calculator}
\alias{wbt_raster_calculator}
\title{Raster calculator}
\usage{
wbt_raster_calculator(
  output,
  statement = "",
  wd = NULL,
  verbose_mode = FALSE,
  compress_rasters = FALSE
)
}
\arguments{
\item{output}{Name of the output raster file.}

\item{statement}{Statement e.g. cos("raster1") * 35.0 + "raster2". This statement must be a valid Rust statement.}

\item{wd}{Changes the working directory.}

\item{verbose_mode}{Sets verbose mode. If verbose mode is False, tools will not print output messages.}

\item{compress_rasters}{Sets the flag used by WhiteboxTools to determine whether to use compression for output rasters.}
}
\value{
Returns the tool text outputs.
}
\description{
This tool performs a conditional evaluaton (if-then-else) operation on a raster.
}