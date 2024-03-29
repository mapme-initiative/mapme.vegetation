% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buffer_scl.R
\name{.scl}
\alias{.scl}
\title{Helper function to apply SCL cloud mask buffer}
\usage{
.scl(scl_file, outdir, mask_values, mask_buffer, grass_bin)
}
\arguments{
\item{scl_file}{A chrachter vector indicating the path to the input SCL file}

\item{outdir}{A path to a directory where the output file will be written to.}

\item{mask_values}{A numeric vector indicating which values should be masked}

\item{mask_buffer}{The distance for the buffer in map units (typically in meters).}

\item{grass_bin}{A path to the GRASS GIS binary directory.}
}
\value{
Nothing. Writes buffered SCL mask to outdir. All cells within the
  buffer will obtain the value of the first element in mask_values.
}
\description{
This function is used internally to buffer a given SCL cloud mask. All buffered
cells are set to the value of the first object in mask_values
}
\author{
Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
\cr
\emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
\cr
\emph{Contact Person:} Dr. Johannes Schielein
\cr
\emph{Copyright:} MAPME-Initiative
\cr
\emph{License:} GPL-3
}
\keyword{internal}
