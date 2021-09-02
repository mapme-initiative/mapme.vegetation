#' Function to fill gaps and smooth a time series of vegetation indices
#'
#'
#' This function is used to conduct a linear interpolation of missing values
#' in the time series of vegetation indices on a pixel basis. By default
#' a Savitzkiy-Golay filter is used to smooth the time series. A custom
#' function for the interpolation and smoothing can be specified. It can be used
#' on equally spaced data cubes in the time dimension where missing values, e.g.
#' through clouds, are an issue. The result of the function is a dense time series
#' where missing values are imputed and a smoothing function is applied to reduce
#' the noise in the signal.
#'
#' @param files A character vector with the filepaths to the raster files for which
#'   gap-filling and a smoothing function shall be applied.
#' @param bands A character vector indicating the names of the bands in each
#'   of the raster files specified in the \code{files} argument.
#'   (see \code{\link[gdalcubes]{create_image_collection}})
#' @param times A character vector indicating the date of each raster file specified
#'   in the \code{files} argument in the form of "%Y%m%d".
#'   (see \code{\link[gdalcubes]{create_image_collection}})
#' @param dx The spatial resolution of the outcome raster files in the \code{x} dimension
#'   in the units of the coordinate reference system specified witht the \code{srs} argument.
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param dy The spatial resolution of the outcome raster files in the \code{y} dimension
#'   in the units of the coordinate reference system specified witht the \code{srs} argument.
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param dt The temporal resolution of the outcome raster files.
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param srs Target spatial reference system as a string; can be a proj4 definition,
#'   WKT, or in the form "EPSG:XXXX" (see \code{\link[gdalcubes]{cube_view}})
#' @param after A length one character vector in the form "%Y-%m-%d" specifying
#'   the earliest date (inclusive) included in the temporal extent.
#' @param before A length one character vector in the form "%Y-%m-%d" specifying
#'   the latest date (inclusive) included in the temporal extent.
#' @param timeframe A character of either \code{"seasonal"} or \code{"complete"} specifying
#'   if the calculation should be applied for the complete time series or for
#'   each year independently. Defaults to \code{"complete"}. If \code{"seasonal"} is specified
#'   the \code{years} variables cannot be left empty.
#' @param bbox A numeric vector of lenght four indicating the spatial bounding
#'   box of the query (xmin, ymin, xmax, ymax) in geographic coordinates. Defaults
#'   to `NULL` which will set the spatial extent to global, i.e. \code{c(-180, -90, 180, 90)}.
#' @param aggregation aggregation method as string, defining how to deal with pixels
#'   containing data from multiple images, can be "min", "max", "mean", "median", or "first"
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param resampling resampling method used in gdalwarp when images are read, can be
#'   "near", "bilinear", "bicubic" or others as supported by gdalwarp
#'   (see https://gdal.org/programs/gdalwarp.html and \code{\link[gdalcubes]{cube_view}})
#' @param smoothing_function Defaults to NULL which means that a linear
#'   interpolation coupled with \code{\link[signal]{sgolayfilt}}` at its standard
#'   parametrization is applied. Can be substituted by a function.
#'   This function is applied across the time dimension of the data cube and imputes
#'   NA's. The return vector must be of equal length to the time dimension, must
#'   return one row for each band in the data cube and must not include any NA's.
#'   See the example for the standard function which is applied when the \code{smoothing_function}
#'   is omitted.
#' @param chunking Vector of length 3 defining the size of data cube chunks in the
#'   order time, y, x. (see \code{\link[gdalcubes]{raster_cube}})
#' @param threads Number of threads used to process data cubes
#'   (see \code{link[gdalcubes]{gdalcubes_options}})
#' @param outdir A character vector pointing to an existing directory where
#'  the output rasters are written to.
#' @param overwrite A logical indicating if existing files in outdir should be
#'   overwritten.
#' @param label A character vector which is used as a prefix to the output filenames.
#' @param n The ratio of pixels for which a custom \code{smoothing_function} is tested
#'   on randomly selected pixels to test if it is applicable. Defaults to \code{0.01}.
#' @param verbose  A logical indicating the verbosity.
#' @param ... additional arguments to  \code{\link[gdalcubes]{write_tif}}
#'
#' @export smooth_cube
#' @import assertthat
#' @importFrom stringr str_sub
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
#'
#' @examples
#' \dontrun{
#' smoothing_function = function(x){
#' results = lapply(1:nrow(x), function(i) {
#'  y = x[i,]
#'  lin = zoo::na.approx(y, na.rm = F, rule = 2)
#'  svg = signal::sgolayfilt(lin)
#'  svg
#'  })
#'do.call(rbind, results)
#' }
#' }
smooth_cube <- function(
  files = NULL,
  bands = NULL,
  times = NULL,
  dx = NULL,
  dy = NULL,
  dt = NULL,
  srs = NULL,
  after = NULL,
  before = NULL,
  timeframe = "full",
  bbox = NULL,
  aggregation = "mean",
  resampling = "bilinear",
  smoothing_function = NULL,
  chunking = c(1,256,256),
  threads = 1,
  outdir = ".",
  overwrite = F,
  label = "",
  n = 100,
  verbose = T,
  ...){


  # TODO: checking inputs
  assert_that(is.dir(outdir))
  assert_that(is.number(dx))
  assert_that(is.number(dy))
  # assert_that(is.integer(n))
  assert_that(is.logical(verbose))
  .check_times(times)

  if(!timeframe %in% c("seasonal", "full")){
    stop("Variable timeframe need to be one of seasonal or complete!")
  }


  st_crs(srs)
  assert_that(aggregation %in% c("min", "max", "mean", "median", "first"),
              msg = paste0("Aggregation ", aggregation, " not a valid aggregation method."))
  assert_that(resampling %in% c("near", "bilinear", "cubic", "cubicspline",
                                "lanczos", "average", "rms", "mode", "max",
                                "min", "med", "q1", "q3", "sum"),
              msg = paste0("Aggregation ", resampling, " not a valid aggregation method."))
  gdalcubes_options(threads = threads, progress = verbose)


  if(timeframe == "seasonal"){

    if(!is.list(files)) {
      stop("With seasonal timeframe files is expected to be a list.")
    }

    years = as.numeric(str_sub(c(after, before), 1, 4))
    years = as.character(seq(years[1], years[2]))
    dates = str_sub(c(after, before), 5, -1)

    out = lapply(years, function(year) {
      y_after = sprintf("%s%s", year, dates[1])
      y_before = sprintf("%s%s", year, dates[2])

      .calc_smooth(
        files = files[[year]]$files,
        times = files[[year]]$time,
        bands = files[[year]]$band_order,
        dx = dx,
        dy = dy,
        dt = dt,
        srs = srs,
        after = y_after,
        before = y_before,
        bbox = bbox,
        aggregation = aggregation,
        resampling = resampling,
        smoothing_function = smoothing_function,
        chunking = chunking,
        threads = threads,
        outdir = outdir,
        overwrite = overwrite,
        label = label,
        n = n,
        verbose = verbose,
        ...)

    })
    names(out) = years

  } else {

    out = .calc_smooth(
      files = files,
      times = times,
      bands = bands,
      dx = dx,
      dy = dy,
      dt = dt,
      srs = srs,
      after = after,
      before = before,
      bbox = bbox,
      aggregation = aggregation,
      resampling = resampling,
      smoothing_function = smoothing_function,
      chunking = chunking,
      threads = threads,
      outdir = outdir,
      overwrite = overwrite,
      label = label,
      n = n,
      verbose = verbose,
      ...)
  }
  out
}


#' Internal function to smooth a datacube
#'
#'
#' @param files A character vector with the filepaths to the raster files for which
#'   gap-filling and a smoothing function shall be applied.
#' @param bands A character vector indicating the names of the bands in each
#'   of the raster files specified in the \code{files} argument.
#'   (see \code{\link[gdalcubes]{create_image_collection}})
#' @param times A character vector indicating the date of each raster file specified
#'   in the \code{files} argument in the form of "%Y%m%d".
#'   (see \code{\link[gdalcubes]{create_image_collection}})
#' @param dx The spatial resolution of the outcome raster files in the \code{x} dimension
#'   in the units of the coordinate reference system specified witht the \code{srs} argument.
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param dy The spatial resolution of the outcome raster files in the \code{y} dimension
#'   in the units of the coordinate reference system specified witht the \code{srs} argument.
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param dt The temporal resolution of the outcome raster files.
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param srs Target spatial reference system as a string; can be a proj4 definition,
#'   WKT, or in the form "EPSG:XXXX" (see \code{\link[gdalcubes]{cube_view}})
#' @param after A length one character vector in the form "%Y-%m-%d" specifying
#'   the earliest date (inclusive) included in the temporal extent.
#' @param before A length one character vector in the form "%Y-%m-%d" specifying
#'   the latest date (inclusive) included in the temporal extent.
#' @param bbox A numeric vector of lenght four indicating the spatial bounding
#'   box of the query (xmin, ymin, xmax, ymax) in geographic coordinates. Defaults
#'   to `NULL` which will set the spatial extent to global, i.e. \code{c(-180, -90, 180, 90)}.
#' @param aggregation aggregation method as string, defining how to deal with pixels
#'   containing data from multiple images, can be "min", "max", "mean", "median", or "first"
#'   (see \code{\link[gdalcubes]{cube_view}})
#' @param resampling resampling method used in gdalwarp when images are read, can be
#'   "near", "bilinear", "bicubic" or others as supported by gdalwarp
#'   (see https://gdal.org/programs/gdalwarp.html and \code{\link[gdalcubes]{cube_view}})
#' @param smoothing_function Defaults to NULL which means that a linear
#'   interpolation coupled with \code{\link[signal]{sgolayfilt}}` at its standard
#'   parametrization is applied. Can be substituted by a function.
#'   This function is applied across the time dimension of the data cube and imputes
#'   NA's. The return vector must be of equal length to the time dimension, must
#'   return one row for each band in the data cube and must not include any NA's.
#'   See the example for the standard function which is applied when the \code{smoothing_function}
#'   is omitted.
#' @param chunking Vector of length 3 defining the size of data cube chunks in the
#'   order time, y, x. (see \code{\link[gdalcubes]{raster_cube}})
#' @param threads Number of threads used to process data cubes
#'   (see \code{link[gdalcubes]{gdalcubes_options}})
#' @param outdir A character vector pointing to an existing directory where
#'  the output rasters are written to.
#' @param overwrite A logical indicating if existing files in outdir should be
#'   overwritten.
#' @param label A character vector which is used as a prefix to the output filenames.
#' @param p The ratio of pixels for which a custom \code{smoothing_function} is tested
#'   on randomly selected pixels to test if it is applicable. Defaults to \code{0.01}.
#' @param verbose  A logical indicating the verbosity.
#' @param ... additional arguments to  \code{\link[gdalcubes]{write_tif}}
#'
#' @export .calc_smooth
#' @keywords internal
#' @importFrom signal sgolayfilt
#' @importFrom zoo na.approx
#' @import gdalcubes
#' @import assertthat
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3

.calc_smooth <- function(
  files = NULL,
  bands = NULL,
  times = NULL,
  dx = NULL,
  dy = NULL,
  dt = NULL,
  srs = NULL,
  after = NULL,
  before = NULL,
  bbox = NULL,
  aggregation = "mean",
  resampling = "bilinear",
  smoothing_function = NULL,
  chunking = c(1,256,256),
  threads = 1,
  outdir = ".",
  overwrite = F,
  label = "",
  n = 100,
  verbose = T,
  ...){


  col = create_image_collection(files, date_time = times, band_names = bands)

  if(is.null(bbox)){
    message("No bbox specified. Setting spatial extent to the extent of the image collection.")
    bbox = as.data.frame(matrix(data = as.numeric(gdalcubes::extent(col)[1:4]),
                                nrow = 2))
    bbox2 = matrix(data =
                     c(bbox[1,1], bbox[2,2],
                       bbox[2,1], bbox[2,2],
                       bbox[2,1], bbox[1,2],
                       bbox[1,1], bbox[1,2],
                       bbox[1,1], bbox[2,2]),
                   nrow = 5, byrow=T)
    bbox2 = st_sfc(st_polygon(list(bbox2)))
    sf::st_crs(bbox2) = sf::st_crs(4326)
    bbox2 = st_transform(bbox2, st_crs(srs))
    bbox = st_bbox(bbox2)
  }

  ext = .update_ext(col, bbox, after, before)

  cubeview = cube_view(extent = ext,
                       srs = srs,
                       dx = dx,
                       dy = dy,
                       dt = dt,
                       aggregation = aggregation,
                       resampling = resampling)

  tsteps = length(dimension_values(cubeview)$t)
  if(tsteps != chunking[[1]]){
    warning("Time dimension in chunking is different from time dimension of cube. Make sure that the appropriate size of time steps is availabe for the smoothing function.")
    Sys.sleep(5)
  }

  if(verbose){
    message("Current cube view is:")
    print(cubeview)
  }

  outnames = file.path(outdir, paste0(label, dimension_values(cubeview)$t, ".tif", sep = ""))

  if(any(file.exists(outnames)) & !overwrite){
    stop("Some output files exist in outdir and overwrite == FALSE")
  }

  if(is.null(smoothing_function)) {
    message("No smoothing function specified.")
    message("Using Linear Interpolation and Savitzkiy-Golay-Filter with standard parameters.")

    smoothing_function = function(x){
      results = lapply(1:nrow(x), function(i) {
        y = x[i,]
        lin = zoo::na.approx(y, na.rm = F, rule = 2)
        svg = signal::sgolayfilt(lin)
        svg
      })
      do.call(rbind, results)
    }
  }

  cube = raster_cube(col, cubeview, chunking = chunking)
  #.check_smoother(cube, smoothing_function, n = n)
  #if(verbose) message(sprintf("Smoother successfully checked on %s random pixels.", n))

  cube %>%
    apply_time(names = bands,
               FUN = smoothing_function) -> smoothed_cube

  band_order = bands(smoothed_cube)$name
  time_vec = dimension_values(smoothed_cube)$t
  if(nchar(time_vec[1]) == 7 )  time_vec = paste(time_vec, "-01", sep = "")

  # gdalcubes returns all existing files in directory
  smoothed_cube %>%
    write_tif(dir = outdir, prefix = label, ...)
  # smoothed_cube %>%
  #   write_ncdf(fname = file.path(outdir, paste0(label, ".nc")), with_VRT = TRUE, ...)
  list(files = outnames, band_order = band_order, time = time_vec)
}
