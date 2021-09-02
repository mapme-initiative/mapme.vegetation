#' Compute a trend raster on a raster time series
#'
#' This function is used to calculate the estimate and a p-value of a trend
#' through a raster cube time series.
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
#' @param trend_function Defaults to NULL which means that \code{\link[trend]{mk.test}}
#'   with its standard parametrization is applied. Can be substituted by a function.
#'   This function is applied across the time dimension of the data cube and imputes
#'   NA's. The return vector must be of equal length to the time dimension, must
#'   return one row for each band in the data cube and must not include any NA's.
#'   See the example for the standard function which is applied when the \code{smoothing_function}
#'   is omitted.
#' @param filter_p Defaults to FALSE, that means pixels are not filtered based on
#'   the p-value. Can be set to a numeric value below 1 to mask keep all values
#'   with a p-value smaller \code{p}.
#' @param chunking Vector of length 3 defining the size of data cube chunks in the
#'   order time, y, x. (see \code{\link[gdalcubes]{raster_cube}})
#' @param threads Number of threads used to process data cubes
#'   (see \code{link[gdalcubes]{gdalcubes_options}})
#' @param outdir A character vector pointing to an existing directory where
#'  the output rasters are written to.
#' @param label A character vector which is used as a prefix to the output filenames.
#' @param verbose  A logical indicating the verbosity.

#'
#' @export trend_cube
#' @import gdalcubes
#' @import assertthat
#' @importFrom sf st_sfc st_crs st_bbox st_polygon st_transform
#' @importFrom trend mk.test
#' @importFrom stats lm
#' @author Darius Görgen (MapTailor Geospatial Consulting GbR) \email{info@maptailor.net}
#' \cr
#' \emph{Maintainer:} MAPME-Initiative \email{contact@mapme-initiative.org}
#' \cr
#' \emph{Contact Person:} Dr. Johannes Schielein
#' \cr
#' \emph{Copyright:} MAPME-Initiative
#' \cr
#' \emph{License:} GPL-3
trend_cube <- function(
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
  aggregation = "median",
  resampling = "bilinear",
  trend_function=NULL,
  filter_p = NULL,
  chunking = c(1,256,256),
  threads = 1,
  outdir  = ".",
  label = NULL,
  verbose = T){


  . = NULL
  # TODO: checking inputs
  if(!all(file.exists(files))) stop("Some files seam not to exist.")
  assert_that(is.dir(outdir))
  assert_that(is.number(dx))
  assert_that(is.number(dy))
  if(!is.null(filter_p)){
    assert_that(is.number(filter_p))
    assert_that(filter_p < 1, msg = "filter_p must be smaller 1")
  }
  assert_that(is.logical(verbose))
  .check_times(times)

  assert_that(aggregation %in% c("min", "max", "mean", "median", "first"),
              msg = paste0("Aggregation ", aggregation, " not a valid aggregation method."))
  assert_that(resampling %in% c("near", "bilinear", "cubic", "cubicspline",
                                "lanczos", "average", "rms", "mode", "max",
                                "min", "med", "q1", "q3", "sum"),
              msg = paste0("Aggregation ", resampling, " not a valid aggregation method."))

  # create image collection and cubeview
  gdalcubes_options(threads = threads, progress = verbose)
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

  ext = .update_ext(col, bbox, after = after, before = before)
  cubeview = cube_view(extent = ext,
                       srs = srs,
                       dx = dy,
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

  # set trend function
  if(is.null(trend_function)) {
    message("No smoothing function specified.")
    message("Using Mann-Kendall test with standard parameters.")

    trend_function = function(x){
      results = lapply(1:nrow(x), function(i) {
        y = x[i,]
        trend = trend::mk.test(y)
        time = seq_along(y)
        lmodel = stats::lm(y ~ time)
        c(as.numeric(lmodel$coefficients[2]), trend$p.value)
      })
      do.call(rbind, results)
    }
  }

  # create data cube
  cube = raster_cube(col, cubeview, chunking = chunking)

  # TODO: smarter way of handling multi-band cubes and pixel masking

  if(length(bands)>1){ # multi-band cube
    if(!is.null(filter_p)) { # p filter applied
      if(verbose){
        message("More than one band in raster cube and p-value filter is active.")
        message("Filtering each individual band and returning a single file per band.")
      }

      files = c()
      for(band in bands){
        out_name = paste(band, label, sep = "-")
        cube = raster_cube(col, cubeview, chunking = chunking)
        cube %>%
          select_bands(band) %>%
          reduce_time(names = c("est", "p"),
                      FUN = trend_function) %>%
          filter_pixel(., pred = paste0("p < ", filter_p)) %>%
          write_tif(dir = outdir, prefix = out_name) -> filename
        files[which(bands == band)] = filename
      }

    } else { # p filter not applied

      if(verbose) {
        message("More than one band in raster cube but p-value filter is not active.")
        message("Returning a single file containing all bands.")
      }
      out_bands = paste(c("est", "p"), rep(bands, each = 2), sep = "-")
      cube %>%
        reduce_time(names = out_bands,
                    FUN = trend_function) %>%
        write_tif(dir = outdir, prefix = label) -> files
    }

  } else { # single band cube

    if(!is.null(filter_p)) { # p filter applied

      cube %>%
        reduce_time(names = paste(c("est", "p"), bands, sep = "-"),
                    FUN = trend_function) %>%
        filter_pixel(., pred = paste0("p < ", filter_p)) %>%
        write_tif(dir = outdir, prefix = label) -> files
    } else { # p filter not applied
      cube %>%
        reduce_time(names = paste(c("est", "p"), bands, sep = "-"),
                    FUN = trend_function) %>%
        write_tif(dir = outdir, prefix = label) -> files

    }

  }

  files

}
